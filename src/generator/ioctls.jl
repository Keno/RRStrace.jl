using Cxx
CCompiler = Cxx.new_clang_instance(false, true)

cxx"""
#include "clang/AST/RecordLayout.h"
"""

# Iteration for ArrayRef
import Base: start, next, length, done
const ArrayRef = cxxt"llvm::ArrayRef<$T>" where T
start(AR::ArrayRef) = 0
function next(AR::cxxt"llvm::ArrayRef<$T>", i) where T
    (icxx"""
        // Force a copy, otherwise we'll retain reference semantics in julia
        // which is not what people expect.
        $T element = ($AR)[$i];
        return element;
    """, i+1)
end
length(AR::ArrayRef) = icxx"$AR.size();"
done(AR::ArrayRef, i) = i >= length(AR)

# Working with clang::IdentifierInfo
Base.String(II::pcpp"clang::IdentifierInfo") = unsafe_string(icxx"$II->getName().str();")

# Printing for tokens
function Base.String(Tok::cxxt"clang::Token")
    II = icxx"$Tok.getIdentifierInfo();"
    @assert II != C_NULL
    String(II)
end
tok_is_identifier(Tok) = icxx"$Tok.is(clang::tok::identifier);"
tok_is_comma(Tok) = icxx"$Tok.is(clang::tok::comma);"
tok_is_numeric(Tok) = icxx"$Tok.is(clang::tok::numeric_constant);"
getSpelling(PP, Tok) = unsafe_string(icxx"$PP->getSpelling($Tok);")
function Base.show(io::IO, Tok::cxxt"clang::Token")
    print(io, unsafe_string(icxx"clang::tok::getTokenName($Tok.getKind());"))
    if haskey(io, :preprocessor)
        PP = get(io, :preprocessor, nothing)
        print(io, " '", getSpelling(PP, Tok), "'")
    elseif tok_is_identifier(Tok)
        print(io, " '", String(Tok), "'")
    end
end

# Iteration over IdentifierTable
start(tab::rcpp"clang::IdentifierTable") = icxx"$tab.begin();"
next(tab::rcpp"clang::IdentifierTable", it) = (icxx"$it->second;", icxx"++$it;")
done(tab::rcpp"clang::IdentifierTable", it) = icxx"$it == $tab.end();"
length(tab::rcpp"clang::IdentifierTable") = icxx"$tab.size();"

function AdvanceIfEof(P)
  icxx"""
  if ($P->getPreprocessor().isIncrementalProcessingEnabled() &&
    $P->getCurToken().is(clang::tok::eof))
      $P->ConsumeToken();
  """
end

# Parsing
function EnterTokenStream(PP::pcpp"clang::Preprocessor", tokens::Vector{cxxt"clang::Token"})
  # Vector memory layout is incompatible, convert to clang::Token**
  toks = typeof(tokens[1].data)[x.data for x in tokens]
  icxx"$PP->EnterTokenStream(llvm::ArrayRef<clang::Token>{
    (clang::Token*)$(pointer(toks)),
    (size_t)$(length(toks))
  },false);"
end
function ParseTypeName(P::pcpp"clang::Parser")
  AdvanceIfEof(P)
  res = icxx"$P->ParseTypeName(nullptr, clang::Declarator::TypeNameContext);"
  !icxx"$res.isUsable();" && error("Parsing failed")
  Cxx.QualType(icxx"clang::Sema::GetTypeFromParser($res.get());")
end

PP = icxx"&$(Cxx.active_instances[2].CI)->getPreprocessor();"
P  = Cxx.active_instances[2].Parser

Cxx.cxxinclude(CCompiler, "linux/termios.h")
Cxx.cxxinclude(CCompiler, "linux/usbdevice_fs.h")
map(String, Iterators.filter(II->icxx"$II->hasMacroDefinition();", icxx"$PP->getIdentifierTable();"))

Base.String(F::pcpp"clang::FieldDecl") = unsafe_string(icxx"$F->getName();")
# C structure to julia array of fields
function inspectStruct(CC, S)
    CC = Cxx.instance(CC)
    ASTCtx = icxx"&$(CC.CI)->getASTContext();"
    fields = Any[]
    icxx"""
        $S->dump();
        auto &ARL = $ASTCtx->getASTRecordLayout($S);
        for (auto field : ($S)->fields()) {
          unsigned i = field->getFieldIndex();
          field->dump();
          if (field->getType()->isUnionType())
            continue;
          if (field->getType()->isArrayType())
            continue;
          $:(begin
            QT = Cxx.QualType(icxx"return field->getType();")
            push!(fields, (
              String(icxx"return field;"),
              Cxx.juliatype(QT),
              icxx"return $ASTCtx->toCharUnitsFromBits(ARL.getFieldOffset(i)).getQuantity();"
            ))
          nothing
        end);
        }
    """
    fields
end

function desugar(C, QT)
  C = Cxx.instance(C)
  Cxx.QualType(icxx"$QT.getDesugaredType($(C.CI)->getASTContext());")
end
isPointerType(QT) = icxx"$QT->isPointerType();"
isRecordType(QT) = icxx"$QT->isRecordType();"

println("Hello World")

getMacroInfo(PP, II) = icxx"$PP->getMacroInfo($II);"
tokens(x) = icxx"$x->tokens();"

ioctl_defs = filter(map(II->(II,collect(tokens(getMacroInfo(PP, II)))),
  Iterators.filter(II->icxx"$II->hasMacroDefinition();", icxx"$PP->getIdentifierTable();"))) do x
      II, tokens = x
      isempty(tokens) && return false
      tok_is_identifier(tokens[1]) && String(tokens[1]) in ["_IO","_IOR","_IOW","_IOWR"]
  end;
  
function argumenttype_for_QT(QT)
  QT = desugar(CCompiler, QT)
  argumenttype = nothing
  # skip pointer/interger types for now
  if !isPointerType(QT) && isRecordType(QT)
      RD = icxx"((clang::RecordType*)$(Cxx.extractTypePtr(QT)))->getDecl();"
      if !icxx"$RD->isCompleteDefinition();"
          warn(string("RecordDecl ",unsafe_string(icxx"$RD->getName();")," used in ioctl but forward defined"))
      else
          argumenttype = inspectStruct(CCompiler, RD)
      end
  end
  argumenttype
end
  
ioctls = map(ioctl_defs) do x
  II, tokens = x
  # Processes e.g.
  # _IOR('U', 19, struct usbdevfs_hub_portinfo)
  # identifier '_IOR', l_paren, char_constant, comma, numeric_constant, comma, struct, identifier 'usbdevfs_hub_portinfo', r_paren
  category = getSpelling(PP,tokens[3])[2]
  code = parse(Int, getSpelling(PP,tokens[5]))
  argumenttype = nothing
  if String(tokens[1]) != "_IO"
      subt = tokens[findlast(tok_is_comma, tokens)+1:end-1]
      EnterTokenStream(PP, Vector{cxxt"clang::Token"}(subt))
      QT = ParseTypeName(P)
      argumenttype = argumenttype_for_QT(QT)
  end
  (String(II), category, code, 0, argumenttype)
end


getIdentifierInfo(PP, name) = icxx"$PP->getIdentifierInfo($name);"

# Some ioctls are not defined with the _IOR macros. Process them manually
function addLegacyIoctls(names)
  for name in names
    argumenttype = nothing
    if isa(name, Tuple)
        name, structname = name
        Cxx.EnterBuffer(Cxx.instance(CCompiler), structname)
        argumenttype = argumenttype_for_QT(ParseTypeName(P))
    end
    toks = collect(tokens(getMacroInfo(PP, getIdentifierInfo(PP, name))))
    (length(toks) == 1 && tok_is_numeric(toks[])) || continue
    val = parse(Int,getSpelling(PP,toks[]))
    push!(ioctls,(name, Char((val & 0xFF00)>>8), val&0xFF, 0, argumenttype))
  end
end

addLegacyIoctls([
 ("TCGETS", "struct termios"),
 ("TIOCGWINSZ", "struct winsize"),
 "TCSETS", "TCSETSW", "TCSETSF", "TCGETA", "TCSETA", "TCSETAW", 
 "TCSETAF", "TCSBRK", "TCXONC", "TCFLSH", "TIOCEXCL", "TIOCNXCL", "TIOCSCTTY",
 "TIOCGPGRP", "TIOCSPGRP", "TIOCOUTQ", "TIOCSTI", "TIOCSWINSZ",
 "TIOCMGET", "TIOCMBIS", "TIOCMBIC", "TIOCMSET", "TIOCGSOFTCAR", "TIOCSSOFTCAR",
 "FIONREAD", "TIOCINQ", "TIOCLINUX", "TIOCCONS", "TIOCGSERIAL", "TIOCSSERIAL",
 "TIOCPKT", "FIONBIO", "TIOCNOTTY", "TIOCSETD", "TIOCGETD", "TCSBRKP",
 "TIOCSBRK", "TIOCCBRK", "TIOCGSID", "TIOCGRS485", "TIOCSRS485", "TCGETX",
 "TCSETX", "TCSETXF", "TCSETXW", "TIOCVHANGUP", "FIONCLEX", "FIOCLEX",
 "FIOASYNC", "TIOCSERCONFIG", "TIOCSERGWILD", "TIOCSERSWILD", "TIOCGLCKTRMIOS",
 "TIOCSLCKTRMIOS", "TIOCSERGSTRUCT", "TIOCSERGETLSR", "TIOCSERGETMULTI",
 "TIOCSERSETMULTI", "TIOCMIWAIT", "TIOCGICOUNT", "FIOQSIZE"])

open(joinpath(dirname(@__FILE__), "../generated/alltheioctls.jl"),"w") do f
    println(f,"const ioctls = Tuple{String,Char,Int64,Int64,Any}[")
    sort!(ioctls, by = x->(x[2],x[3]))
    for ioctl in ioctls
        println(f, "   ", ioctl)
    end
    println(f,"]")
end

#=
cxx"""
#include <iostream>
class IoctlCatcher : public clang::PPCallbacks {
public:
    virtual void MacroExpands(const clang::Token &MacroNameTok,
                 const clang::MacroDefinition &MD, clang::SourceRange Range,
                 const clang::MacroArgs *Args) override
    {
        std::cout << MacroNameTok.getIdentifierInfo()->getName().str() << std::endl;
    }
    
    virtual void MacroDefined(const clang::Token &MacroNameTok,
                              const clang::MacroDirective *MD) override
    {
        std::cout << "DEFINED " << MacroNameTok.getIdentifierInfo()->getName().str() << std::endl;
    }
};
"""
icxx"$(Cxx.active_instances[2].CI)->getPreprocessor().addPPCallbacks(
  std::unique_ptr<clang::PPCallbacks>{new IoctlCatcher});"
Cxx.cxxinclude(CCompiler, "termios.h")
Cxx.cxxinclude(CCompiler, "linux/usbdevice_fs.h")

function getMacroInfo(PP, name)
  icxx"$PP->getMacroInfo($PP->getIdentifierInfo($name));"
end
function dump_macro(PP, name)
  icxx"$PP->DumpMacro(*$(getMacroInfo(PP, name)));"
end

PP=icxx"&$(Cxx.active_instances[2].CI)->getPreprocessor();"
=#
