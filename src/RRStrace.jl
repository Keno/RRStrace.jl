module RRStrace

    using NativeDebugger
    using DebuggerFramework
    using RR

    include("syscalls.jl")
    include("generated/alltheioctls.jl")
    using Cxx

    signal_names = [
        "HUP", "INT", "QUIT", "ILL", "TRAP", "ABRT", "BUS", "FPE",
        "KILL", "USR1", "SEGV", "USR2", "PIPE", "ALRM", "TERM", "STKFLT",
        "CHLD", "CONT", "STOP", "TSTP", "TTIN", "TTOU", "URG", "XCPU", "XFSZ",
        "VTALRM", "PROF", "WINCH", "POLL", "IO", "PWR", "SYS"
    ]

    function apply_property!(task, property, arguments)
        if isa(property, CstringArgument)
            data = NativeDebugger.fallible_load(task, RemotePtr{UInt8}(arguments[property.which]), 40)
            idx = findfirst(data, 0); more = false;
            if idx == 0
                more = true
            else
                data = data[1:idx-1]
            end
            arguments[property.which] = string(repr(String(data)),more ? "..." : "")
        elseif isa(property, CustomTransform)
            arguments[property.which] = property.F(arguments[property.which])
        elseif isa(property, FDArgument)
            arguments[property.which] = sprint((args...)->print_with_color(:yellow,args...), string(arguments[property.which]))
        elseif isa(property, SigmaskPtrArgument) && arguments[property.which] != 0
            mask = NativeDebugger.load(task, RemotePtr{UInt64}(arguments[property.which]))
            sigs = signal_names[filter(x->((1<<(x-1))&mask != 0),1:length(signal_names))]
            arguments[property.which] =
                string("[",join(map(s->string("SIG",s),sigs), " "),"]")
        elseif isa(property, BufferArgument)
            more = arguments[property.whichLength] > 40
            size = min(arguments[property.whichLength], 40)
            data = NativeDebugger.fallible_load(task, RemotePtr{UInt8}(arguments[property.whichData]), size)
            arguments[property.whichData] = string(repr(String(data)),more ? "..." : "")
        end
    end

    function print_syscall_arguments(io, task, ev)
        arch, num = icxx"$ev.arch();", icxx"$ev.Syscall().number;"
        syscall = syscall_table[arch][num]
        if syscall[4] == -1
            print_with_color(:red, io, "(<printing not implemented>)")
        else
            arguments = Any[icxx"$task->regs().arg($i);" for i in 1:syscall[4]]
            if syscall[1] == :ioctl
                category = Char((arguments[2]&0xff00)>>8)
                code = arguments[2]&0xff
                size = (arguments[2]&0xffff0000) >> 16
                idxs = searchsorted(ioctls, (nothing,category,code,nothing), by=x->(x[2],x[3]))
                # Try to find ioctls whose size match exactly. This is useful
                # because some ioctls are only distinguised by size
                idxs′ = filter(x->ioctls[x][4] == size, idxs)
                idxs = isempty(idxs′) ? idxs : idxs′
                if length(idxs) > 1
                    print(io, "({", join(map(x->ioctls[x][1], idxs), " or "), "}, ")
                else
                    print(io, "(", ioctls[idxs[1]][1], ", ")
                end
                ioctl = first(ioctls[idxs])
                print(io,"{",join(map(ioctl[5]) do field
                    name, T, offset = field
                    string(".",name,"=",NativeDebugger.load(task, RemotePtr{T}(arguments[3]+offset)))
                end,", "),"}")
                print(io,")")
                return
            end
            for property in syscall[5]
                apply_property!(task, property, arguments)
            end
            print(io, "(", join(arguments, ", "),")")
        end
    end

    const syscalls_with_hex_printing = [:mmap, :brk]
    function print_syscall_return(io, task, ev)
        arch, num = icxx"$ev.arch();", icxx"$ev.Syscall().number;"
        result = icxx"$task->regs().syscall_result_signed();"
        syscallname = syscall_table[arch][num][1]
        if result < 0
          print(io, " = ")
          print_with_color(:red, io, unsafe_string(icxx"rr::errno_name(-$result);"))
        else
          print(io, " = ", (syscallname in syscalls_with_hex_printing) ?
              string("0x",hex(result, 2sizeof(UInt64))) :
              result)
        end
    end

    const x86_arch = icxx"rr::SupportedArch::x86;"
    const x64_arch = icxx"rr::SupportedArch::x86_64;"

    syscall_table = Dict(
      x86_arch => Dict(x[2] => x for x in filter(x->x[2] != -1, syscalls)),
      x64_arch => Dict(x[3] => x for x in filter(x->x[3] != -1, syscalls))
    )
    function strace(timeline)
        result = icxx"rr::REPLAY_CONTINUE;"
        while icxx"$result == rr::REPLAY_CONTINUE;"
            session = RR.current_session(timeline)
            ev = icxx"rr::Event ev = $session->current_trace_frame().event(); ev;"
            tid = RR.tid(icxx"$session->current_trace_frame();")
            time = RR.time(icxx"$session->current_trace_frame();")
            result = icxx"$session->replay_step(rr::RUN_CONTINUE).status;"
            if icxx"""$ev.is_syscall_event() &&
                      !$session->current_step_key().in_execution() &&
                      $ev.Syscall().state == rr::EXITING_SYSCALL;"""
                print(STDOUT,"[",tid," ",time,"] ")
                arch, num = icxx"$ev.arch();", icxx"$ev.Syscall().number;"
                task = RR.current_task(session)
                print_with_color(:bold, STDOUT, string(syscall_table[arch][num][1]))
                print_syscall_arguments(STDOUT, task, ev)
                print_syscall_return(STDOUT, task, ev)
                println()
            end
        end
    end

end # module
