using Base.Meta

abstract SyscallProperty
immutable FDArgument <: SyscallProperty
  which::UInt8
end
immutable BufferArgument <: SyscallProperty
  whichData::UInt8
  whichLength::UInt8
end
immutable CstringArgument <: SyscallProperty
  which::UInt8
end
immutable SigmaskPtrArgument <: SyscallProperty
  which::UInt8
end
if !isdefined(:CustomTransform)
immutable CustomTransform{T}
  which::UInt8
  F::T
end
end
(::Type{CustomTransform}){T}(which::Integer, F::T) = CustomTransform{T}(UInt8(which), F)

immutable Constant
  name::Symbol
  value::Int
end

macro constants(array, stripprefix, expr)
    ret = Expr(:block)
    # Initialize the name lookup array
    push!(ret.args,:(const $array = Vector{Constant}()))
    for e in expr.args
        if !isexpr(e,:const)
            continue
        end
        eq = e.args[1]
        @assert isexpr(eq,:(=))
        name = string(eq.args[1])
        name = replace(name,stripprefix,"",1)
        push!(ret.args,e)
        push!(ret.args,:(push!($array, Constant($(quot(Symbol(name))), $(eq.args[1])))))
    end
    return esc(ret)
end

function bitfield(val, constants)
    join(map(x->String(x.name), filter(x->(x.value&val)!=0, constants)), "|")
end

# Some constants - others we'll pull right from the headers
@constants RWX_OK "" begin
    const R_OK = 4
    const W_OK = 2
    const X_OK = 1
end
const F_OK = 0


const syscalls = [
#  name , x86, x64, nargs
  (:exit, 1, 60, -1, Any[]),
  (:fork, 2, 57, -1, Any[]),
  (:read, 3, 0, 3, Any[]),
  (:write, 4, 1, 3, Any[FDArgument(1), BufferArgument(2, 3)]),
  (:open, 5, 2, 3, Any[]),
  (:close, 6, 3, 1, Any[FDArgument(1)]),
  (:waitpid, 7, -1, -1, Any[]),
  (:creat, 8, 85, -1, Any[]),
  (:link, 9, 86, -1, Any[]),
  (:unlink, 10, 87, -1, Any[]),
  (:execve, 11, 59, 3, Any[]),
  (:chdir, 12, 80, -1, Any[]),
  (:time, 13, 201, -1, Any[]),
  (:mknod, 14, 133, -1, Any[]),
  (:chmod, 15, 90, -1, Any[]),
  (:lchown, 16, 94, -1, Any[]),
  (:_break, 17, -1, -1, Any[]),
  (:oldstat, 18, -1, -1, Any[]),
  (:lseek, 19, 8, -1, Any[]),
  (:getpid, 20, 39, -1, Any[]),
  (:mount, 21, 165, -1, Any[]),
  (:umount, 22, -1, -1, Any[]),
  (:setuid, 23, 105, -1, Any[]),
  (:getuid, 24, 102, -1, Any[]),
  (:stime, 25, -1, -1, Any[]),
  (:ptrace, 26, 101, -1, Any[]),
  (:alarm, 27, 37, -1, Any[]),
  (:oldfstat, 28, -1, -1, Any[]),
  (:pause, 29, 34, -1, Any[]),
  (:utime, 30, 132, -1, Any[]),
  (:stty, 31, -1, -1, Any[]),
  (:gtty, 32, -1, -1, Any[]),
  (:access, 33, 21, 2, Any[
    CstringArgument(1),
    CustomTransform(2,arg->arg == F_OK ? "F_OK" : bitfield(arg, RWX_OK))
  ]),
  (:nice, 34, -1, -1, Any[]),
  (:ftime, 35, -1, -1, Any[]),
  (:sync, 36, 162, -1, Any[]),
  (:kill, 37, 62, -1, Any[]),
  (:rename, 38, 82, -1, Any[]),
  (:mkdir, 39, 83, -1, Any[]),
  (:rmdir, 40, 84, -1, Any[]),
  (:dup, 41, 32, -1, Any[]),
  (:pipe, 42, 22, -1, Any[]),
  (:times, 43, 100, -1, Any[]),
  (:prof, 44, -1, -1, Any[]),
  (:brk, 45, 12, 1, Any[]),
  (:setgid, 46, 106, -1, Any[]),
  (:getgid, 47, 104, -1, Any[]),
  (:signal, 48, -1, -1, Any[]),
  (:geteuid, 49, 107, 0, Any[]),
  (:getegid, 50, 108, 0, Any[]),
  (:acct, 51, 163, -1, Any[]),
  (:umount2, 52, 166, -1, Any[]),
  (:lock, 53, -1, -1, Any[]),
  (:ioctl, 54, 16, 3, Any[FDArgument(1)]),
  (:fcntl, 55, 72, 2, Any[]),
  (:mpx, 56, -1, -1, Any[]),
  (:setpgid, 57, 109, -1, Any[]),
  (:ulimit, 58, -1, -1, Any[]),
  (:oldolduname, 59, -1, -1, Any[]),
  (:umask, 60, 95, -1, Any[]),
  (:chroot, 61, 161, -1, Any[]),
  (:ustat, 62, 136, -1, Any[]),
  (:dup2, 63, 33, -1, Any[]),
  (:getppid, 64, 110, -1, Any[]),
  (:getpgrp, 65, 111, -1, Any[]),
  (:setsid, 66, 112, -1, Any[]),
  (:sigaction, 67, -1, -1, Any[]),
  (:sgetmask, 68, -1, -1, Any[]),
  (:ssetmask, 69, -1, -1, Any[]),
  (:setreuid, 70, 113, -1, Any[]),
  (:setregid, 71, 114, -1, Any[]),
  (:sigsuspend, 72, -1, -1, Any[]),
  (:sigpending, 73, -1, -1, Any[]),
  (:sethostname, 74, 170, -1, Any[]),
  (:setrlimit, 75, 160, -1, Any[]),
  (:getrlimit, -1, 97, 2, Any[]),
  (:getrusage, 77, 98, -1, Any[]),
  (:gettimeofday, 78, 96, -1, Any[]),
  (:settimeofday, 79, 164, -1, Any[]),
  (:getgroups, 80, 115, -1, Any[]),
  (:setgroups, 81, 116, -1, Any[]),
  (:select, 82, 23, -1, Any[]),
  (:symlink, 83, 88, -1, Any[]),
  (:oldlstat, 84, -1, -1, Any[]),
  (:readlink, 85, 89, -1, Any[]),
  (:uselib, 86, 134, -1, Any[]),
  (:swapon, 87, 167, -1, Any[]),
  (:reboot, 88, 169, -1, Any[]),
  (:readdir, 89, -1, -1, Any[]),
  (:mmap, 90, 9, 6, Any[]),
  (:munmap, 91, 11, 2, Any[]),
  (:truncate, 92, 76, -1, Any[]),
  (:ftruncate, 93, 77, -1, Any[]),
  (:fchmod, 94, 91, -1, Any[]),
  (:fchown, 95, 93, -1, Any[]),
  (:getpriority, 96, 140, -1, Any[]),
  (:setpriority, 97, 141, -1, Any[]),
  (:profil, 98, -1, -1, Any[]),
  (:statfs, 99, 137, 2, Any[]),
  (:fstatfs, 100, 138, -1, Any[]),
  (:ioperm, 101, 173, -1, Any[]),
  (:socketcall, 102, -1, -1, Any[]),
  (:syslog, 103, 103, -1, Any[]),
  (:setitimer, 104, 38, -1, Any[]),
  (:getitimer, 105, 36, -1, Any[]),
  (:stat, 106, 4, -1, Any[]),
  (:lstat, 107, 6, -1, Any[]),
  (:fstat, 108, 5, 2, Any[]),
  (:olduname, 109, -1, -1, Any[]),
  (:iopl, 110, 172, -1, Any[]),
  (:vhangup, 111, 153, -1, Any[]),
  (:idle, 112, -1, -1, Any[]),
  (:vm86old, 113, -1, -1, Any[]),
  (:wait4, 114, 61, -1, Any[]),
  (:swapoff, 115, 168, -1, Any[]),
  (:sysinfo, 116, 99, -1, Any[]),
  (:ipc, 117, -1, -1, Any[]),
  (:fsync, 118, 74, -1, Any[]),
  (:sigreturn, 119, -1, -1, Any[]),
  (:clone, 120, 56, -1, Any[]),
  (:setdomainname, 121, 171, -1, Any[]),
  (:uname, 122, 63, -1, Any[]),
  (:modify_ldt, 123, 154, -1, Any[]),
  (:adjtimex, 124, 159, -1, Any[]),
  (:mprotect, 125, 10, 3, Any[]),
  (:sigprocmask, 126, -1, -1, Any[]),
  (:create_module, 127, 174, -1, Any[]),
  (:init_module, 128, 175, -1, Any[]),
  (:delete_module, 129, 176, -1, Any[]),
  (:get_kernel_syms, 130, 177, -1, Any[]),
  (:quotactl, 131, 179, -1, Any[]),
  (:getpgid, 132, 121, -1, Any[]),
  (:fchdir, 133, 81, -1, Any[]),
  (:bdflush, 134, -1, -1, Any[]),
  (:sysfs, 135, 139, -1, Any[]),
  (:personality, 136, 135, -1, Any[]),
  (:afs_syscall, 137, 183, -1, Any[]),
  (:setfsuid, 138, 122, -1, Any[]),
  (:setfsgid, 139, 123, -1, Any[]),
  (:_llseek, 140, -1, -1, Any[]),
  (:getdents, 141, 78, -1, Any[]),
  (:_newselect, 142, -1, -1, Any[]),
  (:flock, 143, 73, -1, Any[]),
  (:msync, 144, 26, -1, Any[]),
  (:readv, 145, 19, -1, Any[]),
  (:writev, 146, 20, -1, Any[]),
  (:getsid, 147, 124, -1, Any[]),
  (:fdatasync, 148, 75, -1, Any[]),
  (:_sysctl, 149, 156, -1, Any[]),
  (:mlock, 150, 149, -1, Any[]),
  (:munlock, 151, 150, -1, Any[]),
  (:mlockall, 152, 151, -1, Any[]),
  (:munlockall, 153, 152, -1, Any[]),
  (:sched_setparam, 154, 142, -1, Any[]),
  (:sched_getparam, 155, 143, -1, Any[]),
  (:sched_setscheduler, 156, 144, -1, Any[]),
  (:sched_getscheduler, 157, 145, -1, Any[]),
  (:sched_yield, 158, 24, -1, Any[]),
  (:sched_get_priority_max, 159, 146, -1, Any[]),
  (:sched_get_priority_min, 160, 147, -1, Any[]),
  (:sched_rr_get_interval, 161, 148, -1, Any[]),
  (:nanosleep, 162, 35, -1, Any[]),
  (:mremap, 163, 25, -1, Any[]),
  (:setresuid, 164, 117, -1, Any[]),
  (:getresuid, 165, 118, -1, Any[]),
  (:vm86, 166, -1, -1, Any[]),
  (:query_module, 167, 178, -1, Any[]),
  (:poll, 168, 7, -1, Any[]),
  (:nfsservctl, 169, 180, -1, Any[]),
  (:setresgid, 170, 119, -1, Any[]),
  (:getresgid, 171, 120, -1, Any[]),
  (:prctl, 172, 157, 5, Any[]),
  (:rt_sigreturn, 173, 15, -1, Any[]),
  (:rt_sigaction, 174, 13, 4, Any[]),
  (:rt_sigprocmask, 175, 14, 4, Any[SigmaskPtrArgument(2),SigmaskPtrArgument(3)]),
  (:rt_sigpending, 176, 127, -1, Any[]),
  (:rt_sigtimedwait, 177, 128, -1, Any[]),
  (:rt_sigsuspend, 179, 130, -1, Any[]),
  (:pread64, 180, 17, -1, Any[]),
  (:pwrite64, 181, 18, -1, Any[]),
  (:chown, 182, 92, -1, Any[]),
  (:getcwd, 183, 79, -1, Any[]),
  (:capget, 184, 125, -1, Any[]),
  (:capset, 185, 126, -1, Any[]),
  (:sigaltstack, 186, 131, -1, Any[]),
  (:sendfile, 187, 40, -1, Any[]),
  (:getpmsg, 188, 181, -1, Any[]),
  (:putpmsg, 189, 182, -1, Any[]),
  (:vfork, 190, 58, -1, Any[]),
  (:ugetrlimit, 191, -1, -1, Any[]),
  (:mmap2, 192, -1, -1, Any[]),
  (:truncate64, 193, -1, -1, Any[]),
  (:ftruncate64, 194, -1, -1, Any[]),
  (:stat64, 195, -1, -1, Any[]),
  (:lstat64, 196, -1, -1, Any[]),
  (:fstat64, 197, -1, -1, Any[]),
  (:lchown32, 198, -1, -1, Any[]),
  (:getuid32, 199, -1, -1, Any[]),
  (:getgid32, 200, -1, -1, Any[]),
  (:geteuid32, 201, -1, -1, Any[]),
  (:getegid32, 202, -1, -1, Any[]),
  (:setreuid32, 203, -1, -1, Any[]),
  (:setregid32, 204, -1, -1, Any[]),
  (:getgroups32, 205, -1, -1, Any[]),
  (:setgroups32, 206, -1, -1, Any[]),
  (:fchown32, 207, -1, -1, Any[]),
  (:setresuid32, 208, -1, -1, Any[]),
  (:getresuid32, 209, -1, -1, Any[]),
  (:setresgid32, 210, -1, -1, Any[]),
  (:getresgid32, 211, -1, -1, Any[]),
  (:chown32, 212, -1, -1, Any[]),
  (:setuid32, 213, -1, -1, Any[]),
  (:setgid32, 214, -1, -1, Any[]),
  (:setfsuid32, 215, -1, -1, Any[]),
  (:setfsgid32, 216, -1, -1, Any[]),
  (:pivot_root, 217, 155, -1, Any[]),
  (:mincore, 218, 27, -1, Any[]),
  (:madvise, 219, 28, -1, Any[]),
  (:getdents64, 220, 217, -1, Any[]),
  (:fcntl64, 221, -1, -1, Any[]),
  (:gettid, 224, 186, 0, Any[]),
  (:readahead, 225, 187, -1, Any[]),
  (:setxattr, 226, 188, -1, Any[]),
  (:lsetxattr, 227, 189, -1, Any[]),
  (:fsetxattr, 228, 190, -1, Any[]),
  (:getxattr, 229, 191, -1, Any[]),
  (:lgetxattr, 230, 192, -1, Any[]),
  (:fgetxattr, 231, 193, -1, Any[]),
  (:listxattr, 232, 194, -1, Any[]),
  (:llistxattr, 233, 195, -1, Any[]),
  (:flistxattr, 234, 196, -1, Any[]),
  (:removexattr, 235, 197, -1, Any[]),
  (:lremovexattr, 236, 198, -1, Any[]),
  (:fremovexattr, 237, 199, -1, Any[]),
  (:tkill, 238, 200, -1, Any[]),
  (:sendfile64, 239, -1, -1, Any[]),
  (:futex, 240, 202, 6, Any[]),
  (:sched_setaffinity, 241, 203, -1, Any[]),
  (:sched_getaffinity, 242, 204, -1, Any[]),
  (:set_thread_area, 243, 205, -1, Any[]),
  (:get_thread_area, 244, 211, -1, Any[]),
  (:io_setup, 245, 206, -1, Any[]),
  (:io_destroy, 246, 207, -1, Any[]),
  (:io_getevents, 247, 208, -1, Any[]),
  (:io_submit, 248, 209, -1, Any[]),
  (:io_cancel, 249, 210, -1, Any[]),
  (:fadvise64, 250, 221, -1, Any[]),
  (:exit_group, 252, 231, -1, Any[]),
  (:lookup_dcookie, 253, 212, -1, Any[]),
  (:epoll_create, 254, 213, -1, Any[]),
  (:epoll_ctl, 255, 233, -1, Any[]),
  (:epoll_wait, 256, 232, -1, Any[]),
  (:remap_file_pages, 257, 216, -1, Any[]),
  (:set_tid_address, 258, 218, 1, Any[]),
  (:timer_create, 259, 222, -1, Any[]),
  (:timer_settime, 260, 223, -1, Any[]),
  (:timer_gettime, 261, 224, -1, Any[]),
  (:timer_getoverrun, 262, 225, -1, Any[]),
  (:timer_delete, 263, 226, -1, Any[]),
  (:clock_settime, 264, 227, -1, Any[]),
  (:clock_gettime, 265, 228, -1, Any[]),
  (:clock_getres, 266, 229, -1, Any[]),
  (:clock_nanosleep, 267, 230, -1, Any[]),
  (:statfs64, 268, -1, -1, Any[]),
  (:fstatfs64, 269, -1, -1, Any[]),
  (:tgkill, 270, 234, -1, Any[]),
  (:utimes, 271, 235, -1, Any[]),
  (:fadvise64_64, 272, -1, -1, Any[]),
  (:vserver, 273, 236, -1, Any[]),
  (:mbind, 274, 237, -1, Any[]),
  (:get_mempolicy, 275, 239, -1, Any[]),
  (:set_mempolicy, 276, 238, -1, Any[]),
  (:mq_open, 277, 240, -1, Any[]),
  (:mq_unlink, 278, 241, -1, Any[]),
  (:mq_timedsend, 279, 242, -1, Any[]),
  (:mq_timedreceive, 280, 243, -1, Any[]),
  (:mq_notify, 281, 244, -1, Any[]),
  (:mq_getsetattr, 282, 245, -1, Any[]),
  (:kexec_load, 283, 246, -1, Any[]),
  (:waitid, 284, 247, -1, Any[]),
  (:add_key, 286, 248, -1, Any[]),
  (:request_key, 287, 249, -1, Any[]),
  (:keyctl, 288, 250, -1, Any[]),
  (:ioprio_set, 289, 251, -1, Any[]),
  (:ioprio_get, 290, 252, -1, Any[]),
  (:inotify_init, 291, 253, -1, Any[]),
  (:inotify_add_watch, 292, 254, -1, Any[]),
  (:inotify_rm_watch, 293, 255, -1, Any[]),
  (:migrate_pages, 294, 256, -1, Any[]),
  (:openat, 295, 257, -1, Any[]),
  (:mkdirat, 296, 258, -1, Any[]),
  (:mknodat, 297, 259, -1, Any[]),
  (:fchownat, 298, 260, -1, Any[]),
  (:futimesat, 299, 261, -1, Any[]),
  (:fstatat64, 300, 262, -1, Any[]),
  (:unlinkat, 301, 263, -1, Any[]),
  (:renameat, 302, 264, -1, Any[]),
  (:linkat, 303, 265, -1, Any[]),
  (:symlinkat, 304, 266, -1, Any[]),
  (:readlinkat, 305, 267, -1, Any[]),
  (:fchmodat, 306, 268, -1, Any[]),
  (:faccessat, 307, 269, -1, Any[]),
  (:pselect6, 308, 270, -1, Any[]),
  (:ppoll, 309, 271, -1, Any[]),
  (:unshare, 310, 272, -1, Any[]),
  (:set_robust_list, 311, 273, 3, Any[]),
  (:get_robust_list, 312, 274, 3, Any[]),
  (:splice, 313, 275, -1, Any[]),
  (:sync_file_range, 314, 277, -1, Any[]),
  (:tee, 315, 276, -1, Any[]),
  (:vmsplice, 316, 278, -1, Any[]),
  (:move_pages, 317, 279, -1, Any[]),
  (:getcpu, 318, 309, -1, Any[]),
  (:epoll_pwait, 319, 281, -1, Any[]),
  (:utimensat, 320, 280, -1, Any[]),
  (:signalfd, 321, 282, -1, Any[]),
  (:timerfd_create, 322, 283, -1, Any[]),
  (:eventfd, 323, 284, -1, Any[]),
  (:fallocate, 324, 285, -1, Any[]),
  (:timerfd_settime, 325, 286, -1, Any[]),
  (:timerfd_gettime, 326, 287, -1, Any[]),
  (:signalfd4, 327, 289, -1, Any[]),
  (:eventfd2, 328, 290, -1, Any[]),
  (:epoll_create1, 329, 291, -1, Any[]),
  (:dup3, 330, 292, -1, Any[]),
  (:pipe2, 331, 293, -1, Any[]),
  (:inotify_init1, 332, 294, -1, Any[]),
  (:preadv, 333, 295, -1, Any[]),
  (:pwritev, 334, 296, -1, Any[]),
  (:rt_sigqueueinfo, 178, 129, -1, Any[]),
  (:rt_tgsigqueueinfo, 335, 297, -1, Any[]),
  (:perf_event_open, 336, 298, 5, Any[]),
  (:recvmmsg, 337, 299, -1, Any[]),
  (:fanotify_init, 338, 300, -1, Any[]),
  (:fanotify_mark, 339, 301, -1, Any[]),
  (:prlimit64, 340, 302, -1, Any[]),
  (:name_to_handle_at, 341, 303, -1, Any[]),
  (:open_by_handle_at, 342, 304, -1, Any[]),
  (:clock_adjtime, 343, 305, -1, Any[]),
  (:syncfs, 344, 306, -1, Any[]),
  (:sendmmsg, 345, 307, -1, Any[]),
  (:setns, 346, 308, -1, Any[]),
  (:process_vm_readv, 347, 310, -1, Any[]),
  (:process_vm_writev, 348, 311, -1, Any[]),
  (:kcmp, 349, 312, -1, Any[]),
  (:finit_module, 350, 313, -1, Any[]),
  (:sched_setattr, 351, 314, -1, Any[]),
  (:sched_getattr, 352, 315, -1, Any[]),
  (:renameat2, 353, 316, -1, Any[]),
  (:seccomp, 354, 317, -1, Any[]),
  (:getrandom, 355, 318, -1, Any[]),
  (:memfd_create, 356, 319, -1, Any[]),
  (:bpf, 357, 321, -1, Any[]),
  (:execveat, 358, 322, -1, Any[]),
  (:userfaultfd, 374, 323, -1, Any[]),
  (:membarrier, 375, 324, -1, Any[]),
  (:mlock2, 376, 325, -1, Any[]),
  (:copy_file_range, 377, 326, -1, Any[]),
  (:preadv2, 378, 327, -1, Any[]),
  (:pwritev2, 379, 328, -1, Any[]),
  (:restart_syscall, -1, 219, -1, Any[]),
  (:rrcall_init_preload, 442, 442, 0, Any[]),
  (:rrcall_init_buffers, 443, 443, 1, Any[]),
  (:rrcall_notify_syscall_hook_exit, 444, 444, -1, Any[]),
  (:rrcall_notify_control_msg, 445, 445, -1, Any[]),
  (:rrcall_reload_auxv, 446, 446, -1, Any[]),
  (:rrcall_mprotect_record, 447, 447, -1, Any[]),
  (:socket, 359, 41, -1, Any[]),
  (:connect, 362, 42, -1, Any[]),
  (:accept, -1, 43, -1, Any[]),
  (:sendto, 369, 44, -1, Any[]),
  (:recvfrom, 371, 45, -1, Any[]),
  (:sendmsg, 370, 46, -1, Any[]),
  (:recvmsg, 372, 47, -1, Any[]),
  (:shutdown, 373, 48, -1, Any[]),
  (:bind, 361, 49, -1, Any[]),
  (:listen, 363, 50, -1, Any[]),
  (:getsockname, 367, 51, -1, Any[]),
  (:getpeername, 368, 52, -1, Any[]),
  (:socketpair, 360, 53, -1, Any[]),
  (:setsockopt, 366, 54, -1, Any[]),
  (:getsockopt, 365, 55, -1, Any[]),
  (:accept4, 364, 288, -1, Any[]),
  (:shmget, -1, 29, -1, Any[]),
  (:shmat, -1, 30, -1, Any[]),
  (:shmctl, -1, 31, -1, Any[]),
  (:semget, -1, 64, -1, Any[]),
  (:semop, -1, 65, -1, Any[]),
  (:semctl, -1, 66, -1, Any[]),
  (:shmdt, -1, 67, -1, Any[]),
  (:msgget, -1, 68, -1, Any[]),
  (:msgsnd, -1, 69, -1, Any[]),
  (:msgrcv, -1, 70, -1, Any[]),
  (:msgctl, -1, 71, -1, Any[]),
  (:semtimedop, -1, 220, -1, Any[]),
  (:arch_prctl, -1, 158, 2, Any[]),
  (:tuxcall, -1, 184, -1, Any[]),
  (:security, -1, 185, -1, Any[]),
  (:epoll_ctl_old, -1, 214, -1, Any[]),
  (:epoll_wait_old, -1, 215, -1, Any[])
]
