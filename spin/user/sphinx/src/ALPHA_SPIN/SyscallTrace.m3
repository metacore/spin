

MODULE SyscallTrace;
IMPORT Trap, CPU, Strand;
IMPORT IO, Fmt;
IMPORT Dispatcher;
IMPORT Proc;

VAR  syscalls := ARRAY [0..260] OF TEXT {
		"syscall",	(*	0 *)
		"exit",	(*	1 *)
		"fork",	(*	2 *)
		"read",	(*	3 *)
		"write",	(*	4 *)
		"old_open",	(*	5 *)
		"close",	(*	6 *)
		"wait4",	(*	7 *)
		"old_creat",	(*	8 *)
		"link",	(*	9 *)
		"unlink",	(*	10 *)
		"execv",	(*	11 *)
		"chdir",	(*	12 *)
		"fchdir",	(*	13 *)
		"mknod",	(*	14 *)
		"chmod",	(*	15 *)
		"chown",	(*	16 *)
		"obreak",	(*	17 *)
		"getfsstat",	(*	18 *)
		"lseek",	(*	19 *)
		"getpid",	(*	20 *)
		"mount",	(*	21 *)
		"unmount",	(*	22 *)
		"setuid",	(*	23 *)
		"getuid",	(*	24 *)
		"exec_with_loader",	(*	25 *)
		"ptrace",	(*	26 *)
		"recvmsg",	(*	27 *)
		"sendmsg",	(*	28 *)
		"recvfrom",	(*	29 *)
		"accept",	(*	30 *)
		"getpeername",	(*	31 *)
		"getsockname",	(*	32 *)
		"access",	(*	33 *)
		"chflags",	(*	34 *)
		"fchflags",	(*	35 *)
		"sync",	(*	36 *)
		"kill",	(*	37 *)
		"old_stat",	(*	38 *)
		"setpgid",	(*	39 *)
		"old_lstat",	(*	40 *)
		"dup",	(*	41 *)
		"pipe",	(*	42 *)
		"set_program_attributes",	(*	43 *)
		"profil",	(*	44 *)
		"open",	(*	45 *)
		"old osigaction ",	(*	46 *)
		"getgid",	(*	47 *)
		"sigprocmask",	(*	48 *)
		"getlogin",	(*	49 *)
		"setlogin",	(*	50 *)
		"acct",	(*	51 *)
		"sigpending",	(*	52 *)
		" 53 ",	(*	53 *)
		"ioctl",	(*	54 *)
		"reboot",	(*	55 *)
		"revoke",	(*	56 *)
		"symlink",	(*	57 *)
		"readlink",	(*	58 *)
		"execve",	(*	59 *)
		"umask",	(*	60 *)
		"chroot",	(*	61 *)
		"old_fstat",	(*	62 *)
		"getpgrp",	(*	63 *)
		"getpagesize",	(*	64 *)
		"mremap",	(*	65 *)
		"vfork",	(*	66 *)
		"stat",	(*	67 *)
		"lstat",	(*	68 *)
		"sbrk",	(*	69 *)
		"sstk",	(*	70 *)
		"mmap",	(*	71 *)
		"ovadvise",	(*	72 *)
		"munmap",	(*	73 *)
		"mprotect",	(*	74 *)
		"madvise",	(*	75 *)
		"old_vhangup",	(*	76 *)
		"kmodcall",	(*	77 *)
		"mincore",	(*	78 *)
		"getgroups",	(*	79 *)
		"setgroups",	(*	80 *)
		"old_getpgrp",	(*	81 *)
		"setpgrp",	(*	82 *)
		"setitimer",	(*	83 *)
		"old_wait",	(*	84 *)
		"table",	(*	85 *)
		"getitimer",	(*	86 *)
		"gethostname",	(*	87 *)
		"sethostname",	(*	88 *)
		"getdtablesize",	(*	89 *)
		"dup2",	(*	90 *)
		"fstat",	(*	91 *)
		"fcntl",	(*	92 *)
		"select",	(*	93 *)
		"poll",	(*	94 *)
		"fsync",	(*	95 *)
		"setpriority",	(*	96 *)
		"socket",	(*	97 *)
		"connect",	(*	98 *)
		"old_accept",	(* 99 is old accept *)
		"getpriority",	(*	100 *)
		"old_send",	(* 101 is old send *)
		"old_recv",	(* 102 is old recv *)
		"sigreturn",	(*	103 *)
		"bind",	(*	104 *)
		"setsockopt",	(*	105 *)
		"listen",	(*	106 *)
		"plock",	(*	107 *)
		"old_sigvec",	(* 108 is old sigvec *)
		"old_sigblock",	(* 109 is old sigblock *)
		"old_sigsetmask",	(* 110 is old sigsetmask *)
		"sigsuspend",	(*	111 *)
		"sigstack",	(*	112 *)
		"old_recvmsg",	(* 113 is old recvmsg *)
		"old_sendmsg",	(* 114 is old sendmsg *)
		"obsolete vtrace ",	(* 115 is obsolete vtrace *)
		"gettimeofday",	(*	116 *)
		"getrusage",	(*	117 *)
		"getsockopt",	(*	118 *)
		" 119 ",	(*	119 *)
		"readv",	(*	120 *)
		"writev",	(*	121 *)
		"settimeofday",	(*	122 *)
		"fchown",	(*	123 *)
		"fchmod",	(*	124 *)
		"old_recvfrom",	(* 125 is old recvfrom *)
		"setreuid",	(*	126 *)
		"setregid",	(*	127 *)
		"rename",	(*	128 *)
		"truncate",	(*	129 *)
		"ftruncate",	(*	130 *)
		"flock",	(*	131 *)
		"setgid",	(*	132 *)
		"sendto",	(*	133 *)
		"shutdown",	(*	134 *)
		"socketpair",	(*	135 *)
		"mkdir",	(*	136 *)
		"rmdir",	(*	137 *)
		"utimes",	(*	138 *)
		"obsolete 4.2 sigreturn ", (* 139 is obsolete 4.2 sigreturn *)
		"adjtime",	(*	140 *)
		"old_getpeername",	(* 141 is old getpeername *)
		"gethostid",	(*	142 *)
		"sethostid",	(*	143 *)
		"getrlimit",	(*	144 *)
		"setrlimit",	(*	145 *)
		"old_killpg",	(* 146 is old killpg *)
		"setsid",	(*	147 *)
		"quotactl",	(*	148 *)
		"oldquota",	(*	149 *)
		"old_getsockname",	(* 150 is old getsockname *)
		" empty ",	
		" empty ",	
		"pid_block",	(*	153 *)
		"pid_unblock",	(*	154 *)
		" empty ",	
		"sigaction",	(*	156 *)
		"sigwaitprim",	(*	157 *)
		"nfssvc",	(*	158 *)
		"getdirentries",	(*	159 *)
		"statfs",	(*	160 *)
		"fstatfs",	(*	161 *)
		" empty ",	
		"async_daemon",	(*	163 *)
		"getfh",	(*	164 *)
		"getdomainname",	(*	165 *)
		"setdomainname",	(*	166 *)
		" empty ",	
		" empty ",	
		"exportfs",	(*	169 *)
		" empty ",	
		" empty ",	
		" empty ",	
		" empty ",	
		" empty ",	
		" empty ",	
		" empty ",	
		" empty ",	
		" empty ",	
		" empty ",	
		" empty ",	
		"alt_plock",	(* 181 is alternate plock *)
		" empty ",	
		" empty ",	
		"getmnt",	(*	184 *)
		" empty ",	
		" empty ",	
		"alt_sigpending",	(* 187 is alternate sigpending *)
		"alt_setsid",	(* 188 is alternate setsid *)
		" empty ",	
		" empty ",	
		" empty ",	
		" empty ",	
		" empty ",	
		" empty ",	
		" empty ",	
		" empty ",	
		" empty ",	
		" empty ",	
		"swapon",	(*	199 *)
		"msgctl",	(*	200 *)
		"msgget",	(*	201 *)
		"msgrcv",	(*	202 *)
		"msgsnd",	(*	203 *)
		"semctl",	(*	204 *)
		"semget",	(*	205 *)
		"semop",	(*	206 *)
		"uname",	(*	207 *)
		"lchown",	(*	208 *)
		"shmat",	(*	209 *)
		"shmctl",	(*	210 *)
		"shmdt",	(*	211 *)
		"shmget",	(*	212 *)
		"mvalid",	(*	213 *)
		"getaddressconf",	(*	214 *)
		"msleep",	(*	215 *)
		"mwakeup",	(*	216 *)
		"msync",	(*	217 *)
		"signal",	(*	218 *)
		"utc_gettime",	(*	219 *)
		"utc_adjtime",	(*	220 *)
		" empty ",	
		"security",	(*	222 *)
		"kloadcall",	(*	223 *)
		" empty ",	
		" empty ",	
		" empty ",	
		" empty ",	
		" empty ",	
		" empty ",	
		" empty ",	
		" empty ",	
		" empty ",	
		"getpgid",	(*	233 *)
		"getsid",	(*	234 *)
		"sigaltstack",	(*	235 *)
		"waitid",	(*	236 *)
		"priocntlset",	(*	237 *)
		"sigsendset",	(*	238 *)
		"set_speculative",	(*	239 *)
		"msfs_syscall",	(*	240 *)
		"sysinfo",	(*	241 *)
		"uadmin",	(*	242 *)
		"fuser",	(*	243 *)
		"proplist_syscall",	(*	244 *)
		" empty ",	
		" empty ",	
		" empty ",	
		" empty ",	
		" empty ",	
		"uswitch",	(*	250 *)
		"usleep_thread",	(*	251 *)
		"audcntl",	(*	252 *)
		"audgen",	(*	253 *)
		"sysfs",	(*	254 *)
		"subsys_info",	(*	255 *)
		"getsysinfo",	(*	256 *)
		"setsysinfo",	(*	257 *)
		"afs_syscall",	(*	258 *)
		"swapctl",	(*	259 *)
		"memcntl" 	(*	260 *)
	};

PROCEDURE Syscall(<*UNUSED*>strand: Strand.T; VAR ss: CPU.SavedState) =
  VAR
    proc : Proc.T;
  BEGIN
    IO.Put("syscall" & Fmt.Int(ss.v0));
    TRY
      proc := Proc.Self();
      IF proc.path # NIL THEN
	IO.Put("(" & proc.path & ") ");
      ELSE
	IO.Put("(" & Fmt.Int(proc.pid) & ") ");
      END;
    EXCEPT
    ELSE
    END;

    

    IF FIRST(syscalls)<=ss.v0 AND ss.v0<=LAST(syscalls) THEN
      IO.Put(":  " & syscalls[ss.v0]);
    ELSE
      IO.Put(":  0x" & Fmt.Unsigned(ss.v0));
    END;
    IO.Put("(0x" & Fmt.Unsigned(ss.a0) & ", 0x" & Fmt.Unsigned(ss.a1) & ")\n");

  END Syscall;


VAR tracer: Dispatcher.Binding;

PROCEDURE Install() =
  BEGIN
    tracer := Dispatcher.InstallHandler(Trap.Syscall, NIL, Syscall,
			  options:=Dispatcher.Options{Dispatcher.Opt.First},
			    key:=NEW(Trap.AuthKey,
				minProcID := -1000,
				maxProcID := 1000));
  END Install;

PROCEDURE Uninstall() =
  BEGIN
    IF tracer # NIL THEN 
      Dispatcher.Uninstall(tracer);
      tracer := NIL;
    END;
  END Uninstall;

BEGIN 
END SyscallTrace.
