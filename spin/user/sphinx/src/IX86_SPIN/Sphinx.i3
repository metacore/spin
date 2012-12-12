(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *  HISTORY
 * 24-Jan-98  Tsutomu Owa (owa) at the University of Washington
 *	Added Syscall_.
 * 13-Jun-97  David Becker at the University of Washington
 *      Added Error.E to raises list of all procs that use File
 * 31-May-97  David Becker at the University of Washington
 *      Unified Socket.Error and Error.E into Errno exception 
 * 22-May-97  Charles Garrett (garrett) at the University of Washington
 *	Changed signature of Recvfrom to match Sockets.m3.
 *
 * 19-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	Moved Profile to usyscall.
 * 24-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Modified to make it look similar to FreeBSD.
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Created from Alpha version.
 *
 *)

(*
 *
 * This file defines a FreeBSD system call interface.
 * This file is processed by Sieg.
 *
 *)
INTERFACE Sphinx;
IMPORT FileStat, StatFs;
IMPORT Word, CPU;
IMPORT SocketRep;
IMPORT Ctypes, Strand;
IMPORT Error, Errno, VMError;
IMPORT MachineSigContext;
IMPORT NameServer;

<*EPILOG_BRANCH_ON_REGISTER _Seterrno*>
<*INTERFACE_PROC_BASE 1*>

CONST Brand = "Sphinx"; 

CONST
  (* from i386_freebsd/sys/mman.h *)
  MAP_SHARED = 1;		(* share changes *)
  MAP_PRIVATE = 2;		(* changes are private *)
  MAP_FIXED = 16_10;		(* map addr must be exactly as specified *)
  MAP_ANONYMOUS = 16_1000;	(* allocated from memory, swap space *)

TYPE
  sockaddrT = SocketRep.sockaddr;
  sigaction = RECORD
    sa_handler: Word.T; (* Address of handler *)
    sa_mask: Ctypes.unsigned_int;
    sa_flags: Ctypes.int;
  END;
  
  TimeZone = RECORD
    tz_minuteswest : Ctypes.unsigned_int;
    tz_dsttime : Ctypes.unsigned_int;
  END;
  
  TimeVal = RECORD
    tv_sec, tv_usec : Ctypes.unsigned_int;
  END;

  ItimerVal = RECORD
    interval, value : TimeVal;
  END;
  Rusage = RECORD
    utime, stime: TimeVal;
    maxrss: Ctypes.long;
    ixrss: Ctypes.long;		(* integral shared memory size *)
    idrss: Ctypes.long;		(* integral unshared data " *)
    isrss: Ctypes.long;		(* integral unshared stack " *)
    minflt: Ctypes.long;	(* page reclaims - total vmfaults *)
    majflt: Ctypes.long;	(* page faults *)
    nswap: Ctypes.long;		(* swaps *)
    inblock: Ctypes.long;	(* block input operations *)
    oublock: Ctypes.long;	(* block output operations *)
    msgsnd: Ctypes.long;	(* messages sent *)
    msgrcv: Ctypes.long;	(* messages received *)
    nsignals: Ctypes.long;	(* signals received *)
    nvcsw: Ctypes.long;		(* voluntary context switches *)
    nivcsw: Ctypes.long;	(* involuntary " *)
  END;

PROCEDURE Exit(status: INTEGER);
  
PROCEDURE Fork(<*INTERNAL*>us: Strand.T;
	       <*INTERNAL*>VAR s: CPU.SavedState)
  RAISES {Errno.E, VMError.E};

PROCEDURE Read(fh: INTEGER; t: INTEGER; size: INTEGER)
  : INTEGER RAISES {Errno.E, VMError.E, Error.E};
  
PROCEDURE Write(fh: INTEGER; t: INTEGER; size: INTEGER)
  : INTEGER RAISES {Errno.E, VMError.E, Error.E};
  
PROCEDURE Open(<*CTEXT*>READONLY path: NameServer.Name;
		mode, createMode: INTEGER)
  : INTEGER RAISES {Errno.E, Error.E, NameServer.Error};
  
PROCEDURE Close(fd: Word.T): INTEGER RAISES {Errno.E, Error.E};

PROCEDURE Waitpid(pid : INTEGER; statusAddr : INTEGER; opt : INTEGER)
  : INTEGER RAISES {Errno.E, VMError.E};
  (* XXX Actually, this is wait4. *)
  (* if PID = -1, then wait any chid proc *)
  
<*PROCID 9*>
PROCEDURE Link(<*CTEXT*>src : TEXT;
	       <*CTEXT*>dest : TEXT)
  : INTEGER RAISES {Errno.E, VMError.E};

PROCEDURE Unlink(<*CTEXT*>READONLY path: NameServer.Name): INTEGER
  RAISES {Errno.E, NameServer.Error};

(* execv *)

<*PROCID 12*>
PROCEDURE Chdir(<*CTEXT*>path: TEXT) : INTEGER RAISES {Errno.E};
PROCEDURE Fchdir(fd : INTEGER): INTEGER RAISES {Errno.E};
PROCEDURE Mknod(<*CTEXT*>path: TEXT; mode, dev: INTEGER)
  	: INTEGER RAISES {Errno.E};
PROCEDURE Chmod(<*CTEXT*>path : TEXT; mode : INTEGER) 
  	: INTEGER RAISES {Errno.E};
PROCEDURE Chown(<*CTEXT*>path : TEXT;
		pid, gid : INTEGER) : INTEGER RAISES {Errno.E};
PROCEDURE Break (newBreak: Word.T) : INTEGER RAISES {VMError.E};

(* old lseek *)
(* getfsstat *)

<*PROCID 20*>
PROCEDURE Getpid(<*INTERNAL*>us : Strand.T;
		 <*INTERNAL*>VAR s : CPU.SavedState);

(* mount *)
(* unmount *)

<*PROCID 23*>  
PROCEDURE Setuid(uid: INTEGER) : INTEGER RAISES {Errno.E};
PROCEDURE Getuid(<*INTERNAL*>us: Strand.T;
		 <*INTERNAL*>VAR s: CPU.SavedState);
(* geteuid *)
(* ptrace *)
(* recvmsg *)
(* sendmsg *)
  
<*PROCID 29*>  
PROCEDURE Recvfrom(fh: INTEGER;
		   buffer, length: INTEGER;
		   flags: INTEGER;
		   address, addrlen: INTEGER)
 : INTEGER RAISES {Errno.E, VMError.E};

PROCEDURE Accept(fh: INTEGER; address: INTEGER; VAR length: INTEGER)
  : INTEGER RAISES {Errno.E, VMError.E, Error.E};

PROCEDURE GetPeerName(fd, addr, addrLen: INTEGER)
  : INTEGER RAISES {Errno.E, VMError.E};
PROCEDURE Getsockname(fh, addr: INTEGER; VAR length : CARDINAL)
  : INTEGER RAISES {Errno.E, VMError.E};
PROCEDURE Access(<*CTEXT*>path : TEXT; how : INTEGER)
  	: INTEGER RAISES {Errno.E, Error.E};

(* chflags *)
(* fchflags *)
(* sync *)

<*PROCID 37*>
PROCEDURE Kill(pid, signo : INTEGER) : INTEGER RAISES {Errno.E};
  

(* old stat *)
(* getppid *)
(* old lstat *)

<*PROCID 41*>
PROCEDURE Dup(fh: INTEGER): INTEGER RAISES {Errno.E, Error.E};
PROCEDURE Pipe(<*INTERNAL*>us: Strand.T;
	       <*INTERNAL*>VAR s: CPU.SavedState)
  RAISES {Errno.E};

(* getegid *)
(* profil *)
(* ktrace *)
  
<*PROCID 46*>
PROCEDURE Sigaction(signo: INTEGER; naction, oaction, tramp: Word.T)
  : INTEGER RAISES {Errno.E,VMError.E};
(* XXX "tramp" is not used in FreeBSD. But I don't care *)

PROCEDURE Getgid(<*INTERNAL*>us : Strand.T;
		 <*INTERNAL*>VAR s : CPU.SavedState);

<*PROCID 48*>
PROCEDURE Sigprocmask(how: INTEGER; mask: Ctypes.unsigned_long): INTEGER
  RAISES {Errno.E};
PROCEDURE Getlogin(n: INTEGER; ptr : Word.T)
  : INTEGER RAISES {Errno.E, VMError.E};
PROCEDURE Setlogin(): INTEGER RAISES {Errno.E};
  
(* acct *)
(* sigpending *)
(* sigaltstack  *)

<*PROCID 54*>
PROCEDURE Ioctl(fh : INTEGER; cmd : INTEGER;
		arg : INTEGER)
  : INTEGER RAISES {Errno.E, VMError.E, Error.E};

(* reboot *)
(* revoke *)

<*PROCID 57*>
PROCEDURE Symlink(<*CTEXT*>src : TEXT; <*CTEXT*>dest : TEXT)
  : INTEGER RAISES {Errno.E, VMError.E};

PROCEDURE Readlink(<*CTEXT*>path: TEXT; buf, size: INTEGER)
  : INTEGER RAISES {Errno.E, VMError.E};

PROCEDURE Execve (<*INTERNAL*>us : Strand.T;
		  <*INTERNAL*>VAR s : CPU.SavedState)
  RAISES {Errno.E, VMError.E, Error.E};
PROCEDURE Umask(mask: INTEGER): INTEGER RAISES {Errno.E};
(* chroot *)
(* old fstat *)
(* old getkerninfo *)
(* msync *)

<*PROCID 66*>
PROCEDURE Vfork(<*INTERNAL*>us : Strand.T;
		<*INTERNAL*>VAR s : CPU.SavedState)
  RAISES {Errno.E, VMError.E};

(* sbrk *)
(* sstk *)
(* old mmap *)

<*PROCID 73*>
PROCEDURE Munmap(addr, len: INTEGER): INTEGER RAISES {Errno.E, VMError.E};
PROCEDURE Mprotect(addr, size, prot: INTEGER): INTEGER RAISES {VMError.E};
PROCEDURE Madvise (addr, len, behav : INTEGER) : INTEGER RAISES {Errno.E};

(* old vhangup *)
(* old vlimit *)
(* mincore *)

<*PROCID 79*>
PROCEDURE Getgroups(n: INTEGER; ptr: Word.T)
  : INTEGER RAISES {Errno.E, VMError.E};
PROCEDURE Setgroups(n: INTEGER; ptr: Word.T)
  	: INTEGER RAISES {Errno.E, VMError.E};
PROCEDURE Getpgrp() : INTEGER;
PROCEDURE Setpgid(pid, pgid: INTEGER) RAISES {Errno.E};
  
<*PROCID 83*>
<*SYNONYMS setitimer,__setitimer*>
(* XXX.  ovalue: returning the previous value of the timer if ovalue
 * is nonzero.
 *)
PROCEDURE Setitimer(which: INTEGER; VAR value: ItimerVal; ovalue: INTEGER)
	: INTEGER RAISES {Errno.E};

(* swapon *)
  
<*PROCID 86*>
PROCEDURE Getitimer(which : INTEGER; VAR value : ItimerVal)
	 : INTEGER RAISES {Errno.E};
(* old gethostname *)
(* old sethostname *)
  
<*PROCID 89*>
PROCEDURE Getdtablesize() : INTEGER;
PROCEDURE Dup2(old, new: INTEGER): INTEGER RAISES {Errno.E, Error.E};

<*PROCID 92*>  
PROCEDURE Fcntl(fh, cmd, arg: INTEGER): INTEGER RAISES {Errno.E, Error.E};

PROCEDURE Select(nfds, inputs, outputs, excepts, tv: INTEGER)
	 : INTEGER RAISES {Errno.E, VMError.E};

(* fsync *)
(* setpriority *)
  
<*PROCID 97*>
PROCEDURE CreateSocket(addrFamily, type, protocol : INTEGER)
  : INTEGER RAISES {Errno.E};

PROCEDURE Connect(fh, addr: INTEGER)
  : INTEGER RAISES {Errno.E, VMError.E};

(* old accept *)  
(* getpriority *)
(* old send *)
(* old recv *)
  
<*PROCID 103*>  
PROCEDURE Sigreturn(<*INTERNAL*>VAR s : CPU.SavedState;
		    READONLY sigContext: MachineSigContext.T);
PROCEDURE Bind(fh, addr: INTEGER)
  : INTEGER RAISES {Errno.E, VMError.E};
PROCEDURE SetSockOpt(fd, level: INTEGER;
		     optName, optVal, optLen: INTEGER)
  : INTEGER RAISES {Errno.E, VMError.E};
PROCEDURE Listen(fd, backlog: INTEGER): INTEGER RAISES {Errno.E};

(* four old syscalls *)
  
<*PROCID 111*>    
PROCEDURE Sigsuspend(mask : INTEGER);
  
(* four old syscalls *)

<*PROCID 116*>    
PROCEDURE Gettimeofday(tv, tz: INTEGER) RAISES {VMError.E};
PROCEDURE Getrusage(who: INTEGER; <*OUT*>VAR r: Rusage): INTEGER
  RAISES {Errno.E, VMError.E};
  
(* getsockopt *)
(* resuba *)

<*PROCID 120*>    
PROCEDURE Readv(fd, ptr, n: INTEGER): INTEGER RAISES {Errno.E, VMError.E, Error.E};
PROCEDURE Writev(fd, ptr, n: INTEGER): INTEGER RAISES {Errno.E, VMError.E, Error.E};

(* settimeofday *)
(* fchown *)
(* fchmod *)

<*PROCID 126*>    
PROCEDURE Setreuid(ruid, euid: INTEGER) : INTEGER RAISES {Errno.E};
PROCEDURE Setregid(rgid, egid: INTEGER) : INTEGER RAISES {Errno.E};
PROCEDURE Rename(<*CTEXT*>from : TEXT;
		 <*CTEXT*>to : TEXT) : INTEGER RAISES {Errno.E};
(* old truncate *)
(* old ftruncate *)
(* flock *)
(* mkfifo *)

<*PROCID 133*>    
PROCEDURE Sendto(fh, message, len, flags, dest, destLen: INTEGER)
  : INTEGER RAISES {Errno.E, VMError.E};
PROCEDURE ShutDown(fd: INTEGER; how: INTEGER )
  : INTEGER RAISES {Errno.E};
  
(* socketpair *)
(* mkdir *)
(* rmdir *)
(* utimes *)
(* obsolete 4.2 sigreturn  *)
(* adjtime *)
(* six old syscalls *)  
(* setsid *)
(* quotactl *)
(* oldquota *)
(* old getsockname *)
(* nfssvc *)
  
<*PROCID 157*>  
PROCEDURE Statfs(<*CTEXT*>path: TEXT; VAR buf: StatFs.T; len: INTEGER)
    : INTEGER RAISES {Errno.E, Error.E};

PROCEDURE Fstatfs(fd: INTEGER; VAR buf: StatFs.T; len: INTEGER)
    : INTEGER RAISES {Errno.E, Error.E};
(* getfh *)

<*PROCID 162*>  
PROCEDURE Getdomainname(ptr: Word.T; len: INTEGER)
        : INTEGER RAISES {Errno.E, VMError.E};
  
PROCEDURE Setdomainname(ptr: Word.T; len: INTEGER)
        : INTEGER RAISES {Errno.E, VMError.E};

(* uname *)
(* sysarch *)
(* rtprio *)
(* semsys *)
(* msgsys *)
(* shmsys *)
(* ntp_adjtime *)
<*PROCID 181*>  
PROCEDURE Setgid(gid : INTEGER) : INTEGER RAISES {Errno.E};
(* setegid *)
(* seteuid *)
(* lfs_bmapv *)
(* lfs_markv *)
(* lfs_segclean *)
(* lfs_segwait *)  

<*PROCID 188*>  
PROCEDURE Stat(<*CTEXT*>READONLY path: NameServer.Name;
	       <*OUT*>VAR s: FileStat.T): INTEGER
  RAISES {Errno.E, Error.E, NameServer.Error};
<*PROCID 189*>  
PROCEDURE Fstat(fh: INTEGER;<*OUT*>VAR s: FileStat.T)
  : INTEGER RAISES {Errno.E, Error.E};
PROCEDURE Lstat(<*CTEXT*>READONLY path: NameServer.Name;
		<*OUT*>VAR s: FileStat.T)
  : INTEGER RAISES {Errno.E, Error.E, NameServer.Error};

TYPE Rlimit = RECORD
  cur, max: Ctypes.unsigned_long;
END;
  
(* pathconf *)
(* fpathconf *)
(* ??? *)
  
<*PROCID 194*>  
PROCEDURE Getrlimit(type: INTEGER; VAR l: Rlimit) : INTEGER RAISES {Errno.E};
PROCEDURE Setrlimit(type: INTEGER; VAR l: Rlimit) : INTEGER RAISES {Errno.E};
PROCEDURE Getdirentries(fh, buf, size: INTEGER; <*OUT*>VAR basep: INTEGER)
  : INTEGER RAISES {Errno.E, VMError.E};
(*
PROCEDURE Mmap (addr, len: INTEGER; <*UNUSED*>prot: INTEGER;
  flags, fd, off: INTEGER): INTEGER RAISES {Errno.E, VMError.E};
*)

<*PROCID 198*>  
PROCEDURE Syscall_(<*INTERNAL*>us : Strand.T;
		  <*INTERNAL*>VAR s : CPU.SavedState) RAISES {Errno.E};
<*PROCID 199*>
PROCEDURE Seek(fh : INTEGER; off, whence : INTEGER) : INTEGER RAISES {Errno.E, Error.E};
  
<*PROCID 202*>
PROCEDURE Sysctl(mib, mibLen, oldBuf: INTEGER;
		 VAR oldBufLen: INTEGER;
		 newBuf, newLen: INTEGER): INTEGER RAISES {Errno.E, VMError.E};
		 
END Sphinx.
