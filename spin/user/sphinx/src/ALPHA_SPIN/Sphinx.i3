(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 *  HISTORY
 * 24-Jul-97  Yasushi Saito (yasushi) at the University of Washington
 *	Use NameServer.Name instead of TEXT
 * 14-Jul-97  Yasushi Saito (yasushi) at the University of Washington
 *	Removed unused pragmas.
 * 25-Jul-97  Tsutomu Owa (owa) at the University of Washington
 *	Added Setitimer dummy routine.
 * 13-Jun-97  David Becker at the University of Washington
 *      Added Error.E to raises list of all procs that use File
 * 31-May-97  David Becker at the University of Washington
 *      Unified Socket.Error and Error.E into Errno exception 
 *      Replace SAL with Kernel, Clock and CPU interfaces
 *
 * 19-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	Moved Profile to usyscall.
 * 16-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	Added truncate, ftruncate
 * 14-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Added mmap, munmap, getrusage.
 *
 * 6-21-96  becker at the University of Washington
 *	Aligned in osf libc syscall order
 *	added Fork
 *	Changed WaitInternal to use osf args.
 *	Rename Spawn to Exec
 *	Commented out gettimeofday because then dlib_user.h will not compile
 *
 * 11-Jun-96 oystr at the University of Washington
 *	All syscalls, including Open, now return an integer.
 *	Add ReturnFH dummy procedure so FH's automatically
 *	internalize/externalized.  WARNING:  Do *not* change
 *	the relative position of procedures in this interface.
 *	Syscall vectors are assigned by text position.
 *
 * 05-Jun-96  Yasushi Saito (yasushi) at the University of Washington
 *	All syscalls now return a value.
 * 29-May-96  Stefan Savage (savage) at the University of Washington
 *	Changed break signature to allow negative length and changed
 *	Mprotect signature to change errno.
 *
 * 20-May-96 oystr at the University of Washington
 *	Added Select, Sleep, fixed up GetTimeOfDay.
 *
 * 14-May-96  Yasushi Saito (yasushi) at the University of Washington
 *	Added lots of exceptions.
 *
 * 17-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Added mprotect and signal
 *
 * 13-Jan-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(*
 *  This file defines the Sieg system call interface of Sphinx UNIX emulation
 *  library.
 *  
 *)
INTERFACE Sphinx;
IMPORT FileStat, Word, CPU;
IMPORT SocketRep;
IMPORT Ctypes, Strand;
IMPORT Error, Errno, VMError;
IMPORT StatFs;
IMPORT Clock;
IMPORT NameServer;
<*PRAGMA NO_SMALL_VAR_OPTIMIZATION*>
<*NO_SMALL_VAR_OPTIMIZATION*>
<*EPILOG_BRANCH_ON_REGISTER _Seterrno*>
<*INTERFACE_PROC_BASE 1*>

CONST Brand = "Sphinx"; 

CONST
  MAP_PRIVATE = 2;
  MAP_ANONYMOUS = 16_10;        (* man an unnamed region *)
  MAP_FIXED = 16_100;           (* map addr must be exactly as specified *)


TYPE
  sockaddrT = SocketRep.sockaddr;
  sigaction = RECORD
    sa_handler: Word.T; (* Address of handler *)
    sa_mask: Ctypes.unsigned_int;
    sa_flags: Ctypes.int;
  END;
  
  TimeZone = RECORD
    tz_minuteswest: Ctypes.unsigned_int;
    tz_dsttime: Ctypes.unsigned_int;
  END;
  
  ItimerVal = RECORD
    interval, value: Clock.TimeVal;
  END;

  Rusage = RECORD
    utime, stime: Clock.TimeVal;
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
    
<*SYNONYMS _exit*>  
PROCEDURE Exit(status: INTEGER);
  
<*SYNONYMS fork,__fork*>
PROCEDURE Fork(<*INTERNAL*>us: Strand.T;
	       <*INTERNAL*>VAR s: CPU.SavedState)
  RAISES {Errno.E, VMError.E};

<*SYNONYMS read,__read*>  
PROCEDURE Read(fh, addr, size: INTEGER)
 : INTEGER RAISES {Errno.E, VMError.E, Error.E};
  
<*SYNONYMS write,__write*>  
PROCEDURE Write(fh, addr, size: INTEGER)
 : INTEGER RAISES {Errno.E, VMError.E, Error.E};
  

(* old open *)
<*PROCID 6*>
<*SYNONYMS close,__close*>  
PROCEDURE Close(fd: Word.T): INTEGER RAISES {Errno.E, Error.E};
  (* Close has to deal with the extern ref directly. So it takes
   "Word.T" instead of "FH". *)

PROCEDURE Waitpid(pid: INTEGER; statusAddr: INTEGER; opt: INTEGER)
 : INTEGER RAISES {Errno.E, VMError.E};
  (* if PID = -1, then wait any chid proc *)
  

(* old_creat *)
<*PROCID 9*>
PROCEDURE Link(<*CTEXT*>src: TEXT;
	       <*CTEXT*>dest: TEXT)
 : INTEGER RAISES {Errno.E, VMError.E};

PROCEDURE Unlink(<*CTEXT*>READONLY path: NameServer.Name): INTEGER
  RAISES {Errno.E, NameServer.Error};

(* execv *)
<*PROCID 12*>
  
PROCEDURE Chdir(<*CTEXT*>path: TEXT): INTEGER RAISES {Errno.E};

PROCEDURE Fchdir(fd: INTEGER): INTEGER RAISES {Errno.E};
  
PROCEDURE Mknod(<*CTEXT*>path: TEXT; mode, dev: INTEGER)
  	: INTEGER RAISES {Errno.E};

PROCEDURE Chmod(<*CTEXT*>path: TEXT; mode: INTEGER) 
  	: INTEGER RAISES {Errno.E};
  
PROCEDURE Chown(<*CTEXT*>path: TEXT;
		pid, gid: INTEGER): INTEGER RAISES {Errno.E};
  
(* obreak *)
PROCEDURE Break (newBreak: Word.T): INTEGER RAISES {VMError.E};

(* getfsstat *)

<*PROCID 19*>
PROCEDURE Seek(fh: INTEGER; off, whence: INTEGER): INTEGER RAISES {Errno.E, Error.E};
  
PROCEDURE Getpid(<*INTERNAL*>us: Strand.T;
		 <*INTERNAL*>VAR s: CPU.SavedState);


(* mount *)
(* unmount *)
<*PROCID 23*>
PROCEDURE Setuid(uid: INTEGER): INTEGER RAISES {Errno.E};

PROCEDURE Getuid(<*INTERNAL*>us: Strand.T;
		 <*INTERNAL*>VAR s: CPU.SavedState);


(* exec_with_loader *)
(* ptrace *)
(* recvmsg *)
(* sendmsg *)
  
<*PROCID 33*>
PROCEDURE Access(<*CTEXT*>path: TEXT; how: INTEGER)
  	: INTEGER RAISES {Errno.E, Error.E};

(* chflags *)
(* fchflags *)
(* sync *)

<*PROCID 37*>  
PROCEDURE Kill(pid, signo: INTEGER): INTEGER RAISES {Errno.E};
  
(* old_stat *)

<*PROCID 39*>    
PROCEDURE Setpgid(pid, pgid: INTEGER) RAISES {Errno.E};

(* old_lstat *)

<*PROCID 41*>      
PROCEDURE Dup(fh: INTEGER): INTEGER RAISES {Errno.E, Error.E};

PROCEDURE Pipe(<*INTERNAL*>us: Strand.T;
	       <*INTERNAL*>VAR s: CPU.SavedState)
  RAISES {Errno.E};

(* set_program_attributes *)
(* profil *)
  
<*PROCID 45*>      
PROCEDURE Open(<*CTEXT*>READONLY path: NameServer.Name;
	       mode, creat_mode: INTEGER): INTEGER
  RAISES {Errno.E, Error.E, NameServer.Error};
  
(* old osigaction  *)

<*PROCID 47*>        
PROCEDURE Getgid(<*INTERNAL*>us: Strand.T;
		 <*INTERNAL*>VAR s: CPU.SavedState);
  

PROCEDURE Sigprocmask(how: INTEGER; mask: Ctypes.unsigned_long): INTEGER
  RAISES {Errno.E};
  

PROCEDURE Getlogin(n: INTEGER; ptr: Word.T)
	: INTEGER RAISES {Errno.E, VMError.E};

PROCEDURE Setlogin(): INTEGER RAISES {Errno.E};


(* sigpending *)
(*  53  ?? *)
  
<*PROCID 54*>
PROCEDURE Ioctl(fh, cmd, arg: INTEGER) : INTEGER
  RAISES {Errno.E, VMError.E, Error.E};


(* reboot *)
(* revoke *)
<*PROCID 57*>
PROCEDURE Symlink(<*CTEXT*>src: TEXT;
		  <*CTEXT*>dest: TEXT)
 : INTEGER RAISES {Errno.E, VMError.E};

PROCEDURE Readlink(<*CTEXT*>path: TEXT; buf, size: INTEGER)
 : INTEGER RAISES {Errno.E, VMError.E};
  
PROCEDURE Execve (<*INTERNAL*>us: Strand.T;
		  <*INTERNAL*>VAR s: CPU.SavedState)
  RAISES {Errno.E, VMError.E, Error.E};
  

PROCEDURE Umask(mask: INTEGER): INTEGER RAISES {Errno.E};

(* chroot *)
(* old_fstat *)

<*PROCID 63*>
PROCEDURE Getpgrp(): INTEGER;

PROCEDURE Getpagesize(): INTEGER;

(* mremap *)
(* vfork *)
  
<*PROCID 66*>
PROCEDURE Vfork(<*INTERNAL*>us: Strand.T;
		<*INTERNAL*>VAR s: CPU.SavedState)
  RAISES {Errno.E, VMError.E};

PROCEDURE Stat(<*CTEXT*>READONLY path: NameServer.Name;
               <*OUT*>VAR s: FileStat.T): INTEGER
  RAISES {Errno.E, Error.E, NameServer.Error};
  
PROCEDURE Lstat(<*CTEXT*>READONLY path: NameServer.Name;
		<*OUT*>VAR s: FileStat.T): INTEGER
  RAISES {Errno.E, Error.E, NameServer.Error};
  

(* sbrk *)
(* sstk *)
<*PROCID 71*>
PROCEDURE Mmap(addr, len, prot, flags, fd, off: INTEGER): INTEGER
  RAISES {Errno.E, VMError.E};
  
(* ovadvise *)

<*PROCID 73*>

PROCEDURE Munmap(addr, len: INTEGER): INTEGER RAISES {Errno.E, VMError.E};
  
PROCEDURE Mprotect(addr: INTEGER; size: INTEGER; prot: INTEGER): INTEGER
  RAISES {VMError.E};

PROCEDURE Madvise (addr, len, behav: INTEGER): INTEGER RAISES {Errno.E};

(* old_vhangup *)
(* kmodcall *)
(* mincore *)
  
<*PROCID 79*>  
PROCEDURE Getgroups(n: INTEGER; ptr: Word.T) : INTEGER
  RAISES {Errno.E, VMError.E};
  

PROCEDURE Setgroups(n: INTEGER; ptr: Word.T) : INTEGER
  RAISES {Errno.E, VMError.E};

(* old_getpgrp *)
(* setpgrp *)
<*PROCID 83*>
PROCEDURE Setitimer(which: INTEGER; VAR value: ItimerVal; ovalue: INTEGER)
	: INTEGER RAISES {Errno.E};

(* old_wait *)
(* table *)

<*PROCID 86*>    
PROCEDURE Getitimer(which: INTEGER; VAR value: ItimerVal)
	: INTEGER RAISES {Errno.E};
  
PROCEDURE Gethostname(ptr: Word.T; len: INTEGER): INTEGER
  RAISES {Errno.E, VMError.E};
  
PROCEDURE Sethostname(ptr: Word.T; len: INTEGER): INTEGER
  RAISES {Errno.E, VMError.E};

PROCEDURE Getdtablesize(): INTEGER;

PROCEDURE Dup2(fh: INTEGER; new: INTEGER): INTEGER RAISES {Errno.E, Error.E};

PROCEDURE Fstat(fh: INTEGER;
		<*OUT*>VAR s: FileStat.T): INTEGER RAISES {Errno.E, Error.E};

PROCEDURE Fcntl(fh: INTEGER; cmd: INTEGER; arg: INTEGER)
 : INTEGER RAISES {Errno.E, Error.E};

PROCEDURE Select(nfds, inputs, outputs, excepts, tv: INTEGER)
	: INTEGER RAISES {Errno.E, VMError.E};

(* poll *)
(* fsync *)
(* setpriority *)
<*PROCID 97*>
PROCEDURE CreateSocket(addrFamily, type, protocol: INTEGER)
  : INTEGER RAISES {Errno.E, Errno.E};

PROCEDURE Connect(fh: INTEGER; address: INTEGER)
  	: INTEGER RAISES {Errno.E, VMError.E, Errno.E};
  
PROCEDURE Accept(fh: INTEGER; address: INTEGER; VAR length: INTEGER): INTEGER
  RAISES {Errno.E, VMError.E, Errno.E};
  
  
(* old_accept *)
<*PROCID 101*>  
PROCEDURE Send(fh: INTEGER; message: INTEGER;
	       length, flags: INTEGER)
 : INTEGER RAISES {Errno.E, VMError.E, Errno.E};

PROCEDURE Recv(fh: INTEGER; buffer: INTEGER;
	       length, flags: INTEGER)
 : INTEGER RAISES {Errno.E, VMError.E, Errno.E};


PROCEDURE Sigreturn(<*INTERNAL*>VAR s: CPU.SavedState;
		    context: Word.T) RAISES {VMError.E};


PROCEDURE Bind(fh: INTEGER; address: INTEGER)
 : INTEGER RAISES {Errno.E, VMError.E, Errno.E};

PROCEDURE SetSockOpt( fd: INTEGER; level, opt_name: INTEGER;
		     opt_VAL: INTEGER;
		     opt_len: INTEGER)
	: INTEGER RAISES {Errno.E, VMError.E, Errno.E};

PROCEDURE Listen(fd: INTEGER; backlog: INTEGER)
 : INTEGER RAISES {Errno.E, Errno.E};
  
(* plock *)
(* old_sigvec *)
<*PROCID 109*>
PROCEDURE Sigblock(mask: Ctypes.unsigned_long)
	: Ctypes.unsigned_long RAISES {Errno.E};

PROCEDURE Sigsetmask(mask: Ctypes.unsigned_long)
	: Ctypes.unsigned_long RAISES {Errno.E};

PROCEDURE Sigsuspend(mask: INTEGER) RAISES {Errno.E};

PROCEDURE Sigstack (in, out: Word.T): INTEGER RAISES {Errno.E, VMError.E};

  (* old_recvmsg *)
  (* old_sendmsg *)
  (* obsolete vtrace  *)

<*PROCID 116*>
PROCEDURE Gettimeofday(tv, tz: INTEGER) RAISES {VMError.E};

PROCEDURE Getrusage(who: INTEGER; <*OUT*>VAR r: Rusage): INTEGER
  		RAISES {Errno.E, VMError.E};
  

(* getsockopt *)
(* unused *)
<*PROCID 120*>		    
PROCEDURE Readv(fd: INTEGER; ptr: INTEGER; n: INTEGER): INTEGER
  RAISES {Errno.E, VMError.E, Error.E};
PROCEDURE Writev(fd: INTEGER; ptr: Word.T; n: INTEGER): INTEGER
  RAISES {Errno.E, VMError.E, Error.E};
  
(* settimeofday *)
(* fchown *)
(* fchmod *)
<*PROCID 125*>		    
PROCEDURE Recvfrom(fh: INTEGER;
		   buffer, length: INTEGER;
		   flags: INTEGER;
		   address, addrlen: INTEGER)
 : INTEGER RAISES {Errno.E, VMError.E, Errno.E};


PROCEDURE Setreuid(ruid, euid: INTEGER): INTEGER RAISES {Errno.E};
PROCEDURE Setregid(rgid, egid: INTEGER): INTEGER RAISES {Errno.E};

PROCEDURE Rename(<*CTEXT*>from: TEXT;
		 <*CTEXT*>to: TEXT): INTEGER RAISES {Errno.E};

PROCEDURE Truncate(<*CTEXT*>path: TEXT; len: INTEGER)
 : INTEGER RAISES {Errno.E, Error.E};
PROCEDURE Ftruncate(fd, len: INTEGER): INTEGER RAISES {Errno.E, Error.E};
  
(* flock *)
<*PROCID 132*>
PROCEDURE Setgid(gid: INTEGER): INTEGER RAISES {Errno.E};

PROCEDURE Sendto(fh, message, len, flags, dest, destLen: INTEGER): INTEGER
  RAISES {Errno.E, VMError.E, Errno.E};
  
PROCEDURE ShutDown ( fd: INTEGER; how: INTEGER )
	: INTEGER RAISES {Errno.E, Errno.E};
  
(* socketpair *)
  
(* mkdir *)
(* rmdir *)
(* utimes *)
(* obsolete 4.2 sigreturn  *)
(* adjtime *)
  
<*PROCID 141*>  
PROCEDURE GetPeerName(  fd: INTEGER;
		      addr   : INTEGER;
			addr_len: INTEGER)
	: INTEGER RAISES {Errno.E, VMError.E, Errno.E};


PROCEDURE Gethostid(): INTEGER;
  
PROCEDURE Sethostid(): INTEGER RAISES {Errno.E};

TYPE Rlimit = RECORD
  cur, max: Ctypes.unsigned_long;
END;
  
PROCEDURE Getrlimit(type: INTEGER; VAR l: Rlimit): INTEGER RAISES {Errno.E};
  
PROCEDURE Setrlimit(type: INTEGER; VAR l: Rlimit): INTEGER RAISES {Errno.E};

(* old_killpg *)

<*PROCID 147*>  
PROCEDURE Setsid (): INTEGER RAISES {Errno.E};
  
(* quotactl *)
(* oldquota *)

<*PROCID 150*>
PROCEDURE Getsockname(fh: INTEGER;
		      address: INTEGER;
		      VAR length: CARDINAL)
 : INTEGER RAISES {Errno.E, VMError.E, Errno.E};

(* empty *)

(* pid_block *)
(* pid_unblock *)
(* empty *)
<*PROCID 156*>
PROCEDURE Sigaction(signo: INTEGER; naction, oaction, tramp: Word.T)
 : INTEGER RAISES {Errno.E,VMError.E};

(* sigwaitprim *)
(* nfssvc *)
<*PROCID 159*>
PROCEDURE Getdirentries(fh: INTEGER;
			t: INTEGER;
			size: INTEGER;
			<*OUT*>VAR basep: INTEGER)
 : INTEGER RAISES {Errno.E, VMError.E};

PROCEDURE Statfs(<*CTEXT*>path: TEXT; VAR buf: StatFs.T; len: INTEGER)
   : INTEGER RAISES {Errno.E, Error.E};

PROCEDURE Fstatfs(fd: INTEGER; VAR buf: StatFs.T; len: INTEGER)
   : INTEGER RAISES {Errno.E, Error.E};

(* empty *)
(* async_daemon *)
(* getfh *)
  
<*PROCID 165*>
PROCEDURE Getdomainname(ptr: Word.T; len: INTEGER)
       : INTEGER RAISES {Errno.E, VMError.E};
  
PROCEDURE Setdomainname(ptr: Word.T; len: INTEGER)
       : INTEGER RAISES {Errno.E, VMError.E};

(* empty *)

(* exportfs *)

(* empty *)

(* alt_plock *)

(* empty *)

(* getmnt *)

(* empty *)

(* alt_sigpending *)
(* alt_setsid *)

(* empty *)

(* swapon *)
(* msgctl *)
(* msgget *)
(* msgrcv *)
(* msgsnd *)
(* semctl *)
(* semget *)
(* semop *)

TYPE Utsname = RECORD
  sysname, nodename, release, version, machine: ARRAY [0..31] OF CHAR;
END;
  
<*PROCID 207*>  
PROCEDURE Uname(<*OUT*>VAR buf: Utsname);
  
(* uname *)
(* lchown *)
(* shmat *)
(* shmctl *)
(* shmdt *)
(* shmget *)
(* mvalid *)
(* getaddressconf *)
(* msleep *)
(* mwakeup *)
(* msync *)
(* signal *)
(* utc_gettime *)
(* utc_adjtime *)
(* empty *)
(* security *)
(* kloadcall *)

(* empty *)

(* getpgid *)
(* getsid *)
(* sigaltstack *)
(* waitid *)
(* priocntlset *)
(* sigsendset *)
(* set_speculative *)
(* msfs_syscall *)
(* sysinfo *)
(* uadmin *)
(* fuser *)
(* proplist_syscall *)

(* empty *)

(* uswitch *)
(* usleep_thread *)
<*PROCID 251*>
PROCEDURE Usleep (newp, oldp: INTEGER): INTEGER RAISES {VMError.E};

(* audcntl *)
(* audgen *)
(* sysfs *)
(* subsys_info *)
(* getsysinfo *)
(* setsysinfo *)
(* afs_syscall *)
(* swapctl *)
(* memcntl *)

END Sphinx.
