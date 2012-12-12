(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *	Renamed SALExtern.i3 to SalExtern.i3.
 *	Collected most sal externs into here.
 *
 * 27-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed the definiton of salbegin and salend.  Moved corebegin
 *	and coreend to the CoreExtern interface.
 *      Removed SetupSALLimits and SetupCoreLimits procedures.
 *
 * 28-May-96  David Becker (becker) at the University of Washington
 *	putchar redirected to cnputc to save going thru the printf code
 *
 * 23-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Added hardclock procedure.
 *
 * 19-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Added ptraced_start hack for initial cut at mapped heap
 *
 * 10-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Deleted spinknio. Replaced it with indirection through the interrupt
 *	dispatch vector set up by SAL.
 *
 * 09-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Deleted atoi. It is superceded by TextRd and Lex.Int.
 *
 * 14-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Added support for accessing Todr and version string.
 *
 * 28-Oct-95  Brian Bershad (bershad) at the University of Washington
 *	Made unsafe to reflect externals.
 *
 * 27-Sep-95  Stefan Savage (savage) at the University of Washington
 *	Created
 *
 *)
UNSAFE (* This interface is unsafe because it defines externals. *)
INTERFACE SalExtern;

IMPORT Ctypes, VirtAddr, Word;
IMPORT PhysAddr;
IMPORT Protection;
IMPORT Clock;


<*EXTERNAL*>PROCEDURE bcopy(src: Ctypes.void_star; dst: Ctypes.const_void_star; n: Ctypes.long);

<*EXTERNAL*>PROCEDURE bzero(dst: Ctypes.void_star; n: Ctypes.long);


(*
 * Implemented in: arch/alpha/pal_lib.s
 * Function: Get a 64bit monotonic timestamp
 *)
<* EXTERNAL  *> PROCEDURE rpcc(): Word.T;

(*
 * Implemented in: standalone/upcalls.c
 * Function: Get the number of CPU cycles per second
 *)
<* EXTERNAL *> PROCEDURE get_rpb_clock(): INTEGER;

(*
 * Implemented in: standalone/upcalls.c
 * Function: Get the number 
 *)
<* EXTERNAL *> PROCEDURE get_rpb_counter(): INTEGER;

(*
 * Implemented in: arch/alpha/clock.c
 * Function: Initialize clock vis system time of day register
 *)
TYPE
  TimeUnit = Word.T;
<* EXTERNAL *> PROCEDURE inittodr (base: TimeUnit);


(*
 * Implemented in: <SYS>vers.c
 * Function: contains seconds since 1/1/1970 GMT of Sal build time.
 *)
<* EXTERNAL *> VAR version_build_time : TimeUnit;

(*
 * Implemented in: <SYS>vers.c
 * Function: Sal version information
 *)
<* EXTERNAL *> VAR version: Ctypes.const_char_star;
<* EXTERNAL *> VAR version_version: Ctypes.const_char_star;
<* EXTERNAL *> VAR version_major: INTEGER;
<* EXTERNAL *> VAR version_minor: INTEGER;
<* EXTERNAL *> VAR version_builder: Ctypes.const_char_star;
<* EXTERNAL *> VAR version_build_dir: Ctypes.const_char_star;
<* EXTERNAL *> VAR copyright: Ctypes.const_char_star;

(* 
 * Implemented in arch/alpha/locore.s
 * Function: to serve as a switchboard between io interrupts and their handlers
 *)
TYPE SCBT = RECORD 
  proc: PROCEDURE(arg: ADDRESS);
  arg : ADDRESS;
END;

<* EXTERNAL "_scb" *> VAR scb: ARRAY [0..2048] OF SCBT;

(* 
 * Implemented in kern/subr_prf.c
 * Function: Throw a character onto the console.
 *)
<* EXTERNAL cnputc *> PROCEDURE putchar(char: CHAR);
<* EXTERNAL *> PROCEDURE printf(string: ADDRESS; arg:INTEGER); (* baby printf for m3 *)

(*
 * Implemented in: arch/alpha/locore.s
 * Function: Issues a processor breakpoint (vector to debugger)
 *)
<* EXTERNAL gimmeabreak *>PROCEDURE Breakpoint();

(*
 * Implemented in: arch/alpha/hal/cpusw.c
 * Function: correctly deals with anticipated machine checks. Used for
 * device initialization
 *)
<* EXTERNAL mach_error *>PROCEDURE HandleMachineCheck(
                             phys:VirtAddr.Address;
			     phys_logout:VirtAddr.Address;
			     frame:VirtAddr.Address);

(*
 * Implemented in: arch/alpha/locore.s
 * Function: Halts CPU
 *)
<* EXTERNAL standalone_halt *>PROCEDURE standalone_halt();

(*
<* EXTERNAL ??? *>PROCEDURE HandleAlignmentFault(addr: Machine.Register);
 *)

(*
 * Implemented in: arch/alpha/clock.c
 * Function: Convert nice timeval structure to secs since GMT.
 *)

<* EXTERNAL *> VAR time : Clock.TimeVal;


(*
 * Device initializaton facilities 
 *)
<* EXTERNAL configure *>         PROCEDURE InitDevices();
<* EXTERNAL scc_enable_console*> PROCEDURE InitConsole();
<* EXTERNAL ttd_init *>		 PROCEDURE InitTeleDebugger();
<* EXTERNAL bootp_init *>	 PROCEDURE InitBootp();

(*
 * Cache control functions
 *)
<* EXTERNAL *> PROCEDURE imb();
<* EXTERNAL *> PROCEDURE cflush();

(* ************************************** *)
(* Non-preemptive SAL code ranges         *)
(* ************************************** *)

(* XXX 
   THIS CONSTANT MUST BE SYNCHRONIZED WITH sal/conf/template.mk ALPHA_TEXTBASE *)
CONST salbegin = 16_FFFFFC0000000000; 
(* "salbegin" is address of where text begins. *)

<* EXTERNAL spin_init *> PROCEDURE salend();

(* ************************************** *)
(* Stack limits for GC'ing boot stack     *)
(* ************************************** *)

<*EXTERNAL*> VAR boot_stack_bottom: ADDRESS;


(*
 * Implemented in: arch/alpha/clock.c
 * Function: Convert secs since 1/1/70 GMT to a nice timeval structure.
 *)

TYPE
   TM = RECORD
       tm_sec : Ctypes.int;
       tm_min : Ctypes.int;
       tm_hour: Ctypes.int;
       tm_mday: Ctypes.int;
       tm_mon : Ctypes.int;
       tm_year: Ctypes.int;
       tm_wday: Ctypes.int;
       tm_yday: Ctypes.int;
       tm_isdst: Ctypes.int;
       tm_gmtoff: Ctypes.int;
       tm_zone: Ctypes.char_star;
     END;

<* EXTERNAL *> PROCEDURE cvt_sec_to_tm(time: TimeUnit; VAR tm: TM);

(*
 * Implemented in: arch/alpha/clock.c
 * Function: Convert nice timeval structure to secs since GMT.
 *)

<* EXTERNAL *> PROCEDURE cvt_tm_to_sec(VAR tm: TM): TimeUnit;

(* Prints info about malloc'd heap *)
<*EXTERNAL*> PROCEDURE mallocstats();

<*EXTERNAL *>PROCEDURE kmeminit_thread();

<*EXTERNAL  *>VAR cold: BOOLEAN;  (* suppress malloc SPL warning *)


<*EXTERNAL*>VAR
  (* traced heap *)
  traced_start   : ADDRESS;
  traced_current : ADDRESS; 
  traced_end     : ADDRESS;

<*EXTERNAL*>
VAR
  refcount_start : ADDRESS;
  refcount_end   : ADDRESS;
  
<*EXTERNAL*>PROCEDURE spin_malloc(size: INTEGER): ADDRESS;
<* EXTERNAL *> PROCEDURE Printi (x: Ctypes.long);

<* EXTERNAL *> PROCEDURE Printx (x: Ctypes.long);

<* EXTERNAL *> PROCEDURE Prints (x: Ctypes.const_char_star);

<* EXTERNAL *> PROCEDURE Printc (x: CHAR);
(* BUGBUG hack [savage] *)
<* EXTERNAL *> VAR pmem_start      : Word.T;
<* EXTERNAL *> VAR pmem_size       : Word.T;
<* EXTERNAL *> VAR ptraced_start: Word.T;
<* EXTERNAL *> VAR traced_size: Word.T;

<* EXTERNAL *> PROCEDURE hardclock();

<* EXTERNAL *>VAR kernel_pmap: Word.T;
<* EXTERNAL *>VAR current_pmap: Word.T;

<* EXTERNAL *>PROCEDURE pmap_create(size: Word.T): PhysAddr.Address;
<* EXTERNAL *>PROCEDURE pmap_destroy(pmap: PhysAddr.Address);
<* EXTERNAL *>PROCEDURE Activate(pmap: PhysAddr.Address);

<* EXTERNAL *>PROCEDURE pmap_pte(pmap: PhysAddr.Address;
			      v: VirtAddr.Address): Word.T;
<* EXTERNAL *>PROCEDURE pmap_get_phys_and_protection(pmap: PhysAddr.Address;
			     v: VirtAddr.Address;
			     VAR frame : PhysAddr.Address;
			     VAR prot : Protection.T);
<* EXTERNAL *>
PROCEDURE pmap_enter(pmap: PhysAddr.Address;v: VirtAddr.Address;
	             p: PhysAddr.Address; prot: Protection.T; 
		     wired: Word.T; access: Protection.T);

<* EXTERNAL *>
PROCEDURE pmap_protect(pmap: PhysAddr.Address; 
		       begin: VirtAddr.Address;
		       end: VirtAddr.Address; prot: Protection.T);

<* EXTERNAL *>
PROCEDURE pmap_remove(pmap: PhysAddr.Address; 
		      start, end: VirtAddr.Address);

<* EXTERNAL *>
PROCEDURE pmap_remove_all(phys: PhysAddr.Address; force: BOOLEAN);
  
<* EXTERNAL *>PROCEDURE pmap_fault_on(addr: PhysAddr.Address;
			      reftype: Word.T): Word.T;

<* EXTERNAL *>PROCEDURE checkpte(pmap: PhysAddr.Address;
				 addr: PhysAddr.Address);

<* EXTERNAL *>
PROCEDURE phys_to_kseg(phys : PhysAddr.Address) : VirtAddr.Address;
  
<* EXTERNAL *>
PROCEDURE kseg_to_phys(virt : VirtAddr.Address): PhysAddr.Address;

<* EXTERNAL *>
PROCEDURE pmap_extract(pmap : PhysAddr.Address;
		       virt : VirtAddr.Address) : PhysAddr.Address;

<* EXTERNAL *>
PROCEDURE pmap_change_wiring(pmap : PhysAddr.Address; virt : VirtAddr.Address;
			     wired : BOOLEAN);

  
<* EXTERNAL *>
PROCEDURE copy_from_phys(src : PhysAddr.Address; dst : VirtAddr.Address;
			 cnt : INTEGER);
  
<*EXTERNAL*>
PROCEDURE copy_to_phys(src : VirtAddr.Address; dst : PhysAddr.Address;
		       cnt : INTEGER);

<*EXTERNAL*>
PROCEDURE pmap_copy_page(src, dest: VirtAddr.Address);
  
<*EXTERNAL*>
PROCEDURE pmap_page_protect(phys: PhysAddr.Address; prot: Protection.T);

<*EXTERNAL*>
PROCEDURE do_scan(pollfiles : ADDRESS; nfds: INTEGER;
		  (*OUT*)VAR retval: INTEGER;
		  READONLY atv: Clock.TimeVal;
		  hasTimeout: BOOLEAN): INTEGER;

END SalExtern.
