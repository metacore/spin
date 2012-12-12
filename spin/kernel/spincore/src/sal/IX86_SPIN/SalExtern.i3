(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 28-Aug-97  Marc Fiuczynski (mef) at the University of Washington
 *	Added an argument to the Debugger() definition.
 *
 * 23-Jun-97  Marc Fiuczynski (mef) at the University of Washington
 *	 Added interface to salhoold_clock_intr so that the
 *	 TrapPrivate module can redirect clock interrupts
 *	 dynamically. 
 *
 * 31-May-97  David Becker at the University of Washington
 *	Renamed SALExtern.i3 to SalExtern.i3.
 *	Collected most sal externs into here.
 *
 # 28-May-96  David Becker (becker) at the University of Washington
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
IMPORT Ctypes, Word; (*m3 runtime*)
IMPORT MachineDep, CPU, PhysAddr, VirtAddr, Protection, Clock; (*spin*)

<*EXTERNAL*>
PROCEDURE bcopy(src: Ctypes.void_star; dst: Ctypes.const_void_star; n: Ctypes.long);

<*EXTERNAL*>
PROCEDURE bzero(dst: Ctypes.void_star; n: Ctypes.long);

(*
<*EXTERNAL*>PROCEDURE memcpy(dst: Ctypes.void_star; src: Ctypes.const_void_star; n: Ctypes.long): Ctypes.void_star;
<*EXTERNAL*>PROCEDURE memset(dst: Ctypes.void_star; c: Ctypes.int; n: Ctypes.long) : Ctypes.void_star;
<*EXTERNAL*>PROCEDURE strlen(str: Ctypes.const_char_star) : Ctypes.long;
 *)

(*
 * Implemented in: arch/alpha/clock.c
 * Function: Initialize clock vis system time of day register
 *)
<*EXTERNAL*>
PROCEDURE inittodr (base: MachineDep.TimeUnit);

(*
 * Implemented in: <SYS>vers.c
 * Function: Sal version information
 *)
<*EXTERNAL*>
VAR version: Ctypes.const_char_star;
<*EXTERNAL*>
VAR version_version: Ctypes.const_char_star;
<*EXTERNAL*>
VAR version_major: INTEGER;
<*EXTERNAL*>
VAR version_minor: INTEGER;
<*EXTERNAL*>
VAR version_builder: Ctypes.const_char_star;
<*EXTERNAL*>
VAR version_build_dir: Ctypes.const_char_star;
<*EXTERNAL*>
VAR copyright: Ctypes.const_char_star;

(* 
 * Implemented in kern/subr_prf.c
 * Function: Throw a character onto the console.
 *)
<*EXTERNAL "cnputc"*>
PROCEDURE putchar(char: CHAR);
<*EXTERNAL*>
PROCEDURE printf(string: ADDRESS; arg:INTEGER); (* baby printf for m3 *)

(*
 * Implemented in: arch/alpha/locore.s
 * Function: Issues a processor breakpoint (vector to debugger)
 *)
<*EXTERNAL "Debugger"*>PROCEDURE Breakpoint(msg: Ctypes.char_star := NIL);

(*
 * Implemented in: arch/alpha/locore.s
 * Function: Halts CPU
 *)
<*EXTERNAL "standalone_halt"*>PROCEDURE standalone_halt();

(*
 * Implemented in: arch/alpha/clock.c
 * Function: Convert nice timeval structure to secs since GMT.
 *)
<*EXTERNAL*>
VAR time : Clock.TimeVal;


(* ************************************** *)
(* Non-preemptive SAL code ranges         *)
(* ************************************** *)

(* CAUTION: synchronize this constant with the base address of the text
 * segment of the Sal!
 *)
CONST salbegin = 16_F0100000; 
(* "salbegin" is address of where text begins. *)

<*EXTERNAL "spin_init"*>
PROCEDURE salend();
(* "salend" is not an actual procedure.  It is used to demarcate the
   sal code region.  In this case, we always know that _m3main.o is 
   linked in immediately following SAL, and spin_init is the first
   procedure defined in that file. *)

(* ************************************** *)
(* Stack limits for GC'ing boot stack     *)
(* ************************************** *)

<*EXTERNAL*>
VAR boot_stack_bottom: ADDRESS;


(* Prints info about malloc'd heap *)
<*EXTERNAL*>
PROCEDURE mallocstats();

<*EXTERNAL*>PROCEDURE kmeminit_thread();

<*EXTERNAL*>
VAR cold: BOOLEAN;  (* suppress malloc SPL warning *)


<*EXTERNAL*>
VAR
  (* traced heap *)
  traced_start   : ADDRESS;
  traced_end     : ADDRESS;
  ptraced_start  : INTEGER;
  traced_size    : INTEGER;

<*EXTERNAL*>
VAR
  refcount_start : ADDRESS;
  refcount_end   : ADDRESS;
  
<*EXTERNAL*>PROCEDURE spin_malloc(size: INTEGER): ADDRESS;
(* BUGBUG hack [savage] *)
<*EXTERNAL*>
VAR pmem_start      : Word.T;
<*EXTERNAL*>
VAR pmem_size       : Word.T;

<*EXTERNAL*>
PROCEDURE hardclock();
<*EXTERNAL "salhook_clock_intr"*> 
VAR ClockTick: PROCEDURE (VAR ss: CPU.SavedState);

TYPE InAddr = RECORD
  bytes: ARRAY [0..3] OF CHAR;
END;

<*EXTERNAL*>
PROCEDURE fuword(address: ADDRESS): Word.T;

<*EXTERNAL*>
VAR hz: INTEGER;
<*EXTERNAL*>
VAR pentium_mhz: INTEGER;

<*EXTERNAL*>
VAR kernel_pmap: Word.T;
<*EXTERNAL*>
VAR current_pmap: Word.T;

<*EXTERNAL*>
PROCEDURE pmap_create(size: Word.T): PhysAddr.Address;
<*EXTERNAL*>
PROCEDURE pmap_destroy(pmap: PhysAddr.Address);
<*EXTERNAL*>
PROCEDURE Activate(pmap: PhysAddr.Address);

<*EXTERNAL*>
PROCEDURE pmap_pte(pmap: PhysAddr.Address;
				 v: VirtAddr.Address): Word.T;

<*EXTERNAL*>
PROCEDURE pmap_get_phys_and_protection(pmap: PhysAddr.Address;
			     v: VirtAddr.Address;
			     VAR frame : PhysAddr.Address;
			     VAR prot : Protection.T);

<*EXTERNAL*>
PROCEDURE pmap_enter(pmap: PhysAddr.Address;v: VirtAddr.Address;
	             p: PhysAddr.Address; prot: Protection.T; 
		     wired: Word.T; access: Protection.T);

<*EXTERNAL*>
PROCEDURE pmap_protect(pmap: PhysAddr.Address; 
		       begin: VirtAddr.Address;
		       end: VirtAddr.Address; prot: Protection.T);

<*EXTERNAL*>
PROCEDURE pmap_remove(pmap: PhysAddr.Address; 
                         start: VirtAddr.Address;
                           end: VirtAddr.Size);

<*EXTERNAL*>
PROCEDURE pmap_fault_on(addr: PhysAddr.Address;
			      reftype: Word.T): Word.T;

<*EXTERNAL*>
PROCEDURE checkpte(pmap: PhysAddr.Address;
				 addr: PhysAddr.Address);

<*EXTERNAL*>
PROCEDURE phys_to_kseg(phys : PhysAddr.Address) : VirtAddr.Address;

<* EXTERNAL *>
PROCEDURE kseg_to_phys(virt : VirtAddr.Address): PhysAddr.Address;

<*EXTERNAL*>
PROCEDURE pmap_extract(pmap: PhysAddr.Address; 
                                        v: VirtAddr.Address): PhysAddr.Address;

<*EXTERNAL*>
PROCEDURE pmap_change_wiring(pmap : PhysAddr.Address; 
                                           virt : VirtAddr.Address;
		                          wired : BOOLEAN);

<*EXTERNAL*>
PROCEDURE pmap_remove_all(p: PhysAddr.Address; x: BOOLEAN);
(* unmap "p" from whatever pmaps it's mapped to. There is actually no
  "x" parameter in pmap.c, but it's there because DEC UNIX version of pmap
  requires it, and I want the interface to be consistent *)

<*EXTERNAL*>
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
