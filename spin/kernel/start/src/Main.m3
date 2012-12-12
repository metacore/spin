(* 
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)

(* "Main"

   The compiler is aware of this module name and calls its main body
   last of all main bodies.

   SPIN bootstrap path:
    arch dependent bootstrap calls
    setup_main()    -> kernel/sal/TARGET/TARGET_subr.c calls
    salhook_main_program()   -> kernel/start/src/TARGET/SpinProgram.c calls
    spin_start()    -> kernel/start/src/TARGET/SpinProgram.c calls
    spin_init()     -> kernel/spincore/TARGET/_m3main.c calls
    RTLinker.main() -> kernel/m3core_sa/src/runtime/common/RTLinker.m3
        
   RTLinker initializes the runtime and calls all the mainbodies of
   the modules in the system.  The SPIN modified RTLinker first
   initializes all of the runtime and spin core services.  Then
   RTLinker runs all mainbodies and these mainbodies can use all
   runtime and SPIN core services.

   The normal M3 compiler main body calling order cannot be controlled
   directly by users. It tries to evaluate a graph of dependencies and
   the resulting order changes and was often incorrect for the SPIN
   core.  Ordering the core init routines in an explicitly called
   procedure, Init(), worked around this problem.

   RTLinker initializes the spin core services by calling
   SpinStart.Init() which is exported by Boot module.
   Boot.m3 exports SpinStart. 
   Main's interface cannot be imported or exported.

   Initializing the SPIN services early allows regular main bodies to
   use SPIN services.  This allows statically linked code to use main
   bodies just as the dynamic code does.

   Main's main body is called last of all main bodies.  This is not
   changed.  It starts the primary scheduler, which is the final
   bootstrap step.
*)

(* HISTORY
 * 31-May-97  David Becker at the University of Washington
 *	Moved remaining Init calls into Boot.m3
 *
 * 24-Feb-97  Emin Gun Sirer (egs) at the University of Washington
 *	Added DebugOption.VerboseInit.
 *
 * 25-Sep-96  Marc Fiuczynski (mef) at the University of Washington
 *	Added CoreNS and SpinControl initialization.
 *
 * 20-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	Fixed up comments and bootstrap path description.
 *
 * 07-Apr-96  Przemek Pardyak (pardy) at the University of Washington
 *	The collector is now enabled in Boot.m3 before Modula-3 linker is
 *	initialized.
 *
 * 01-Apr-96  Emin Gun Sirer (egs) at the University of Washington
 *	Sched -> SchedPrivate
 *
 * 28-Oct-95  Brian Bershad (bershad) at the University of Washington
 *	Made Unsafe.
 *
 * 21-Sep-95  Stefan Savage (savage) at the University of Washington
 *    Call IntializeSALServices
 *
 * 22-Aug-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added debug flag warning.
 *	Changed to use to coresize instead of coreend for fast range check.
 *
 * 07-Aug-95  Stefan Savage (savage) at the University of Washington
 *	Added measurement and debug interfaces
 *
 * 07-Aug-95  Brian Bershad (bershad) at the University of Washington
 *	Moved "postcore" functions out into Boot.
 *
 * 03-Aug-95  Stefan Savage (savage) at the University of Washington
 *	Added memory management modules
 *
 * 01-Aug-95  Brian Bershad (bershad) at the University of Washington
 *	revised heavily for changes in incarnation ordering and naming.
 *
 * 06-Jul-95  Emin Gun Sirer (egs) at the University of Washington
 *	Changed to disable interrupts and GC during startup. Got
 *      rid of SpinCore et al.
 *
 * 12-Apr-95  Marc Fiuczynski (mef) at the University of Washington
 *	Last module to be initialized by M3 runtime.  Starts up
 *	spinshell.  In the future it will replace
 *	mk/kernel/kern/startup.c.
 *
 *)

MODULE Main;
IMPORT SchedPrivate;

BEGIN
    (* Shazam *)
    SchedPrivate.Start();
END Main.
