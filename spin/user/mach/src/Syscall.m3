(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *      Replace SAL with Kernel, Clock and CPU interfaces
 *
 * 11-Feb-97  Robert Grimm (rgrimm) at the University of Washington
 *      Removed Identity
 *
 * 20-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed the dispatcher interface.
 *
 * 20-Apr-96  Wilson Hsieh (whsieh) at the University of Washington
 *	PROCANY changes
 *
 * 04-Apr-96  David Dion (ddion) at the University of Washington
 *	Updated for spin-20
 *
 * 15-Jan-96  David Dion (ddion) at the University of Washington
 *	Changed handler installation to use Dispatcher interface.
 *
 * 28-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Changed to conform to the new syscall interface.
 *
 * 15-Dec-95  David Dion (ddion) at the University of Washington
 *	Updated to spin-8 and prepared for CVS.
 *
 * ??-Jul-95  David Dion (ddion) at the University of Washington
 *      Took over and expanded to handle OSF1 system calls.
 *
 * ??-Jul-95  David Becker (becker) at the University of Washington
 *      Created to handle system calls in an extension.
 *) 
MODULE Syscall;

IMPORT Strand, CPU;
IMPORT Fmt;
IMPORT Word;
IMPORT UserSpaceThread;
IMPORT Trap, Dispatcher;
IMPORT Sal; (* for Halt *)
IMPORT Clock;
IMPORT TimeHandlers;
IMPORT Text; 
IMPORT DeviceHandlers, VMHandlers, ThreadHandlers, MachHandlers, ExcHandlers;
IMPORT VMTaskSupport;
IMPORT IO, Textify;
IMPORT RTCollector;
(* IMPORT GC; *)(* for SanityCheck and Stat *)
IMPORT HandlerUtils;
FROM HandlerUtils IMPORT Debug;
FROM HandlerUtils IMPORT Print;

(* IMPORT SocketIntercept; *)

IMPORT Spy;
(* IMPORT Debugger; *)


CONST
  (* Print flags *)
  STR = 16_0;
  NUM = 16_1;
  STRAND = 16_2;


PROCEDURE Syscall(strand: Strand.T; VAR ms: CPU.SavedState) =
  VAR 
    t: Clock.TimeVal;
  BEGIN
    IF ms.v0 < 1000 AND ms.v0 # -61 THEN
      IF ms.v0 < 0 THEN
        IF Debug THEN Print("Syscall " & 
          Fmt.Int(ms.v0) & " from strand " & 
          Textify.Ref(strand) & ": "); END;
      ELSE
        IF Debug THEN Print("Syscall " & 
          Fmt.Int(ms.v0) & " strand " & 
          Textify.Ref(strand) & " ra 0x" &
          Fmt.Unsigned(ms.ra) & "\n"); END;
      END;
    END;
    CASE ms.v0 OF
    |  0 => HandlerUtils.Print("System call 0x0???\n");

    |  5 => IO.Put(Fmt.Char(VAL(ms.a0, CHAR)));

    | 2001 => (* justgettimeofday *)
      WITH space =UserSpaceThread.GetSpace(NARROW(strand,UserSpaceThread.T))DO
        Clock.TimeOfDay(t);
        WITH timeArray = VIEW(t.tv_sec, ARRAY[0..3] OF CHAR) DO
          IF NOT HandlerUtils.CopyOut(space, timeArray, ms.a0, 4) THEN
            HandlerUtils.PrintError("gettimeofday: CopyOut failed!  " & 
              "Continuing anyway.\n");
          END;
        END;
        WITH timeArray = VIEW(t.tv_usec, ARRAY[0..3] OF CHAR) DO
          IF NOT HandlerUtils.CopyOut(space, timeArray, ms.a0+4, 4) THEN
            HandlerUtils.PrintError("gettimeofday: CopyOut failed!  " & 
              "Continuing anyway.\n");
          END;
        END;
      END;
    | 1001 => (* set print debugging switch to value in ms.a0 *)
      ms.v0 := HandlerUtils.SetDebugSyscall(ms.a0);
    | 1004 => (* set HandlerUtil space debugging switch to value in ms.a0 *)
      (* ms.v0 := HandlerUtils.SetDebugSpace(ms.a0); *)
      HandlerUtils.Print("Space debugging not currently supported.\n");
    | 1100 => (* gc sanity *) 
      (* GC.SanityCheck(); *)
      (* Space.RWDebug := TRUE; *)
      (* DebugSpaceRead := TRUE; *)
      ms.v0 := KERN_SUCCESS;
    | 1101 => (* gc stat *) 
      (* Space.RWDebug := FALSE; *)
      (* DebugSpaceRead := FALSE; *)
      (* GC.Stat(); *)
      ms.v0 := KERN_SUCCESS;
    | 1102 => (* gc enable *) 
      (* GC.Enable(); *)
      RTCollector.Enable();
      ms.v0 := KERN_SUCCESS;
    | 1103 => (* gc disable *) 
      RTCollector.Disable();
      (* GC.Disable(); *)
      ms.v0 := KERN_SUCCESS;
    | 1105 => (* simple_wait *)
      HandlerUtils.SimpleWait();
      ms.v0 := KERN_SUCCESS;
    | 1006 => HandlerUtils.Print("0x" & Fmt.Unsigned(ms.a0) & "\n");
    | 1007 => HandlerUtils.Print(Fmt.Char(VAL(ms.a0, CHAR)));
    | 1206 => 
      (* sysprintstr(ms); *)
      sysprint(strand, ms, STR);
    | 1207 =>
      (* sysprintstrd(ms); *)
      sysprint(strand, ms, Word.Or(STR, NUM));
    | 1208 =>
      (* strandprintstrd(ms); *)
      sysprint(strand, ms, Word.Or(STR, Word.Or(NUM, STRAND)));
    | 1209 =>
      sysgetstr(strand, ms);
    | 1210 =>
      VMTaskSupport.MemUsage();
    | 1500 => (* Just stop the stupid machine *)
      HandlerUtils.Print("\tMAYDAY!!!  MAYDAY!!!  Abandon ship!!!\n\n");
      Sal.Halt();
      (* Debugger.Enter(); *)
    | 1600 => (* maintain_time *)
      TimeHandlers.MaintainTime(strand, ms);
    | 1601 => (* set_softclock *)
      IF Debug AND FALSE THEN
        Print("SetSoftclock: ms.a0 = " & Fmt.Unsigned(ms.a0) & "\n");
      END;
      (* HandlerUtils.PrintError("set_softclock called!\n"); *)
      TimeHandlers.SetSoftclock(strand, ms);
      IF Debug AND FALSE THEN
        Print("\t... returning " & Fmt.Unsigned(ms.v0) & "\n");
      END;      
    | 1602 => (* poke_softclock *)
      IF Debug AND FALSE THEN
        Print("PokeSoftclock.\n");
      END;
      (* HandlerUtils.PrintError("poke_softclock called!\n"); *)
      TimeHandlers.PokeSoftclock(strand, ms);
    | 1603 => (* get_cpu_clock_rate *)
      ms.v0 := CPU.Hertz();
    | 1700 => (* get_ether_packet *)
      DeviceHandlers.get_ether_packet(strand, ms);
    | 1804 => (* extension_init_timer *)
      TimeHandlers.TimerInit(ms.a0);
    | 1805 => (* extension_print_timer *)
      TimeHandlers.TimerPrint(ms.a0);
    | 1806 => (* extension_enable_timers *)
      TimeHandlers.TimersEnabled := TRUE;
    | 1807 => (* extension_disable_timers *)
      TimeHandlers.TimersEnabled := FALSE;
    | 1900 => (* start_short_circuit *)
      (*SocketIntercept.StartShortCircuit(strand, ms); *)
    | 1901 => (* stop_short_circuit *)
      (* SocketIntercept.StopShortCircuit(strand, ms); *)
    | 2000 => (* spy *)
      Print("Calling spy. ms.a0 = " & Fmt.Int(ms.a0) & "\n");
      IF ms.a0 = 0 THEN Spy.On(); END;
      IF ms.a0 = 1 THEN Spy.Off(); END;
      IF ms.a0 = 2 THEN Spy.Dump(); END;
    | 1 =>
      (*
       * This is UNIX exit.  But we're not quite ready for libc exit stuff
       * yet, so we want to call this earlier in our program.  Unfortunately,
       * mach_task_terminate expects ss.a0 to carry the port (extern ref)
       * of the task to terminate.  So we'll put it there using mach_task_self.
       *)
      IF Debug THEN Print("exit: calling task_terminate.\n"); END;
      MachHandlers.mach_task_self(strand, ms);
      ms.a0 := ms.v0;
      MachHandlers.task_terminate(strand, ms);
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;
      (*
       * Following cases are handlers for Mach system calls.
       *)
    | -25 =>
      IF Debug THEN Print("mach_msg_trap(): not implemented!\n"); END;
      HandlerUtils.PrintError("mach_msg_trap() called.\n");
      ms.v0 := 16_00000800; (* some error message *)
      EVAL HandlerUtils.SetDebugSyscall(1); (* turn on syscall debugging *)
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;
    | -26 =>
      IF Debug THEN Print("mach_reply_port():  spoofing\n"); END;
      ms.v0 := 0;
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;
    | -27 =>
      IF Debug THEN Print("mach_thread_self():\n"); END;
      MachHandlers.mach_thread_self(strand, ms);
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;
    | -28 =>
      IF Debug THEN Print("mach_task_self():\n"); END;
      MachHandlers.mach_task_self(strand, ms);
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;
    | -29 => 
      IF Debug THEN Print("mach_host_self():\n"); END;
      MachHandlers.mach_host_self(strand, ms);
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;
    | -31 => 
      IF Debug THEN Print("vm_write():\n"); END;
      VMHandlers.vm_write(strand, ms);
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;
    | -33 => 
      IF Debug THEN Print("vm_read():\n"); END;
      VMHandlers.vm_read(strand, ms);
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;
    | -34 => 
      IF Debug THEN Print("vm_inherit():\n"); END;
      VMHandlers.vm_inherit(strand, ms);
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;
    | -50 => 
      IF Debug THEN Print("device_read_inband():\n"); END;
      DeviceHandlers.device_read(strand, ms, TRUE);
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;
    | -51 =>
      IF Debug THEN Print("host_kernel_version():\n"); END;
      MachHandlers.host_kernel_version(strand, ms);
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;
    | -52 =>
      IF Debug THEN Print("thread_rendezvous():\n"); END;
      ThreadHandlers.thread_rendezvous(strand, ms);
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;
    | -53 =>
      IF Debug THEN Print("mach_port_move_member(): not implemented!\n"); END;
      ms.v0 := KERN_SUCCESS;
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;
    | -57 =>
      IF Debug THEN Print("vm_region():\n"); END;
      VMHandlers.vm_region(strand, ms);
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;
    | -58 =>
      IF Debug THEN Print("task_info(): not entirely implemented!\n"); END;
      MachHandlers.task_info(strand, ms);
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;
    | -61 =>
      (* IF Debug THEN Print("thread_switch():\n"); END; *)
      ThreadHandlers.thread_switch(strand, ms);
      (* IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END; *)
    | -63 =>
      IF Debug THEN Print("vm_statistics(): barely implemented!\n"); END;
      VMHandlers.vm_statistics(strand, ms);
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;
    | -64 =>
      IF Debug THEN Print("vm_map(): barely implemented!\n"); END;
      VMHandlers.vm_map(strand, ms);
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;
    | -65 => 
      IF Debug THEN Print("vm_allocate().\n"); END;
      VMHandlers.vm_allocate(strand, ms);
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;
    | -66 =>
      IF Debug THEN Print("vm_deallocate():\n"); END;
      VMHandlers.vm_deallocate(strand, ms);
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;
    | -67 =>
      IF Debug THEN Print("vm_protect():\n"); END;
      VMHandlers.vm_protect(strand, ms);
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;
    | -68 =>
      IF Debug THEN Print("task_create(): \n"); END;
      MachHandlers.task_create(strand, ms);
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;
    | -69 =>
      IF Debug THEN Print("task_terminate(): blocking strand\n"); END;
      MachHandlers.task_terminate(strand, ms);
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;
    | -70 =>
      IF Debug THEN Print("task_suspend(): \n"); END;
      MachHandlers.task_suspend(strand, ms);
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;
    | -71 =>
      IF Debug THEN Print("task_set_special_port(): spoofing\n"); END;
      ms.v0 := KERN_SUCCESS;
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;
    | -72 =>
      IF Debug THEN Print("mach_port_allocate(): spoofing\n"); END;
      MachHandlers.mach_port_allocate(strand, ms);
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;
    | -73 =>
      IF Debug THEN Print("mach_port_deallocate(): spoofing\n"); END;
      ms.v0 := KERN_SUCCESS;
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;
    | -74 =>
      IF Debug THEN Print("mach_port_insert_right(): spoofing\n"); END;
      ms.v0 := KERN_SUCCESS;
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;
    | -75 =>
      IF Debug THEN Print("mach_port_allocate_name(): spoofing\n"); END;
      ms.v0 := KERN_SUCCESS;
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;
    | -77 =>
      IF Debug THEN Print("device_read():\n"); END;
      DeviceHandlers.device_read(strand, ms, FALSE);
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;      
    | -78 =>
      IF TRUE THEN Print("device_read_request(): not implemented!\n"); END;
      ms.v0 := Failure;
      IF TRUE THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;      
    | -79 =>
      (* IF Debug THEN Print("device_write():\n"); END; *)
      IF Debug THEN Print("\n"); END; (* makes formatting nicer *)
      DeviceHandlers.device_write(strand, ms);
      (* IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;       *)
    | -80 =>
      IF TRUE THEN Print("device_write_request(): not implemented!\n"); END;
      ms.v0 := Failure;
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;      
    | -81 =>
      IF TRUE THEN Print("device_read_overwrite(): not implemented!\n"); END;
      ms.v0 := Failure;
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;      
    | -82 =>
      IF TRUE THEN Print("device_read_overwrite_request(): " & 
        "not implemented!\n"); END;
      ms.v0 := Failure;
      IF TRUE THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;      
    | -84 =>
      IF Debug THEN Print("device_open():\n"); END;
      DeviceHandlers.device_open(strand, ms); 
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;      
    | -85 =>
      IF Debug THEN Print("device_close():\n"); END;
      DeviceHandlers.device_close(strand, ms);
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;      
    | -86 =>
      IF TRUE THEN Print("device_map(): not implemented!\n"); END;
      ms.v0 := Failure;
      IF TRUE THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;      
    | -87 =>
      IF Debug THEN Print("device_set_status():\n"); END;
      DeviceHandlers.device_set_status(strand, ms);
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;      
    | -88 =>
      IF Debug THEN Print("device_get_status():\n"); END;
      DeviceHandlers.device_get_status(strand, ms);
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;      
    | -89 =>
      IF TRUE THEN Print("device_set_filter(): not implemented!\n"); END;
      ms.v0 := Failure;
      IF TRUE THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;      
    | -90 =>
      IF Debug THEN Print("exception_task_get_next_exception():\n"); END;
      ExcHandlers.exc_task_get_next_exc(strand, ms);
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;      
    | -91 =>
      IF Debug THEN Print("task_get_special_port():\n"); END;
      MachHandlers.task_get_special_port(strand, ms);
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;
    | -92 =>
      IF Debug THEN Print("task_get_master_ports():\n"); END;
      MachHandlers.task_get_master_ports(strand, ms);
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;
    | -93 =>
      IF Debug THEN Print("processor_set_default():\n"); END;
      MachHandlers.processor_set_default(strand, ms);
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;
    | -94 =>
      IF Debug THEN Print("host_processor_set_priv():\n"); END;
      MachHandlers.host_processor_set_priv(strand, ms);
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;
    | -95 =>
      IF Debug THEN Print("host_info():\n"); END;
      MachHandlers.host_info(strand, ms);
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;
    | -96 =>
      IF Debug THEN Print("thread_create():\n"); END;
      ThreadHandlers.thread_create(strand, ms);
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;
    | -97 =>
      IF Debug THEN Print("thread_set_state():\n"); END;
      ThreadHandlers.thread_set_state(strand, ms);
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;
    | -98 =>
      IF Debug THEN Print("thread_resume():\n"); END;
      ThreadHandlers.thread_resume(strand, ms);
      IF Debug THEN Print("\t... returning " &
        Fmt.Unsigned(ms.v0) & "\n\n"); END;
    ELSE
      IF ms.v0 > 0 THEN
        ExcHandlers.syscall_handler(strand, ms);
        IF Debug THEN Print("\t\t... returning " &
          Fmt.Unsigned(ms.v0) & "\n\n"); END;
      ELSE
        HandlerUtils.PrintError("NOT HANDLED.\n\n");
        ms.v0 := Failure;
      END;
    END;
  END Syscall;

PROCEDURE sysprint(strand: Strand.T; 
                   VAR ms: CPU.SavedState; flags: Word.T) =
  VAR
    userstringTxt: TEXT;
    userstring: ARRAY[0..79] OF CHAR;
    preciseArray: REF ARRAY OF CHAR;
    stringEnd: CARDINAL := LAST(userstring);
  BEGIN
    WITH caller = NARROW(strand, UserSpaceThread.T),
         space = UserSpaceThread.GetSpace(caller) DO
      IF NOT HandlerUtils.CopyIn(space, ms.a0, userstring, 80) THEN
        HandlerUtils.PrintError("CopyIn exception.\n");
        ms.v0 := Failure;
        RETURN;
      END;
    END;
    (*
     * Chances are the string to print will not be 80 characters long,
     * but that's how many we read over.  Find the end of the string,
     * alloc an array of that size, and create the TEXT instance from it.
     *)
    FOR i := FIRST(userstring) TO LAST(userstring) DO
      IF userstring[i] = '\000' THEN
        stringEnd := i;
        EXIT;
      END;
    END;
    preciseArray := NEW(REF ARRAY OF CHAR, stringEnd);
    preciseArray^ := SUBARRAY(userstring, 0, stringEnd);
    userstringTxt := Text.FromChars(preciseArray^);

    (*
     * Print different fields depending on flags.
     *)
    IF Word.And(flags, STRAND) # 0 THEN
      HandlerUtils.Print("0x" & Textify.Ref(strand) & ": ");
    END;
    HandlerUtils.Print(userstringTxt);
    IF Word.And(flags, NUM) # 0 THEN
      HandlerUtils.Print("0x" & Fmt.Unsigned(ms.a1) & "\n");
    END;
  END sysprint;

(*
 * Get an input line from the console.  The input should be written
 * to the address in ms.a0 and should be no longer than the number
 * of bytes passed in ms.a1.  Return the number of characters read.
 *)
PROCEDURE sysgetstr(strand: Strand.T; 
                    VAR ms: CPU.SavedState) = (* 1206 *)
  VAR
    inputTxt: TEXT;
    inputStr: ARRAY[0..79] OF CHAR;
    inputTxtLength: CARDINAL; (* length returned from console.readLine. *)
    inputStrLength: CARDINAL; (* length returned to caller. *)
  BEGIN
    WITH caller = NARROW(strand, UserSpaceThread.T),
         space = UserSpaceThread.GetSpace(caller) DO

      (*
       * Get an input line from the console.  This call echoes the call
       * made in SpinShell.ShellLoop.
       *)
      IF Debug AND FALSE THEN
        Print("  sysgetstr called.\n");
      END;
      TRY
        inputTxt := IO.ReadLine();
      EXCEPT
      | IO.Error => 
        HandlerUtils.PrintError("sysgetstr: IO.ReadLine failed.\n");
        ms.v0 := Failure;
        RETURN;
      ELSE
        HandlerUtils.PrintError("sysgetstr: unknown exc in IO.ReadLine.\n");
        ms.v0 := Failure;
        RETURN;
      END;
      IF Debug AND FALSE THEN
        Print("  sysgetstr: IO.ReadLine() returned: " & inputTxt & "\n");
      END;

      (*
       * Record the length and adjust according to ms.a1 and the size of
       * the inputStr array.
       *)
      inputTxtLength := Text.Length(inputTxt);
      IF Debug AND FALSE THEN
        Print("  sysgetstr: IO.ReadLine() returned - length: " & 
          Fmt.Int(inputTxtLength) & "\n");
      END;
      inputTxtLength := inputTxtLength + 1; (* need room for null-terminate *)
      inputStrLength := MIN(inputTxtLength, MIN(ms.a1, NUMBER(inputStr)));

      (*
       * Convert it to an array.
       *)
      Text.SetChars(inputStr, inputTxt);

      (*
       * Verify that it is null-terminated.
       *)
      IF inputStr[inputStrLength-1] # '\000' THEN
        inputStr[inputStrLength-1] := '\000';
      END;

      (*
       * Write the input string back to the caller's space.
       *)
      IF NOT HandlerUtils.CopyOut(space, inputStr, ms.a0, inputStrLength) THEN
        HandlerUtils.PrintError("sysgetstr: CopyOut exception.\n");
        ms.v0 := Failure;
        RETURN;
      END;

      IF Debug AND FALSE THEN
        FOR i := FIRST(inputStr) TO FIRST(inputStr)+inputStrLength DO
          Print("  " & Fmt.Int(i) & ": " & 
            Fmt.Unsigned(ORD(inputStr[i])) & "\n");
        END;
      END;
    END;

    (*
     * Return the number of characters written back.
     *)
    ms.v0 := inputStrLength;
  END sysgetstr;


PROCEDURE Guard (<* UNUSED *>strand: Strand.T;
                 <* UNUSED *> VAR ms: CPU.SavedState):BOOLEAN =
  BEGIN
    RETURN TRUE;
  END Guard;  


BEGIN
  WITH guard = Guard,
       hndlr = Syscall,
       event = Trap.Syscall
   DO
    (* Install this handler as the first and with cancellation.  This
       means NO OTHER HANDLERS WILL RUN.  Some UNIX system call numbers
       overlap with default system call numbers.  Until guards have some
       sort of per thread or per application check, I need for other
       handlers not to be fired. *)
    TRY
      EVAL Dispatcher.InstallHandler(event, 
                                     NIL (*guard*),
                                     hndlr, 
                                     options := Dispatcher.Options{
                                                   Dispatcher.Opt.Cancel,
                                                   Dispatcher.Opt.First},
                                     key := NEW(Trap.AuthKey, 
                                                minProcID := FIRST(Word.T),
                                                maxProcID := LAST(Word.T)));
    EXCEPT
    | Dispatcher.Error =>
      HandlerUtils.PrintError("Syscall.m3: dispatcher exception " &
        "installing syscall handler.\n");
    ELSE
      HandlerUtils.PrintError("Syscall.m3: unknown exception " &
        "installing syscall handler.\n");
    END;
    
  END;

END Syscall.
