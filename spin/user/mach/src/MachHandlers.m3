(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 16-Jan-97  Marc Fiuczynski (mef) at the University of Washington
 *	Change BuildInfo arg not to use buildtime arg.
 *
 * 04-oct-96  becker at the University of Washington
 *	updated for new BuildInfo arg
 *
 * 04-Apr-96  David Dion (ddion) at the University of Washington
 *	Updated for spin-20
 *
 * 28-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Changed to conform to the new syscall interface.
 *
 * 15-Dec-95  David Dion (ddion) at the University of Washington
 *	Updated to spin-8 for CVS.
 *
 * 19-Aug-95  David Dion (ddion) at the University of Washington
 *       Created.
 *
 *)

MODULE MachHandlers;

IMPORT CPU, UserSpaceThread, Space, Strand, Word, Fmt, Textify, Text;
IMPORT BuildInfo; (* for version *)
IMPORT Syscall;
IMPORT VMTaskSupport;
IMPORT ThreadHandlers; (* for Rendezvous *)
IMPORT HandlerUtils;
FROM HandlerUtils IMPORT Debug;
FROM HandlerUtils IMPORT Print;


(*
 * Following RECORDs added by ddion for syscall handling.
 *)
TYPE timeThing = RECORD (* for task_info *)
  oneTimeThing: Word.T;
  anotherTimeThing: Word.T;
END;

TYPE TaskBasicInfo = RECORD (* for task_info *)
  suspendCount: Word.T;
  basePriority: Word.T;
  virtualSize: Word.T;
  residentSize: Word.T;
  userTime: timeThing;
  systemTime: timeThing;
END;

TYPE HostBasicInfo = RECORD (* for host_info *)
  maxCPUs: Word.T;
  availCPUs: Word.T;
  memorySize: Word.T;
  cpuType: Word.T;
  cpuSubtype: Word.T;
END;

CONST
  WordSize = BYTESIZE(Word.T);
  KERNEL_VERSION_MAX = 512;

VAR
  (* Needed static variables. *)
  MachPort: Word.T := 1;
  (* I made up the following constants.  Should be temporary, but who knows? 
     They must be VARs for VIEWing *)
  BootstrapPort: Word.T := 16_add; (* returned by task_get_special_port *)
  PrivilegedHostPort: Word.T := 16_b1ab; (* " " task_get_master_ports *)
  DeviceServerPort: Word.T := 16_cab;    (* " " task_get_master_ports *)
  HostNamePort: Word.T := 16_babe;       (* " " mach_host_self *)
  DefaultProcessorSetName: Word.T := 16_bad; (* " " processor_set_default *)
  DefaultProcessorSet: Word.T := 16_cafe;  (* " " host_processor_set_default *)

  FirstMachTaskSelf: BOOLEAN := TRUE;

PROCEDURE mach_thread_self(strand: Strand.T; 
                           VAR ms: CPU.SavedState) = (* -27 *)
  BEGIN
    (*
      * -27, mach_thread_self(): mapped to return an externalized reference
      *                          to a UserSpaceThread.T.
      *
      * No arguments.
      * Return externalized reference to the calling thread.
      *
      * Need to IMPORT:  UST
      * Need variables:  caller
    *)
    WITH caller = NARROW(strand, UserSpaceThread.T) DO
      ms.v0 := HandlerUtils.Externalize(caller, caller);
    END;
  END mach_thread_self;

PROCEDURE mach_task_self(strand: Strand.T; 
                         VAR ms: CPU.SavedState) = (* -28 *)
  VAR 
    dummy: REF Word.T;
  BEGIN
    (*
      * -28, mach_task_self(): mapped to return an externalized reference
      *                        to a Space.T.
      *
      * No arguments.
      * Return externalized reference to the space of the calling thread.
      *
      * Need to IMPORT:  Space, UST.
      * Need variables:  space, caller
    *)
    WITH caller = NARROW(strand, UserSpaceThread.T),
         space = UserSpaceThread.GetSpace(caller) DO
      ms.v0 := HandlerUtils.Externalize(caller, space);
      IF FirstMachTaskSelf THEN
        ServerTask := space;
        FirstMachTaskSelf := FALSE;
        (*
         * We do not want a task extern ref to be 0, because this corresponds
         * to MACH_PORT_NULL.  Make sure this does not happen by setting
         * extern ref 0 be a dummy node.
         *)
        IF ms.v0 = 0 THEN
          dummy := NEW(REF Word.T);
          Space.DeleteExtRef(space, 0); (* remove 0 extern ref *)
          EVAL Space.Externalize(space, dummy, 0); (* replace with dummy *)
          ms.v0 := HandlerUtils.Externalize(caller, space);
        END;
      END;
    END;
  END mach_task_self;

PROCEDURE mach_host_self(<*UNUSED*>strand: Strand.T; 
                         VAR ms: CPU.SavedState) = (* -29 *)
  BEGIN
    ms.v0 := HostNamePort;
  END mach_host_self;

PROCEDURE host_kernel_version(strand: Strand.T; 
                              VAR ms: CPU.SavedState) = (* -51 *)
  VAR
    versionStr: ARRAY[0..KERNEL_VERSION_MAX-1] OF CHAR;
    textlength: CARDINAL;
    version: TEXT;
    target: TEXT;
    buildDate: TEXT;
    builder: TEXT;
    thisTree: TEXT;
  BEGIN
    (* Check to make sure inquiry is about proper host.  HostPortName should
       have been dished out earlier. *)
    IF ms.a0 # HostNamePort THEN
      HandlerUtils.PrintError("host_kernel_version: hostname mismatch!\n");
      ms.v0 := Syscall.KERN_INVALID_ARGUMENT;
      RETURN;
    END;

    BuildInfo.GetInfo(version, target, buildDate, builder, thisTree);

    Text.SetChars(versionStr, version);
    textlength := Text.Length(version);

    (* Get space to write version information to. *)
    WITH space = UserSpaceThread.GetSpace(NARROW(strand, UserSpaceThread.T))
      DO
       IF NOT HandlerUtils.CopyOut(space, versionStr, ms.a1, textlength+1) THEN
         HandlerUtils.PrintError("host_kernel_version: " & 
           "CopyOut exception.\n");
         ms.v0 := Syscall.Failure;
         RETURN;
       END;
     END;
    ms.v0 := Syscall.KERN_SUCCESS;
  END host_kernel_version;

PROCEDURE task_info(strand: Strand.T; 
                    VAR ms: CPU.SavedState) = (* -58 *)
  VAR
    taskInfoData: TaskBasicInfo;
  BEGIN
    (*
      * -58, task_info(): This used to be an IPC call.
      *
      * Set basePriority to be caller.pri -- default should be 100.
      * Then point to it and loophole that pointer to an array for the
      * vm copy operation into the address passed in ms.a2.
    *)
    taskInfoData.basePriority := Strand.defPriority;
    IF Debug THEN Print("\tSetting basePriority to " & 
      Fmt.Unsigned(taskInfoData.basePriority) & "\n");
    END;

    (* copy out *)
    WITH virtaddr = ms.a2,
         caller = NARROW(strand, UserSpaceThread.T),
         space = UserSpaceThread.GetSpace(caller),
         copyArray = VIEW(taskInfoData, 
                          ARRAY[0..BYTESIZE(TaskBasicInfo)-1] OF CHAR) DO
      IF NOT HandlerUtils.CopyOut(space, 
                                  copyArray, 
                                  virtaddr, 
                                  BYTESIZE(TaskBasicInfo)) THEN
        HandlerUtils.PrintError("task_info: CopyOut exception.\n");
        ms.v0 := Syscall.Failure;
        RETURN;
      END;
    END;
    ms.v0 := Syscall.KERN_SUCCESS;
  END task_info;

PROCEDURE task_create(strand: Strand.T; 
                      VAR ms: CPU.SavedState) = (* -68 *)
  VAR
    calleruthread: UserSpaceThread.T;
    newspace, parentspace: Space.T;
    spaceextern: Word.T;
  BEGIN
    (*
     * Get calling thread for internalizing and externalizing.
     *)
    calleruthread := NARROW(strand, UserSpaceThread.T);
    parentspace := NARROW(HandlerUtils.Internalize(calleruthread, ms.a0),
                          Space.T);

    (*
     * Space.Create() a new space and register it.
     *)
    IF Debug THEN Print("\ttask_create: calling Space.Create.\n"); END;
    newspace := Space.Create();
    (* would like to error check here, but not clear how from Space.[im]3 *)

    (*
     * Create an externalized reference to the new space. 
     *)
    spaceextern := HandlerUtils.Externalize(calleruthread, newspace);

    IF Debug THEN Print("\ttask_create: new space = 0x" & 
      Fmt.Unsigned(spaceextern) & ": 0x" & 
      Textify.Ref(newspace) & " parent space = 0x" &
      Fmt.Unsigned(ms.a0) & ": 0x" & 
      Textify.Ref(parentspace) & "\n");
    END;

    IF Debug THEN Print("\ttask_create: calling RegisterNewSpace.\n"); END;
    VMTaskSupport.RegisterNewSpace(newspace);

    (*
     * ms.a0 has parent task and ms.a1 indicates whether that task should
     * be inherited.  But, if the parent space is the server, we don't 
     * want to inherit.  
     *)
    IF parentspace # ServerTask AND ms.a1 # 0 THEN
      IF Debug THEN Print("\ttask_create: calling DupTask.\n"); END;
      TRY
        VMTaskSupport.DupTask(parentspace, newspace);
      EXCEPT
      | VMTaskSupport.Failure =>
        ms.v0 := Syscall.Failure;
        RETURN;
      END;
      IF Debug THEN Print("\ttask_create: back from DupTask.\n"); END;
    END;

    IF ms.a1 # 0 THEN
      IF Debug THEN 
        Print("\ttask_create: inheriting extern refs.\n"); 
      END;
      Space.CopyExterns(parentspace, newspace);
    END;

    (*
     * Write the child space's externalized reference back to the
     * address in ms.a2.
     *)
    WITH copyArray = VIEW(spaceextern, ARRAY[0..WordSize-1] OF CHAR),
         space = UserSpaceThread.GetSpace(calleruthread) DO
      IF NOT HandlerUtils.CopyOut(space, copyArray, ms.a2, WordSize) THEN
        HandlerUtils.PrintError("task_create: CopyOut exception.\n");
        ms.v0 := Syscall.Failure;
        RETURN;
      END;
    END;

    ms.v0 := Syscall.KERN_SUCCESS;
  END task_create;

PROCEDURE task_terminate(strand: Strand.T; 
                         VAR ms: CPU.SavedState) = (* -69 *)
  VAR
    space: Space.T;
  BEGIN
    (*
     * What are our arguments?
     *)
    IF Debug THEN Print("task_terminate: a0 = 0x" & 
      Fmt.Unsigned(ms.a0) & 
      "\n                GetSpace = 0x" & 
      Textify.Ref(UserSpaceThread.GetSpace(NARROW(strand,
                                                     UserSpaceThread.T))) & 
      "\n");
    END;
    (*
     * Get the space.
     *)
    space := NARROW(HandlerUtils.Internalize(NARROW(strand, 
                                                       UserSpaceThread.T),
                                                ms.a0),
                    Space.T);
    
    (*
     * Keep data structures up to date.
     *)
    VMTaskSupport.RegisterSpaceDestroy(space);

    (* 
     * Release anybody waiting on this process
     *)
    HandlerUtils.EndWait();

    (*
     * Destroy the space.  But I hear Space.Destroy isn't such a good
     * idea right now.
     *)
    TRY
      Space.Destroy(space);
    EXCEPT
    ELSE
      HandlerUtils.PrintError("task_terminate: exception in Space.Destroy.\n");
    END;

    (*
     * Block the thread if this is it.
     *)
    IF UserSpaceThread.GetSpace(NARROW(strand, UserSpaceThread.T)) =
      space THEN
      Strand.Block(strand);
    END;

    (*
     * Sometimes task_terminate() will be called on a task besides the
     * calling address space.  In this case Strand.Block will not happen.
     * We'd better return KERN_SUCCESS here, or an infinite loop will
     * occur in which the task is repeatedly blocked.
     *)
    ms.v0 := Syscall.KERN_SUCCESS;
  END task_terminate;

PROCEDURE task_suspend(strand: Strand.T; 
                       VAR ms: CPU.SavedState) = (* -70 *)
  BEGIN
    (*
     * We get an extern ref to the task (space) through ms.a0.  If 
     * this thread is in that space, suspend it using Rendezvous.
     * Otherwise, I'm not sure what to do.  Return error.
     *)
    WITH uthread = NARROW(strand, UserSpaceThread.T),
         space = NARROW(HandlerUtils.Internalize(uthread, ms.a0), 
                        Space.T) DO

      IF UserSpaceThread.GetSpace(uthread) = space THEN
        ThreadHandlers.Rendezvous(TRUE, NIL, uthread);
        (* like anybody ever gets this ret value *)
      ELSE
        HandlerUtils.PrintError("task_suspend: calling thread not in task\n");
        (* ms.v0 := -1; *)
      END;
    END;
    ms.v0 := Syscall.KERN_SUCCESS;
  END task_suspend;

PROCEDURE mach_port_allocate(strand: Strand.T; 
                             VAR ms: CPU.SavedState) = (* -72 *)
  BEGIN
    (* Copy back MachPort and increment MachPort.  We probably want to
       make sure that we get something besides 0 (MACH_PORT_NULL) in the
       copy-back space, since error checks will look for MACH_PORT_NULL
       and panic. *)
    WITH uthread = NARROW(strand, UserSpaceThread.T),
         space = UserSpaceThread.GetSpace(uthread),
         copyArray = VIEW(MachPort, ARRAY[0..WordSize-1] OF CHAR) DO
      IF NOT HandlerUtils.CopyOut(space, copyArray, ms.a2, WordSize) THEN
        HandlerUtils.PrintError("mach_port_allocate: CopyOut exception.\n");
        ms.v0 := Syscall.Failure;
        RETURN;
      END;
    END;
    IF Debug THEN Print("\twriting port name " & 
      Fmt.Unsigned(MachPort) & "\n");
    END;
    INC(MachPort, 1);
    ms.v0 := Syscall.KERN_SUCCESS;
  END mach_port_allocate;

PROCEDURE task_get_special_port(strand: Strand.T; 
                                VAR ms: CPU.SavedState) = (* -91 *)
  BEGIN
    IF ms.a1 = 4 THEN (* took number 4 from preprocessed server_init.c *)

      WITH uthread = NARROW(strand, UserSpaceThread.T),
           space = UserSpaceThread.GetSpace(uthread),
           copyArray = VIEW(BootstrapPort, ARRAY[0..WordSize-1] OF CHAR) DO
        IF NOT HandlerUtils.CopyOut(space, copyArray, ms.a2, WordSize) THEN
          HandlerUtils.PrintError("task_get_special_port: " & 
            "CopyOut exception.\n");
          ms.v0 := Syscall.Failure;
          RETURN;
        END;
      END;

      ms.v0 := Syscall.KERN_SUCCESS;
    ELSE
      ms.v0 := -1;
    END;
  END task_get_special_port;

PROCEDURE task_get_master_ports(strand: Strand.T; 
                                VAR ms: CPU.SavedState) = (* -92 *)
  BEGIN
    (*
     * Get the device_host_port and privileged_server_port.  For now,
     * privileged_host_port address must be ms.a1 and device_server_port
     * address must be ms.a2.  Verify identity of caller by checking
     * against bootstrap port, passed in ms.a3.
     *)
    IF ms.a3 = BootstrapPort THEN
      WITH uthread = NARROW(strand, UserSpaceThread.T),
           space = UserSpaceThread.GetSpace(uthread) DO
        WITH copyArray = VIEW(PrivilegedHostPort, 
                              ARRAY[0..WordSize-1] OF CHAR) DO
          IF NOT HandlerUtils.CopyOut(space, copyArray, ms.a1, WordSize) THEN
            HandlerUtils.PrintError("task_get_master_ports: " & 
              "CopyOut exception.\n");
            ms.v0 := Syscall.Failure;
            RETURN;
          END;
        END;
        WITH copyArray = VIEW(DeviceServerPort, 
                              ARRAY[0..WordSize-1] OF CHAR) DO
          IF NOT HandlerUtils.CopyOut(space, copyArray, ms.a2, WordSize) THEN
            HandlerUtils.PrintError("task_get_master_ports: " & 
              "CopyOut exception.\n");
            ms.v0 := Syscall.Failure;
            RETURN;
          END;
        END;
      END;

      ms.v0 := Syscall.KERN_SUCCESS;
    ELSE
      HandlerUtils.PrintError("task_get_master_ports: bad bootstrap port.\n");
      ms.v0 := Syscall.KERN_INVALID_ARGUMENT;
    END;
  END task_get_master_ports;

PROCEDURE processor_set_default(strand: Strand.T; 
                                VAR ms: CPU.SavedState) = (* -93 *)
  BEGIN
    (* Make sure it's looking for this hostname; otherwise we don't know
       what to do. *)
    IF ms.a0 = HostNamePort THEN
      WITH uthread = NARROW(strand, UserSpaceThread.T),
           space = UserSpaceThread.GetSpace(uthread),
           copyArray = VIEW(DefaultProcessorSetName, 
                            ARRAY[0..WordSize-1] OF CHAR) DO
        IF NOT HandlerUtils.CopyOut(space, copyArray, ms.a1, WordSize) THEN
          HandlerUtils.PrintError("processor_set_default: " &
            "CopyOut exception.\n");
          ms.v0 := Syscall.Failure;
          RETURN;
        END;
      END;
      ms.v0 := Syscall.KERN_SUCCESS;
    ELSE
      HandlerUtils.PrintError("processor_set_default: bad host name port.\n");
      ms.v0 := Syscall.KERN_INVALID_ARGUMENT;
    END;
  END processor_set_default;

PROCEDURE host_processor_set_priv(strand: Strand.T; 
                                  VAR ms: CPU.SavedState) = (* -94 *)
  BEGIN
    (* Make sure it has this host privilege and wants this default set *)
    IF ms.a0 = PrivilegedHostPort AND 
      ms.a1 = DefaultProcessorSetName THEN
      WITH uthread = NARROW(strand, UserSpaceThread.T),
           space = UserSpaceThread.GetSpace(uthread),
           copyArray = VIEW(DefaultProcessorSet, 
                            ARRAY[0..WordSize-1] OF CHAR) DO
        IF NOT HandlerUtils.CopyOut(space, copyArray, ms.a2, WordSize) THEN
          HandlerUtils.PrintError("host_processor_set_priv: " & 
            "CopyOut exception.\n");
          ms.v0 := Syscall.Failure;
          RETURN;
        END;
      END;
      ms.v0 := Syscall.KERN_SUCCESS;
    ELSE
      HandlerUtils.PrintError("host_processor_set_priv: bad arguments.\n");
      ms.v0 := Syscall.KERN_INVALID_ARGUMENT;
    END;
  END host_processor_set_priv; 

PROCEDURE host_info(strand: Strand.T; 
                    VAR ms: CPU.SavedState) = (* -95 *)
  VAR 
    info: HostBasicInfo;
  BEGIN
    (* First check that query is about this host. *)
    IF ms.a0 # HostNamePort THEN
      HandlerUtils.PrintError("host_info: wrong host name!\n");
      ms.v0 := Syscall.KERN_INVALID_ARGUMENT;
      RETURN;
    END;
    (* Currently only support flavor HOST_BASIC_INFO, which is 1. *)
    IF ms.a1 # 1 THEN
      HandlerUtils.PrintError("host_info: wrong flavor!\n");
      ms.v0 := Syscall.KERN_INVALID_ARGUMENT;
      RETURN;
    END;

    (* Set whatever we actually know or currently care about. *)
    (* There might be a SAL interface which can give me this info. *)
    info.maxCPUs := 1;
    info.availCPUs := 1;
    info.memorySize := 64000000;  (* 64 megs, hope decimal is okay *)

    WITH uthread = NARROW(strand, UserSpaceThread.T),
         space = UserSpaceThread.GetSpace(uthread),
         copyArray = VIEW(info, ARRAY[0..BYTESIZE(HostBasicInfo)-1] OF CHAR) DO
      IF NOT HandlerUtils.CopyOut(space, 
                                  copyArray, 
                                  ms.a2, 
                                  BYTESIZE(HostBasicInfo)) THEN
        HandlerUtils.PrintError("host_info: CopyOut exception.\n");
        ms.v0 := Syscall.Failure;
        RETURN;
      END;
    END;
    ms.v0 := Syscall.KERN_SUCCESS;
  END host_info;

BEGIN
END MachHandlers.

