(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 04-Apr-96  David Dion (ddion) at the University of Washington
 *	Updated for spin-20
 *
 * 28-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Changed to conform to the new syscall interface.
 *
 * 15-Dec-95  David Dion (ddion) at the University of Washington
 *	Updated to spin-8 for CVS.
 *
 * 27-Jul-95  David Dion (ddion) at the University of Washington
 *      Created to separate virtual memory calls from other system calls.
 *)
MODULE VMHandlers;

IMPORT UserSpaceThread, Space, Translation, CPU, Word, Strand;
IMPORT Fmt; (* mainly for debugging *)
IMPORT MachHandlers;
IMPORT Syscall;
IMPORT VMTaskSupport;
IMPORT Textify;
IMPORT HandlerUtils;
IMPORT VMError;
FROM HandlerUtils IMPORT Debug;
FROM HandlerUtils IMPORT Print;
IMPORT Protection;
IMPORT Buffer;

CONST
  (* VM constants *)
  VM_FREE_COUNT = 512; (* chose this pretty randomly *)
  (*VM_PROT_NONE = 16_0;*) (* stolen from mk/kernel/mach/vm_prot.h *)
  VM_PROT_READ = 16_1;
  VM_PROT_WRITE = 16_2;
  VM_PROT_EXECUTE = 16_4;
  VM_PROT_ALL = 16_7;
  VM_MIN_ADDRESS = 16_0;
  VM_MAX_ADDRESS = 16_fffffc0000000000;

  MaxNEW = 16_200000; (* 256 pages - this is a hack - the idea is that
                         anything bigger than this is an error and could
                         cause problems *)
 
  WordSize = BYTESIZE(Word.T);


TYPE VMRegionCallNode = RECORD
  externSpace: Word.T;
  numcalls: CARDINAL;
END;

VAR
  vmRegionCalled: ARRAY[0..63] OF VMRegionCallNode;
(*  vmMapCalled: INTEGER := 0; *)

PROCEDURE vm_write(strand: Strand.T; VAR ms: CPU.SavedState) = (* -31 *)
  VAR
    callerUthread: UserSpaceThread.T;
    callerSpace, targetSpace: Space.T;
    data: REF ARRAY OF CHAR;
    buffer: Buffer.T;
    size, sourceAddress: Word.T;
  BEGIN
    (* Get target space and calling (source) space. *)
    callerUthread := NARROW(strand, UserSpaceThread.T);
    callerSpace := UserSpaceThread.GetSpace(callerUthread);
    targetSpace := NARROW(HandlerUtils.Internalize(callerUthread, ms.a0),
                          Space.T);

    (* Size of data is in ms.a3.  Allocate appropriately sized buffer. *)
    size := ms.a3;
    
    (* Allocate a buffer or correct size.  The other option here is to 
       have a static buffer and then loop on Read,Write until size is
       reached. *)
    (*
    IF size > 16_0 AND size < MaxNEW THEN
      data := NEW(REF ARRAY OF CHAR, size);
    ELSE
      HandlerUtils.PrintError("vm_write: called NEW with size 0x" &
        Fmt.Unsigned(size) & "\n");
      ms.v0 := Syscall.Failure;
      RETURN;
    END;
    *)
    IF size > 16_0 AND size < MaxNEW THEN
      buffer := Buffer.Allocate(size);
      data := buffer.data;
    ELSE
      HandlerUtils.PrintError("vm_write: called NEW with size 0x" &
        Fmt.Unsigned(size) & "\n");
      ms.v0 := Syscall.Failure;
      RETURN;
    END;

    (* Read data from calling space. *)
    sourceAddress := ms.a2;
    TRY
      Translation.Read(callerSpace, sourceAddress, SUBARRAY(data^, 0, size));
    EXCEPT
    | VMError.E(ec) =>
      HandlerUtils.PrintError("\tvm_write: BadSize exception in " & 
        "Translation.Read:" & VMError.Message(ec) & ".\n");
      ms.v0 := Syscall.KERN_INVALID_ARGUMENT;
      Buffer.Deallocate(buffer);
      RETURN;
    ELSE
      HandlerUtils.PrintError("\tvm_write: unknown exception in " & 
        "Translation.Read.\n");
      ms.v0 := Syscall.Failure;
      Buffer.Deallocate(buffer);
      RETURN;
    END;

    (* Write data to target space. *)
    WITH targetAddress = ms.a1 DO
      TRY
	Translation.Write(targetSpace, SUBARRAY(data^, 0, size), targetAddress);
      EXCEPT
      | VMError.E(ec) =>
	HandlerUtils.PrintError("\tvm_write: BadSize exception in " & 
        "Translation.Write:" & VMError.Message(ec) & ".\n");
	ms.v0 := Syscall.Failure;
        Buffer.Deallocate(buffer);
        RETURN;
      ELSE
        HandlerUtils.PrintError("\tvm_write: exception in Translation.Write.\n");
        ms.v0 := Syscall.Failure;
        Buffer.Deallocate(buffer);
        RETURN;
      END;
      IF Debug THEN Print("\tvm_write: wrote targetSpace: 0x" &
        Textify.Ref(targetSpace) & " targetAddress: 0x" &
        Fmt.Unsigned(targetAddress) & " size: 0x" & Fmt.Unsigned(size) &"\n"); 
      END;
    END;

    Buffer.Deallocate(buffer);
    ms.v0 := Syscall.KERN_SUCCESS;
  END vm_write;

PROCEDURE vm_read(strand: Strand.T; VAR ms: CPU.SavedState) = (* -33 *)
  VAR
    callerUthread: UserSpaceThread.T;
    callerSpace, sourceSpace: Space.T;
    data: REF ARRAY OF CHAR;
    buffer: Buffer.T;
    size: Word.T;
    targetBuffer, bufferSize: Word.T;
  BEGIN
    (* Get caller (data destination) and source address spaces. *)
    callerUthread := NARROW(strand, UserSpaceThread.T);
    callerSpace := UserSpaceThread.GetSpace(callerUthread);
    sourceSpace := NARROW(HandlerUtils.Internalize(callerUthread, ms.a0),
                          Space.T);

    (* Allocate a buffer or correct size.  The other option here is to 
       have a static buffer and then loop on Read,Write until size is
       reached. *)
    size := ms.a2;
    (*
    IF size > 16_0 AND size < MaxNEW THEN
      data := NEW(REF ARRAY OF CHAR, size);
    ELSE
      HandlerUtils.PrintError("vm_read: called NEW with size 0x" &
        Fmt.Unsigned(size) & "\n");
      ms.v0 := Syscall.Failure;
      RETURN;
    END;
    *)
    IF size > 16_0 AND size < MaxNEW THEN
      buffer := Buffer.Allocate(size);
      data := buffer.data;
    ELSE
      HandlerUtils.PrintError("vm_read: called NEW with size 0x" &
        Fmt.Unsigned(size) & "\n");
      ms.v0 := Syscall.Failure;
      RETURN;
    END;

    IF ms.a1 = 0 THEN
      (*
      HandlerUtils.PrintError("vm_read: Translation.Read from NULL.\n");
      *)
      ms.v0 := Syscall.Failure;
      Buffer.Deallocate(buffer);
      RETURN;
    END;

    (* Read data into data array. *)
    WITH sourceAddress = ms.a1 DO
      IF Word.Mod(sourceAddress, VM_PAGE_SIZE) # 0 THEN
        HandlerUtils.PrintError("\tvm_read: source address not page " & 
          "boundary.\n");
        ms.v0 := Syscall.KERN_INVALID_ARGUMENT;
        Buffer.Deallocate(buffer);
        RETURN;
      END;
      IF Debug THEN Print("\tvm_read: calling Translation.Read:\n" & 
        "\tsourceSpace: " &
        Textify.Ref(sourceSpace) & "\n\tsourceAddress: 0x" &
        Fmt.Unsigned(sourceAddress) & "\n\tsize: 0x" & 
        Fmt.Unsigned(size) &"\n"); END;
      TRY
        Translation.Read(sourceSpace, sourceAddress,
			 SUBARRAY(data^, 0, size));
      EXCEPT
      | VMError.E(ec) =>
        HandlerUtils.PrintError("\tvm_read: BadSize exception in " & 
          "Translation.Read:"& VMError.Message(ec) & ".\n");
	ms.v0 := Syscall.Failure;
        Buffer.Deallocate(buffer);
        RETURN;
      ELSE
        HandlerUtils.PrintError("\tvm_read: exception in Translation.Read.\n");
        ms.v0 := Syscall.Failure;
        Buffer.Deallocate(buffer);
        RETURN;
      END;
    END;

    (* Allocate buffer to give back to target space. *)
    bufferSize := size;
    VMTaskSupport.VMAllocAnywhere(callerSpace, targetBuffer, bufferSize);
    IF bufferSize < size OR targetBuffer = 0 THEN
      HandlerUtils.PrintError("\tvm_read: could not allocate " & 
        "target buffer.\n");
      ms.v0 := Syscall.Failure;
      Buffer.Deallocate(buffer);
      RETURN;
    END;

    (* Register capability for this new region of virtual memory. *)
    VMTaskSupport.RegisterMemAlloc(callerSpace, targetBuffer, bufferSize);

    (* Put data into target buffer. *)
    TRY
      Translation.Write(callerSpace, SUBARRAY(data^, 0, size), targetBuffer);
    EXCEPT
    | VMError.E(ec) =>
      HandlerUtils.PrintError("\tvm_read: BadSize exception in " & 
			      "Translation.Write:"& VMError.Message(ec) & ".\n");
      ms.v0 := Syscall.Failure;
      Buffer.Deallocate(buffer);
      RETURN;
    ELSE
      HandlerUtils.PrintError("\tvm_read: exception copying data " & 
        "into buffer.\n");
      ms.v0 := Syscall.Failure;
      Buffer.Deallocate(buffer);
      RETURN;
    END;

    Buffer.Deallocate(buffer);

    (* Give buffer address and size back to calling address space. *)
    WITH targetAddressPtr = ms.a3,
         datacountPtr = ms.a4 DO
      WITH copyArray = VIEW(targetBuffer, ARRAY[0..WordSize-1] OF CHAR) DO
        IF NOT HandlerUtils.CopyOut(callerSpace, 
                                    copyArray, 
                                    targetAddressPtr,
                                    WordSize) THEN
          HandlerUtils.PrintError("vm_read: CopyOut exception.\n");
          ms.v0 := Syscall.Failure;
          RETURN;
        END;
      END;
      WITH copyArray = VIEW(size, ARRAY[0..WordSize-1] OF CHAR) DO
        IF NOT HandlerUtils.CopyOut(callerSpace, 
                                    copyArray, 
                                    datacountPtr,
                                    WordSize) THEN
          HandlerUtils.PrintError("vm_read: CopyOut exception.\n");
          ms.v0 := Syscall.Failure;
          RETURN;
        END;
      END;
    END;

    ms.v0 := Syscall.KERN_SUCCESS;
  END vm_read;

PROCEDURE vm_inherit(strand: Strand.T; 
                     VAR ms: CPU.SavedState) = (* -34 *)
  BEGIN
    (*
     * Set up to redirect this to HandlerUtils.SetInheritance.  Note
     * this call only works on regions not allocated on the heap.  But
     * that should be okay for now, since this is all just a hack until
     * Spaces support inheritance.
     *)
    (*
     * Get the space.
     *)
    WITH space = NARROW(HandlerUtils.Internalize(NARROW(strand,
                                                           UserSpaceThread.T),
                                                    ms.a0),
                        Space.T) DO

      (*
       * ms.a1 contains the address, ms.a2 contains the size, 
       * and ms.a3 contains the inheritance.
       *)
      (* HandlerUtils.Print("\tvm_inherit: setting inheritance for addr 0x" &
         Fmt.Unsigned(ms.a1) & " size 0x" & Fmt.Unsigned(ms.a2) & 
         " to " & Fmt.Int(ms.a3) & "\n"); *)
      VMTaskSupport.SetInheritance(space, ms.a1, ms.a2, ms.a3);
    END;
    ms.v0 := Syscall.KERN_SUCCESS;
  END vm_inherit;

PROCEDURE vm_region(strand: Strand.T; 
                    VAR ms: CPU.SavedState) = (* -57 *)
  VAR
    index: CARDINAL;
    size, virtaddr: Word.T;
    space: Space.T;
    extraArgs: ARRAY[6..8] OF Word.T;
    protection: Word.T;
    port: Word.T;
  BEGIN
    (*
      * -57, vm_region(): This used to be an IPC call.
      *
      * The first time vm_region is called, we're looking for the
      * bounds of our stack.  If the caller is the server space,
      * use the stack definied in SpinShell.m3.  Otherwise, look
      * through the table in HandlerUtils.  Return the beginning
      * and size of appropriate region.
      
      * The second time vm_region is called ...
      * probe_stack() is outside of the bounds of the stack, since we
      * gave it the entire stack on the first call.  Return something
      * that has error written all over it.
      *
      * The third time vm_region is called ...
      * Who knows where it's coming from.  We're in trouble.
      *
      * Will need mucho Space support before this one can be implemented
      * fully.
    *)

    space := NARROW(HandlerUtils.Internalize(NARROW(strand, 
                                                       UserSpaceThread.T), 
                                                ms.a0), 
                    Space.T);
    (*
     * Get additional arguments off the stack.
     *)
    IF NOT HandlerUtils.GetNArgsOffStack(space, ms.usp, 3, extraArgs) THEN
      HandlerUtils.PrintError("vm_region: cannot retrieve arguments " & 
        "off of stack.\n");
      ms.v0 := Syscall.Failure;
      RETURN;
    END;

    (*
     * Find index in vmRegionCalled table.  Need while loop here because
     * loop variable must have scope greater than the loop.
     *
     * This will all be obsolete soon.
     *)
    index := 0;
    WHILE index <= LAST(vmRegionCalled) DO 
      (* Check for match. *)
      IF vmRegionCalled[index].externSpace = ms.a0 THEN
        EXIT;
      END; 
      (* Check if reached end of filled spots in table.  If so, fill. *)
      IF vmRegionCalled[index].externSpace = 0 THEN
        vmRegionCalled[index].externSpace := ms.a0;
        EXIT;
      END;
      INC(index);
    END;
    IF index > LAST(vmRegionCalled) THEN
      HandlerUtils.PrintError("vmRegionCalled table full!  " &
        "Get rid of this stupid hack!\n");
      ms.v0 := Syscall.Failure;
      RETURN;
    END;
    
    IF vmRegionCalled[index].numcalls = 0 THEN (* first call *)
      vmRegionCalled[index].numcalls := 1;
      IF Debug THEN Print("\tvm_region: first call: extern space = 0x" &
        Fmt.Unsigned(ms.a0) & "\n"); END;
    ELSIF vmRegionCalled[index].numcalls = 1 THEN (* second call *)
      INC(vmRegionCalled[index].numcalls);
      IF Debug THEN Print("\tvm_region: second call: extern space = 0x" &
        Fmt.Unsigned(ms.a0) & "\n"); END;
      ms.v0 := Syscall.Failure;
      RETURN;
    ELSE
      INC(vmRegionCalled[index].numcalls);
      HandlerUtils.Print("\tvm_region: extern space 0x" & 
        Fmt.Unsigned(ms.a0) & " called again!\n");
      ms.v0 := Syscall.Failure;
      RETURN;
    END;

    (* Get current value of address.  ms.a1 is a pointer, 
       so Translation.Read. *)
    WITH copyArray = VIEW(virtaddr, ARRAY[0..WordSize-1] OF CHAR) DO
      IF NOT HandlerUtils.CopyIn(space, ms.a1, copyArray, WordSize) THEN
        HandlerUtils.PrintError("vm_region: CopyIn exception.\n");
        ms.v0 := Syscall.Failure;
        RETURN;
      END;
    END;

    IF space = MachHandlers.ServerTask THEN
      (* 
       * XXX - Hack.  Lose this as soon as possible.  We know where the
       * server's stack is, since it's constructed in Exec off the SpinShell.
       *)
      virtaddr := 16_0800000000; (* start of Gun's stack *)
      size := 64 * 1024; (* 64K -- size of Gun's stack *)
    ELSE
      (*
       * This is not the server task.  That means we have to take the
       * address passed in, find it in our SpaceTable, and figure out
       * which region it's in.  Then we'll return the beginning of that
       * region and its size.
       *)
      TRY
        VMTaskSupport.GetRegion(space, virtaddr, size);
      EXCEPT
      | VMTaskSupport.BadAddr =>
        HandlerUtils.PrintError("vm_region: address not found in space.\n");
        ms.v0 := Syscall.Failure;
        RETURN;
      | VMTaskSupport.BadSpace =>
        HandlerUtils.PrintError("vm_region: space unknown.\n");
        ms.v0 := Syscall.Failure;
        RETURN;
      ELSE
        HandlerUtils.PrintError("vm_region: unknown exception in GetRegion.\n");
        ms.v0 := Syscall.Failure;
        RETURN;
      END;
    END;

    (*
     * Now write back the begin addr of the region, the size, the 
     * protection (hardcoded to VM_PROT_ALL), and some positive number
     * which OSF/1 will think is a valid memory object port.
     *)
    WITH copyArray = VIEW(virtaddr, ARRAY[0..WordSize-1] OF CHAR) DO
      IF NOT HandlerUtils.CopyOut(space, copyArray, ms.a1, WordSize) THEN
        HandlerUtils.PrintError("vm_region: CopyOut exception.\n");
        ms.v0 := Syscall.Failure;
        RETURN;
      END;
    END;
    WITH copyArray = VIEW(size, ARRAY[0..WordSize-1] OF CHAR) DO
      IF NOT HandlerUtils.CopyOut(space, copyArray, ms.a2, WordSize) THEN
        HandlerUtils.PrintError("vm_region: CopyOut exception.\n");
        ms.v0 := Syscall.Failure;
        RETURN;
      END;
    END;
    protection := VM_PROT_ALL;
    WITH copyArray = VIEW(protection, ARRAY[0..WordSize-1] OF CHAR) DO
      IF NOT HandlerUtils.CopyOut(space, copyArray, ms.a3, WordSize) THEN
        HandlerUtils.PrintError("vm_region: CopyOut exception.\n");
        ms.v0 := Syscall.Failure;
        RETURN;
      END;
    END;
    port := 16_2; (* chosen pretty randomly *)
    WITH copyArray = VIEW(port, ARRAY[0..WordSize-1] OF CHAR) DO
      IF NOT HandlerUtils.CopyOut(space, copyArray, extraArgs[7],WordSize) THEN
        HandlerUtils.PrintError("vm_region: CopyOut exception.\n");
        ms.v0 := Syscall.Failure;
        RETURN;
      END;
    END;
    IF Debug THEN Print("\tvm_region: wrote back addr 0x" &
      Fmt.Unsigned(virtaddr) & " size 0x" & Fmt.Unsigned(size) & "\n"); END;
  END vm_region;

PROCEDURE vm_statistics(strand: Strand.T; 
                        VAR ms: CPU.SavedState) = (* -63 *)
  (*
   * -63, vm_statistics(): This used to be an IPC call.
   *
   * This is supposed to fill in a vm_statistics_data_t, which is
   * a struct consisting of 13 integer_t (long) fields.  The only
   * occurrences encountered so far, however, care about page_size, 
   * which is the first field, and free_count, which is the second.  
   * So write the page_size to the address passed in ms.a1 and write
   * the free_count to the address in ms.a1 plus 8 bytes.
   *)
  VAR
    vmPageSize := VM_PAGE_SIZE;
    vmFreeCount := VM_FREE_COUNT;
  BEGIN
    WITH uthread = NARROW(strand, UserSpaceThread.T),
         space = NARROW(HandlerUtils.Internalize(uthread, ms.a0), Space.T) DO
      WITH copyArray = VIEW(vmPageSize, ARRAY[0..WordSize-1] OF CHAR) DO
        IF NOT HandlerUtils.CopyOut(space, copyArray, ms.a1, WordSize) THEN
          HandlerUtils.PrintError("vm_statistics: CopyOut exception.\n");
          ms.v0 := Syscall.Failure;
          RETURN;
        END;
      END;
      WITH copyArray = VIEW(vmFreeCount, ARRAY[0..WordSize-1] OF CHAR) DO
        IF NOT HandlerUtils.CopyOut(space, copyArray, ms.a1 + 8, WordSize) THEN
          HandlerUtils.PrintError("vm_statistics: CopyOut exception.\n");
          ms.v0 := Syscall.Failure;
          RETURN;
        END;
      END;
    END;

    ms.v0 := Syscall.KERN_SUCCESS;
  END vm_statistics;

PROCEDURE vm_map(strand: Strand.T; VAR ms: CPU.SavedState) = (* -64 *)
  VAR
    space: Space.T;
    begin, end, size: Word.T;
    protection := Protection.T{FALSE,FALSE,FALSE,0};
    extraArgs: ARRAY[6..10] OF Word.T;
  BEGIN
    IF Debug THEN Print("In vm_map.\n"); END;

    (* Get the space. *)
    space := NARROW(HandlerUtils.Internalize(NARROW(strand, 
                                                         UserSpaceThread.T), 
                                                ms.a0), 
                    Space.T);

    (* 
     * Not all args make it in through registers.  Get the others off
     * the stack now. 
     *)
    IF NOT HandlerUtils.GetNArgsOffStack(space, ms.usp, 5, extraArgs) THEN
      HandlerUtils.PrintError("vm_map: cannot retrieve arguments " & 
        "from stack.\n");
      ms.v0 := Syscall.Failure;
      RETURN;
    END;      

    (* Figure out what the protection parameter is.  It is passed in
       through extraArgs[8] (we want current protection, not max protection). 
       Recall that protection is initialized to be all false. *)
    IF Word.And(extraArgs[8], VM_PROT_READ) # 0 THEN
      protection.read := TRUE;
    END;
    IF Word.And(extraArgs[8], VM_PROT_WRITE) # 0 THEN
      protection.write := TRUE;
    END;
    IF Word.And(extraArgs[8], VM_PROT_EXECUTE) # 0 THEN
      protection.execute := TRUE;
    END;

    (* Get begin and end of region. *)
    IF ms.a1 = 0 THEN
      begin := 0;
    ELSE
      WITH copyArray = VIEW(begin, ARRAY[0..WordSize-1] OF CHAR) DO
        IF NOT HandlerUtils.CopyIn(space, ms.a1, copyArray, WordSize) THEN
          HandlerUtils.PrintError("vm_map: CopyIn exception.\n");
          ms.v0 := Syscall.Failure;
          RETURN;
        END;
      END;
    END;
    size := ms.a2;
    end := Word.Plus(begin, size);

    (* If the memory object parameter is NULL, then allocate zero-filled
       memory.  Don't worry about the zero part for now and pray it works. 
       Also check the anywhere parameter (ms.a4).  We are not currently
       handling the case where anywhere is TRUE. *)
    IF ms.a5 = 0 AND ms.a4 = 0 THEN
      TRY
	Space.Allocate(space, begin, size, FALSE);
      EXCEPT
      | VMError.E(ec) =>
        HandlerUtils.PrintError("\tvm_map: Failure exception in " & 
          "Space.Allocate:" & VMError.Message(ec) & ".\n");
        ms.v0 := Syscall.Failure;
        RETURN;
      ELSE
        HandlerUtils.PrintError("\tvm_map: exception in Space.Allocate.\n");
        ms.v0 := Syscall.Failure;
        RETURN;
      END;

      (* Register the VirtAddr capability for future use. *)
      VMTaskSupport.RegisterMemAlloc(space, begin, size);
    ELSIF ms.a5 = 0 AND ms.a4 # 0 THEN
      HandlerUtils.PrintError("\tvm_map: anywhere TRUE.  Handle this case.\n");
      ms.v0 := Syscall.Failure;
      RETURN;
    ELSE
    END;

    (* Protect the region. *)
    TRY
      Space.Protect(space, begin, Word.Minus(end, begin),
                    protection);
    EXCEPT
    | VMError.E(ec) =>
      HandlerUtils.PrintError("\tvm_map: Failure exception in " & 
        "Space.Protect:" & VMError.Message(ec) & ".\n");
      ms.v0 := Syscall.Failure;
      RETURN;
    ELSE
      HandlerUtils.PrintError("\tvm_map: Space.Protect exception.\n");
      ms.v0 := Syscall.Failure;
      RETURN;
    END;

    ms.v0 := Syscall.KERN_SUCCESS;
  END vm_map;

PROCEDURE vm_allocate(strand: Strand.T; 
                      VAR ms: CPU.SavedState) = (* -65 *)
  (*
    * -65, vm_allocate():  mapped to Space.Allocate().
    *
    * arg1 must be a Word.T.  However, in vm_allocate() the second
    *      parameter is inout.  Thus the data must be accessed using
    *      Translation.Read() and Translation.Write().
    * vm_allocate() sends an arg3, which is a boolean variable 
    *      indicating whether the space can be allocated anywhere (TRUE)
    *      or whether it must be allocated starting at the virtual
    *      address passed through ms.a1.  This parameter will be 
    *      ignored since Space.Allocate() is coded for the former case.
    *
    * Need to IMPORT:  Space, UST.
    * Need variables:  caller, space, virtaddr, size
  *)
  VAR
    caller: UserSpaceThread.T;
    space: Space.T;
    virtaddr: Word.T;
    size: Word.T;
    zeroArray: ARRAY[0..VM_PAGE_SIZE-1] OF CHAR;
    bytesZeroed: CARDINAL;
  BEGIN
    IF Debug THEN Print("\textern space = 0x" & 
      Fmt.Unsigned(ms.a0) & "\n"); END;
    caller := NARROW(strand, UserSpaceThread.T);
    space := NARROW(HandlerUtils.Internalize(caller, ms.a0), Space.T);

    IF Debug THEN Print("\tintern space = 0x" &
      Textify.Ref(space) & "\n"); END;

    (*
     * Allocate anywhere?
     *)
    IF ms.a3 = 1 THEN  (* address in ms.a1 is most likely garbage *)
      size := ms.a2;
      VMTaskSupport.VMAllocAnywhere(space, virtaddr, size);
      IF virtaddr = 0 THEN (* returns 0 on failure *)
        HandlerUtils.PrintError("\tvm_allocate: VMAllocAnywhere failed.\n");
        ms.v0 := Syscall.Failure;
        RETURN;
      END;
    ELSE  (* Go to location ms.a1 and get the address at which to alloc *)
      WITH callerspace = UserSpaceThread.GetSpace(caller),
           copyArray = VIEW(virtaddr, ARRAY[0..WordSize-1] OF CHAR) DO
        IF NOT HandlerUtils.CopyIn(callerspace, 
                                   ms.a1, 
                                   copyArray, 
                                   WordSize) THEN
          HandlerUtils.PrintError("vm_allocate: CopyIn exception.\n");
          ms.v0 := Syscall.Failure;
          RETURN;
        END;
      END;
      IF Debug THEN Print("\tRequest to allocate at location " & 
        Fmt.Unsigned(virtaddr) & "\n"); 
      END;
      (* cthread alloc_stack tries to allocate stacks starting at zero
         until it finds a valid location.  At each unsuccessful attempt,
         it adds 2 pages and tries again.  We want to avoid allocating
         at zero, so don't allow it to allocate at zero. *)
      IF virtaddr = 0 THEN
        ms.v0 := Syscall.Failure;
        RETURN;
      ELSE
        (*
         * Get size and make sure it is page-aligned.  (Round up if not.) 
         *)
        size := ms.a2;
        HandlerUtils.RoundUpToPage(size);
        (*
         * (Finally) allocate at the location specified grabbed from ms.a1.
         *)
        TRY
	  Space.Allocate(space, virtaddr, size, FALSE);
        EXCEPT
        | VMError.E(ec) =>
          HandlerUtils.PrintError("vm_allocate: Failure exception in " & 
            "Space.Allocate: space " & Textify.Ref(space) &
            " addr 0x" & Fmt.Unsigned(virtaddr) & " size 0x" &
            Fmt.Unsigned(size) & ":" & VMError.Message(ec) & "\n");
          TRY
            VMTaskSupport.GetRegion(space, virtaddr, size);
          EXCEPT
          | VMTaskSupport.BadSpace =>
            HandlerUtils.PrintError("vm_allocate: bad space.\n");
            ms.v0 := Syscall.Failure;
            RETURN;
          | VMTaskSupport.BadAddr =>
            HandlerUtils.PrintError("vm_allocate: bad addr.\n");
            ms.v0 := Syscall.Failure;
            RETURN;
          END;
          HandlerUtils.Print("vm_allocate: region already " & 
            "allocated in space!\n");
          (* Return, through inout addr parameter, the beginning of this
             region plus its size. *)
          virtaddr := virtaddr + size;
          WITH callerspace = UserSpaceThread.GetSpace(caller),
               copyArray = VIEW(virtaddr, ARRAY[0..WordSize-1] OF CHAR) DO
            IF Debug THEN 
              Print("vm_allocate: writing back addr 0x" &
                Fmt.Unsigned(virtaddr) & "\n"); 
            END;
            IF NOT HandlerUtils.CopyOut(callerspace, 
                                        copyArray,
                                        ms.a1, 
                                        WordSize) THEN
              HandlerUtils.PrintError("vm_allocate: CopyOut exception.\n");
              ms.v0 := Syscall.Failure;
              RETURN;
            END;
          END;
          ms.v0 := Syscall.Failure;
          RETURN;
        ELSE
          HandlerUtils.PrintError("Space.Allocate exception.\n");
          ms.v0 := Syscall.Failure;
          RETURN;
        END;
      END;
    END;

    (*
     * Write back virtaddr to the location specified in ms.a1.
     *)
    WITH callerspace = UserSpaceThread.GetSpace(caller),
         copyArray = VIEW(virtaddr, ARRAY[0..WordSize-1] OF CHAR) DO
      IF NOT HandlerUtils.CopyOut(callerspace, 
                                  copyArray, 
                                  ms.a1,
                                  WordSize) THEN
        HandlerUtils.PrintError("vm_allocate: CopyOut exception.\n");
        ms.v0 := Syscall.Failure;
        RETURN;
      END;
    END;

    (*
     * Now we want to register this memory allocation.  Among other
     * things, this (temporarily) provides support for inheritance.  It
     * also stores the VirtAddr capability so that we can perform other 
     * operations from the Space interface on this region of memory.  
     * Recall that the server does not provide a means to store this 
     * capability (i.e. there is no port to externalize to.).
     *)
    VMTaskSupport.RegisterMemAlloc(space, virtaddr, size);

    IF Debug THEN 
      Print("\tvm_allocate: space = 0x" & Textify.Ref(space) & "\n"); 
    END;

    (*
     * Now we want to zero the memory we just allocated, since the server
     * occasionally relies on zeroed memory.  This probably isn't a good
     * idea, since it kills whatever remnants of efficiency this handler
     * has, but it may save some bugs.  
     *
     * Space.Zero would be nice here.  Instead create an array of zeros
     * and write it in.
     *)
    FOR i := 0 TO LAST(zeroArray) DO
      zeroArray[i] := VAL(0, CHAR);
    END;
    bytesZeroed := size;
    WHILE bytesZeroed < size DO
      TRY
	Translation.Write(space, zeroArray, Word.Plus(virtaddr, bytesZeroed));
      EXCEPT
      | VMError.E(ec) =>
        HandlerUtils.PrintError("\tvm_allocate: BadSize exception " & 
          "in Translation.Write:" & VMError.Message(ec) & ".\n");
        ms.v0 := Syscall.Failure;
	RETURN;
      ELSE
        HandlerUtils.PrintError("\tvm_allocate: exception zeroing " & 
          "with Translation.Write.\n");
        ms.v0 := Syscall.Failure;
        RETURN;
      END;
      INC(bytesZeroed, NUMBER(zeroArray));
    END;
    IF Debug THEN 
      Print("\tAllocated 0x" & Fmt.Unsigned(size) & 
        " bytes of " & "memory at location 0x" & 
        Fmt.Unsigned(virtaddr) & "\n");
    END;
    ms.v0 := Syscall.KERN_SUCCESS;
  END vm_allocate;

PROCEDURE vm_deallocate(strand: Strand.T; 
                        VAR ms: CPU.SavedState) = (* -66 *)
  VAR
    space: Space.T;
    begin, size: Word.T;
  BEGIN
    (* Get the space. *)
    space:= NARROW(HandlerUtils.Internalize(NARROW(strand, 
                                                        UserSpaceThread.T), 
                                               ms.a0), 
                   Space.T);

    (* Get beginning and size of region. *)
    begin := ms.a1;
    size := ms.a2;
    IF Debug THEN Print("\tvm_deallocate: extern space = 0x" & 
      Fmt.Unsigned(ms.a0) & " begin = 0x" & 
      Fmt.Unsigned(begin) & " size = 0x" & Fmt.Unsigned(size) & "\n"); END;

    (*
     * It's possible that this call is in vm_exec and is intended to blank
     * out a tasks entire address space.  In this case, go through all the
     * records we have and deallocate everything.
     *)
    IF begin = VM_MIN_ADDRESS AND size = VM_MAX_ADDRESS THEN
      TRY
        IF Debug THEN Print("\tvm_deallocate: calling ClearSpace: " & 
          "extern space 0x" & Fmt.Unsigned(ms.a0) & "\n"); END;
        VMTaskSupport.ClearSpace(space);
      EXCEPT
      | VMTaskSupport.Failure =>
        ms.v0 := Syscall.Failure;
        RETURN;
      END;
    ELSE

      IF Debug THEN Print("\tvm_deallocate: space = 0x" &
        Textify.Ref(space) & "\n"); END;

      (* 
       * Register the deallocation.  If TRUE is returned, the region should
       * be deallocated.  Otherwise, it can be reused and it should NOT be
       * deallocated.
       *)
      IF VMTaskSupport.RegisterMemDealloc(space, begin, size) THEN
        (* Deallocate the region. *)
        TRY
	  Space.Deallocate(space, begin, size);
        EXCEPT
        | VMError.E(ec) =>
          HandlerUtils.PrintError("\tvm_deallocate: Failure exception in " & 
            "Space.Deallocate:" & VMError.Message(ec) & ".\n");
          ms.v0 := Syscall.Failure;
          RETURN;
        ELSE
          HandlerUtils.PrintError("vm_deallocate: Space.Deallocate " & 
            "exception.\n");
          ms.v0 := Syscall.Failure;
          RETURN;
        END;
      END;
    END;

    ms.v0 := Syscall.KERN_SUCCESS;
  END vm_deallocate;

PROCEDURE vm_protect(strand: Strand.T; 
                     VAR ms: CPU.SavedState) = (* -67 *)
  VAR
    protection := Protection.T{FALSE,FALSE,FALSE,0};
  BEGIN    
    (* Figure out what the protection parameter is.  It is passed in
       through ms.a4 (we want current protection, not max protection). 
       Recall that protection is initialized to be all false. *)
    IF Word.And(ms.a4, VM_PROT_READ) # 0 THEN
      protection.read := TRUE;
    END;
    IF Word.And(ms.a4, VM_PROT_WRITE) # 0 THEN
      protection.write := TRUE;
    END;
    IF Word.And(ms.a4, VM_PROT_EXECUTE) # 0 THEN
      protection.execute := TRUE;
    END;

    WITH space = NARROW(HandlerUtils.Internalize(NARROW(strand, 
                                                           UserSpaceThread.T), 
                                                    ms.a0), 
                        Space.T),
         begin = ms.a1,
         size = ms.a2 DO
      (* Space.Protect requires a virtual address argument.  Get this from
         the VirtAddr.T capability table now. *)
      TRY
        Space.Protect(space, begin, size, protection);
      EXCEPT
      | VMError.E =>
        (*
        HandlerUtils.PrintError("\tvm_protect: Failure exception in " & 
          "Space.Protect:" & VMError.Message(ec) & ".\n");
          *)
        ms.v0 := Syscall.Failure;
        RETURN;
      ELSE
        HandlerUtils.PrintError("\tvm_protect: Space.Protect exception.\n");
        ms.v0 := Syscall.Failure;
        RETURN;
      END;
    END;

    ms.v0 := Syscall.KERN_SUCCESS;
  END vm_protect;

BEGIN
  FOR i := FIRST(vmRegionCalled) TO LAST(vmRegionCalled) DO
    vmRegionCalled[i].externSpace := 0;
    vmRegionCalled[i].numcalls := 0;
  END;
END VMHandlers.
