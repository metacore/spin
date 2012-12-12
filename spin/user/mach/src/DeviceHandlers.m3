(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *      Removed ifp network interface system.  I did not try to put in
 *      the new netdev system since I do not know how to test.
 *
 * 11-May-97  Yasushi Saito (yasushi) at the University of Washington
 *	Removed VirtAddr related codes.
 * 28-May-96  becker at the University of Washington
 *	Removed *withInterlock calls from CharDevice
 *
 * 20-May-96  David Dion (ddion) at the University of Washington
 *	Added network support
 *
 * 04-Apr-96  David Dion (ddion) at the University of Washington
 *	Updated for spin-20
 *
 * 10-Mar-96  David Dion (ddion) at the University of Washington
 *	Made user-level libc I/O work.  Change carriage return to line feed.
 *	Echo input character at device_read from console.
 *
 * 28-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Changed to conform to the new syscall interface.
 *
 * 15-Dec-95  David Dion (ddion) at the University of Washington
 *	Updated to spin-8 for CVS.
 *
 * 27-Jul-95  David Dion (ddion) at the University of Washington
 *      Created to separate device calls from other system calls.
 *)
MODULE DeviceHandlers;

IMPORT UserSpaceThread, Strand, Space, Translation, CPU, Text, Word;
IMPORT Fmt; (* mainly for debugging *)
IMPORT Device;
IMPORT CharDevice;
IMPORT Disk;
IMPORT Error;
IMPORT VMTaskSupport;
IMPORT Syscall;
IMPORT HandlerUtils;
FROM HandlerUtils IMPORT Debug;
FROM HandlerUtils IMPORT Print;
IMPORT StrongRef;
IMPORT NameServer;
IMPORT Buffer;

(* networking stuff *)

IMPORT If, Net, Ctypes, Mbuf, EtherPacket, SocketRep;
IMPORT Ether, EtherPktFormat;
IMPORT Sema;

IMPORT IO;


CONST
  IO_INBAND_MAX = 128; (* max inband data - taken from device_types.h *)
  (*IO_BUF_MAX = 1024;*) (* from io_buf_ptr_t - it's an array of 1024 bytes *)
  OVERWRITE_MASK = 16_10000; (* from libmach/spin_device_read_overwrite.c *)

  MaxNEW = 16_20000; (* hack - don't crash machine by allocating too much *)

  D_SUCCESS = 0;
  D_NO_SUCH_DEVICE = 2502;

  WordSize = BYTESIZE(Word.T); (* more compact representation *)


VAR 
  (*
  etherIfp: UNTRACED REF If.ifnet;
  *)
  etherExtern: Word.T := 16_daddad; (* hack - how to externalize untraced? *)
  serverReadyForEther: Sema.T := Sema.Alloc(0);
  mbufFreeMethod := NEW(Mbuf.Methods, free := Buffer.FreeBuffer);


PROCEDURE device_read(strand: Strand.T; 
                      VAR ms: CPU.SavedState; 
                      inband: BOOLEAN) = (* -77 *)
  VAR
    callerUthread: UserSpaceThread.T;
    space: Space.T;
    device: Device.T; (* saves a NARROW *)
    datacount: Word.T;
    dataPtr: REF ARRAY OF CHAR := NIL;
    virtaddr: Word.T;
    allocSize: Word.T;
    devIsConsole: BOOLEAN := FALSE;
  BEGIN
    callerUthread := NARROW(strand, UserSpaceThread.T);
    space := UserSpaceThread.GetSpace(callerUthread);

    (*
     * First check if this is a read from the ether port.  Choke if it is.
     *)

    IF ms.a0 = etherExtern THEN
      Print("\n\nTrying to read from ether!!!\n\n");
      ms.v0 := D_NO_SUCH_DEVICE;
    END;


    (*
     * Internalize device pointer.  This is a pointer to a device object
     * which supports the read method.  
     *)
    device := NARROW(HandlerUtils.Internalize(callerUthread, ms.a0), Device.T);

    (*
     * First make sure we understand this type of device.
     *)
    IF NOT ISTYPE(device, CharDevice.T) THEN
      HandlerUtils.PrintError("Trying to read from a non-CharDevice.T.\n");
      ms.v0 := D_NO_SUCH_DEVICE;
      RETURN;
    END;

    (*
     * Next determine whether we are dealing with the console or the
     * disk.  We must handle things differently for the two.
     *)
    IF Text.Equal(device.name(), "console") THEN
      devIsConsole := TRUE;
    END;

    (*
     * The number of bytes wanted is passed in through ms.a3.  This will
     * be the size of the buffer allocated for reading.
     *
     * We need to check whether the number of bytes to read is greater
     * than the maximum allowed value.  For device_read_inband, this 
     * number is IO_INBAND_MAX.  However, the device_read_inband call is
     * also used for device_read_overwrite, for which the limit is much
     * larger.  To figure out if we're in a device_read_overwrite, we
     * make use of the OVERWRITE_MASK hack on the mode.
     *
     * Hence, if mode AND OVERWRITE_MASK is 0, then we're inband and we
     * need to limit the datacount.  Someday we might check the datacount
     * for overwrite, too ...
     *)
    datacount := ms.a3;
    IF inband AND Word.And(ms.a1, OVERWRITE_MASK) = 0 THEN
      IF datacount > IO_INBAND_MAX THEN
        datacount := IO_INBAND_MAX;
      END;
      IF Debug THEN Print("\tdevice_read_inband: datacount = 0x" &
        Fmt.Unsigned(datacount) & "\n"); END;
    ELSIF inband THEN
      IF Debug THEN Print("\tdevice_read_overwrite: datacount = 0x" &
        Fmt.Unsigned(datacount) & "\n"); END;
    END;

    (*
     * Allocate a data buffer to read into.
     *)
    IF datacount > 16_0 AND datacount < MaxNEW THEN
      dataPtr := NEW(REF ARRAY OF CHAR, datacount);
    ELSE
      HandlerUtils.PrintError("device_read: called NEW with size 0x" &
        Fmt.Unsigned(datacount) & "\n");
      ms.v0 := Syscall.Failure;
      RETURN;
    END;

    (*
     * Read the data.
     *)
    TRY
      IF devIsConsole THEN
        WITH charDev = NARROW(device, CharDevice.T) DO
          datacount := charDev.read(dataPtr^);
        END;
      ELSE
        WITH diskDev = NARROW(device, Disk.T) DO
          (* the disk offset is stored in ms.a2 *)
          StrongRef.Add(dataPtr);
          datacount := diskDev.read(dataPtr^, ms.a2 * 512);
          StrongRef.Remove(dataPtr);
        END;
      END;
    EXCEPT
    | Error.E(v) =>
      IF v.resultCode() # D_SUCCESS THEN
        ms.v0 := v.resultCode();
        RETURN;
      END;
    ELSE
      HandlerUtils.PrintError("device_read: unknown exception.\n");
      ms.v0 := Syscall.Failure;
      RETURN;
    END;

    (*
     * The server will not consider carriage return a "break" character.
     * Hence, change carriage returns to line feeds.
     * XXX This is probably a bad hack which affects applications
     * XXX which demand raw control over characters.
     *)
    IF devIsConsole THEN
      IF dataPtr^[datacount-1] = '\r' THEN
        dataPtr^[datacount-1] := '\n';
      END;
    END;

    (*
     * Now copy back the data.
     *
     * If inband = TRUE, then the address in ms.a4 is the address we want
     * to write the data back to.  This corresponds to device_read_inband.
     * Otherwise, we need to allocate a chunk of memory, write the data to 
     * that memory, and set the pointer pointed to by ms.a4 to that chunk 
     * of memory.  This corresponds to device_read.
     *)
    IF inband THEN
      IF NOT HandlerUtils.CopyOut(space, dataPtr^, ms.a4, datacount) THEN
        HandlerUtils.PrintError("device_read: CopyOut exception.\n");
        ms.v0 := Syscall.Failure;
        RETURN;
      END;
    ELSE
      (* First allocate the data using VMTaskSupport.VMAllocAnywhere *)
      allocSize := datacount;
      VMTaskSupport.VMAllocAnywhere(space, virtaddr, allocSize);
      IF virtaddr = 0 THEN (* virtaddr is set to zero on error *)
        HandlerUtils.PrintError("device_read: VMAllocAnywhere failed.\n");
        ms.v0 := Syscall.Failure;
        RETURN;
      END;
      (* Register the new virtual address region. *)
      VMTaskSupport.RegisterMemAlloc(space, virtaddr, allocSize);

      (* Now write the data there. *)
      IF NOT HandlerUtils.CopyOut(space, dataPtr^, virtaddr, datacount) THEN
        HandlerUtils.PrintError("device_read: CopyOut exception.\n");
        ms.v0 := Syscall.Failure;
        RETURN;
      END;

      (* Now write the address back. *)
      WITH vmCopyArray = VIEW(virtaddr, ARRAY [0..WordSize-1] OF CHAR) DO
        IF NOT HandlerUtils.CopyOut(space, vmCopyArray, ms.a4, WordSize) THEN
          HandlerUtils.PrintError("device_read: CopyOut exception.\n");
          ms.v0 := Syscall.Failure;
          RETURN;
        END;
      END;
    END;

    (*
     * Write back how many bytes were read.
     *)
    WITH vmCopyArray = VIEW(datacount, ARRAY [0..WordSize-1] OF CHAR) DO
      IF NOT HandlerUtils.CopyOut(space, vmCopyArray, ms.a5, WordSize) THEN
        HandlerUtils.PrintError("device_read: CopyOut exception.\n");
        ms.v0 := Syscall.Failure;
        RETURN;
      END;
    END;

    ms.v0 := D_SUCCESS;
  END device_read;

PROCEDURE device_write(strand: Strand.T; 
                       VAR ms: CPU.SavedState) = (* -79 *)
  VAR
    callerUthread: UserSpaceThread.T;
    space: Space.T;
    device: Device.T; (* saves us a NARROW *)
    dataPtr: REF ARRAY OF CHAR;
    buffer: Buffer.T;
    datacount: Word.T;
    devIsConsole: BOOLEAN := FALSE;

    mbuf: Mbuf.T;
    sockaddr: SocketRep.sockaddr;
 
  BEGIN
    callerUthread := NARROW(strand, UserSpaceThread.T);
    space := UserSpaceThread.GetSpace(callerUthread);

    (*
     * First check if this is a write to the ether device.  We handle
     * this case completely separately until all devices are standardized.
     *)

    IF ms.a0 = etherExtern THEN
      (*
       * Allocate a buffer for the data.
       *)
      (*
      IF ms.a4 > 16_0 AND ms.a4 < MaxNEW THEN
        dataPtr := NEW(REF ARRAY OF CHAR, ms.a4);
      ELSE
        HandlerUtils.PrintError("device_write: called NEW with size 0x" &
          Fmt.Unsigned(ms.a4) & "\n");
        ms.v0 := Syscall.Failure;
        RETURN;
      END;
      *)
      IF ms.a4 > 16_0 AND ms.a4 < MaxNEW THEN
        buffer := Buffer.Allocate(ms.a4);
        dataPtr := buffer.data;
      ELSE
        HandlerUtils.PrintError("device_write: called NEW with size 0x" &
          Fmt.Unsigned(ms.a4) & "\n");
        ms.v0 := Syscall.Failure;
        RETURN;
      END;

      (*
       * Read the data into the kernel buffer.
       *)
      IF NOT HandlerUtils.CopyIn(space, ms.a3, dataPtr^, ms.a4) THEN
        HandlerUtils.PrintError("device_write: CopyIn exception.\n");
        ms.v0 := Syscall.Failure;
        Buffer.Deallocate(buffer);
        RETURN;
      END;

      (*
       * Wrap the data into an Mbuf.
       *)
      TRY
        mbuf := Mbuf.MclGetOa(dataPtr, ms.a4, mbufFreeMethod, buffer);
      EXCEPT
      | Mbuf.LengthMismatch =>
        HandlerUtils.PrintError("device_write: mbuf length mismatch.\n");
        ms.v0 := Syscall.Failure;
        Buffer.Deallocate(buffer);
        RETURN;
      ELSE
        HandlerUtils.PrintError("device_write: unknown mbuf exception.\n");
        ms.v0 := Syscall.Failure;
        Buffer.Deallocate(buffer);
        RETURN;
      END;

      (*
       * Fire it off directly to the Ether interface.
      EVAL EtherPacket.Output(etherIfp^, mbuf, sockaddr);
       *)
IO.Put("mach extension: etherIfp was replaced by ether device\n");

      ms.v0 := Syscall.KERN_SUCCESS;
      RETURN;
    END;
 

    (*
     * Internalize device pointer.  This is a pointer to a device object
     * which supports the write method.
     *)
    device := NARROW(HandlerUtils.Internalize(callerUthread, ms.a0), Device.T);

    (*
     * First make sure we understand this type of device.
     *)
    IF NOT ISTYPE(device, CharDevice.T) THEN
      ms.v0 := D_NO_SUCH_DEVICE;
      RETURN;
    END;

    (*
     * Next determine whether we are dealing with the console or the
     * disk.  We must handle things differently for the two.
     *)
    IF Text.Equal(device.name(), "console") THEN
      devIsConsole := TRUE;
    END;

    (*
     * The data to be written is in an array starting at the location 
     * passed in ms.a3 and consists of ms.a4 bytes.  Allocate an
     * appropriately sized array and copy the data over.
     *)
    IF ms.a4 > 16_0 AND ms.a4 < MaxNEW THEN
      dataPtr := NEW(REF ARRAY OF CHAR, ms.a4);
    ELSE
      HandlerUtils.PrintError("device_write: called NEW with size 0x" &
        Fmt.Unsigned(ms.a4) & "\n");
      ms.v0 := Syscall.Failure;
      RETURN;
    END;

    IF NOT HandlerUtils.CopyIn(space, ms.a3, dataPtr^, ms.a4) THEN
      HandlerUtils.PrintError("device_write: CopyIn exception.\n");
      ms.v0 := Syscall.Failure;
      RETURN;
    END;

    (*
     * Write the data to the device.
     *)
    TRY
      IF devIsConsole THEN
        WITH charDev = NARROW(device, CharDevice.T) DO
          datacount := charDev.write(dataPtr^);
        END;
      ELSE
        WITH diskDev = NARROW(device, Disk.T) DO
          (* the disk offset is passed in ms.a2 *)
          datacount := diskDev.write(dataPtr^, ms.a2);
        END;
      END;
    EXCEPT
    | Error.E(v) =>
      IF v.resultCode() # D_SUCCESS THEN
        ms.v0 := v.resultCode();
        RETURN;
      END;
    ELSE
      HandlerUtils.PrintError("device_write: unknown exception.\n");
      ms.v0 := Syscall.Failure;
      RETURN;
    END;

    (*
     * Now write back to caller how many bytes were actually written and
     * return D_SUCCESS.
     *)
    WITH copyArray = VIEW(datacount, ARRAY [0..WordSize-1] OF CHAR) DO
      IF NOT HandlerUtils.CopyOut(space, copyArray, ms.a5, WordSize) THEN
        HandlerUtils.PrintError("device_write: CopyOut exception.\n");
        ms.v0 := Syscall.Failure;
        RETURN;
      END;
    END;

    ms.v0 := D_SUCCESS;
  END device_write;

PROCEDURE device_open(strand: Strand.T; 
                      VAR ms: CPU.SavedState) = (* -84 *)
  VAR
    callerUthread: UserSpaceThread.T;
    space: Space.T;
    deviceNameStr: REF ARRAY OF CHAR;
    deviceNameTxt: TEXT;
    device: Device.T;
    deviceExtern: Word.T;
  BEGIN
    callerUthread := NARROW(strand, UserSpaceThread.T);
    space := UserSpaceThread.GetSpace(callerUthread);

    (*
     * Read in name of device.  The string length of the devicename is 
     * passed in ms.a3, so first allocate appropriately sized array.
     *)
    IF ms.a3 > 16_0 AND ms.a3 < MaxNEW THEN
      deviceNameStr := NEW(REF ARRAY OF CHAR, ms.a3);
    ELSE
      HandlerUtils.PrintError("device_open: called NEW with size 0x" &
        Fmt.Unsigned(ms.a3) & "\n");
      ms.v0 := Syscall.Failure;
      RETURN;
    END;

    IF NOT HandlerUtils.CopyIn(space, ms.a2, deviceNameStr^, ms.a3) THEN
      HandlerUtils.PrintError("device_open: CopyIn exception.\n");
      ms.v0 := Syscall.Failure;
      RETURN;
    END;

    (*
     * Convert the array of chars into a TEXT instance.
     *)
    deviceNameTxt := Text.FromChars(deviceNameStr^);

    IF Debug THEN Print("\tdeviceNameTxt = " & deviceNameTxt & "\n"); END;

    (*
     * The ether device is not registered in the name server yet.  Check 
     * if "ln0" is the device name and treat it differently.
     *)
    IF Text.Equal(deviceNameTxt, "ln0") THEN
 
      (*
      etherIfp := IfUtil.GetIf("ln", 0);
      *)
IO.Put("mach extension: etherIfp was replaced by ether device\n");
      deviceExtern := etherExtern;
 
    ELSE
      (*
       * Get handle on this device using Device.Lookup.
       *)
      TRY
        device := Device.Lookup(deviceNameTxt);
      EXCEPT
      | NameServer.Error(ec) =>
        IF ec = NameServer.EC.NameNotFound THEN
          HandlerUtils.PrintError("Device Lookup failed on device name " & 
            deviceNameTxt & "\n");
          ms.v0 := D_NO_SUCH_DEVICE;
        ELSIF ec = NameServer.EC.Unauthorized THEN
          HandlerUtils.PrintError("Device Lookup NameServer error.\n");
          ms.v0 := D_NO_SUCH_DEVICE;
        END;
        RETURN;
      ELSE
        HandlerUtils.PrintError("Unknown exception in Device Lookup on " & 
          "device name " & deviceNameTxt & "\n");
        ms.v0 := Syscall.Failure;
        RETURN;
      END;

      (*
       * Open the device.  All registered devices should have an open() 
       * method which takes zero arguments.  But we want to set off an
       * alarm if anybody manages to get a handle on a non-CharDevice.T.
       *)
      IF NOT ISTYPE(device, CharDevice.T) THEN
        HandlerUtils.PrintError("Trying to open a non-CharDevice.T: " &
          deviceNameTxt & "\n");
        ms.v0 := D_NO_SUCH_DEVICE;
        RETURN;
      END;

      TRY
        WITH charDev = NARROW(device, CharDevice.T) DO
          charDev.open();
        END;
      EXCEPT
      | Error.E(v) =>
        HandlerUtils.PrintError("char device_open failed on device name " &
          deviceNameTxt & "\n");
        HandlerUtils.PrintError("char device_open: " & v.message() & "\n");
        ms.v0 := v.resultCode();
        RETURN;
      ELSE
        HandlerUtils.PrintError("char device_open: unknown " & 
          "exception opening device name " & deviceNameTxt & "\n");
        ms.v0 := Syscall.Failure;
        RETURN;
      END;

      (*
       * Externalize device object to write it back to caller.
       *)
      deviceExtern := HandlerUtils.Externalize(callerUthread, device);
    END;
      
    (*
     * Write back externalized device pointer.
     *)
    WITH copyArray = VIEW(deviceExtern, ARRAY[0..WordSize-1] OF CHAR) DO
      IF NOT HandlerUtils.CopyOut(space, copyArray, ms.a4, WordSize) THEN
        HandlerUtils.PrintError("device_open: CopyOut exception.\n");
        ms.v0 := Syscall.Failure;
        RETURN;
      END;
    END;

    ms.v0 := D_SUCCESS;
  END device_open;

PROCEDURE device_close(strand: Strand.T; 
                       VAR ms: CPU.SavedState) = (* -85 *)
  VAR
    device: Device.T;
  BEGIN

    IF ms.a0 = etherExtern THEN
      Print("\n\nTrying to close ether!!!\n\n");
      ms.v0 := D_NO_SUCH_DEVICE;
    END;


    (*
     * Internalize device pointer.
     *)
    device := NARROW(HandlerUtils.Internalize(NARROW(strand, 
                                                     UserSpaceThread.T), 
                                              ms.a0),
                     Device.T);

    (*
     * Make sure we know what this device is.
     *)
    IF NOT ISTYPE(device, CharDevice.T) THEN
      HandlerUtils.PrintError("Trying to close a non-CharDevice.T. \n");
      ms.v0 := D_NO_SUCH_DEVICE;
      RETURN;
    END;

    (*
     * Close it.
     *)
    TRY
      WITH charDev = NARROW(device, CharDevice.T) DO
        charDev.close();
      END;
    EXCEPT
    | Error.E(v) =>
      ms.v0 := v.resultCode();
      RETURN;
    END;

    ms.v0 := D_SUCCESS;
  END device_close;

VAR (* values in the tty status structure *)
  tt_ispeed: Word.T := 16_c;
  tt_ospeed: Word.T := 16_c;
  tt_breakc: Word.T := 16_0;
  tt_flags: Word.T  := 16_38c;

  flavor: Word.T := Word.Shift(ORD('t'),16) + 1;

PROCEDURE device_set_status(strand: Strand.T; 
                            VAR ms: CPU.SavedState) = (* -87 *)
  VAR
    callerUthread: UserSpaceThread.T;
    space: Space.T;
    device: Device.T;
    status: REF ARRAY OF CHAR;
    statusSize: (* MachTypes.MachMsgTypeNumberT *) Word.T ;
    status0, status1, status2, status3: Word.T;
  BEGIN
    callerUthread := NARROW(strand, UserSpaceThread.T);
    space := UserSpaceThread.GetSpace(callerUthread);
    (*
     * Internalize device pointer.
     *)
 
    IF ms.a0 = etherExtern THEN
      IF ms.a1 = 12345 (* magic number hack *) THEN
        Sema.V(serverReadyForEther);
        ms.v0 := Syscall.KERN_SUCCESS;
      ELSE
        Print("device_set_status on ln0: flavor = 0x" & 
          Fmt.Unsigned(ms.a1) & "\n");
        ms.v0 := D_NO_SUCH_DEVICE;
      END;
      RETURN;
    END;
 

    device := NARROW(HandlerUtils.Internalize(callerUthread, ms.a0),
                     Device.T);
                

    (*
     * Allocate and copy in status array.
     *)
    statusSize := ms.a3;
    IF statusSize * 4 > 16_0 AND statusSize * 4 < MaxNEW THEN
      status := NEW(REF ARRAY OF CHAR, statusSize * 4);
    ELSE
      (*
      HandlerUtils.PrintError("device_set_status: called NEW with size 0x" &
        Fmt.Unsigned(statusSize) & "\n");
        *)
      ms.v0 := Syscall.Failure;
      RETURN;
    END;

    IF ms.a2 = 0 THEN
      (*
      HandlerUtils.PrintError("device_set_status: Translation.Read from NULL.\n");
      *)
      ms.v0 := Syscall.Failure;
      RETURN;
    END;

    IF NOT HandlerUtils.CopyIn(space, ms.a2, status^, statusSize) THEN
      HandlerUtils.PrintError("device_set_status: CopyIn exception.\n");
      ms.v0 := Syscall.Failure;
      RETURN;
    END;

    (*
     * Set device status.
     *)
(*
    TRY
      device.setstatus(ms.a1, status^, statusSize);
    EXCEPT
    | Error.E(v) =>
      ms.v0 := v.resultCode();
      RETURN;
    END;
*)

    (*
     * The following is not correct.  Often the server will call
     * device_set_status followed by device_get_status.  We want to
     * make sure fixpoint is reached -- that is, the server thinks
     * the status is set as it wants and does not continue to try to
     * set it.  
     *
     * XXX This is temporary until we have real device_{get|set}_status
     * XXX routines.
     *)
    (* IF ISTYPE(device, CharDevice.T) THEN *)
    IF Text.Equal(device.name(), "console") THEN
      IF statusSize # 4 THEN
        HandlerUtils.PrintError("\tdevice_set_status: statusSize = " & 
          Fmt.Int(statusSize) & "\n");
      ELSIF ms.a1 # flavor THEN
        HandlerUtils.PrintError("\tdevice_set_status: flavor = " &
          Fmt.Int(ms.a1) & "\n");
      ELSE
        HandlerUtils.ArrayToWord(status1, status^, 4, 7);
        HandlerUtils.ArrayToWord(status2, status^, 8, 11);
        HandlerUtils.ArrayToWord(status3, status^, 12, 15);
        HandlerUtils.ArrayToWord(status0, status^, 0, 3);

        IF Debug THEN 
          Print("  device_set_status new status:\n\t" &
          "ispeed: " & Fmt.Unsigned(status0) & "\n\t" &
          "ospeed: " & Fmt.Unsigned(status1) & "\n\t" &
          "breakc: " & Fmt.Unsigned(status2) & "\n\t" &
          "flags:  " & Fmt.Unsigned(status3) & "\n");
        END;

        tt_ispeed := status0;
        tt_ospeed := status1;
        tt_breakc := status2;
        tt_flags  := status3;
      END;
    END;

    ms.v0 := D_SUCCESS;
  END device_set_status;


(* Hack ethernet support until devices are standardized. *)
TYPE NetStatus = RECORD
  min_packet_size: Ctypes.int;
  max_packet_size: Ctypes.int;
  header_format: Ctypes.int;
  header_size: Ctypes.int;
  address_size: Ctypes.int;
  flags: Ctypes.int;
  mapped_size: Ctypes.int;
END;

CONST
  NET_ADDRESS = Word.Shift(ORD('n'),16) + 2;
  NET_STATUS = Word.Shift(ORD('n'),16) + 1;
  HDR_ETHERNET = 1;
  ETHERHEADERLEN = BYTESIZE(EtherPktFormat.Header);


PROCEDURE device_get_status(strand: Strand.T; 
                            VAR ms: CPU.SavedState) = (* -88 *)
  VAR
    callerUthread: UserSpaceThread.T;
    space: Space.T;
    device: Device.T;
    vmCopyStatus: REF ARRAY OF CHAR;
    statusSize: Word.T;
    status0, status1, status2, status3: Word.T;
 
    ifdevea: ARRAY[0..BYTESIZE(If.ifdevea)-1] OF Net.BYTE;
    ifstat: NetStatus;

  BEGIN
    callerUthread := NARROW(strand, UserSpaceThread.T);
    space := UserSpaceThread.GetSpace(callerUthread);

    (*
     * First check if this is the ether device.  We handle it differently.
     *)
 
    IF ms.a0 = etherExtern THEN
      (*
       * This could actually be checking the device status, or it could be
       * querying for the hardware address.  We know the hardware address.
       * For the actual status, return some reasonable flags and hope it 
       * works.  We can tell which is which by checking the "flavor" in ms.a1.
       *)
      IF ms.a1 = NET_STATUS THEN
        (* set status structure values *)
        ifstat.min_packet_size := 0;
        ifstat.max_packet_size := 1500;
        ifstat.header_format := HDR_ETHERNET;
        ifstat.header_size := 14;
        ifstat.address_size := 6;
        ifstat.flags :=     If.IFF_UP (* interface is up *) + 
                            If.IFF_BROADCAST (* broadcast address valid *) + 
                            If.IFF_NOTRAILERS (* avoid use of trailers *) +
                            If.IFF_RUNNING (* resources allocated *) +
                            If.IFF_ALLMULTI (* receive all multicast pkts *) +
                            If.IFF_SIMPLEX (* can't hear own transmissions *) ;
        ifstat.mapped_size := 0;

        (* copyout *)
        WITH vmCopy = VIEW(ifstat, ARRAY[0..BYTESIZE(ifstat)-1] OF CHAR) DO
          IF NOT HandlerUtils.CopyOut(space, 
                                      vmCopy, 
                                      ms.a2, 
                                      BYTESIZE(ifstat)) THEN
            HandlerUtils.PrintError("device_get_status: CopyOut exception.\n");
            ms.v0 := Syscall.Failure;
            RETURN;
          END;
        END;
        ms.v0 := Syscall.KERN_SUCCESS;
        RETURN;

      ELSIF ms.a1 = NET_ADDRESS THEN
IO.Put("mach extension: etherIfp was replaced by ether device\n");
	(*
        IF IfUtil.ioctl(etherIfp, IoctlPosix.SIOCRPHYSADDR, ifdevea) # 0 THEN
          HandlerUtils.PrintError("Could not get ethernet address!\n");
          ms.v0 := D_NO_SUCH_DEVICE;
          RETURN;
        ELSE
          WITH etherInfo = VIEW(ifdevea, If.ifdevea),
               vmCopyEtherPA = VIEW(etherInfo.current_pa,
                                    ARRAY[0..5] OF CHAR) DO
            IF NOT HandlerUtils.CopyOut(space, 
                                        vmCopyEtherPA, 
                                        ms.a2, 
                                        6) THEN
              HandlerUtils.PrintError("device_get_status: " & 
                "CopyOut exception.\n");
              ms.v0 := Syscall.Failure;
              RETURN;
            END;
          END;
            
          ms.v0 := Syscall.KERN_SUCCESS;
          RETURN;
        END;
	  *)
      ELSE
        HandlerUtils.PrintError("device_get_status: unknown ether flavor.\n");
        ms.v0 := Syscall.Failure;
        RETURN;
      END;
    END;
 

    (*
     * Internalize device pointer.  Then try to NARROW it to a CharDevice.T.
     * We currently only support get_status for console, so if the NARROW
     * fails then the function fails.
     *)
    device := HandlerUtils.Internalize(callerUthread, ms.a0);

    IF NOT Text.Equal(device.name(), "console") THEN
      HandlerUtils.PrintError("device_get_status: device is not console!\n");
      ms.v0 := Syscall.Failure;
      RETURN;
    ELSIF ms.a1 # flavor THEN
      HandlerUtils.PrintError("device_get_status: wrong flavor: " & 
        Fmt.Unsigned(ms.a1) & "\n");
      ms.v0 := Syscall.Failure;
      RETURN;
    END;

    (*
     * ms.a3 contains a pointer to the number of 4-byte integers in the
     * status array.  Read that number over into an array of bytes.
     *
     * But, even though the server is SUPPOSED to send a pointer to a long,
     * it may send a pointer to an int, which means we'd better mask off
     * the top 4 bytes of our result so that we don't get junk.
     *)
    WITH copyArray = VIEW(statusSize, ARRAY[0..WordSize-1] OF CHAR) DO
      IF NOT HandlerUtils.CopyIn(space, ms.a3, copyArray, WordSize) THEN
        HandlerUtils.PrintError("device_get_status: CopyIn exception.\n");
        ms.v0 := Syscall.Failure;
        RETURN;
      END;
    END;
    statusSize := Word.And(statusSize, 16_00000000ffffffff);

    (*
     * Allocate an array of bytes to hold the status.
     *)
    IF statusSize * 4 > 16_0 AND statusSize  * 4 < MaxNEW THEN
      vmCopyStatus := NEW(REF ARRAY OF CHAR, statusSize * 4);
    ELSE
      HandlerUtils.PrintError("device_get_status: called NEW with size 0x" &
        Fmt.Unsigned(statusSize * 4) & "\n");
      ms.v0 := Syscall.Failure;
      RETURN;
    END;

    (*
     * Ideally here you call device.getStatus, but that's not quite
     * implemented.  Fortunately, we know the crucial time this
     * is called during the server boot, and we can just send back
     * the values Mach sends back.  Set them now.
     *)
    status0 := tt_ispeed;
    status1 := tt_ospeed;
    status2 := tt_breakc;
    status3 := tt_flags;

    (*
     * Now put these values into the vmCopyStatus array.  The vmCopyStatus
     * array will be interpreted as an array of 32-bit INTEGERS up in
     * server-land, so we must make sure to interpret our status data as
     * 16 bytes of 4-byte integers instead of 32 bytes of 8-byte integers.
     *)
    HandlerUtils.WordToArray(status0, vmCopyStatus^, 0, 3);
    HandlerUtils.WordToArray(status1, vmCopyStatus^, 4, 7);
    HandlerUtils.WordToArray(status2, vmCopyStatus^, 8, 11);
    HandlerUtils.WordToArray(status0, vmCopyStatus^, 12, 15);

    (*
     * Now write back the status. 
     *)
    IF NOT HandlerUtils.CopyOut(space, 
                                vmCopyStatus^, 
                                ms.a2, 
                                statusSize * 4) THEN
      HandlerUtils.PrintError("device_get_status: CopyOut exception.\n");
      ms.v0 := Syscall.Failure;
      RETURN;
    END;

    (*
     * Convert the statusSize back to an array and write it back.  
     * Theoretically, vmCopy4 has not changed since we read in statusSize,
     * but if the call to device.getStatus ever happens, statusSize will
     * be a VAR parameter.
     *)
    WITH copyArray = VIEW(statusSize, ARRAY[0..3] OF CHAR) DO
      IF NOT HandlerUtils.CopyOut(space, copyArray, ms.a3, 4) THEN
        HandlerUtils.PrintError("device_get_status: CopyOut exception.\n");
        ms.v0 := Syscall.Failure;
        RETURN;
      END;
    END;

    ms.v0 := D_SUCCESS;
  END device_get_status;


VAR
  etherPacketArrived: Sema.T;
  etherPacketQueue: If.ifqueue;
  etherPacketQueueLock: MUTEX;
  etherReceiveInit: BOOLEAN := FALSE;


PROCEDURE EtherGuard (<* UNUSED *>packet: Mbuf.T; 
                      <* UNUSED *>curr: Mbuf.T; 
                      <* UNUSED *>offset: CARDINAL) : BOOLEAN =
  BEGIN
    RETURN TRUE;
  END EtherGuard;

PROCEDURE EtherPacketHandler(packet: Mbuf.T;
                             <* UNUSED *>curr: Mbuf.T;
                             <* UNUSED *>offset: CARDINAL) : BOOLEAN =
  VAR
    mbuf: Mbuf.T;
  BEGIN
    (*
     * The packet will be freed after this handler returns, so we need
     * to copy it.
     *)
    mbuf := Mbuf.m_copym(packet, 0, Mbuf.M_COPYALL, Mbuf.M_WAIT);

    (*
     * Enqueue the copied packet and signal its arrival. 
     *)
    IF mbuf = NIL THEN
      (* drop it on the floor *)
      RETURN FALSE;
    END;

    LOCK etherPacketQueueLock DO
      IF etherPacketQueue.ifq_len < etherPacketQueue.ifq_maxlen THEN
        If.Enqueue(etherPacketQueue, mbuf);
      ELSE
        INC(etherPacketQueue.ifq_drops);
        RETURN FALSE;
      END;
    END;
    Sema.V(etherPacketArrived);
    RETURN TRUE;
  END EtherPacketHandler;

PROCEDURE get_ether_packet(strand: Strand.T; 
                           VAR ms: CPU.SavedState) = (* 1700 *)
  VAR
    header: Mbuf.T;
    data: Mbuf.T;
    typeval: Word.T;
    headerAddr: Word.T;
    dataAddr: Word.T;
    typeAddr: Word.T;
    lenAddr: Word.T;
    replyAddr: Word.T;
    pos: INTEGER;
    space: Space.T;
  BEGIN
    space := UserSpaceThread.GetSpace(NARROW(strand, UserSpaceThread.T));

    (*
     * Perform initialization duties if this is the first pass through.
     *)
    IF NOT etherReceiveInit THEN
      etherReceiveInit := TRUE;

      (*
       * Do nothing until we know that everything is ready on the server
       * side of the world.
       *)
      Sema.P(serverReadyForEther);

      (*
       * Allocate the packet arrived semaphore.
       *)
      etherPacketArrived := Sema.Alloc(0);

      (*
       * Initialize the packet receive queue.
       *)
      etherPacketQueue.ifq_head := NIL;
      etherPacketQueue.ifq_tail := NIL;
      etherPacketQueue.ifq_len := 0;
      etherPacketQueue.ifq_maxlen := If.MAXLEN;
      etherPacketQueue.ifq_drops := 0;
      etherPacketQueueLock := NEW(MUTEX);

      (*
       * Install the handler on the packet arrived event.
       *)
      WITH guard = EtherGuard,
           hndlr = EtherPacketHandler,
           event = Ether.PacketArrived DO
        EVAL Ether.Install(event, guard, hndlr);
      END;

    END;
    
    (*
     * Wait to be signalled that a packet has arrived.
     *)
    LOOP
      Sema.P(etherPacketArrived);

      (*
       * Dequeue the header.  Then locate the data mbuf.  
       *
       * header: the entire mbuf chain
       * data: the same mbuf chain with the header lopped off the top
       *)
      LOCK etherPacketQueueLock DO
        header := If.Dequeue(etherPacketQueue);
      END;
      IF header # NIL THEN
        EXIT;
      ELSE
        (* This should never happen ... but just in case we check. *)
        Print("get_ether_packet: dropped packet with NIL header.\n");
      END;
    END;

    data := header.mh_hdr.mh_next;

    (*
     * Copy it out.
     *)
    headerAddr := ms.a0;
    dataAddr := ms.a1;
    typeAddr := ms.a2;
    lenAddr := ms.a3;
    replyAddr := ms.a4;

    TRY
      (* first the header - ONLY COPY THE FIRST MBUF in chain *)
      WITH etherHeaderBuf = SUBARRAY(Mbuf.Array(header)^, 0, ETHERHEADERLEN) DO
        Translation.Write(space, etherHeaderBuf, headerAddr);
      END;
      (* now the data *)
      pos := 0;
      WHILE data # NIL DO
        WITH recvBuf = Mbuf.Array(data)^ DO
          IF BYTESIZE(recvBuf) # 0 THEN
            Translation.Write(space, recvBuf, dataAddr + pos);
            INC(pos, BYTESIZE(recvBuf));
          END;
        END;
        data := Mbuf.m_free(data);
      END;
      EVAL Mbuf.m_free(header);
      (* now the type *)
      typeval := HDR_ETHERNET;
      WITH type = VIEW(typeval, ARRAY[0..3] OF CHAR) DO
        Translation.Write(space, type, typeAddr);
      END;
      (* now the length *)
      WITH len = VIEW(pos, ARRAY[0..3] OF CHAR) DO
        Translation.Write(space, len, lenAddr);
      END;
      (* and finally the reply port, which is the externalized ether *)
      WITH reply = VIEW(etherExtern, ARRAY[0..7] OF CHAR) DO
        Translation.Write(space, reply, replyAddr);
      END;
    EXCEPT
    ELSE
      HandlerUtils.PrintError("get_ether_packet: exception during copyout.\n");
      ms.v0 := Syscall.Failure;
      RETURN;
    END;

    ms.v0 := Syscall.KERN_SUCCESS;
  END get_ether_packet;


BEGIN
END DeviceHandlers.
