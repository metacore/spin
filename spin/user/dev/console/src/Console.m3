(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 31-May-97  David Becker at the University of Washington
 *      Added new select support
 *
 * 23-Oct-96  Marc Fiuczynski (mef) at the University of Washington
 *	Added a default select method.
 *
 * 12-Jun-96 oystr at the University of Washington
 *	Maintain a reference count so that we never close the console
 *	(a shared resource) unless *everybody* wants it closed.
 *	XXX should do a better job locking XXX
 *
 * 28-May-96  becker at the University of Washington
 *	Moved exclusive open to dlib
 *
 * 29-Feb-96  Brian Bershad (bershad) at the University of Washington
 *	New Auth/NS interface.
 *
 * 21-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Deleted exceptions that are never raised.
 *
 * 15-Dec-95  David Dion (ddion) at the University of Washington
 *	Commented out annoying trace printf's in Write and Open.
 *
 * 14-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Commented out GetConsole until we really need it.
 *
 * 20-Nov-95  Charlie Garrett (garrett) at the University of Washington
 *      Converting to IO interface.
 * 
 * 06-Aug-95  Brian Bershad (bershad) at the University of Washington
 *	Created.
 *)

(* The console tty corresponds to unix device major 0, minor 0 and the
   cdevsw[0], which uses {cnopen, cnread, cnwrite, cnioctl, cnclose} *)

UNSAFE (* imports extern interface *)
MODULE Console;

IMPORT ConsoleExtern, ConsoleDepExtern;
IMPORT IO, Error, Fmt, NameServer, Auth, ConsoleInterface, Device,
       BSDtty, Word, SalDep;

TYPE
  T = BSDtty.T BRANDED OBJECT
		  OVERRIDES
		    open       := Open;
		    close      := Close;
		    read       := Read;
		    write      := Write;
		    ioctl      := Ioctl;
		    selectproc := SelectProc;
		    descriptor := Descriptor;
		    charsAvail := CharsAvail;
		    sigHandler := SigHandler;
		  END;

  Exception = Device.Error BRANDED OBJECT
  OVERRIDES
    message := Message;
  END;

CONST
  ExceptionMsg = ARRAY [BASE_ERROR..LAST_ERROR] OF TEXT {
    "console: device not in name server",
    "console: cannot use non-zero read offset",
    "console: no select on console device"
    };

VAR
  refcnt : INTEGER := 0;
  
PROCEDURE Message(self:Exception): TEXT =
  VAR
    rc := self.resultCode();
  BEGIN
    IF BASE_ERROR <= rc AND rc < BASE_ERROR+NUMBER(ExceptionMsg) THEN
      RETURN ExceptionMsg[rc]; 
    END;
    IF rc >= BASE_ERROR+NUMBER(ExceptionMsg) THEN
      RETURN "Console: unknown result " & Fmt.Int(rc);
    END;

    RETURN "Console dev unix error: " & Fmt.Int(rc);
  END Message;

PROCEDURE Open(<*UNUSED*> self: T) RAISES {Error.E} =
  VAR
    rc := 0;
  BEGIN
    rc := ConsoleExtern.console_open();
    IF rc # 0 THEN RAISE Error.E(NEW(Exception).init(rc)) END;
    INC(refcnt);      
  END Open;

PROCEDURE Close(<*UNUSED*> self: T) RAISES {Error.E} =
  VAR
    rc := 0;
  BEGIN
    IF refcnt > 1 THEN
      DEC(refcnt);
      RETURN;
    END;
    IO.Put("closing the console\n");
    refcnt := 0;
    rc := ConsoleExtern.console_close();
    IF rc # 0 THEN RAISE Error.E(NEW(Exception).init(rc)) END;
  END Close;

PROCEDURE Write(<*UNUSED*> self: T; READONLY data: ARRAY OF CHAR;
		<*UNUSED*>offset: CARDINAL := 0): CARDINAL RAISES {Error.E} =
  VAR
    rc := 0;
    bytes := NUMBER(data);
  BEGIN
    rc := ConsoleExtern.console_write(data,bytes);
    IF rc # 0 THEN RAISE Error.E(NEW(Exception).init(rc)) END;
    RETURN bytes;
  END Write;

PROCEDURE Read(<*UNUSED*> self: T; VAR data: ARRAY OF CHAR; 
	offset: CARDINAL := 0): CARDINAL RAISES {Error.E} =
  VAR
    rc := 0;
    bytes := NUMBER(data);
  BEGIN
    IF offset # 0 THEN RAISE Error.E(NEW(Exception).init(NO_READSEEK)) END;
    rc := ConsoleExtern.console_read(data,bytes);
    IF rc#0 THEN RAISE Error.E(NEW(Exception).init(rc)) END;
    RETURN bytes;
  END Read;

PROCEDURE Ioctl(<*UNUSED*> self: T; cmd : INTEGER; VAR data : ARRAY OF CHAR;
		flags : INTEGER) RAISES {Error.E} =
VAR
  rc := 0;
BEGIN
  rc := ConsoleExtern.console_ioctl(cmd, data, flags);
  IF rc#0 THEN RAISE Error.E(NEW(Exception).init(rc)) END;
END Ioctl;

PROCEDURE SelectProc(<*UNUSED*> self: T): SalDep.SelectProc =
  BEGIN
    RETURN LOOPHOLE(ConsoleDepExtern.select, SalDep.SelectProc);
  END SelectProc;

PROCEDURE Descriptor(<*UNUSED*> self: T): Word.T =
  BEGIN
    RETURN 0; (* dev_t major 0 minor 0 *)
  END Descriptor;

PROCEDURE CharsAvail(<*UNUSED*> self: T): CARDINAL
	RAISES {Error.E} =
  VAR
    rc := 0;
    bytes:CARDINAL;
  BEGIN
    rc := ConsoleExtern.console_nread(bytes);
    IF rc#0 THEN RAISE Error.E(NEW(Exception).init(rc)) END;
    RETURN bytes;
  END CharsAvail;

PROCEDURE SigHandler(<*UNUSED*> self:T; handler: BSDtty.SignalHandler) =
  VAR
    rc := 0;
  BEGIN
    rc := ConsoleExtern.console_spgrp(handler);
  END SigHandler;

(*
PROCEDURE GetStatus(   <*UNUSED*>self: Wr;
                       <*UNUSED*>flavor:      INTEGER;
                        status_size: CARDINAL;
                    VAR status:      ARRAY OF Ctypes.int) 
  RAISES {Error.E} =
  BEGIN
    (* IF flavor # TTY_STATUS THEN CRASH AND BURN *)
    (* console.print("In Console.GetStatus.\n"); *)
    IF status_size # 4 THEN
      RAISE Error.E(NEW(Exception).init(Device.D_INVALID_SIZE));
    END;

    (* These data values taken from boot time of DEC OSF/1 on Mach *)
    status[0] := 16_c;
    status[1] := 16_c;
    status[2] := 16_0;
    status[3] := 16_38c;
  END GetStatus;
*)

VAR
  console := NEW(T);
BEGIN
  TRY
    EVAL ConsoleInterface.Export(NEW(Auth.AuthAlways));
    Device.Register("console",console);

  EXCEPT
  | NameServer.Error =>
    IO.Put("Console: cannot register console device name\n");
  END;

END Console.
