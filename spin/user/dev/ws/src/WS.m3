(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *      Added new select support
 *
 * 28-May-96  becker at the University of Washington
 *	Moved exclusive open to dlib
 *
 *  1-Apr-96 oystr at the University of Washington
 *	Created
 *
 *)

(* Wrapper routines for underlying workstation (WS) device *)

UNSAFE (* imports ws externs *)
MODULE WS;
IMPORT NameServer, Auth, WSInterface;
IMPORT Device, BSDtty;
IMPORT IO, Error, Fmt;
IMPORT SALWSExtern;
IMPORT Space, Translation, VirtAddr, PhysAddr;
IMPORT SalDep, Word;

REVEAL WSDevice = BSDtty.T BRANDED OBJECT 
    mu : MUTEX;
    isOpen : BOOLEAN;
  OVERRIDES
    open := WSopen;
    close := WSclose;
    ioctl := WSioctl;
    read := WSBadRead;
    write := WSBadWrite;
		    selectproc := SelectProc;
		    descriptor := Descriptor;
  END;

VAR mouse: WSDevice;

TYPE Exception = Error.T BRANDED OBJECT
  OVERRIDES
    message := WSMessage;
  END;

VAR
  WSMessages := ARRAY [WS_BASE_ERROR..WS_LAST_ERROR] OF TEXT {
	"WS frame buffer already open",
	"cannot read WS frame buffer",
	"cannot write WS frame buffer",
	"WS frame buffer not open"
  };

PROCEDURE WSMessage(self:Exception): TEXT =
  VAR rc := self.resultCode();
  BEGIN
    IF WS_BASE_ERROR <= rc AND rc < WS_BASE_ERROR+NUMBER(WSMessages) THEN
      RETURN WSMessages[rc];
    END;
    IF rc >= WS_BASE_ERROR+NUMBER(WSMessages) THEN
      RETURN "WS frame buffer: unknown result " & Fmt.Int(rc);
    END;

    RETURN "WS frame buffer dev unix error: " & Fmt.Int(rc);
  END WSMessage;

PROCEDURE WSopen( self: BSDtty.T;
 		 (* <*UNUSED*> mode: Device.DevModeT *))
		  RAISES {Error.E} =
  VAR dev: WSDevice;
       rc: INTEGER;
  BEGIN
    dev := NARROW(self,WSDevice);
    IF dev.isOpen THEN 
      RAISE Error.E(NEW(Exception).init(WS_ALREADY_OPEN));
    END;
    
    (*
     * wsopen(dev_t, flag)
     *     For mouse, major(dev) == 0, minor(dev) = 1
     *	   Flag is ignored by the OSF code.
     *)
    rc := SALWSExtern.wsopen(1,0);
    IF rc # 0 THEN
      RAISE Error.E(NEW(Exception).init(rc));
    END;
    dev.isOpen := TRUE;
  END WSopen;

PROCEDURE WSclose( self: BSDtty.T ) RAISES {Error.E} =
  VAR dev : WSDevice;
      rc  : INTEGER;
  BEGIN
    dev := NARROW(self,WSDevice);
    IF NOT dev.isOpen THEN
      RAISE Error.E(NEW(Exception).init(WS_NOT_OPEN));
    END;
    rc := SALWSExtern.wsclose(1,0);
    IF rc # 0 THEN
      RAISE Error.E(NEW(Exception).init(rc));
    END;
    dev.isOpen := FALSE;
  END WSclose;

PROCEDURE WSMap(phys : PhysAddr.Address ;
		virt : VirtAddr.Address ;
		size : VirtAddr.Size ) : INTEGER =
BEGIN
  RETURN Space.MapPhysToVirt(Translation.GetCurrent(), phys, virt, size);
END WSMap;

PROCEDURE WSioctl(self: BSDtty.T;
		  iocmd : INTEGER;
		  VAR iocdata : ARRAY OF CHAR;
		  <*UNUSED*>flags: INTEGER ) 
	  RAISES { Error.E } =
  VAR dev: WSDevice;
       rc: INTEGER;
  BEGIN
    dev := NARROW(self,WSDevice);
    IF NOT dev.isOpen THEN
      RAISE Error.E(NEW(Exception).init(WS_NOT_OPEN));
    END;
    rc := SALWSExtern.wsioctl( 1, iocmd, iocdata, 0, WSMap);
    IF rc # 0 THEN
      RAISE Error.E(NEW(Exception).init(rc));
    END;
  END WSioctl;

PROCEDURE SelectProc(<*UNUSED*> self: BSDtty.T): SalDep.SelectProc =
  BEGIN
    RETURN SALWSExtern.wsselect;
  END SelectProc;

PROCEDURE Descriptor(<*UNUSED*> self: BSDtty.T): Word.T =
  BEGIN
    RETURN 1; (* dev_t major 0 minor 1 *)
  END Descriptor;

PROCEDURE WSBadRead( <*UNUSED*>self: BSDtty.T;
		   <*UNUSED*>VAR data: ARRAY OF CHAR;
		   <*UNUSED*>offset: CARDINAL := 0) : CARDINAL
		RAISES {Error.E} =
BEGIN
  IO.Put("Cannot read mouse device\n");
  RAISE Error.E(NEW(Exception).init(WS_INVALID_READ));
END WSBadRead;

PROCEDURE WSBadWrite( <*UNUSED*>self: BSDtty.T;
		   <*UNUSED*>READONLY data: ARRAY OF CHAR;
		   <*UNUSED*>offset: CARDINAL := 0) : CARDINAL
		RAISES {Error.E} =
BEGIN
  IO.Put("Cannot write mouse device\n");
  RAISE Error.E(NEW(Exception).init(WS_INVALID_WRITE));
END WSBadWrite;

BEGIN
(* Tell everybody that the mouse is here *)
  mouse := NEW(WSDevice);
  mouse.isOpen := FALSE;
  TRY 
    EVAL WSInterface.Export(NEW (Auth.AuthAlways));
    Device.Register("mouse", mouse);
  EXCEPT
  |  NameServer.Error(ec) =>
    IO.PutError("mouse: " & NameServer.Fmt(ec) & ".\n");
  END;
  IO.Put("Registered mouse device\n");
END WS.
