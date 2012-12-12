(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 26-Mar-96  Brian Bershad (bershad) at the University of Washington
 *	Changed to rely on name server instead of local tables.
 *
 * 09-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Changed to use IO instead of ConsolePrivate.
 *
 * 28-Oct-95  Brian Bershad (bershad) at the University of Washington
 *	Made SAFE.
 *
 * 07-Aug-95  Brian Bershad (bershad) at the University of Washington
 *	Support for device specific errors.  We shouldn't need this, 
 *	but I'm reluctant to go introducing an error object for 
 *	exceptions at this point.
 *	
 *
 * 05-Aug-95  Brian Bershad (bershad) at the University of Washington
 *	Created.
 * 
 *)

(* "Device" - Generic device implementation. 

   The root of the device tree exists primarily to help clients locate
   particular instances of other devices. Eg, open on the root device
   for a specific name will return a device instance of some child
   device that has been registed as willing to serve that device name.
*)

MODULE Device;
IMPORT IO, Fmt, NameServer, Text, Auth, DeviceInterface, 
       Error AS ErrorBase;
IMPORT NSName;

REVEAL T = Public BRANDED OBJECT
    id: Text.T;
  OVERRIDES
    name := Name;
  END;

PROCEDURE Name (self: T): TEXT =
  BEGIN
    RETURN self.id;
  END Name;

REVEAL Error = ErrorBase.T BRANDED OBJECT
  OVERRIDES
    message := Message;
  END;

PROCEDURE Message(self: Error): Text.T =
  BEGIN
    RETURN "Device exception: error code " & Fmt.Int(self.resultCode());
  END Message;

PROCEDURE Register(name: TEXT; dev: T; <* UNUSED *> auth: Auth.T:=NIL)
	RAISES {NameServer.Error} =
  BEGIN
    dev.id := name;
    NameServer.Attach(deviceNS, name, dev);
  END Register;

PROCEDURE Lookup (name: TEXT): REFANY RAISES {NameServer.Error} =
  BEGIN
    RETURN NameServer.LookupSoft(NIL,"/../svc/devices/" & name);
  END Lookup;

VAR
  deviceNS  : NameServer.T;

PROCEDURE Init(verbose:BOOLEAN) =
  VAR
    svcDir: NameServer.T;
  BEGIN
    TRY 
      EVAL DeviceInterface.Export(NEW (Auth.AuthAlways));
      (* create the domain namespace *)
      svcDir := NameServer.Lookup(NIL, "/../svc");
      deviceNS := svcDir.create(NSName.FromText("devices"));
      
      IF verbose THEN IO.Put("Device namespace initialized\n"); END;
    EXCEPT
    | NameServer.Error =>
      IF verbose THEN
	IO.PutError("Device namespace initialization failed\n");
      END;
    END;
  END Init;

BEGIN
  Init(TRUE);
END Device.
