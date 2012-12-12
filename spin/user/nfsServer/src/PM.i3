INTERFACE PM;
IMPORT Ctypes;

TYPE T = RECORD
  prog: Ctypes.unsigned_int;
  vers: Ctypes.unsigned_int;
  prot: Ctypes.unsigned_int;
  port: Ctypes.unsigned_int;
END;
CONST IPPROTO_TCP = 6;
CONST IPPROTO_UDP = 17;

CONST
  (* Port mapper constants. *)
  Prog : Ctypes.unsigned_int = 100000;
  Vers : Ctypes.unsigned_int = 2;

PROCEDURE Init(verbose:BOOLEAN);
PROCEDURE Uninit(verbose:BOOLEAN);

VAR debug : BOOLEAN := FALSE;
END PM.
