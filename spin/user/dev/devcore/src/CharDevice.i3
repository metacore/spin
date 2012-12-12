(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 28-May-96  becker at the University of Washington
 *	Moved exclusive open to dlib
 *
 * 06-Apr-96  Brian Bershad (bershad) at the University of Washington
 *	whisted.  
 *
 *)

(* "CharDevice"
  
   CharDevice.T is the base class for read/write devices that operate
   on CHAR data.  A reader/writer interface is in CharDev{Rd,Wr}.

   These are akin to the 'c' devices that appear in /dev on unix like
   ttys and disks and have unix-like semantics.  (yes disks all have
   'c' interfaces.  DECOSF SAL provides both a 'c' and 'b' (block)
   interfaces for disks that both wind up in a common DMA based
   driver.)  *)

INTERFACE CharDevice;
IMPORT Device, Error;

TYPE T <: Public;
  Public = Device.T OBJECT
  METHODS
    open() RAISES {Error.E};
    close() RAISES {Error.E};
    (* Note: implementors of the device should NOT assume that clients
       call open and close properly. They are just hints. *)
    read(VAR data: ARRAY OF CHAR; off: CARDINAL := 0) : CARDINAL
      RAISES {Error.E};
    write(READONLY data: ARRAY OF CHAR; off: CARDINAL := 0) : CARDINAL
      RAISES {Error.E};
  END;  

END CharDevice.





