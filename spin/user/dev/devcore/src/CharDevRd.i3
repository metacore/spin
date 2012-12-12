(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
   CharDevRd.i3

   CharDevRd is a reader for CharDevice.T devices.
 *)
INTERFACE CharDevRd;

IMPORT Rd, CharDevice;

TYPE
  T <: Public;
  Public = Rd.T OBJECT METHODS init(dev: CharDevice.T):T; END;

(* The init method will attach the reader object to dev. 
   init and close are for manipulating the reader not the device.
   "init" does not call dev.open, nor does "close" call dev.close.
   Use the CharDevice.T methods to control the device.
 *)

END CharDevRd.
