(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
   CharDevWr.i3

   CharDevWr is a writer for CharDevice.T devices.
 *)
INTERFACE CharDevWr;

IMPORT Wr, CharDevice;

TYPE
  T <: Public;
  Public = Wr.T OBJECT METHODS init(dev: CharDevice.T):T; END;

(* The init method will attach the writer object to dev. 
   init and close are for manipulating the writer not the device.
   "init" does not call dev.open, nor does "close" call dev.close.
   Use the CharDevice.T methods to control the device.
 *)

END CharDevWr.
