(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 01-Aug-96  Marc Fiuczynski (mef) at the University of Washington
 *	Made extents a subtype of Disk.T.
 *
 * 02-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. 
 *)

(* "Extent" provides a low level disk manager that safely
   space-multiplexes disks. *)

INTERFACE Extent;
IMPORT Disk, Error, ParseParams;

CONST CommandName = "extent";
CONST CommandHelp = "mkdev devName extName [[-size bytes][-offset bytes]]";
PROCEDURE Run(pp: ParseParams.T): BOOLEAN;

TYPE T <: TPublic;

(*
 * Extents are used to safely space-multiplex disks.
 *)
TYPE Flags = {ExactSize, ExactOffset};
TYPE FlagSet = SET OF Flags;

TYPE TPublic = Disk.T OBJECT
  METHODS
    init() : TPublic;
    (* "init" is used to initialize a newly allocated extent object. *)

    allocate(
        devName  : TEXT; 
        VAR size : CARDINAL;
        offset   : CARDINAL := 0;
        flags    : FlagSet := FlagSet{})
      RAISES { Error.E };
    (* "allocate" is used to initialize the extent object and allocate
       space on the underlying device.  "devName" may be a raw disk
       device name or another extent.  All other devices are not
       supported.  Both "size" and "offset" must be specified in
       bytes.  

       The flags parameter determines the behaviour of the allocate
       method.  The exactSize flags determines whether the allocation
       will be the exactSize as the "size" parameter.  Otherwise, it
       will be the entire size of the "devName" extent.  The
       exactOffset will attempt to allocate the extent at the
       specified offset.  Otherwise, it will fail.  The "persistant"
       flag is currentl ignored. *)
    (* XXX I rather pass in a Disk.T instead of the text "devName".
       However, I don't want to spend the time right now to figure out
       how to read the partition size information if I'm only given
       the Disk.T for a particular partition.  The extent code
       currently opens the entire disk to get the overall
       partition/label information. (mef) *)
  (* I have no idea why devname is needed at all.... need discussion with mef.
     -yasushi *)

    deallocate();
    (* "deallocate" marks all of the used disk blocks as free and
       returns these blocks to the parent extent. *)

    getBaseOffset() : CARDINAL;
    (* "getBaseOffset" returns the base offset of this extent *)
  END;

END Extent.
