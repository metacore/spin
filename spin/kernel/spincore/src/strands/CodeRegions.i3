(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 27-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	Name clean up.
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Generic code region support.
 *      Clients are expected to create a CR.T and add their RAS regions
 *      with the AddRegion method. Regions should not overlap.
 *      The regions are kept in sorted order and RASCheck does binary search
 *      over them to determine if the pc is in a RAS range and fixes it 
 *      to be equal to the beginning of the region if it is.
 *      RangeCheck returns true if the pc is in the given range.
 *      The primitive operations do not give any atomicity guarantees, and
 *      locking (if necessary) has to be done by the client of this module.
 *)
INTERFACE CodeRegions;
IMPORT Word, Region;

TYPE Public = OBJECT METHODS
    contains(VAR pc: Word.T) : BOOLEAN;
    rasEnsure(VAR pc: Word.T);
    add(READONLY newRegion: Region.T);
    dump() : TEXT;
    iterate(processRegion: PROCEDURE(regno: INTEGER; READONLY region: Region.T));
  END;
TYPE T <: Public;

PROCEDURE New() : T;
(* "New" allocates and initializes a new code region. *)

END CodeRegions.



