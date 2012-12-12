(* 
 * Copyright 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * Permission - the abstract base class for permissions.
 *
 * A permission authorizes some subject to execute a specific operation
 * on an object. A list of such permissions is an access mode.
 * Extensions that introduce new permissions should define an opaque
 * subtype of Permission.T.
 *
 * HISTORY
 *
 * 27-Oct-97  Robert Grimm (rgrimm) at the University of Washington
 *      Changed "equals" to "contains"
 *
 * 29-Jul-97  Robert Grimm (rgrimm) at the University of Washington
 *      Updated documentation
 *
 * 03-Jul-97  Robert Grimm (rgrimm) at the University of Washington
 *      Changed contract for equals
 *
 * 02-Mar-97  Robert Grimm (rgrimm) at the University of Washington
 *      Created
 *
 *)

INTERFACE Permission;

TYPE
  T = OBJECT METHODS       (* A single permission object   *)

    contains( other : T ) : BOOLEAN;
    (*
     * Does given permission contain the "other" permission,
     * i.e. "self" >= "other"?
     * Before calling on "contains", all access mode operations
     * must first test whether the two types are equal, i.e.
     * the TYPECODEs of the two permissions must be the same,
     * as to avoid one Permission.T object declaring that it
     * contains another it has no idea about.
     *)

    toText() : TEXT;
    (*
     * Generate a human-readable representation.
     *)
  END;


  ListT = REF ARRAY OF T;  (* A list of permission objects *)

END Permission.
