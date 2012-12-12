(* 
 * Copyright 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * Access Mode revelation
 *
 * HISTORY
 * 07-Mar-97  Robert Grimm (rgrimm) at the University of Washington
 *      Created
 *
 *)

INTERFACE AccessModeRep;

IMPORT Permission, AccessMode;

REVEAL AccessMode.T = BRANDED AccessMode.Brand REF RECORD
  simple      : AccessMode.SimpleT;
  permissions : Permission.ListT;
END;

END AccessModeRep.
