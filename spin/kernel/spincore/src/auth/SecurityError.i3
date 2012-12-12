(* 
 * Copyright 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * SecurityError
 *
 *
 * HISTORY
 * 30-Jul-97  Robert Grimm (rgrimm) at the University of Washington
 *      Changed to an error-code based representation
 *
 * 03-Feb-97  Robert Grimm (rgrimm) at the University of Washington
 *      Created
 *
 * html
 *)

INTERFACE SecurityError;

TYPE EC = {            (*          --- Security error codes ---        *)
  BadFormat,           (* A string does not have the expected format.  *)
  BadNumber,           (* A number is not as expected                  *)
  InvalidId,           (* A user/group/security id is invalid.         *)
  InvalidBinding,      (* A dispatcher binding is invalid.             *)
  InvalidCheck,        (* An access control check is invalid.          *)
  InvalidType,         (* Type is not a securable type.                *)
  Unauthorized,        (* Operation not authorized for subject.        *)
  Disabled,            (* Security is disabled.                        *)
  Panic                (* Unknown, but fatal error condition.          *)
  };

EXCEPTION T(EC);

END SecurityError.
