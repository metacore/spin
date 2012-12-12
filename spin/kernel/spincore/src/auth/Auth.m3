(* 
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 26-Apr-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added an argument to authorization. 
 *
 * 24-Jan-96  Brian Bershad (bershad) at the University of Washington
 *	Created.
 *
 *)

MODULE Auth;


PROCEDURE Deny (<*UNUSED*> a: T; 
                <*UNUSED*> key: Key; 
                <*UNUSED*> arg: REFANY) : BOOLEAN =
  BEGIN
    RETURN FALSE;
  END Deny;

PROCEDURE Allow (<*UNUSED*> a: T;
                 <*UNUSED*> key: Key;
                 <*UNUSED*> arg: REFANY) : BOOLEAN =
  BEGIN
    RETURN TRUE;
  END Allow;

PROCEDURE AllowAuthKey (a: T; 
                        key: Key;
                        <*UNUSED*> arg: REFANY) : BOOLEAN =
  BEGIN
    RETURN a = key;
  END AllowAuthKey;

REVEAL AuthNever =  T BRANDED OBJECT 
	OVERRIDES
          authorize := Deny;
        END;

      AuthAlways = T BRANDED OBJECT
	OVERRIDES
          authorize := Allow;
        END;

      AuthAuthKey = T BRANDED OBJECT
	OVERRIDES
          authorize := AllowAuthKey;
        END;


BEGIN
END Auth.
    
