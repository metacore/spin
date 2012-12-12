(*
 *
 * Copyright 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 *)

(*
 * HISTORY
 *
 *)


MODULE Register;

PROCEDURE FindCallerSave (used: SET OF [0..31]; ignore: INTEGER := 0)
  : [0..31] RAISES {Cannot} =
  BEGIN
    FOR i := 0 TO LAST (caller_save) DO
      WITH regnum = ORD (caller_save [i]) DO
        IF NOT regnum IN used THEN
          IF ignore = 0 THEN
            RETURN regnum;
          ELSE
            DEC (ignore);
          END;
        END;
      END;
    END;
    RAISE Cannot;
  END FindCallerSave;

PROCEDURE FindCalleeSave (used: SET OF [0..31]; ignore: INTEGER := 0)
  : [0..31] RAISES {Cannot} =
  BEGIN
    FOR i := 0 TO LAST (callee_save) DO
      WITH regnum = ORD (callee_save [i]) DO
        IF NOT regnum IN used THEN
          IF ignore = 0 THEN
            RETURN regnum;
          ELSE
            DEC (ignore);
          END;
        END;
      END;
    END;
    RAISE Cannot;
  END FindCalleeSave;

BEGIN
END Register.
