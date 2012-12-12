(*
  This file should not compile.
 *)

MODULE View;

IMPORT Word;

TYPE
  Byte = BITS 8 FOR [0..255];
  Byte1 = BITS 8 FOR [0..200];
  Byte2 = BITS 10 FOR [0..255];
  Byte3 = BITS 10 FOR [0..1023];
  Byte4 = BITS 32 FOR [0..16_ffffffff];
  Byte5 = BITS 32 FOR [0..16_fffffffe];
  Byte6 = BITS 4 FOR [-8..7];
  Byte7 = BITS 4 FOR [-7..7];
  Byte8 = BITS 2 FOR {foo, bar, baz};
  Byte9 = BITS 2 FOR {foo, bar, baz, zot};

  Rec1 = RECORD first: Word.T; second: REF Word.T; third: Word.T END;
  Rec1b = RECORD first: Word.T; second: BITS 4 FOR [0..14]; third: Word.T END;
  Rec1c = RECORD first: Word.T; second: BITS 4 FOR {yaz, lynn, fred, barney, rubble}; third: Word.T END;
  Rec1d = RECORD first: Word.T; second: BITS 1 FOR {a, b}; third: Word.T END;
  Rec1e = RECORD first: Word.T; second: BITS 4 FOR [-8..7]; third: Word.T END;

  Rec2 = RECORD first: Word.T; second: Word.T END;

  WordAlias = Word.T;

PROCEDURE g(VAR x: Rec1; READONLY a: Rec1;
            VAR x2: Rec1b; READONLY a2: Rec1b;
            VAR x3: Rec1c; READONLY a3: Rec1c;
            VAR x4: Rec1d; READONLY a4: Rec1d;
            VAR x5: Rec1e; READONLY a5: Rec1e
  ) =
  BEGIN
    WITH b = VIEW(x, Rec2) DO (* bad *)
    END;
    WITH b = VIEW(a, Rec2) DO
    END;
    WITH b = VIEW(x2, Rec2) DO (* bad *)
    END;
    WITH b = VIEW(a2, Rec2) DO
    END;


    WITH b = VIEW(x3, Byte) DO
    END;
    WITH b = VIEW(a3, Byte) DO
    END;
    WITH b = VIEW(x4, Byte) DO
    END;
    WITH b = VIEW(a4, Byte) DO
    END;
    WITH b = VIEW(x5, Byte) DO
    END;
    WITH b = VIEW(a5, Byte) DO
    END;


    WITH b = VIEW(a, Byte) DO
    END;
    WITH b = VIEW(a, Byte1) DO (* bad *)
    END;
    WITH b = VIEW(a, Byte2) DO (* bad *)
    END;
    WITH b = VIEW(a, Byte3) DO
    END;
    WITH b = VIEW(a, Byte4) DO
    END;
    WITH b = VIEW(a, Byte5) DO (* bad *)
    END;
    WITH b = VIEW(a, Byte6) DO
    END;
    WITH b = VIEW(a, Byte7) DO (* bad *)
    END;
    WITH b = VIEW(a, Byte8) DO (* bad *)
    END;
    WITH b = VIEW(a, Byte9) DO
    END;
    WITH b = VIEW(a, CHAR) DO
    END;

    WITH b = VIEW(x, Word.T) DO
    END;
    WITH b = VIEW(x, RECORD a: Word.T := 3; third: REF Word.T; END) DO
    END;
    WITH b = VIEW(x, RECORD a: Word.T := 3; third: Word.T; END) DO  (* bad *)
    END;
    WITH b = VIEW(x, RECORD a: Word.T := 3; END) DO
    END;

    WITH b = VIEW(x, ARRAY OF Byte) DO              (* bad *)
    END;
    WITH b = VIEW(x, ARRAY [0..3] OF Byte) DO
    END;
    WITH b = VIEW(x, ARRAY [0..7] OF Byte) DO
    END;
    WITH b = VIEW(x, ARRAY [0..9] OF Byte) DO       (* bad *)
    END;
    
    WITH b = VIEW(x, BOOLEAN) DO                    (* bad *)
    END;
    WITH b = VIEW(x, RECORD a: ARRAY [0..3] OF Byte; b: REF Word.T; END) DO                   (* bad *)
    END;
    WITH b = VIEW(x, RECORD a: ARRAY [0..7] OF Byte; b: REF Word.T; END) DO
    END;
    WITH b = VIEW(x, RECORD a: ARRAY [0..7] OF Byte; b: REF WordAlias; END) DO
    END;
    WITH b = VIEW(x, RECORD a: ARRAY [0..3] OF [0..65535]; b: REF Word.T; END) DO
    END;
    WITH b = VIEW(x, RECORD a: ARRAY [0..3] OF [0..65535]; b: REF BOOLEAN; END) DO            (* bad *)
    END;
    WITH b = VIEW(x, RECORD a: ARRAY [0..3] OF [0..65535]; b: REF WordAlias; END) DO
    END;
    WITH b = VIEW(x, RECORD a: ARRAY [0..2] OF [0..65535]; b: REF Word.T; END) DO             (* bad *)
    END;
    WITH b = VIEW(a, RECORD a: ARRAY [0..3] OF [0..65535]; b: REF WordAlias; END) DO
    END;
    WITH b = VIEW(a, RECORD a: ARRAY [0..2] OF [0..65535]; b: REF WordAlias; END) DO          (* bad *)
    END;
    WITH b = VIEW(a, ARRAY OF [0..255]) DO
    END;
    WITH b = VIEW(a, ARRAY OF RECORD a: ARRAY [0..3] OF [0..255]; END) DO
    END;
    WITH b = VIEW(a, ARRAY OF RECORD a: ARRAY [0..3] OF [0..255]; b: REF INTEGER; END) DO     (* bad *)
    END;

  END g;

PROCEDURE Test () : BOOLEAN =
  BEGIN
    RETURN FALSE;
  END Test;

PROCEDURE Start (<* UNUSED *> i: INTEGER) : BOOLEAN =
  BEGIN
    RETURN FALSE;
  END Start;

PROCEDURE End () : BOOLEAN =
  BEGIN
    RETURN FALSE;
  END End;

BEGIN
END View.
