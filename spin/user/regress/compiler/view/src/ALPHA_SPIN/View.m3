(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 10-Oct-96  Wilson Hsieh (whsieh) at the University of Washington
 *	VIEW now accepts rep-equivalent types
 *
 * 20-Aug-96  Wilson Hsieh (whsieh) at the University of Washington
 *	VIEW now accepts an open array type as its second argument
 *	Alpha version of tests
 *
 * 18-Jun-96  Przemek Pardyak (pardy) at the University of Washington
 *	Got rid of warnings.
 *
 * 08-Feb-96  Wilson Hsieh (whsieh) at the University of Washington
 *	check that alignment exception occurs
 *      this MODULE need not be UNSAFE
 *
 * 26-Jan-96  Wilson Hsieh (whsieh) at the University of Washington
 *	added test to ensure that
 *	 SpinException.Exception(SpinException.ExceptionCode.ViewSize)
 *	 exception occurs
 *
 * 27-Dec-95  Przemek Pardyak (pardy) at the University of Washington
 *	Regression test.  Converted from Wilson's user space code.
 *
 *)

(* unsafe so that ADR and LOOPHOLE can be used *)
UNSAFE MODULE View;
IMPORT Fmt, IO, Word, SpinException;

TYPE Byte = [0..255];
     Rec = RECORD first, second: Word.T; END;
     Rec2 = RECORD foo: Word.T END;
     Abyte = ARRAY OF Byte;
     Forty8 = BITS 48 FOR
       RECORD a,b,c: BITS 16 FOR [0..Word.Shift(1,16)-1]; END;
     Forty8a = BITS 48 FOR
       RECORD a,b,c,d,e,f: BITS 8 FOR [0..Word.Shift(1,8)-1]; END;

VAR 
  glob: ARRAY [0..15] OF Byte;
  result: BOOLEAN;
  global2: RECORD first, second: Word.T; third: REF Word.T; END;

PROCEDURE wrong(i: Word.T) =
  BEGIN
    IO.Put ("Wrong answer " & Fmt.Int(i) & "\n");
    result := FALSE;
  END wrong;

PROCEDURE f(VAR x: Abyte; VAR a: Rec; VAR ar: ARRAY OF Forty8) =
  VAR
    count: INTEGER := 1;
  BEGIN
    TRY
      WITH y = VIEW(x, ARRAY [0..1] OF Word.T) DO
        INC(count);
        IF y[0] # 16_FEED0000BEEF THEN wrong (0); END;
        IF y[1] # 16_AAAA0000FFFF THEN wrong (1); END;
        IF VIEW(x, ARRAY[0..1] OF Word.T)[0] # 16_FEED0000BEEF THEN
          wrong (2);
        END;
        IF VIEW(x, ARRAY[0..1] OF Word.T)[1] # 16_AAAA0000FFFF THEN
          wrong (3);
        END;
      END;

      WITH y = VIEW(glob, ARRAY [0..1] OF Word.T) DO
        INC(count);
        IF y[0] # 16_FEED0000BEEF THEN wrong (4); END;
        IF y[1] # 16_AAAA0000FFFF THEN wrong (5); END;
        IF VIEW(glob, ARRAY [0..1] OF Word.T)[0] # 16_FEED0000BEEF THEN
          wrong (6);
        END;
        IF VIEW(glob, ARRAY [0..1] OF Word.T)[1] # 16_AAAA0000FFFF THEN
          wrong (7);
        END;
      END;

      WITH y = VIEW(x, Rec) DO
        INC(count);
        IF y.first # 16_FEED0000BEEF THEN wrong (8); END;
        IF y.second # 16_AAAA0000FFFF THEN wrong (9); END;
        IF VIEW(x, Rec).first # 16_FEED0000BEEF THEN wrong (10); END;
        IF VIEW(x, Rec).second # 16_AAAA0000FFFF THEN wrong (11); END;
      END;

      WITH y = VIEW(glob, Rec) DO
        INC(count);
        IF y.first # 16_FEED0000BEEF THEN wrong (12); END;
        IF y.second # 16_AAAA0000FFFF THEN wrong (13); END;
        IF VIEW(glob, Rec).first # 16_FEED0000BEEF THEN wrong (14); END;
        IF VIEW(glob, Rec).second # 16_AAAA0000FFFF THEN wrong (15); END;
      END;

      WITH y = VIEW(x, Word.T) DO
        INC(count);
        IF y # 16_FEED0000BEEF THEN wrong (16); END;
        IF VIEW(x, Word.T) # 16_FEED0000BEEF THEN wrong (17); END;
      END;

      WITH y = VIEW(glob, Rec2) DO
        INC(count);
        IF y.foo # 16_FEED0000BEEF THEN wrong (18); END;
        IF VIEW(glob, Rec2).foo # 16_FEED0000BEEF THEN wrong (19); END;
      END;

      WITH y = VIEW(x, Word.T) DO
        INC(count);
        IF y # 16_FEED0000BEEF THEN wrong (20); END;
        IF VIEW(x, Word.T) # 16_FEED0000BEEF THEN wrong (21); END;
      END;

      WITH y = VIEW(glob, Rec2) DO
        INC(count);
        IF y.foo # 16_FEED0000BEEF THEN wrong (22); END;
        IF VIEW(glob, Rec2).foo # 16_FEED0000BEEF THEN wrong (23); END;
        y.foo := 16_EAEAEAEA;
      END;

      (*************)

      WITH y = VIEW(a, ARRAY [0..1] OF Word.T) DO
        INC(count);
        IF y[0] # 16_EAEAEAEA THEN wrong (24); END;
        IF y[1] # 16_AAAA0000FFFF THEN wrong (25); END;
        IF VIEW(a, ARRAY [0..1] OF Word.T)[0] # 16_EAEAEAEA THEN
          wrong (26);
        END;
        IF VIEW(a, ARRAY [0..1] OF Word.T)[1] # 16_AAAA0000FFFF THEN
          wrong (27);
        END;
      END;

      WITH y = VIEW(a, Rec) DO
        INC(count);
        IF y.first # 16_EAEAEAEA THEN wrong (28); END;
        IF y.second # 16_AAAA0000FFFF THEN wrong (29); END;
        IF VIEW(a, Rec).first # 16_EAEAEAEA THEN wrong (30); END;
        IF VIEW(a, Rec).second # 16_AAAA0000FFFF THEN wrong (31); END;
      END;

      WITH y = VIEW(a, Word.T) DO
        INC(count);
        IF y # 16_EAEAEAEA THEN wrong (32); END;
        IF VIEW(a, Word.T) # 16_EAEAEAEA THEN wrong (33); END;
        y := 16_bbbbbbbb;
      END;

      (**********)

      WITH y = VIEW(x, ARRAY [0..1] OF Word.T) DO
        INC(count);
        IF y[0] # 16_BBBBBBBB THEN wrong (34); END;
        IF y[1] # 16_AAAA0000FFFF THEN wrong (35); END;
        IF VIEW(x, ARRAY [0..1] OF Word.T)[0] # 16_BBBBBBBB THEN
          wrong (36);
        END;
        IF VIEW(x, ARRAY [0..1] OF Word.T)[1] # 16_AAAA0000FFFF THEN
          wrong (37);
        END;
      END;

      WITH y = VIEW(glob, ARRAY [0..1] OF Word.T) DO
        INC(count);
        IF y[0] # 16_BBBBBBBB THEN wrong(38); END;
        IF y[1] # 16_AAAA0000FFFF THEN wrong(39); END;
        IF VIEW(glob, ARRAY [0..1] OF Word.T)[0] # 16_BBBBBBBB THEN
          wrong(40);
        END;
        IF VIEW(glob, ARRAY [0..1] OF Word.T)[1] # 16_AAAA0000FFFF THEN
          wrong(41);
        END;
      END;

      WITH y = VIEW(x, Rec) DO
        INC(count);
        IF y.first # 16_BBBBBBBB THEN wrong(42); END;
        IF y.second # 16_AAAA0000FFFF THEN wrong(43); END;
        IF VIEW(x, Rec).first # 16_BBBBBBBB THEN wrong(44); END;
        IF VIEW(x, Rec).second # 16_AAAA0000FFFF THEN wrong(45); END;
      END;

      WITH y = VIEW(glob, Rec) DO
        INC(count);
        IF y.first # 16_BBBBBBBB THEN wrong(46); END;
        IF y.second # 16_AAAA0000FFFF THEN wrong(47); END;
        IF VIEW(glob, Rec).first # 16_BBBBBBBB THEN wrong(48); END;
        IF VIEW(glob, Rec).second # 16_AAAA0000FFFF THEN wrong(49); END;

        y.second := 16_badbad;
      END;

      WITH y = VIEW(x, Word.T) DO
        INC(count);
        IF y # 16_BBBBBBBB THEN wrong (50); END;
        IF VIEW(x, Word.T) # 16_BBBBBBBB THEN wrong (51); END;
        y := 16_fadfadfad;
      END;

      WITH y = VIEW(glob, Word.T) DO
        INC(count);
        IF y # 16_FADFADFAD THEN wrong(52); END;
        IF VIEW(glob, Word.T) # 16_FADFADFAD THEN wrong(53); END;
      END;

      (**********)

      WITH y = VIEW(a, ARRAY [0..1] OF Word.T) DO
        INC(count);
        IF y[0] # 16_FADFADFAD THEN wrong(54); END;
        IF y[1] # 16_BADBAD THEN wrong(55); END;
        IF VIEW(a, ARRAY [0..1] OF Word.T) [0] # 16_FADFADFAD THEN
          wrong(56);
        END;
        IF VIEW(a, ARRAY [0..1] OF Word.T) [1] # 16_BADBAD THEN
          wrong(57);
        END;
      END;

      WITH y = VIEW(a, Rec) DO
        INC(count);
        IF y.first # 16_FADFADFAD THEN wrong(58); END;
        IF y.second # 16_BADBAD THEN wrong(59); END;
        IF VIEW(a, Rec).first # 16_FADFADFAD THEN wrong(60); END;
        IF VIEW(a, Rec).second # 16_BADBAD THEN wrong(61); END;
      END;

      WITH y = VIEW(a, Word.T) DO
        INC(count);
        IF y # 16_FADFADFAD THEN wrong(62); END;
        IF VIEW(a, Word.T) # 16_FADFADFAD THEN wrong(63); END;
      END;

      WITH b = VIEW(a, Byte) DO
        INC(count);
        IF b # 16_AD THEN wrong(64); END;
        IF VIEW(a, Byte) # 16_AD THEN wrong(65); END;
      END;

      WITH b = VIEW (a, ARRAY OF Byte) DO
        INC(count);
        IF NUMBER (b) # 16 THEN wrong (200); END;
        IF b[0] # 16_AD THEN wrong (201); END;
        IF VIEW (a, ARRAY OF Byte)[0] # 16_AD THEN wrong (202); END;
        b[0] := 16_DA;
        IF VIEW(a, Byte) # 16_DA THEN wrong (203); END;
      END;

      WITH b = VIEW (a, ARRAY OF [0..65535]) DO
        INC(count);
        IF NUMBER (b) # 8 THEN wrong (204); END;
        IF b[0] # 16_DFDA THEN wrong (205); END;
        IF VIEW (a, ARRAY OF [0..65535])[0] # 16_DFDA THEN wrong (206); END;
        b[0] := 16_DFAD;
        IF VIEW(a, Byte) # 16_AD THEN wrong (207); END;
      END;

      WITH b = VIEW (a, ARRAY OF [0..Word.Shift(1,32)-1]) DO
        INC(count);
        IF NUMBER (b) # 4 THEN wrong (208); END;
        IF b[0] # 16_ADFADFAD THEN wrong (209); END;
        IF VIEW (a, ARRAY OF [0..Word.Shift(1,32)-1])[0] # 16_ADFADFAD THEN
          wrong (210);
        END;
        IF NUMBER (VIEW (a, ARRAY OF [0..Word.Shift(1,32)-1])) # 4 THEN
          wrong (211);
        END;
        b[0] := 16_ADFADFDA;
        IF VIEW(a, Byte) # 16_DA THEN wrong (212); END;

        WITH c = VIEW (b, ARRAY OF Forty8) DO
          IF NUMBER (c) # 2 THEN wrong (213); END;
          IF NUMBER (VIEW (b, ARRAY OF Forty8)) # 2 THEN wrong (214); END;
          IF c[1].b # 16_DBAD THEN wrong (215); END;
          IF VIEW (b, ARRAY OF Forty8)[1].b # 16_DBAD THEN wrong (216); END;
          IF c # ar THEN wrong (217); END;
        END;
      END;

    EXCEPT
    ELSE
      (* if there are any exceptions, we're dead *)
      wrong(66);
    END;

    (* check to make sure that the size exception occurs *)
    TRY
      WITH y = VIEW(x, ARRAY [0..100] OF Word.T) DO <* NOWARN *>
        INC(count);
        wrong(67);
      END;
    EXCEPT
      SpinException.Exception(info) =>
      IF info.code # SpinException.ExceptionCode.ViewSize THEN
        wrong(68);
      END;
    ELSE
      wrong(69);
    END;

    (* check to make sure that the alignment exception occurs *)
    TRY
      WITH aaa = SUBARRAY (x, 1, 10) DO
        WITH y = VIEW(aaa, ARRAY [0..0] OF Word.T) DO  <* NOWARN *>
          wrong(70);
        END;
      END;
    EXCEPT
      SpinException.Exception(info) =>
      (* alignment check happens before size check *)
      IF info.code = SpinException.ExceptionCode.ViewSize THEN
        wrong(71);
      ELSIF info.code # SpinException.ExceptionCode.ViewAlignment THEN
        wrong(72);
      END;
    ELSE
      wrong(73);
    END;

    (* check to make sure that the alignment exception occurs *)
    TRY
      WITH y = VIEW(SUBARRAY (x, 1, 10), ARRAY [0..1] OF Word.T) DO<* NOWARN *>
        wrong(74);
      END;
    EXCEPT
      SpinException.Exception(info) =>
      (* alignment check happens before size check *)
      IF info.code = SpinException.ExceptionCode.ViewSize THEN
        wrong(75);
      ELSIF info.code # SpinException.ExceptionCode.ViewAlignment THEN
        wrong(76);
      END;
    ELSE
      wrong(77);
    END;

    (* check to make sure that assignment into an record field is OK *)
    VAR
      rec : RECORD a: Word.T END;
    BEGIN
      TRY
        rec.a := VIEW (x, Word.T);
      EXCEPT
      ELSE
        wrong(78);
      END;
    END;

    (* check to make sure that the alignment exception occurs for open arrays *)
    TRY
      WITH y = VIEW(SUBARRAY (x, 1, 10), ARRAY OF Forty8) DO<* NOWARN *>
        wrong(300);
      END;
    EXCEPT
      SpinException.Exception(info) =>
      (* alignment check happens before size check *)
      IF info.code = SpinException.ExceptionCode.ViewSize THEN
        wrong(301);
      ELSIF info.code # SpinException.ExceptionCode.ViewAlignment THEN
        wrong(302);
      END;
    ELSE
      wrong(303);
    END;

    (* check to make sure that the alignment exception occurs for open arrays *)
    TRY
      WITH y = VIEW(SUBARRAY (ar, 1, 1), ARRAY OF [0..Word.Shift(1,32)-1]) DO<* NOWARN *>
        wrong(304);
      END;
    EXCEPT
      SpinException.Exception(info) =>
      (* alignment check happens before size check *)
      IF info.code = SpinException.ExceptionCode.ViewSize THEN
        wrong(305);
      ELSIF info.code # SpinException.ExceptionCode.ViewAlignment THEN
        wrong(306);
      END;
    ELSE
      wrong(307);
    END;

    (* these are the same *)
    IF VIEW(x, Rec) # a THEN wrong (400); END;
    IF VIEW(x, ARRAY OF Forty8) # ar THEN wrong (401); END;
    
    (* these are the same *)
    IF VIEW(a, ARRAY OF Forty8) # ar THEN wrong (402); END;
    IF VIEW(a, Abyte) # x THEN wrong (403); END;
    
    (* these are not the same *)
    IF VIEW(ar, Abyte) = x THEN wrong (404); END;
    TRY
      IF VIEW(ar, Rec) # a THEN wrong (405); END;
    EXCEPT
      SpinException.Exception(info) =>
      IF info.code # SpinException.ExceptionCode.ViewSize THEN
        wrong (406);
      END;
    END;

    IF VIEW(ar[FIRST (ar)], BITS 48 FOR [0..Word.Shift(1,48)-1]) # 16_FADFADFDA THEN
      wrong (407);
    END;

    IF VIEW(ar[LAST (ar)], Forty8).a # 16_0000 THEN
      wrong (408);
    END;
    IF VIEW(ar[LAST (ar)], Forty8).b # 16_DBAD THEN
      wrong (409);
    END;
    IF VIEW(ar[LAST (ar)], Forty8).c # 16_BA THEN
      wrong (410);
    END;

    (* test that open array header for casts to same-sized arrays are the same *)
    VAR
      tmp1, tmp2, tmp3: REF RECORD a, b: Word.T; END;
    BEGIN
      tmp1 := LOOPHOLE (ADR (ar),
                        REF RECORD a, b: Word.T; END);
      tmp2 := LOOPHOLE (ADR (VIEW (ar, ARRAY OF Forty8a)),
                        REF RECORD a, b: Word.T; END);
      tmp3 := LOOPHOLE (ADR (VIEW (ar, ARRAY OF Forty8)),
                        REF RECORD a, b: Word.T; END);
      IF tmp1 # tmp2 THEN
        wrong (411);
      END;
      IF tmp1 # tmp3 THEN
        wrong (412);
      END;
      IF tmp2 # tmp3 THEN
        wrong (413);
      END;
    END;

    IF VIEW(global2, RECORD a, b: ARRAY [0..7] OF Byte; c: REF Word.T; END).c #
      global2.third THEN
      wrong (500);
    END;

  END f;

PROCEDURE Test(): BOOLEAN =
  BEGIN
    result := TRUE;
    WITH z = VIEW(glob, Rec), a = VIEW(glob, ARRAY OF Forty8) DO
      f(glob, z, a);
    END;
    RETURN result;
  END Test;

PROCEDURE Start(<* UNUSED *>i: INTEGER): BOOLEAN =
  BEGIN
    glob[0] := 16_EF;
    glob[1] := 16_BE;
    glob[2] := 16_00;
    glob[3] := 16_00;
    glob[4] := 16_ED;
    glob[5] := 16_FE;
    glob[6] := 16_00;
    glob[7] := 16_00;
    glob[8] := 16_FF;
    glob[9] := 16_FF;
    glob[10] := 16_00;
    glob[11] := 16_00;
    glob[12] := 16_AA;
    glob[13] := 16_AA;
    glob[14] := 16_00;
    glob[15] := 16_00;
    global2.third := NEW (REF Word.T);
    RETURN TRUE;
  END Start;

PROCEDURE End(): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END End;

BEGIN
END View.
