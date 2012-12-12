(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: MxIn.m3                                               *)
(* Last Modified On Thu Jan 26 14:22:57 PST 1995 By kalsow     *)

(*
 * HISTORY
 * 10-Jun-97  Wilson Hsieh (whsieh) at the University of Washington
 *	units store pointers back to domain
 *      add SpinReadUnits
 *
 *)

UNSAFE MODULE MxIn;

IMPORT Text, Wr, Fmt, Word, Thread;
IMPORT Mx, MxRep, M3ID, M3FP, MxVS;
IMPORT Domain;

<*FATAL Wr.Failure, Thread.Alerted*>

CONST
  End_of_buffer = '\000';
  Buffer_size   = 1024;
  N_stop_chars  = 5;  (* SPACE, NEWLINE, '*', EOB, QUOTE *)

(* FIXME ??? shouldn't it be 64 for alpha ??? *)
CONST
  SignBit    = Word.LeftShift (1, 31);
  SignExtend = Word.LeftShift (Word.Not (0), 31);

TYPE
  State = RECORD
    cmd       : CHAR;
    info      : UNTRACED REF CHAR := NIL;
    info_len  : INTEGER       := 0;
    errors    : Wr.T          := NIL;
    nErrors   : INTEGER       := 0;
    units     : Mx.UnitList   := NIL;
    cur_file  : Mx.File       := NIL;
    cur_unit  : Mx.Unit       := NIL;
    nameMap   : NameMap       := NIL;
    vsMap     : VSMap         := NIL;
    buf_ptr   : CARDINAL      := 0;
    buf_len   : CARDINAL      := 0;
    buf       : ARRAY [0..Buffer_size + N_stop_chars - 1] OF CHAR;
    domain    : Domain.T      := NIL;
  END;

TYPE
  VSMap    = REF ARRAY OF MxVS.T;
  NameMap  = REF ARRAY OF Mx.Name;

TYPE
  CmdProc = PROCEDURE (VAR s: State): BOOLEAN;

EXCEPTION
  SyntaxError;

VAR
  HexDigit : ARRAY CHAR OF [0..16];
  CmdMap   : ARRAY CHAR OF CmdProc;

(*------------------------------------------------------------------------*)

VAR
  s: State;

PROCEDURE StartReading () =
  BEGIN
    s.nErrors := 0;
    s.units := NIL;
    s.buf_ptr := 0;
    s.buf_len := 0;
  END StartReading;

PROCEDURE SpinReadUnits (info: UNTRACED REF CHAR; errors: Wr.T;
                         d: Domain.T; VAR (* OUT *) u: Mx.Unit)
                          : INTEGER =
  VAR
    value: INTEGER;
  BEGIN
    value := ReadUnits (info, errors, d);
    u := s.units.unit;
    RETURN value;
  END SpinReadUnits;

PROCEDURE ReadUnits (info: UNTRACED REF CHAR; errors: Wr.T; d: Domain.T)
  : INTEGER =
  VAR
    len: INTEGER := 0;
    mul: INTEGER := 1;
  BEGIN
    FOR i := 0 TO 3 DO
      (* RTIO.PutHex(ORD(info^)); RTIO.PutText(" ");*)
      (* FIXME: this assumes little endianness *)
      len := len + mul * ORD(info^);
      INC (info, ADRSIZE(CHAR));
      mul := mul * 256;
    END;
    (* RTIO.PutHex(ORD(info^)); *)
    s.info     := info;
    IF len = 0 THEN
      len := Strlen(info);
    END;
    s.info_len := len;
    (*
    s.info_len := Strlen(info);
    IF s.info_len # len AND len # 0 THEN
      RTIO.PutText("ERROR >> incorrect length of link info: ");
      RTIO.PutInt(len); 
      RTIO.PutText(" ");
      RTIO.PutInt(s.info_len); 
      RTIO.PutText("\n");
    END;
    *)
    s.errors   := errors;
    s.nameMap  := NEW (NameMap, 256);
    s.vsMap    := NEW (VSMap, 4100);
    s.domain   := d;
    TRY
      ReadLinkFile (s);
    EXCEPT
    | SyntaxError     => (* already reported something *)
      RETURN -1;
    END;
    RETURN s.info_len;
  END ReadUnits;

PROCEDURE GetUnits (): Mx.UnitList =
  BEGIN
    IF (s.nErrors > 0) THEN 
      RETURN NIL
    ELSE 
      RETURN s.units;
    END;
  END GetUnits;

(*------------------------------------------------------------------------*)

PROCEDURE ReadLinkFile (VAR s: State)
  RAISES {SyntaxError} =
  BEGIN
    FillBuffer (s);
    ReadMagic (s);
    LOOP
      (* s.cmd := GetNextChar(s);*)

      s.cmd := s.info^;
      INC (s.buf_ptr);
      INC (s.info, ADRSIZE(CHAR));

      IF CmdMap [s.cmd] (s) THEN EXIT END;
    END;
  END ReadLinkFile;

PROCEDURE ReadMagic (VAR s: State)
  RAISES {SyntaxError} =
  BEGIN
    FOR i := 0 TO Text.Length (Mx.LinkerMagic) - 1 DO
      Match (s, Text.GetChar (Mx.LinkerMagic, i));
    END;
    Match (s, '\n');
  END ReadMagic;

PROCEDURE Match (VAR s: State;  ch: CHAR)
  RAISES {SyntaxError} =
  VAR c2 := GetC (s);
  BEGIN
    IF (ch # c2) THEN
      Error (s, "bad linkfile (unrecognized header)");
      RAISE SyntaxError;
    END;
  END Match;

PROCEDURE EndBuffer (<* UNUSED *>VAR s: State): BOOLEAN = 
  BEGIN
    RETURN TRUE;
  END EndBuffer;

PROCEDURE BadChar (VAR s: State): BOOLEAN =
  BEGIN
    Error (s, "unrecognized linker command: ", CharName (s.cmd));
    RETURN TRUE;
  END BadChar;

(*----------------------------------------------------------- global maps ---*)

PROCEDURE ReadName (VAR s: State): BOOLEAN = 
  (* Nx y   --- define id number 'x' to be name 'y' *)
  VAR id := GetInteger (s, ' ');
  VAR nm := GetID      (s, '\n');
  BEGIN
    WHILE (id > LAST (s.nameMap^)) DO ExpandNameMap (s) END;
    s.nameMap [id] := nm;
    RETURN FALSE;
  END ReadName;

(* SPIN does not interpret the flags, scan and ignore *)
PROCEDURE ReadFlags (VAR s: State): BOOLEAN =
  (* Fn f1-fn --- define m3options to be used in recompilation during import *)
  VAR n := GetInteger (s, ' ');
  VAR c : CHAR := ' ';
  BEGIN
    FOR i := 0 TO n-1 DO
      IF i = n-1 THEN c := '\n'; END;
      EVAL GetID(s, c);
    END;
    RETURN FALSE;
  END ReadFlags;

PROCEDURE ExpandNameMap (VAR s: State) =
  VAR n := NUMBER (s.nameMap^);  new := NEW (NameMap, n + n);
  BEGIN
    SUBARRAY (new^, 0, n) := s.nameMap^;
    s.nameMap := new;
  END ExpandNameMap;

PROCEDURE ReadVSInfo (VAR s: State): BOOLEAN = 
  (* Vx a b c --- define version stamp number 'x' for symbol 'a.b' to be 'c' *)
  VAR vs : MxVS.Info;
  VAR id := GetInteger (s, ' ');
  BEGIN
    vs.source := GetName (s, ' ');
    vs.symbol := GetName (s, ' ');
    GetStamp (s, vs.stamp, '\n');

    WHILE (id > LAST (s.vsMap^)) DO ExpandVSMap (s) END;
    s.vsMap [id] := MxVS.Put (vs);
    RETURN FALSE;
  END ReadVSInfo;

PROCEDURE ExpandVSMap (VAR s: State) =
  VAR n := NUMBER (s.vsMap^);  new := NEW (VSMap, n + n);
  BEGIN
    SUBARRAY (new^, 0, n) := s.vsMap^;
    s.vsMap := new;
  END ExpandVSMap;

(*----------------------------------------------------------------- units ---*)

PROCEDURE ReadUnit (VAR s: State): BOOLEAN = 
  (* In  --- Interface 'n'  *)
  (* Mn  --- Module 'n'     *)
  VAR intf                := (s.cmd = 'I');
  VAR nm                  := GetName    (s, ' ');
  VAR n_exported_units    := GetInteger (s, ' ');
  VAR n_imported_units    := GetInteger (s, ' ');
  VAR n_imported_generics := GetInteger (s, ' ');
  VAR n_used_interfaces   := GetInteger (s, ' ');
  VAR n_used_modules      := GetInteger (s, ' ');
  VAR n_import_def_syms   := GetInteger (s, ' ');
  VAR n_import_use_syms   := GetInteger (s, ' ');
  VAR n_export_def_syms   := GetInteger (s, ' ');
  VAR n_export_use_syms   := GetInteger (s, ' ');
  VAR n_imported_types    := GetInteger (s, ' ');
  VAR n_exported_types    := GetInteger (s, ' ');
  VAR n_wishes            := GetInteger (s, '\n');
  VAR u := NEW (Mx.Unit, name := nm, interface := intf, domain := s.domain);
  VAR node := NEW (Mx.UnitList, unit := u, next := s.units);
  VAR n_info := 0;
  BEGIN
    u.mInfo := NEW (Mx.MergeInfo);
    u.mInfo.file := s.cur_file;
    u.mInfo.exported_units.start    := n_info; INC(n_info, n_exported_units);
    u.mInfo.imported_units.start    := n_info; INC(n_info, n_imported_units);
    u.mInfo.imported_generics.start := n_info; INC(n_info,n_imported_generics);
    u.mInfo.used_interfaces.start   := n_info; INC(n_info, n_used_interfaces);
    u.mInfo.used_modules.start      := n_info; INC(n_info, n_used_modules);
    u.mInfo.import_def_syms.start   := n_info; INC(n_info, n_import_def_syms);
    u.mInfo.import_use_syms.start   := n_info; INC(n_info, n_import_use_syms);
    u.mInfo.export_def_syms.start   := n_info; INC(n_info, n_export_def_syms);
    u.mInfo.export_use_syms.start   := n_info; INC(n_info, n_export_use_syms);
    u.mInfo.imported_types.start    := n_info; INC(n_info, n_imported_types);
    u.mInfo.exported_types.start    := n_info; INC(n_info, n_exported_types);
    u.mInfo.wishes.start            := n_info; INC(n_info, n_wishes);

    u.mInfo.info  := NEW (Mx.InfoVec, n_info);
    s.units    := node;
    s.cur_unit := u;
    RETURN FALSE;
  END ReadUnit;

PROCEDURE AddInfo (u: Mx.Unit;  VAR x: Mx.InfoList;  i: INTEGER) =
  BEGIN
    u.mInfo.info [x.start + x.cnt] := i;
    INC (x.cnt);
  END AddInfo;

PROCEDURE ReadPort (VAR s: State): BOOLEAN = 
  (* Am  --- exports interface m *)
  (* Bm  --- imports interface m *)
  VAR export := (s.cmd = 'A');
  VAR nm     := GetName (s, '\n');
  VAR unit   := s.cur_unit;
  BEGIN
    IF (unit = NIL) THEN
      Error (s, "import/export while current unit not defined!");
    ELSIF (export) THEN
      AddInfo (unit, unit.mInfo.exported_units, nm);
    ELSE (* import *)
      AddInfo (unit, unit.mInfo.imported_units, nm);
    END;
    RETURN FALSE;
  END ReadPort;

PROCEDURE ReadUse (VAR s: State): BOOLEAN = 
  (* Cm  --- uses magic info from interface m *)
  (* Dm  --- uses magic info from module m  *)
  VAR intf := (s.cmd = 'C');
  VAR nm   := GetName (s, '\n');
  VAR unit := s.cur_unit;
  BEGIN
    IF (unit = NIL) THEN
      Error (s, "magic import while current unit not defined!");
    ELSIF (intf) THEN
      AddInfo (unit, unit.mInfo.used_interfaces, nm);
    ELSE (* import *)
      AddInfo (unit, unit.mInfo.used_modules, nm);
    END;
    RETURN FALSE;
  END ReadUse;

PROCEDURE ReadGeneric (VAR s: State): BOOLEAN = 
  (* gm    --- imports generic unit m *)
  VAR nm   := GetName (s, '\n');
  VAR unit := s.cur_unit;
  BEGIN
    IF (unit = NIL) THEN
      Error (s, "generic import while current unit not defined!");
    ELSE
      AddInfo (unit, unit.mInfo.imported_generics, nm);
    END;
    RETURN FALSE;
  END ReadGeneric;

PROCEDURE ReadVersionStamp (VAR s: State): BOOLEAN = 
  (* ix       --- import version stamp 'x' *)
  (* Jx       --- import & implement version stamp 'x' *)
  (* ex       --- export version stamp 'x' *)
  (* Ex       --- export & implement version stamp 'x' *)
  VAR cmd  := s.cmd;
  VAR vs   := GetVS (s, '\n');
  VAR unit := s.cur_unit;
  BEGIN
    IF (unit = NIL) THEN
      Error (s, "version stamps while current unit not defined!");
    ELSIF (cmd = 'J') THEN
      AddInfo (unit, unit.mInfo.import_def_syms, vs);
    ELSIF (cmd = 'i') THEN
      AddInfo (unit, unit.mInfo.import_use_syms, vs);
    ELSIF (cmd = 'E') THEN
      AddInfo (unit, unit.mInfo.export_def_syms, vs);
    ELSE (*cmd = 'e'*)
      AddInfo (unit, unit.mInfo.export_use_syms, vs);
    END;
    RETURN FALSE;
  END ReadVersionStamp;

PROCEDURE ReadRevelation (VAR s: State): BOOLEAN = 
  (* Rn x y  --- export REVEAL 'x' = 'y'  to interface #n *)
  (* Xn x y  --- export REVEAL 'x' <: 'y' to interface #n *)
  (* rn x y  --- import REVEAL 'x' = 'y'  from interface #n *)
  (* xn x y  --- import REVEAL 'x' <: 'y' from interface #n *)
  VAR export  := (s.cmd = 'R') OR (s.cmd = 'X');
  VAR partial := (s.cmd = 'x') OR (s.cmd = 'X');
  VAR r       := NEW (Mx.Revelation, export := export, partial := partial);
  VAR unit    := s.cur_unit;
  BEGIN
    r.source := GetName (s, ' ');
    r.lhs    := GetTypeName (s, ' ');
    r.rhs    := GetTypeName (s, '\n');
    IF (unit = NIL) THEN
      Error (s, "revelations while current unit not defined!");
    ELSE
      r.next := unit.revelations;
      unit.revelations := r;
    END;
    RETURN FALSE;
  END ReadRevelation;

PROCEDURE ReadWish (VAR s: State): BOOLEAN = 
  (* wt       --- wish to know the object type 't'. *)
  VAR type := GetTypeName (s, '\n');
  VAR unit := s.cur_unit;
  BEGIN
    IF (unit = NIL) THEN
      Error (s, "object wish while current unit not defined!");
    ELSE
      AddInfo (unit, unit.mInfo.wishes, type);
    END;
    RETURN FALSE;
  END ReadWish;

PROCEDURE ReadType (VAR s: State): BOOLEAN = 
  (* tx       --- import type 'x'   *)
  (* Tx       --- export type 'x'   *)
  VAR cmd  := s.cmd;
  VAR type := GetTypeName (s, '\n');
  VAR unit := s.cur_unit;
  BEGIN
    IF (unit = NIL) THEN
      Error (s, "type import/export while current unit not defined!");
    ELSIF (cmd = 't') THEN
      AddInfo (unit, unit.mInfo.imported_types, type);
    ELSE
      AddInfo (unit, unit.mInfo.exported_types, type);
    END;
    RETURN FALSE;
  END ReadType;

PROCEDURE ReadObjectType (VAR s: State): BOOLEAN = 
  (* on t s ds da ms -- import object type from interface unit n    *)
  (* pn t s ds da ms -- import object type from module unit n       *)
  (* Ot s ds da ms   -- export object type 't' with supertype 's',  *)
  (*                    data size 'ds', data alignment 'da', and    *)
  (*                    method size 'ms' from unit #n               *)
  VAR export := (s.cmd = 'O');
  VAR module := (s.cmd = 'p');
  VAR obj    := NEW (Mx.ObjectType, export := export, from_module := module);
  VAR unit   := s.cur_unit;
  BEGIN
    IF (NOT export) THEN  obj.source := GetName (s, ' ') END;
    obj.type          := GetTypeName (s, ' ');
    obj.super_type    := GetTypeName (s, ' ');
    obj.data_size     := GetInteger (s, ' ');
    obj.data_align    := GetInteger (s, ' ');
    obj.method_size   := GetInteger (s, '\n');

    IF (unit = NIL) THEN
      Error (s, "object info while current unit not defined!");
    ELSIF (export) THEN
      obj.source := unit.name;
      obj.from_module := NOT unit.interface;
      obj.next := unit.exported_objects;
      unit.exported_objects := obj;
    ELSE
      obj.next := unit.imported_objects;
      unit.imported_objects := obj;
    END;

    RETURN FALSE;
  END ReadObjectType;

PROCEDURE ReadOpaqueType (VAR s: State): BOOLEAN = 
  (* Qt s     --- define opaque type 't' with supertype 's'. *)
  VAR opaque := NEW (Mx.OpaqueType);
  VAR unit   := s.cur_unit;
  BEGIN
    opaque.type       := GetTypeName (s, ' ');
    opaque.super_type := GetTypeName (s, '\n');

    IF (unit = NIL) THEN
      Error (s, "opaque type defined while current unit not defined!");
    ELSE
      opaque.next := unit.opaques;
      unit.opaques := opaque;
    END;

    RETURN FALSE;
  END ReadOpaqueType;

PROCEDURE SkipComment (VAR s: State): BOOLEAN = 
  VAR ch := '/';
  BEGIN
    WHILE (ch # '\n') DO ch := GetC (s) END;
    RETURN FALSE;
  END SkipComment;

PROCEDURE SkipBlank (<*UNUSED*> VAR s: State): BOOLEAN =
  BEGIN
    RETURN FALSE;
  END SkipBlank;

PROCEDURE GetName (VAR s: State;  term: CHAR): Mx.Name = 
  VAR id := GetInteger (s, term);
  BEGIN
    IF (0 <= id) AND (id < NUMBER (s.nameMap^)) THEN
      RETURN s.nameMap [id];
    ELSE
      Error (s, "bad unit number: " & Fmt.Int (id));
      RETURN M3ID.NoID;
    END;
  END GetName;

PROCEDURE GetVS (VAR s: State;  term: CHAR): MxVS.T = 
  VAR id := GetInteger (s, term);
  BEGIN
    IF (0 <= id) AND (id < NUMBER (s.vsMap^)) THEN
      RETURN s.vsMap [id];
    ELSE
      Error (s, "bad version stamp number: " & Fmt.Int (id));
      RETURN MxVS.NoVS;
    END;
  END GetVS;

PROCEDURE GetInteger (VAR s: State;  term: CHAR): INTEGER = 
  VAR n   := 0;
  VAR len := 0;
  VAR ch: CHAR;
  BEGIN
    LOOP
      (* ch := GetNextChar(s); *)

      ch := s.info^;
      INC (s.buf_ptr);
      INC (s.info, ADRSIZE(CHAR));

      IF (ch < '0') OR ('9' < ch) THEN
        (* NOTE: none of the stop characters are legal digits *)
        IF (s.buf_ptr <= s.buf_len) THEN EXIT END;
        ch := GetC (s);
        IF (ch < '0') OR ('9' < ch) THEN EXIT END;
      END;
      n := 10 * n + (ORD (ch) - ORD ('0'));
      INC (len);
    END;
    IF (ch = '\r') THEN ch := GetC (s); END;
    IF (len <= 0)  THEN Error (s, "expected integer") END;
    IF (ch # term) THEN Error (s, "expecting separator after integer "); END;
    RETURN n;
  END GetInteger;

PROCEDURE GetTypeName (VAR s: State;  term: CHAR): Mx.TypeName = 
  VAR n   := 0;
  VAR len := 0;
  VAR ch    : CHAR;
  VAR digit : INTEGER;
  BEGIN
    LOOP
      (* ch := GetNextChar(s);*)

      ch := s.info^;
      INC (s.buf_ptr);
      INC (s.info, ADRSIZE(CHAR));

      digit := HexDigit [ch];
      IF (digit > 15) THEN
        (* NOTE: none of the stop characters are legal digits *)
        IF (s.buf_ptr <= s.buf_len) THEN EXIT END;
        ch := GetC (s);
        digit := HexDigit [ch];
        IF (digit > 15) THEN EXIT END;
      END;
      n := Word.Plus (Word.Times (n, 16), digit);
      INC (len);
    END;
    IF (ch = '\r') THEN ch := GetC (s); END;
    IF (len <= 0)  THEN Error (s, "expected typename") END;
    IF (ch # term) THEN Error (s, "expecting separator after typename") END;
    IF Word.And (n, SignBit) # 0 THEN  n := Word.Or (SignExtend, n);  END;
    RETURN n;
  END GetTypeName;

VAR idBuf: ARRAY [0..256] OF CHAR;

PROCEDURE GetID (VAR s: State;  term: CHAR): Mx.Name = 
(* Note: we don't need to check for array overruns since all calls
   to GetString include a terminating character that's in the "stop set"
   at the end of the buffer *)
  VAR stop, start, len: CARDINAL;  overflow: TEXT;  ch: CHAR;
  VAR i: INTEGER;
  BEGIN
    start := s.buf_ptr;
    stop  := start;
    i := 0;
    LOOP
      (* ch := GetNextChar(s);*)

      ch := s.info^;
      INC (s.buf_ptr);
      INC (s.info, ADRSIZE(CHAR));

      IF (ch = '\r') OR (ch = term) THEN 
        (* UndoChar(s); *)
        DEC (s.info, ADRSIZE(CHAR));
        DEC(s.buf_ptr);
        EXIT 
      END;
      idBuf[i] := ch;
      INC(i);
      INC (stop)
    END;

    IF (stop < s.buf_len) THEN
      (* this is the simple case, the string's entirely in the buffer *)
      RETURN M3ID.FromStr (SUBARRAY (idBuf,0,i));
    END;

    overflow := "";
    LOOP
      (* we've overrun the end of the buffer *)
      (* save the current string & refill the buffer *)
      len := MAX (s.buf_len - start, 0);
      FOR i := 0 TO len-1 DO
        (* idBuf[i] := GetNextChar(s);*)

        idBuf[i] := s.info^;
        INC (s.buf_ptr);
        INC (s.info, ADRSIZE(CHAR));

      END;
      overflow := overflow & Text.FromChars (SUBARRAY (idBuf, 0, len));
      RETURN M3ID.Add (overflow);
    END;
    
  END GetID;

PROCEDURE GetStamp (VAR s: State;  VAR x: M3FP.T;  term: CHAR) = 
  VAR
    ch: CHAR;
    i, j, d1, d0: INTEGER;
    len: INTEGER := 0;
    buf: ARRAY [0..15] OF CHAR;
  BEGIN
    LOOP
      ch := GetC (s);
      IF (ch = term) THEN EXIT END;
      IF (len >= NUMBER (buf)) THEN
        Error (s, "version stamp too long!"); EXIT;
      ELSIF (len < NUMBER (buf)) THEN
        buf[len] := ch;
      END;
      INC (len);
    END;
    len := MIN (len, NUMBER (buf));

    (* convert the buffered characters into a fingerprint *)
    i := 0;  j := 0;
    WHILE (i < len) DO
      d1 := HexDigit [buf[i]];  INC (i);
      d0 := HexDigit [buf[i]];  INC (i);
      IF (d1 > 15) OR (d0 > 15) THEN
        Error (s, "illegal hex digit in version stamp");
      END;
      x.byte[j] := Word.LeftShift (d1, 4) + d0;  INC (j);
    END;

    WHILE (j <= LAST (x.byte)) DO
      x.byte[j] := 0;  INC (j);
    END;
  END GetStamp;

(*------------------------------------------------------------------------*)

PROCEDURE GetC (VAR s: State): CHAR = 
  VAR c: CHAR;
  BEGIN
    REPEAT
      (* c := GetNextChar(s);*)
      c := s.info^;
      INC (s.buf_ptr);
      INC (s.info, ADRSIZE(CHAR));
    UNTIL (c # '\r');
    RETURN c;
  END GetC;

PROCEDURE FillBuffer (VAR s: State) =
  BEGIN
    s.buf_len := s.info_len+1;
    s.buf_ptr := 0;
  END FillBuffer;

PROCEDURE Strlen(ptr: UNTRACED REF CHAR): INTEGER =
  VAR
    len: INTEGER := 0;
  BEGIN
    WHILE ptr^ # '\000' DO
      INC(len); 
      INC(ptr, ADRSIZE(CHAR));
    END;
    RETURN len;
  END Strlen;

(*------------------------------------------------------------------------*)

PROCEDURE Error (VAR s: State;  a, b, c, d: Text.T := NIL) =
  BEGIN
    INC  (s.nErrors);
    IF (s.errors = NIL) THEN RETURN END;
    IF (s.cur_file # NIL) AND (s.cur_file.name # NIL) THEN
      Wr.PutText (s.errors, s.cur_file.name);
      IF (s.cur_unit # NIL) THEN
        Wr.PutText (s.errors, " (");
        Wr.PutText (s.errors, MxRep.UnitName (s.cur_unit));
        Wr.PutText (s.errors, ")");
      END;
      Wr.PutText (s.errors, ": ");
    END;
    Wr.PutText (s.errors, "ERROR: ");
    IF (a # NIL) THEN Wr.PutText (s.errors, a); END;
    IF (b # NIL) THEN Wr.PutText (s.errors, b); END;
    IF (c # NIL) THEN Wr.PutText (s.errors, c); END;
    IF (d # NIL) THEN Wr.PutText (s.errors, d); END;
    Wr.PutText (s.errors, Wr.EOL);
    LOOP END;
  END Error;

PROCEDURE CharName (c: CHAR): Text.T =
  BEGIN
    IF (' ' <= c) AND (c <= '~')
      THEN RETURN "\'" & Text.FromChar (c) & "\'" ;
      ELSE RETURN "\'\\" & Fmt.Pad (Fmt.Int (ORD (c), 8), 3, '0') & "\'" ;
    END;
  END CharName;


PROCEDURE Init () =
  BEGIN
    FOR i := FIRST (HexDigit) TO LAST (HexDigit) DO HexDigit[i] := 16 END;
    FOR i := '0' TO '9' DO HexDigit [i] := ORD (i) - ORD ('0') END;
    FOR i := 'a' TO 'f' DO HexDigit [i] := ORD (i) - ORD ('a') + 10 END;
    FOR i := 'A' TO 'F' DO HexDigit [i] := ORD (i) - ORD ('A') + 10 END;

    FOR c := FIRST (CmdMap) TO LAST (CmdMap) DO CmdMap[c] := BadChar END;
    CmdMap ['N'] := ReadName;
    CmdMap ['V'] := ReadVSInfo;
    CmdMap ['I'] := ReadUnit;
    CmdMap ['M'] := ReadUnit;
    CmdMap ['A'] := ReadPort;
    CmdMap ['B'] := ReadPort;
    CmdMap ['C'] := ReadUse;
    CmdMap ['D'] := ReadUse;
    CmdMap ['g'] := ReadGeneric;
    CmdMap ['i'] := ReadVersionStamp;
    CmdMap ['J'] := ReadVersionStamp;
    CmdMap ['e'] := ReadVersionStamp;
    CmdMap ['E'] := ReadVersionStamp;
    CmdMap ['R'] := ReadRevelation;
    CmdMap ['X'] := ReadRevelation;
    CmdMap ['r'] := ReadRevelation;
    CmdMap ['x'] := ReadRevelation;
    CmdMap ['t'] := ReadType;
    CmdMap ['T'] := ReadType;
    CmdMap ['w'] := ReadWish;
    CmdMap ['o'] := ReadObjectType;
    CmdMap ['p'] := ReadObjectType;
    CmdMap ['O'] := ReadObjectType;
    CmdMap ['Q'] := ReadOpaqueType;
    CmdMap ['F'] := ReadFlags;
    CmdMap ['/'] := SkipComment;
    CmdMap [' '] := SkipBlank;
    CmdMap ['\r'] := SkipBlank;
    CmdMap ['\t'] := SkipBlank;
    CmdMap ['\n'] := SkipBlank;
    CmdMap [End_of_buffer] := EndBuffer;
  END Init;

<* UNUSED *>
PROCEDURE GetNextChar(VAR s: State): CHAR =
  VAR c: CHAR;
  BEGIN
    IF s.buf_ptr > s.buf_len THEN
      Wr.PutText(s.errors, "ERROR >> OUT OF BOUND\n" );
    END;
    c := s.info^;
    INC (s.buf_ptr);
    INC (s.info, ADRSIZE(CHAR));
    RETURN c;
  END GetNextChar;

<* UNUSED *>
PROCEDURE UndoChar(VAR s: State) =
  BEGIN
    DEC (s.info, ADRSIZE(CHAR));
    DEC(s.buf_ptr);
  END UndoChar;

BEGIN
END MxIn.
