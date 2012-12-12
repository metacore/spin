(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 09-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	Change interface names so that these don't conflict w/
 *	those used by user/net.
 *
 * 18-Mar-97  Tsutomu Owa (owa) at the University of Washington
 *	Reverted If structure defined as urt/urtcore/src.
 *
 * 03-Feb-97  Tsutomu Owa (owa) at the University of Washington
 *	Created.  utility procedures for this package.
 *)

MODULE StcpUtils;
IMPORT IO, Fmt;
IMPORT StcpIf;
IMPORT StcpMbuf, StcpMbufPublic, Word, Ctypes ; 

(* ---------------------------------------------------------------- *)
(* Print ARARRAY OF CHAR according to passed length and base type *)
PROCEDURE PrintArrayOfChar(
    READONLY data : ARRAY OF CHAR;
    len, base : CARDINAL;		(* len: how many chars to write *)
    needCR : BOOLEAN) =			(* base: base of Fmt.Int() *)
  BEGIN					(* needCR: flag to put CR at the end *)
    FOR i := 0 TO (len - 1)
      DO
        IO.Put(Fmt.Int(ORD(data[i]), base) & ".");
      END;
    IF needCR THEN IO.Put("\n"); END;
  END PrintArrayOfChar;

(* ---------------------------------------------------------------- *)
(* Print StcpIf.ifdevea *)
PROCEDURE PrintStcpIfdevea(ifdevea : StcpIf.ifdevea; <*UNUSED*>  needCR : BOOLEAN) =
  BEGIN
    IO.Put("StcpIfdevea\n");
    IO.Put("  default_pa: ");
    PrintArrayOfChar(ifdevea.default_pa, 6, 16, TRUE); 
    IO.Put("  current_pa: ");
    PrintArrayOfChar(ifdevea.current_pa, 6, 16, TRUE);

  END PrintStcpIfdevea;

(* ---------------------------------------------------------------- *)
PROCEDURE PrintStcpMbuf(m : StcpMbuf.T) =
  PROCEDURE GetStcpMbufType(type : StcpMbuf.StcpMbufTypes) : TEXT =
    BEGIN
      CASE type OF
        | StcpMbuf.MT_FREE => RETURN "MT_FREE";
        | StcpMbuf.MT_DATA => RETURN "MT_DATA";
        | StcpMbuf.MT_HEADER => RETURN "MT_HEADER";
        | StcpMbuf.MT_SOCKET => RETURN "MT_SOCKET";
                (* define more if necessary *)
      ELSE
        RETURN "Unknown StcpMbuf type";
      END;
    END GetStcpMbufType;
  PROCEDURE GetStcpMbufFlags(flags : Ctypes.int) : TEXT =
    VAR
      val : TEXT := " ";
    BEGIN
      IF Word.And(flags, StcpMbuf.M_EXT) # 0 THEN val:= val & "EXT|"; END;
      IF Word.And(flags, StcpMbuf.M_PKTHDR) # 0 THEN val:= val & "PKTHDR|"; END;
      IF Word.And(flags, StcpMbuf.M_EOR) # 0 THEN val:= val & "EOR|"; END;
      IF Word.And(flags, StcpMbuf.M_FASTFREE) # 0 THEN val:= val & "FASTFREE|"; END
;
      IF Word.And(flags, StcpMbuf.M_M3METHODS) # 0 THEN val:= val & "M3METHODS|"; END;
      IF Word.And(flags, StcpMbuf.M_BCAST) # 0 THEN val:= val & "BCAST||"; END;
      IF Word.And(flags, StcpMbuf.M_MCAST) # 0 THEN val:= val & "MCAST||"; END;
      IF Word.And(flags, StcpMbuf.M_WCARD) # 0 THEN val:= val & "WCARD"; END;
      RETURN val;
    END GetStcpMbufFlags;

  PROCEDURE PrintOneStcpMbuf(m : StcpMbuf.T ; number : CARDINAL) =
    BEGIN
      IO.Put("StcpMbuf (" & Fmt.Int(number) & ")");
      IO.Put(" len: " & Fmt.Int(m.mh_hdr.mh_len));
(*    IO.Put(", data: " & Fmt.Int(m.mh_hdr.mh_data)); *)
      IO.Put(", type: " & GetStcpMbufType(m.mh_hdr.mh_type));
      IO.Put(", flags: " & GetStcpMbufFlags(m.mh_hdr.mh_flags));
      IF (Word.And(m.mh_hdr.mh_flags, StcpMbuf.M_PKTHDR) # 0) THEN
	 IO.Put("  pkthdrlen " & Fmt.Int(StcpMbufPublic.GetPktHdrLen(m)));
      END;
      IO.Put("\n");
    END PrintOneStcpMbuf;

  VAR
    t : StcpMbuf.T;
    n, totalLen : CARDINAL := 0;
  BEGIN
    t := m;
    WHILE t # NIL DO
      PrintOneStcpMbuf(t, n);
      INC(totalLen, t.mh_hdr.mh_len);
      INC(n); 
      t := t.mh_hdr.mh_next;
    END;
    IO.Put("  Total length: " & Fmt.Int(totalLen) & "\n");
  END PrintStcpMbuf;

BEGIN
END StcpUtils.
