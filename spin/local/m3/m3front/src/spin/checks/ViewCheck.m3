
(*
 * HISTORY
 * 19-Jun-96  Wilson Hsieh (whsieh) at the University of Washington
 *	created: moved code from Type.m3
 *
 *)

MODULE ViewCheck EXPORTS ViewCheck, Type;

IMPORT TypeRep, NamedType, Word, ArrayType, PackedType, EnumType;
IMPORT RecordType, SubrangeType, Value, Target, TInt, ValueRep, Field;
IMPORT AlignedType;

VAR
  MaxSigned, MinSigned: INTEGER;
  MaxUnsigned, MinUnsigned: Word.T;

(* exported procedures *)

PROCEDURE ViewInit () =
  BEGIN
    EVAL TInt.ToInt(Target.Int_D.max, MaxSigned);
    EVAL TInt.ToInt(Target.Int_D.min, MinSigned);
    EVAL TInt.ToInt(Target.Word_D.max, MaxUnsigned);
    EVAL TInt.ToInt(Target.Word_D.min, MinUnsigned);
  END ViewInit;

PROCEDURE RepresentationComplete (t: T) : BOOLEAN =
  VAR info: Info;
  BEGIN
    EVAL CheckInfo (t, info);
    RETURN RepresentationCompleteInternal (t, TRUE, info.size);
  END RepresentationComplete;

PROCEDURE RepresentationCompleteFor (t: T; b: CARDINAL) : BOOLEAN =
  BEGIN
    RETURN RepresentationCompleteInternal (t, TRUE, b);
  END RepresentationCompleteFor;


(* internal procedure *)

PROCEDURE RepresentationCompleteInternal (t: T; top: BOOLEAN; bits: INTEGER)
  : BOOLEAN =
  (* the top parameter indicates that the check is at the
     top level
     open arrays are legal at the top, but not legal elsewhere
     bits can be negative because T can be an open array
   *)
  VAR
    info: Info;
  BEGIN
    (* not sure what NIL means *)
    IF (t = NIL) THEN RETURN FALSE END;

    CASE t.info.class OF
    | Class.Error,
      Class.Object, Class.Opaque, Class.Procedure, Class.Ref,
      Class.Real, Class.Longreal, Class.Extended =>
      RETURN FALSE;

    | Class.Named =>
      RETURN RepresentationCompleteInternal (NamedType.Strip (t), FALSE, bits);
    | Class.Integer =>
      RETURN TRUE;
    | Class.Aligned =>
      RETURN RepresentationCompleteInternal (AlignedType.Base(t), FALSE, bits);
    | Class.Array => 
      VAR index, element: T;
      BEGIN
        EVAL ArrayType.Split(t, index, element);
        EVAL CheckInfo (element, info);
        RETURN RepresentationCompleteInternal (element, FALSE, info.size);
      END;
    | Class.Enum =>
      VAR
        count: INTEGER := EnumType.NumElts(t);
      BEGIN
        (* enums are packed as tightly as possible by the compiler,
           so we have to check based on the number of bits occupied by
           the enum *)
        EVAL CheckInfo (t, info);
        bits := MIN (bits, info.size);

        IF (bits = Target.Integer.size) THEN
          IF (count = MaxUnsigned) THEN
            RETURN TRUE;
          ELSE
            RETURN FALSE;
          END;
        ELSIF (count = Word.Shift(1, bits)) THEN
          RETURN TRUE;
        ELSE
          RETURN FALSE;
        END;
      END;
    | Class.OpenArray =>
      IF top THEN
        VAR index, element: T;
        BEGIN
          EVAL ArrayType.Split(t, index, element);
          EVAL CheckInfo (element, info);
          RETURN RepresentationCompleteInternal (element, FALSE, info.size);
        END;
      ELSE
        RETURN FALSE;
      END;
    | Class.Packed =>
      VAR new_bits: INTEGER;
          base: T;
      BEGIN
        EVAL PackedType.Split (t, new_bits, base);
        EVAL CheckInfo (base, info);

        IF info.min_size > new_bits THEN
          <* ASSERT FALSE *>
        ELSIF info.min_size < new_bits THEN
          RETURN FALSE;
        ELSE
          RETURN RepresentationCompleteInternal (base, FALSE, new_bits);
        END;
      END;
    | Class.Record =>
      VAR fields: Value.T;
          field_check: BOOLEAN;
          field_info: Field.Info;
      BEGIN
        field_check := RecordType.Split(t, fields);
        <* ASSERT field_check *>
        WHILE (fields # NIL) DO
          field_check := Field.Is(fields);
          <* ASSERT field_check *>
          Field.Split(fields, field_info);
          IF field_info.offset >= bits THEN RETURN TRUE; END;
          EVAL CheckInfo (field_info.type, info);
          IF RepresentationCompleteInternal (field_info.type, FALSE, info.size) THEN
            fields := fields.next;
          ELSE
            RETURN FALSE;
          END;
        END;
        RETURN TRUE;
      END;
    | Class.Set =>
        BEGIN
          EVAL CheckInfo (t, info);
          IF bits = info.min_size THEN
            RETURN TRUE;
          ELSE
            RETURN FALSE;
          END;
        END;
    | Class.Subrange =>
      VAR min, max: Target.Int;
          check: BOOLEAN;
          mini, maxi: INTEGER;
          ubitsmax, sbitsmax: INTEGER;
          info : Info;
      BEGIN
        (* subranges are packed as tightly as possible by the compiler,
           so we have to check based on the number of bits occupied by
           the subrange *)
        EVAL CheckInfo (t, info);
        bits := MIN (bits, info.size);

        check := SubrangeType.Split (t, min, max);
        <* ASSERT check *>
        check := TInt.ToInt (min, mini);
        <* ASSERT check *>
        check := TInt.ToInt (max, maxi);
        <* ASSERT check *>
        ubitsmax := Word.Shift (1, bits)-1;
        sbitsmax := Word.Shift (1, bits-1)-1;

        IF (bits = Target.Integer.size) THEN 
          IF ((mini = MinUnsigned) AND (maxi = MaxUnsigned)) OR
            ((mini = MinSigned) AND (maxi = MaxSigned)) THEN
            RETURN TRUE;
          ELSE
            RETURN FALSE;
          END;
        ELSIF ((mini = 0) AND (maxi = ubitsmax)) OR
          ((mini = -sbitsmax-1) AND (maxi = sbitsmax)) THEN
          RETURN TRUE;
        ELSE
          RETURN FALSE;
        END;
      END;
    END;
  END RepresentationCompleteInternal;


BEGIN
END ViewCheck.
