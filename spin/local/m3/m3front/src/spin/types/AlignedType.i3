
(*
 * HISTORY
 * 03-Mar-96  Wilson Hsieh (whsieh) at the University of Washington
 *	created
 *	based on PackedType.m3
 *
 *)

INTERFACE AlignedType;

IMPORT Type;

PROCEDURE Parse (): Type.T;

PROCEDURE New (size: INTEGER;  base: Type.T): Type.T;

PROCEDURE Split (t: Type.T;  VAR align: INTEGER;  VAR base: Type.T): BOOLEAN;

PROCEDURE Base (t: Type.T): Type.T;

END AlignedType.
