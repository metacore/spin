(* Copyright (C) 1993, Digital Equipment Corporation            *)
(* All rights reserved.                                         *)
(* See the file COPYRIGHT for a full description.               *)
(*                                                              *)
(* File: Target.m3                                              *)
(* Last Modified On Tue Jan 24 09:58:43 PST 1995 By kalsow      *)
(*      Modified On Fri Dec  9 12:56:47 PST 1994 By Olaf Wagner *)

(* HISTORY
 * 28-Dec-97  Przemek Pardyak (pardy) at the University of Washington
 *	Changed some comments.
 *
 * 29-Aug-97  Marc Fiuczynski (mef) at the University of Washington
 *	Changed "First_readable_addr" for the IX86_SPIN platform to
 *	avoid NilRefChecks with offsets less than 64KB.
 *
 * 27-Aug-97  Robert Grimm (rgrimm) at the University of Washington
 *      unrolled change to Jumpbuf_size since there is a better solution
 *
 * 25-Aug-97  Robert Grimm (rgrimm) at the University of Washington
 *      increased Jumpbuf_size by one for ALPHA_SPIN + IX86_SPIN
 *
 * 18-Apr-97  Wilson Hsieh (whsieh) at the University of Washington
 *	add Unaligned_memory_reference_ok variable
 *
 * 01-Feb-97  Wilson Hsieh (whsieh) at the University of Washington
 *	removed ALPHA_SPIN_PROF -- handled in templates now
 *
 * 15-Jan-97  Marc Fiuczynski (mef) at the University of Washington
 *	Decreased the Jumpbuf_size for ALPHA_SPIN from 84 to 9 quadwords.
 *
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Added target IX86_SPIN.
 *
 * 29-Jul-96  Charles Garrett (garrett) at the University of Washington
 *	Check_integer_ops is false for SPIN.
 *
 * 05-May-96  Charles Garrett (garrett) at the University of Washington
 *	New ALPHA_SPIN_PROF target which is ALPHA_SPIN built with the
 *	 "-p" option turned on.
 *
 * 22-Feb-96  Charles Garrett (garrett) at the University of Washington
 *	Added target ALPHA_SPIN which is surprisingly similar to ALPHA_OSF.
 *
 *)

MODULE Target;

IMPORT Text, TargetMap, M3RT;

TYPE
  SystemId = RECORD
    name: TEXT;
    id: INTEGER;
  END;

CONST
  Systems = ARRAY OF SystemId {
    SystemId{"AIX386", 0},
    SystemId{"ALPHA_OSF", 1},
    SystemId{"ALPHA_SPIN", 2},
    SystemId{"AP3000", 3},
    SystemId{"ARM", 4},
    SystemId{"DS3100", 5},
    SystemId{"FreeBSD", 6},
    SystemId{"FreeBSD2", 7},
    SystemId{"HP300", 8},
    SystemId{"HPPA", 9},
    SystemId{"IBMR2", 10},
    SystemId{"IBMRT", 11},
    SystemId{"IRIX5", 12},
    SystemId{"LINUX", 13},
    SystemId{"LINUXELF", 14},
    SystemId{"IX86_SPIN", 15},
    SystemId{"NEXT", 16},
    SystemId{"NT386", 17},
    SystemId{"OKI", 18},
    SystemId{"SEQUENT", 19},
    SystemId{"SOLgnu", 20},
    SystemId{"SOLsun", 21},
    SystemId{"SPARC", 22},
    SystemId{"SUN3", 23},
    SystemId{"SUN386", 24},
    SystemId{"UMAX", 25},
    SystemId{"VAX", 26},
    SystemId{"Lanai3.1", 27}
  };

VAR (*CONST*)
  CCs : REF ARRAY OF CallingConvention;

PROCEDURE Init (system: TEXT): BOOLEAN =
  CONST FF = 16_ffff;
  VAR sys := 0;  max_align := 64;
  BEGIN
    (* lookup the system *)
    IF (system = NIL) THEN RETURN FALSE END;
    WHILE NOT Text.Equal (system, Systems[sys].name) DO
      INC (sys);  IF (sys >= NUMBER (Systems)) THEN RETURN FALSE END;
    END;
    System_name := Systems[sys].name;
    sys := Systems[sys].id;  (* Not necessarily the same number *)

    (* build a generic 32-bit/IEEE system description *)

    Address.cg_type  := CGType.Addr;
    Address.size     := 32;
    Address.align    := 32;
    Address.min.x    := IChunks { 00, 00, 00, 00 };
    Address.max.x    := IChunks { FF, FF, 00, 00 };

    Int_A.cg_type    := CGType.Int_A;
    Int_A.size       := 8;
    Int_A.align      := 8;
    Int_A.min.x      := IChunks { 16_ff80, FF, FF, FF };
    Int_A.max.x      := IChunks { 16_007f, 00, 00, 00 };

    Int_B.cg_type    := CGType.Int_B;
    Int_B.size       := 16;
    Int_B.align      := 16;
    Int_B.min.x      := IChunks { 16_8000, FF, FF, FF };
    Int_B.max.x      := IChunks { 16_7fff, 00, 00, 00 };

    Int_C.cg_type    := CGType.Int;
    Int_C.size       := 32;
    Int_C.align      := 32;
    Int_C.min.x      := IChunks { 00, 16_8000, FF, FF };
    Int_C.max.x      := IChunks { FF, 16_7fff, 00, 00 };

    Int_D            := Int_C;
    Integer          := Int_C;

    Word_A.cg_type   := CGType.Word_A;
    Word_A.size      := 8;
    Word_A.align     := 8;
    Word_A.min.x     := IChunks { 16_0000, 00, 00, 00 };
    Word_A.max.x     := IChunks { 16_00ff, 00, 00, 00 };

    Word_B.cg_type   := CGType.Word_B;
    Word_B.size      := 16;
    Word_B.align     := 16;
    Word_B.min.x     := IChunks { 00, 00, 00, 00 };
    Word_B.max.x     := IChunks { FF, 00, 00, 00 };

    Word_C.cg_type   := CGType.Word;
    Word_C.size      := 32;
    Word_C.align     := 32;
    Word_C.min.x     := IChunks { 00, 16_0000, 00, 00 };
    Word_C.max.x     := IChunks { FF, 16_7fff, 00, 00 };

    Word_D           := Word_C;
    Char             := Word_A;

    Void.cg_type     := CGType.Void;
    Void.size        := 0;
    Void.align       := Byte;
    Void.min.x       := IChunks { 0, 0, 0, 0 };
    Void.max.x       := IChunks { 0, 0, 0, 0 };

    Real.cg_type     := CGType.Reel;
    Real.pre         := Precision.Short;
    Real.size        := 32;
    Real.align       := 32;
    Real.min         := Float { Precision.Short, 0, -3.40282346638528860x+38 };
    Real.max         := Float { Precision.Short, 0,  3.40282346638528860x+38 };

    Longreal.cg_type := CGType.LReel;
    Longreal.pre     := Precision.Long;
    Longreal.size    := 64;
    Longreal.align   := 64;
    Longreal.min     := Float { Precision.Long, 0,-1.79769313486231570x+308 };
    Longreal.max     := Float { Precision.Long, 0, 1.79769313486231570x+308 };

    Extended.cg_type := CGType.XReel;
    Extended.pre     := Precision.Extended;
    Extended.size    := 64;
    Extended.align   := 64;
    Extended.min     := Float{Precision.Extended, 0,-1.79769313486231570x+308};
    Extended.max     := Float{Precision.Extended, 0, 1.79769313486231570x+308};

    Alignments[0] := 8;
    Alignments[1] := 16;
    Alignments[2] := 32;
    Alignments[3] := 64;

    CCs := NIL;

    (* SPIN *)
    Unaligned_memory_reference_ok := FALSE;

    (* add the system-specific customization *)
    CASE sys OF
    |  0 => (* AIX386 *)
                 max_align                 := 32;
                 Little_endian             := TRUE;
                 PCC_bitfield_type_matters := FALSE;
                 Structure_size_boundary   := 8;
                 Bitfield_can_overlap      := FALSE;
                 First_readable_addr       := 0;
                 Jumpbuf_size              := 25 * Address.size;
                 Jumpbuf_align             := Address.align;
                 Fixed_frame_size          := 4 * Address.size;
                 Guard_page_size           := 0;
                 All_floats_legal          := TRUE;
                 Has_stack_walker          := FALSE;
                 Setjmp                    := "_setjmp";
                 Checks_integer_ops        := FALSE;
                 Global_handler_stack      := TRUE;
                 Aligned_procedures        := TRUE;
                 EOL                       := "\n";

    |  1 => (* ALPHA_OSF *)
                 Int_C.cg_type    := CGType.Int_C;
                 Word_C.cg_type   := CGType.Word_C;
                 Word_C.max.x[1]  := FF;

                 Int_D.cg_type    := CGType.Int_D;
                 Int_D.size       := 64;
                 Int_D.align      := 64;
                 Int_D.min.x      := IChunks { 00, 00, 00, 16_8000 };
                 Int_D.max.x      := IChunks { FF, FF, FF, 16_7fff };

                 Word_D.cg_type   := CGType.Word_D;
                 Word_D.size      := 64;
                 Word_D.align     := 64;
                 Word_D.min.x     := IChunks { 00, 00, 00, 00 };
                 Word_D.max.x     := IChunks { FF, FF, FF, FF };

                 Integer          := Int_D;
                 Address          := Word_D;
                 Address.cg_type  := CGType.Addr;

                 max_align                 := 64;
                 Little_endian             := TRUE;
                 PCC_bitfield_type_matters := TRUE;
                 Structure_size_boundary   := 8;
                 Bitfield_can_overlap      := FALSE;
                 First_readable_addr       := 16_400000;
                 Jumpbuf_size              := 84 * Address.size;
                 Jumpbuf_align             := Address.align;
                 Fixed_frame_size          := 4 * Address.size;
                 Guard_page_size           := 4096 * Char.size;
                 All_floats_legal          := TRUE;
                 Has_stack_walker          := FALSE;
                 Setjmp                    := "_setjmp";
                 Checks_integer_ops        := FALSE;
                 Global_handler_stack      := TRUE;
                 Aligned_procedures        := TRUE;
                 EOL                       := "\n";

    |  2 => (* ALPHA_SPIN and ALPHA_SPIN_* *)
            (* Differs from ALPHA_OSF in the following ways:
             * First_readable_addr is 16_10000. This must be coordinated
             * with the SPIN protection fault mechanism in
             * sys/src/memory/Translation.m3
             *)
                 Int_C.cg_type    := CGType.Int_C;
                 Word_C.cg_type   := CGType.Word_C;
                 Word_C.max.x[1]  := FF;

                 Int_D.cg_type    := CGType.Int_D;
                 Int_D.size       := 64;
                 Int_D.align      := 64;
                 Int_D.min.x      := IChunks { 00, 00, 00, 16_8000 };
                 Int_D.max.x      := IChunks { FF, FF, FF, 16_7fff };

                 Word_D.cg_type   := CGType.Word_D;
                 Word_D.size      := 64;
                 Word_D.align     := 64;
                 Word_D.min.x     := IChunks { 00, 00, 00, 00 };
                 Word_D.max.x     := IChunks { FF, FF, FF, FF };

                 Integer          := Int_D;
                 Address          := Word_D;
                 Address.cg_type  := CGType.Addr;

                 max_align                 := 64;
                 Little_endian             := TRUE;
                 PCC_bitfield_type_matters := TRUE;
                 Structure_size_boundary   := 8;
                 Bitfield_can_overlap      := FALSE;
                 First_readable_addr       := 16_10000;
                 Jumpbuf_size              := 9 * Address.size;
                 Jumpbuf_align             := Address.align;
                 Fixed_frame_size          := 4 * Address.size;
                 Guard_page_size           := 4096 * Char.size;
                 All_floats_legal          := TRUE;
                 Has_stack_walker          := FALSE;   (* SPIN  (was TRUE) *)
                 Setjmp                    := "_setjmp";
                 Checks_integer_ops        := FALSE;   (* SPIN  (was TRUE) *)
                 Global_handler_stack      := TRUE;
                 Aligned_procedures        := TRUE;
                 EOL                       := "\n";

    |  3 => (* AP3000 *)
                 max_align                 := 16;
                 Little_endian             := FALSE;
                 PCC_bitfield_type_matters := FALSE;
                 Structure_size_boundary   := 16;
                 Bitfield_can_overlap      := TRUE;
                 First_readable_addr       := 0;
                 Jumpbuf_size              := 83 * Address.size;
                 Jumpbuf_align             := Address.align;
                 Fixed_frame_size          := 4 * Address.size;
                 Guard_page_size           := 0;
                 All_floats_legal          := TRUE;
                 Has_stack_walker          := FALSE;
                 Setjmp                    := "_setjmp";
                 Checks_integer_ops        := FALSE;
                 Global_handler_stack      := TRUE;
                 Aligned_procedures        := TRUE;
                 EOL                       := "\n";

    |  4 => (* ARM *)
                 max_align                 := 32;
                 Little_endian             := FALSE;
                 PCC_bitfield_type_matters := TRUE;
                 Structure_size_boundary   := 32;
                 Bitfield_can_overlap      := FALSE;
                 First_readable_addr       := 0;
                 Jumpbuf_size              := 16 * Address.size;
                 Jumpbuf_align             := Address.align;
                 Fixed_frame_size          := 4 * Address.size;
                 Guard_page_size           := 0;
                 All_floats_legal          := TRUE;
                 Has_stack_walker          := FALSE;
                 Setjmp                    := "_setjmp";
                 Checks_integer_ops        := FALSE;
                 Global_handler_stack      := TRUE;
                 Aligned_procedures        := TRUE;
                 EOL                       := "\n";

    |  5 => (* DS3100 *)
                 max_align                 := 64;
                 Little_endian             := TRUE;
                 PCC_bitfield_type_matters := TRUE;
                 Structure_size_boundary   := 8;
                 Bitfield_can_overlap      := FALSE;
                 First_readable_addr       := 16_400000;
                 Jumpbuf_size              := 84 * Address.size;
                 Jumpbuf_align             := Address.align;
                 Fixed_frame_size          := 4 * Address.size;
                 Guard_page_size           := 4096 * Char.size;
                 All_floats_legal          := TRUE;
                 Has_stack_walker          := TRUE;
                 Setjmp                    := "_setjmp";
                 Checks_integer_ops        := TRUE;
                 Global_handler_stack      := TRUE;
                 Aligned_procedures        := TRUE;
                 EOL                       := "\n";

    |  6, 7 => (* FreeBSD, FreeBSD2 *)
                 max_align                 := 32;
                 Little_endian             := TRUE;
                 PCC_bitfield_type_matters := TRUE;
                 Structure_size_boundary   := 8;
                 Bitfield_can_overlap      := FALSE;
                 First_readable_addr       := 4096 * Char.size;
                 Jumpbuf_size              := 11 * Address.size;
                 Jumpbuf_align             := Address.align;
                 Fixed_frame_size          := 4 * Address.size;
                 Guard_page_size           := 0 * Char.size;
                 All_floats_legal          := TRUE;
                 Has_stack_walker          := FALSE;
                 Setjmp                    := "_setjmp";
                 Checks_integer_ops        := FALSE;
                 Global_handler_stack      := TRUE;
                 Aligned_procedures        := TRUE;
                 EOL                       := "\n";

    |  8 => (* HP300 *)
                 max_align                 := 16;
                 Little_endian             := FALSE;
                 PCC_bitfield_type_matters := FALSE;
                 Structure_size_boundary   := 16;
                 Bitfield_can_overlap      := TRUE;
                 First_readable_addr       := 0;
                 Jumpbuf_size              := 100 * Address.size;
                 Jumpbuf_align             := Address.align;
                 Fixed_frame_size          := 4 * Address.size;
                 Guard_page_size           := 0;
                 All_floats_legal          := TRUE;
                 Has_stack_walker          := FALSE;
                 Setjmp                    := "_setjmp";
                 Checks_integer_ops        := FALSE;
                 Global_handler_stack      := TRUE;
                 Aligned_procedures        := TRUE;
                 EOL                       := "\n";

    |  9 => (* HPPA *)
                 max_align                 := 64;
                 Little_endian             := FALSE;
                 PCC_bitfield_type_matters := TRUE;
                 Structure_size_boundary   := 16;
                 Bitfield_can_overlap      := FALSE;
                 First_readable_addr       := 16_1000;
                 Jumpbuf_size              := 53 * Address.size;
                 Jumpbuf_align             := max_align;
                 Fixed_frame_size          := 8 * Address.size;
                 Guard_page_size           := 0;
                 All_floats_legal          := TRUE;
                 Has_stack_walker          := FALSE;
                 Setjmp                    := "_setjmp";
                 Checks_integer_ops        := FALSE;
                 Global_handler_stack      := TRUE;
                 Aligned_procedures        := FALSE;
                 EOL                       := "\n";

    | 10 => (* IBMR2 *)
                 max_align                 := 32;
                 Little_endian             := FALSE;
                 PCC_bitfield_type_matters := FALSE;
                 Structure_size_boundary   := 32;
                 Bitfield_can_overlap      := FALSE;
                 First_readable_addr       := 0;
                 Jumpbuf_size              := 65 * Address.size;
                 Jumpbuf_align             := Address.align;
                 Fixed_frame_size          := 4 * Address.size;
                 Guard_page_size           := 0;
                 All_floats_legal          := TRUE;
                 Has_stack_walker          := FALSE;
                 Setjmp                    := "_setjmp";
                 Checks_integer_ops        := FALSE;
                 Global_handler_stack      := TRUE;
                 Aligned_procedures        := TRUE;
                 EOL                       := "\n";

    | 11 => (* IBMRT *)
                 max_align                 := 32;
                 Little_endian             := FALSE;
                 PCC_bitfield_type_matters := FALSE;
                 Structure_size_boundary   := 8;
                 Bitfield_can_overlap      := FALSE;
                 First_readable_addr       := 0;
                 Jumpbuf_size              := 17 * Address.size;
                 Jumpbuf_align             := Address.align;
                 Fixed_frame_size          := 4 * Address.size;
                 Guard_page_size           := 0;
                 All_floats_legal          := TRUE;
                 Has_stack_walker          := FALSE;
                 Setjmp                    := "_setjmp";
                 Checks_integer_ops        := FALSE;
                 Global_handler_stack      := TRUE;
                 Aligned_procedures        := TRUE;
                 EOL                       := "\n";

    |  12 => (* IRIX5 *)
                 max_align                 := 64;
                 Little_endian             := FALSE;
                 PCC_bitfield_type_matters := TRUE;
                 Structure_size_boundary   := 8;
                 Bitfield_can_overlap      := FALSE;
                 First_readable_addr       := 16_400000;
                 Jumpbuf_size              := 28 * Address.size;
                 Jumpbuf_align             := Address.align;
                 Fixed_frame_size          := 4 * Address.size;
                 Guard_page_size           := 4096 * Char.size;
                 All_floats_legal          := TRUE;
                 Has_stack_walker          := FALSE;
                 Setjmp                    := "setjmp";
                 Checks_integer_ops        := FALSE;
                 Global_handler_stack      := TRUE;
                 Aligned_procedures        := TRUE;
                 EOL                       := "\n";

    | 13, 14 => (* LINUX, LINUXELF *)
                 max_align                 := 32;
                 Little_endian             := TRUE;
                 PCC_bitfield_type_matters := TRUE;
                 Structure_size_boundary   := 8;
                 Bitfield_can_overlap      := FALSE;
                 First_readable_addr       := 0;
                 Jumpbuf_size              := 8 * Address.size;
                 Jumpbuf_align             := Address.align;
                 Fixed_frame_size          := 4 * Address.size;
                 Guard_page_size           := 0 * Char.size;
                 All_floats_legal          := TRUE;
                 Has_stack_walker          := FALSE;
                 Setjmp                    := "__setjmp";
                 Checks_integer_ops        := FALSE;
                 Global_handler_stack      := TRUE;
                 Aligned_procedures        := TRUE;
                 EOL                       := "\n";

    | 15 => (* IX86_SPIN *)
                 max_align                 := 32;
                 Little_endian             := TRUE;
                 PCC_bitfield_type_matters := TRUE;
                 Structure_size_boundary   := 8;
                 Bitfield_can_overlap      := FALSE;
                 First_readable_addr       := 16_10000;
                 Jumpbuf_size              := 8 * Address.size;
                 Jumpbuf_align             := Address.align;
                 Fixed_frame_size          := 4 * Address.size;
                 Guard_page_size           := 0 * Char.size;
                 All_floats_legal          := TRUE;
                 Has_stack_walker          := FALSE;
                 Setjmp                    := "__setjmp";
                 Checks_integer_ops        := FALSE;
                 Global_handler_stack      := TRUE;
                 Aligned_procedures        := TRUE;
                 EOL                       := "\n";
                 (* SPIN *)
                 Unaligned_memory_reference_ok := TRUE;

    | 16 => (* NEXT *)
                 max_align                 := 16;
                 Little_endian             := FALSE;
                 PCC_bitfield_type_matters := FALSE;
                 Structure_size_boundary   := 16;
                 Bitfield_can_overlap      := FALSE;
                 First_readable_addr       := 0;
                 Jumpbuf_size              := 39 * Address.size;
                 Jumpbuf_align             := Address.align;
                 Fixed_frame_size          := 4 * Address.size;
                 Guard_page_size           := 0;
                 All_floats_legal          := TRUE;
                 Has_stack_walker          := FALSE;
                 Setjmp                    := "_setjmp";
                 Checks_integer_ops        := FALSE;
                 Global_handler_stack      := TRUE;
                 Aligned_procedures        := TRUE;
                 EOL                       := "\n";

    | 17 => (* NT386 *)
                 max_align                 := 32;
                 Little_endian             := TRUE;
                 PCC_bitfield_type_matters := TRUE;
                 Structure_size_boundary   := 8;
                 Bitfield_can_overlap      := FALSE;
                 First_readable_addr       := 4096;
                 Jumpbuf_size              := 8 * Address.size;
                 Jumpbuf_align             := Address.align;
                 Fixed_frame_size          := 0;
                 Guard_page_size           := 0;
                 All_floats_legal          := TRUE;
                 Has_stack_walker          := FALSE;
                 Setjmp                    := "_setjmp";
                 Checks_integer_ops        := FALSE;
                 Global_handler_stack      := FALSE;
                 Aligned_procedures        := TRUE;
                 EOL                       := "\r\n";
                 (* initial experiments indicate that the first 64K of
                    a process's memory on NT are "free" and unreadable.
                    --- WKK  9/9/94 *)

                 CCs := NEW (REF ARRAY OF CallingConvention, 9);
                 NTCall (0, "C",          0); (* __cdecl *)
                 NTCall (1, "WINAPI",     1); (* __stdcall *)
                 NTCall (2, "CALLBACK",   1); (* __stdcall *)
                 NTCall (3, "WINAPIV",    0); (* __cdecl *)
                 NTCall (4, "APIENTRY",   1); (* __stdcall *)
                 NTCall (5, "APIPRIVATE", 1); (* __stdcall *)
                 NTCall (6, "PASCAL",     1); (* __stdcall *)
                 NTCall (7, "__cdecl",    0); (* __cdecl *)
                 NTCall (8, "__stdcall",  1); (* __stdcall *)

    | 18 => (* OKI *)
                 max_align                 := 32;
                 Little_endian             := TRUE;
                 PCC_bitfield_type_matters := TRUE;
                 Structure_size_boundary   := 32;
                 Bitfield_can_overlap      := FALSE;
                 First_readable_addr       := 0;
                 Jumpbuf_size              := 22 * Address.size;
                 Jumpbuf_align             := Address.align;
                 Fixed_frame_size          := 4 * Address.size;
                 Guard_page_size           := 0;
                 All_floats_legal          := TRUE;
                 Has_stack_walker          := FALSE;
                 Setjmp                    := "_setjmp";
                 Checks_integer_ops        := FALSE;
                 Global_handler_stack      := TRUE;
                 Aligned_procedures        := TRUE;
                 EOL                       := "\n";

    | 19 => (* SEQUENT *)
                 max_align                 := 32;
                 Little_endian             := TRUE;
                 PCC_bitfield_type_matters := TRUE;
                 Structure_size_boundary   := 8;
                 Bitfield_can_overlap      := FALSE;
                 First_readable_addr       := 0;
                 Jumpbuf_size              := 84 * Address.size;
                 Jumpbuf_align             := Address.align;
                 Fixed_frame_size          := 4 * Address.size;
                 Guard_page_size           := 4096 * Char.size;
                 All_floats_legal          := TRUE;
                 Has_stack_walker          := FALSE;
                 Setjmp                    := "_setjmp";
                 Checks_integer_ops        := FALSE;
                 Global_handler_stack      := TRUE;
                 Aligned_procedures        := TRUE;
                 EOL                       := "\n";

    | 20, 21 => (* SOLgnu, SOLsun *)
                 max_align                 := 64;
                 Little_endian             := FALSE;
                 PCC_bitfield_type_matters := TRUE;
                 Structure_size_boundary   := 8;
                 Bitfield_can_overlap      := FALSE;
                 First_readable_addr       := 8192;
                 Jumpbuf_size              := 19 * Address.size;
                 Jumpbuf_align             := Address.align;
                 Fixed_frame_size          := 20 * Address.size;
                 Guard_page_size           := 4096 * Char.size;
                 All_floats_legal          := TRUE;
                 Has_stack_walker          := FALSE;
                 Setjmp                    := "setjmp";
                 Checks_integer_ops        := FALSE;
                 Global_handler_stack      := TRUE;
                 Aligned_procedures        := TRUE;
                 EOL                       := "\n";

    | 22 => (* SPARC *)
                 max_align                 := 64;
                 Little_endian             := FALSE;
                 PCC_bitfield_type_matters := TRUE;
                 Structure_size_boundary   := 8;
                 Bitfield_can_overlap      := FALSE;
                 First_readable_addr       := 8192;
                 Jumpbuf_size              := 10 * Address.size;
                 Jumpbuf_align             := Address.align;
                 Fixed_frame_size          := 20 * Address.size;
                 Guard_page_size           := 4096 * Char.size;
                 All_floats_legal          := TRUE;
                 Has_stack_walker          := FALSE;
                 Setjmp                    := "_setjmp";
                 Checks_integer_ops        := FALSE;
                 Global_handler_stack      := TRUE;
                 Aligned_procedures        := TRUE;
                 EOL                       := "\n";

    | 23 => (* SUN3 *)
                 max_align                 := 16;
                 Little_endian             := FALSE;
                 PCC_bitfield_type_matters := FALSE;
                 Structure_size_boundary   := 16;
                 Bitfield_can_overlap      := FALSE;
                 First_readable_addr       := 0;
                 Jumpbuf_size              := 79 * Address.size;
                 Jumpbuf_align             := Address.align;
                 Fixed_frame_size          := 4 * Address.size;
                 Guard_page_size           := 1024 * Char.size;
                 All_floats_legal          := TRUE;
                 Has_stack_walker          := FALSE;
                 Setjmp                    := "_setjmp";
                 Checks_integer_ops        := FALSE;
                 Global_handler_stack      := TRUE;
                 Aligned_procedures        := TRUE;
                 EOL                       := "\n";

    | 24 => (* SUN386 *)
                 max_align                 := 32;
                 Little_endian             := TRUE;
                 PCC_bitfield_type_matters := FALSE;
                 Structure_size_boundary   := 8;
                 Bitfield_can_overlap      := FALSE;
                 First_readable_addr       := 0;
                 Jumpbuf_size              := 8 * Address.size;
                 Jumpbuf_align             := Address.align;
                 Fixed_frame_size          := 4 * Address.size;
                 Guard_page_size           := 0;
                 All_floats_legal          := TRUE;
                 Has_stack_walker          := FALSE;
                 Setjmp                    := "_setjmp";
                 Checks_integer_ops        := FALSE;
                 Global_handler_stack      := TRUE;
                 Aligned_procedures        := TRUE;
                 EOL                       := "\n";

    | 25 => (* UMAX *)
                 max_align                 := 32;
                 Little_endian             := TRUE;
                 PCC_bitfield_type_matters := TRUE;
                 Structure_size_boundary   := 8;
                 Bitfield_can_overlap      := FALSE;
                 First_readable_addr       := 0;
                 Jumpbuf_size              := 10 * Address.size;
                 Jumpbuf_align             := Address.align;
                 Fixed_frame_size          := 4 * Address.size;
                 Guard_page_size           := 0;
                 All_floats_legal          := TRUE;
                 Has_stack_walker          := FALSE;
                 Setjmp                    := "_setjmp";
                 Checks_integer_ops        := FALSE;
                 Global_handler_stack      := TRUE;
                 Aligned_procedures        := TRUE;
                 EOL                       := "\n";

    | 26 => (* VAX *)
                 Real.min.fraction     := -1.70111x+38;
                 Real.max.fraction     := -1.70111x+38;
                 Longreal.min.fraction := -1.70111x+38;
                 Longreal.max.fraction := -1.70111x+38;
                 Extended.min.fraction := -1.70111x+38;
                 Extended.max.fraction := -1.70111x+38;

                 max_align                 := 32;
                 Little_endian             := TRUE;
                 PCC_bitfield_type_matters := TRUE;
                 Structure_size_boundary   := 8;
                 Bitfield_can_overlap      := FALSE;
                 First_readable_addr       := 0;
                 Jumpbuf_size              := 10 * Address.size;
                 Jumpbuf_align             := Address.align;
                 Fixed_frame_size          := 12 * Address.size;
                 Guard_page_size           := 1024 * Char.size;
                 All_floats_legal          := FALSE;
                 Has_stack_walker          := FALSE;
                 Setjmp                    := "_setjmp";
                 Checks_integer_ops        := FALSE;
                 Global_handler_stack      := TRUE;
                 Aligned_procedures        := TRUE;
                 EOL                       := "\n";

    |  27 => (* LINUXELFLanai3.1 *)
      (* XXX VERY EXPERIMENTAL *)
                 max_align                 := 32;               (* XXX no clue *)
                 Little_endian             := TRUE;             (* XXX no clue *)
                 PCC_bitfield_type_matters := TRUE;             (* XXX no clue *)
                 Structure_size_boundary   := 8;                (* XXX no clue *)
                 Bitfield_can_overlap      := FALSE;            (* XXX no clue *)
                 First_readable_addr       := 0;
                 Jumpbuf_size              := 3 * Address.size;
                 Jumpbuf_align             := Address.align; 
                 Fixed_frame_size          := 4 * Address.size; (* XXX no clue *)
                 Guard_page_size           := 0 * Char.size;
                 All_floats_legal          := FALSE;            (* no floating point support! *)
                 Has_stack_walker          := FALSE;
                 Setjmp                    := "_setjmp";
                 Checks_integer_ops        := FALSE;            (* XXX no clue *)
                 Global_handler_stack      := TRUE;             (* XXX no clue *)
                 Aligned_procedures        := TRUE;             (* XXX no clue *)
                 EOL                       := "\n";

    ELSE RETURN FALSE;
    END;

    IF (CCs = NIL) THEN
      CCs := NEW (REF ARRAY OF CallingConvention, 1);
      VAR cc := NEW (CallingConvention);  BEGIN
        CCs[0] := cc;
        cc.name               := "C";
        cc.m3cg_id            := 0;
        cc.args_left_to_right := TRUE;
        cc.results_on_left    := FALSE;
        cc.standard_structs   := TRUE;
      END;
    END;
    DefaultCall := CCs[0];


    <*ASSERT Integer.size MOD ChunkSize = 0 *>
    last_chunk := Integer.size DIV ChunkSize - 1;

    (* fill in the "bytes" and "pack" fields *)
    FixI (Address, max_align);
    FixI (Integer, max_align);
    FixF (Real, max_align);
    FixF (Longreal, max_align);
    FixF (Extended, max_align);
    FixI (Int_A, max_align);
    FixI (Int_B, max_align);
    FixI (Int_C, max_align);
    FixI (Int_D, max_align);
    FixI (Word_A, max_align);
    FixI (Word_B, max_align);
    FixI (Word_C, max_align);
    FixI (Word_D, max_align);
    FixI (Void, max_align);
    FixI (Char, max_align);

    (* sets are always treated as an array of integers *)    
    Set_grain := Integer.size;
    Set_align := Integer.align;

    (* fix the alignments *)
    FOR i := FIRST (Alignments) TO LAST (Alignments) DO
      Alignments[i] := MIN (Alignments[i], max_align);
    END;

    (* initialize the other target-specific modules *)
    TargetMap.Init ();
    M3RT.Init ();

    RETURN TRUE;
  END Init;

PROCEDURE NTCall (x: INTEGER;  nm: TEXT;  id: INTEGER) =
  BEGIN
    CCs[x] := NEW (CallingConvention,
                     name := nm,
                     m3cg_id := id,
                     args_left_to_right := FALSE,
                     results_on_left := TRUE,
                     standard_structs := FALSE);
  END NTCall;

PROCEDURE FixI (VAR i: Int_type;  max_align: INTEGER) =
  BEGIN
    i.align := MIN (i.align, max_align);
    i.bytes := i.size DIV Byte;
    i.pack  := (i.size + i.align - 1) DIV i.align * i.align;
  END FixI;

PROCEDURE FixF (VAR f: Float_type;  max_align: INTEGER) =
  BEGIN
    f.align := MIN (f.align, max_align);
    f.bytes := f.size DIV Byte;
    (* f.pack  := (f.size + f.align - 1) DIV f.align * f.align; *)
  END FixF;

PROCEDURE FindConvention (nm: TEXT): CallingConvention =
  VAR cc: CallingConvention;
  BEGIN
    FOR i := 0 TO LAST (CCs^) DO
      cc := CCs[i];
      IF (cc # NIL) AND Text.Equal (nm, cc.name) THEN RETURN cc; END;
    END;
    RETURN NIL;
  END FindConvention;

PROCEDURE ConventionFromID (id: INTEGER): CallingConvention =
  VAR cc: CallingConvention;
  BEGIN
    FOR i := 0 TO LAST (CCs^) DO
      cc := CCs[i];
      IF (cc # NIL) AND (cc.m3cg_id = id) THEN RETURN cc; END;
    END;
    RETURN NIL;
  END ConventionFromID;

BEGIN
END Target.

