(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Cthreads for users, implemented on top of strand events.
 *) 
INTERFACE CThreadNative;
IMPORT Strand, UserSpaceThread, Word;
IMPORT CPU;

TYPE Public = Strand.T OBJECT
  uth: UserSpaceThread.T;
  tmpTrapFrame: UNTRACED REF CPU.GeneralRegs;
END;
TYPE T <: Public;

PROCEDURE Fork(th: UserSpaceThread.T; startpc,arg,gp,ra,startsp: Word.T) : T;

PROCEDURE Exit(cthread: T; exitcode: Word.T);

PROCEDURE Join(cthread: T) : Word.T;

PROCEDURE RegisterRas(cthread: T; beginpc, len: Word.T);

PROCEDURE RegisterCas(cthread: T; beginpc, endpc, rollforwardlength: Word.T);

END CThreadNative.
