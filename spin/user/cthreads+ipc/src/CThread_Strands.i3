(*
 * HISTORY
 * 16-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *	CThreads, native implementation.
 *)
INTERFACE CThread_Strands;
IMPORT UserSpaceThread, Machine, Strand, Space, Word;
IMPORT SMutex, CPUState, FPUState, Condition;

TYPE T = Strand.T BRANDED OBJECT
  uth: UserSpaceThread.T;
  space: Space.T;
  lock: SMutex.T;
  cpustate: CPUState.T;
  fpustate: FPUState.T;
  fpuused : BOOLEAN := FALSE;
  done: Condition.T;
  returned: BOOLEAN := FALSE;
  result: Word.T;
  ctnext: T;
END;

PROCEDURE Fork(th: UserSpaceThread.T; startpc,arg,gp,ra,startsp: Word.T) : T;

PROCEDURE Exit(cthread: T; exitcode: Word.T);

PROCEDURE Join(cthread: T) : Word.T;

END CThread_Strands.
