/* Copyright (C) 1990, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */
/* Last modified on Wed Mar  9 09:12:47 PST 1994 by kalsow     */
/*      modified on Tue May 18 13:21:11 PDT 1993 by muller     */
/*      modified on Tue Jan 19 15:20:48 PST 1993 by burrows    */

/* This file implements the stack walking functions of
   the RTExRep interface. */

#include <excpt.h>
#include <sym.h>

/* apparently, OSF/1 v1.3 doesn't define sc_sp... */
#ifndef sc_sp
#define sc_sp sc_regs[30]
#endif

extern char _procedure_table_size[];
extern struct runtime_pdr _procedure_table [];
#define PSIZE ((int) _procedure_table_size)
extern char _procedure_string_table[];

/* TYPE Frame = RECORD pc, sp: ADDRESS;  cxt: Usignal.struct_sigcontext END; */
typedef struct {
  unsigned long pc;
  unsigned long sp;
  struct sigcontext cxt;
  long lock;
} Frame;

#define FrameLock 0x1234567890

typedef struct runtime_pdr *Proc;

/*---------------------------------------------------------------------------*/
/* RTException__Find searches the runtime table for the entry corresponding
   to the given pc.  */

Proc RTException__Find (unsigned long pc)
{
  int n = PSIZE;
  int lo = 0;
  int hi = n;
  int mid;

  while (lo < hi) {
    mid = (lo+hi) / 2;
    if (pc < _procedure_table[mid].adr) {
      hi = mid;
    } else {
      lo = mid+1;
    }
  }
  if (lo > 0) {
    lo--;
  }
  return (&_procedure_table[lo]);
}


/*---------------------------------------------------------------------------*/
/* PROCEDURE ProcName (READONLY f: Frame): ADDRESS;
   Return the null-terminated constant string that names the procedure
   corresponding to the stack frame "f".  Returns NIL if no name is
   known. */

char* RTException__ProcName (Frame *f)
{
  Proc p = RTException__Find (f->pc);
  if (!p) return 0;
  return (&_procedure_string_table[p->irpss]);
}

/*---------------------------------------------------------------------------*/
/* PROCEDURE CurrentFrame (VAR(*OUT*) f: Frame)
 * returns the frame that corresponds to its caller. */

void RTException__CurFrame (Frame *f)
{
  f->lock = FrameLock;
  setjmp (& f->cxt);
  __exc_virtual_unwind (0, &(f->cxt));
  f->pc = f->cxt.sc_pc;
  f->sp = f->cxt.sc_sp;
  if (f->lock != FrameLock) abort ();
}

/*---------------------------------------------------------------------------*/
/* PROCEDURE PreviousFrame (READONLY callee: Frame;  VAR(*OUT*)caller: Frame)
   Return the stack frame that called "callee".  Returns with pc = NIL if
   "callee" is the first frame on the stack or its predecessor is ill-formed.
   */

RTException__PrevFrame (Frame* callee, Frame* caller)
{
  PRUNTIME_FUNCTION proc;

  if (callee->lock != FrameLock) abort ();
  *caller = *callee;

  /* see if the unwind has any chance of working... */
  proc = (PRUNTIME_FUNCTION) __exc_lookup_function_entry (caller->cxt.sc_pc);
  if (proc == 0) {
    caller->pc = 0;
    caller->sp = 0;
    return;
  }

  __exc_virtual_unwind (proc, &(caller->cxt));
  caller->pc = caller->cxt.sc_pc;
  caller->sp = caller->cxt.sc_sp;

  if (caller->lock != FrameLock) abort ();
}


/*---------------------------------------------------------------------------*/
/* PROCEDURE Unwind (READONLY f: Frame);
   Restore the machine state back to the frame "f".  All callee-saved
   registers must be restored to the state they were in when frame "f"
   made its last call. */

void RTException__Unwind (Frame *target)
{
  if (target->lock != FrameLock) abort ();
  longjmp (& target->cxt, 1);    /* do a full longjmp to destination */
}

