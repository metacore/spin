(* 
 * Copyright 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * SID Stack
 *   Enqueue and dequeue elements.
 *   Unsafe since it simply provides an alternative view on
 *   the atomic enqueue and dequeue operations defined in
 *   kernel/spincore/src/machine/[Platform]/AtomicOps.s.
 *
 * HISTORY
 *
 * 01-Aug-97  Robert Grimm (rgrimm) at the University of Washington
 *      Created.
 *
 *)

UNSAFE INTERFACE SIDStack;

IMPORT SecurityContext;

<*EXTERNAL*>
PROCEDURE Dequeue(VAR list : SecurityContext.SIDStackT)
  : SecurityContext.SIDStackT;

<*EXTERNAL*>
PROCEDURE Enqueue(     elem : SecurityContext.SIDStackT;
                   VAR list : SecurityContext.SIDStackT );

END SIDStack.
