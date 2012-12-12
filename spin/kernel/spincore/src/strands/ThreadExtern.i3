(* HISTORY 
 * 10-Dec-96  Charles Garrett (garrett) at the University of Washington
 *	Add a variable which holds the current thread's profiling stack.
 *
*)

UNSAFE INTERFACE ThreadExtern;

IMPORT ThreadPrivate;

<*EXTERNAL aux_stack*> VAR curProfileStack : ThreadPrivate.ProfileData;

END ThreadExtern.
