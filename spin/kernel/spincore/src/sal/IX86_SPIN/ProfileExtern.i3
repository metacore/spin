(* HISTORY
 * 05-May-96  Charles Garrett (garrett) at the University of Washington
 *	Created. These variables are defined in machine/alpha/Profile.s.
 *
 *)

UNSAFE INTERFACE ProfileExtern;

IMPORT Word;

TYPE
  (* Values of the spin_prof_flag. Overflow means that we counted
     more arcs than we have room for. *)
  Flag = {ProfOn, ProfOff, ProfOverflow1, ProfOverflow2};

CONST
  hashLen = 65536;

<*EXTERNAL profile_flag*> VAR flag: Flag;
<*EXTERNAL profile_count*> VAR count: INTEGER;
<*EXTERNAL profile_caller*> VAR caller: Word.T;
<*EXTERNAL profile_caller_len*> VAR callerLen: Word.T;
<*EXTERNAL profile_arcs*> VAR arcs: Word.T;
<*EXTERNAL profile_arcs_len*> VAR arcsLen: Word.T;
<*EXTERNAL profile_hash*> VAR hash: Word.T;
<*EXTERNAL profile_call_graph*> VAR callGraph: BOOLEAN;

END ProfileExtern.
