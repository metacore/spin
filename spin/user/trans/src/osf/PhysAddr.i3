(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 30-Aug-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *	
 *)
INTERFACE PhysAddr;
IMPORT CPU;
TYPE
  T <: REFANY;
  State = {Active, Reclaimed, Zombie, Dead};
  Tag = RECORD
    obj: REFANY;
    off: INTEGER;
  END;
  Content = ARRAY [0 .. CPU.PAGESIZE-1] OF CHAR;

VAR MaxPages: CARDINAL;
  
PROCEDURE Allocate(VAR tag : Tag): T;
PROCEDURE Deallocate(t: T);
PROCEDURE ChangeState(t: T; state: State);
PROCEDURE GetTag(t: T): Tag;
PROCEDURE GetState(t: T): State;
PROCEDURE Access(t: T; callback: PROCEDURE (VAR buf: Content));
PROCEDURE Copy(dest, src: T);
PROCEDURE Read (t: T; off: CARDINAL; VAR buf: ARRAY OF CHAR);
PROCEDURE Write (t: T; off: CARDINAL; READONLY buf: ARRAY OF CHAR);
PROCEDURE GetVictims (t: REFANY; VAR frames: ARRAY OF T): CARDINAL;
  
END PhysAddr.
