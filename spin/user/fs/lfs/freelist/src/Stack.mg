
(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 30-May-96  Tim Bradley (tbradley) at the University of Washington
 *	Whisted.
 *
 *)
GENERIC MODULE Stack(Elem);

	REVEAL
		T = BRANDED OBJECT n: INTEGER; a: REF ARRAY OF Elem.T END;
	
	PROCEDURE Create(): T =
		BEGIN RETURN NEW(T, n:= 0, a := NIL) END Create;
	
	PROCEDURE Push(VAR s: T; x: Elem.T) = 
	BEGIN
		IF s.a = NIL THEN
			s.a := NEW(REF ARRAY OF Elem.T, 5)
		ELSIF s.n > LAST(s.a^) THEN
			WITH temp = NEW(REF ARRAY OF Elem.T, 2 * NUMBER(s.a^)) DO
				FOR i := 0 TO LAST(s.a^) DO temp[i] := s.a[i] END;
				s.a := temp
			END
		END;
		s.a[s.n] := x;
		INC(s.n)
	END Push;

	PROCEDURE Pop(VAR s: T) : Elem.T = 
		BEGIN DEC(s.n); RETURN s.a[s.n] END Pop;
BEGIN END Stack.
