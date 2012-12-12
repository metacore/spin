(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *
 * 03-Sep-97  Robert Grimm (rgrimm) at the University of Washington
 *      Created.
 *
 *)

MODULE PreemptionTest;

IMPORT Fmt, RTIO, Thread, ThreadExtra;

CONST
  DEBUG   : BOOLEAN = FALSE;     (* Dump debugging information. *)
  IDLE    : INTEGER = 10000000; (* Length of idle loop.        *)

(*
    Ping-pong
   -----------
*)

TYPE
  PingPongT = MUTEX OBJECT
    error : BOOLEAN := FALSE;
    value : INTEGER := 0;
  END;

PROCEDURE PingSetup() : BOOLEAN =
  VAR
    x : PingPongT;
    t : Thread.T;
  BEGIN
    x := NEW( PingPongT );
    t := ThreadExtra.PFork( PongSetup, x );
    Ping( x, 0 );
    EVAL Thread.Join( t );
    RETURN NOT x.error;
  END PingSetup;

PROCEDURE PongSetup( x : REFANY ) : REFANY =
  BEGIN
    Pong( NARROW( x, PingPongT ), 1 );
    RETURN NIL;
  END PongSetup;

PROCEDURE Ping( x : PingPongT; i : INTEGER ) =
  VAR
    goDown : BOOLEAN;
  BEGIN
    LOOP
      LOCK x DO
        IF x.error THEN RETURN; END;
        IF x.value = i THEN
          INC( x.value );
          IF DEBUG THEN
            RTIO.PutText(" Ping i " & Fmt.Int(i) & "  new value "
                         & Fmt.Int(x.value) & "\n");
          END;
          EXIT;
        END;
      END;
      (* Idle loop to make progress likely *)
      VAR
        x : INTEGER := 0;
      BEGIN
        FOR i := 0 TO IDLE DO
          INC(x);
        END;            
      END;
    END;
    LOCK x DO
      goDown := x.value < 7;
    END;
    IF goDown THEN
      Ping( x, i + 2 );
    END;
  END Ping;

PROCEDURE Pong( x : PingPongT; i : INTEGER ) =
  VAR
    goDown : BOOLEAN;
  BEGIN
    LOOP
      LOCK x DO
        IF x.error THEN RETURN; END;
        IF x.value = i THEN
          IF DEBUG THEN
            RTIO.PutText(" Pong i " & Fmt.Int(i) & "  new value "
                         & Fmt.Int(x.value) & "\n");
          END;
          INC( x.value );
          EXIT;
        END;
      END;
      (* Idle loop to make progress likely *)
      VAR
        x : INTEGER := 0;
      BEGIN
        FOR i := 0 TO IDLE DO
          INC(x);
        END;            
      END;
    END;
    LOCK x DO
      goDown := x.value < 8;
    END;
    IF goDown THEN
      Pong( x, i + 2 );
    END;
    RETURN;
  END Pong;

(*
    Initialization and clean-up
  ------------------------------
*)

PROCEDURE Start(<*UNUSED*>i: INTEGER): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END Start;

PROCEDURE End(): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END End;

(*
    The actual regression test
   ----------------------------
*)

PROCEDURE T1() : BOOLEAN =
  BEGIN
    IF NOT PingSetup() THEN RETURN FALSE; END;
    RETURN TRUE;
  END T1;

BEGIN
END PreemptionTest.
