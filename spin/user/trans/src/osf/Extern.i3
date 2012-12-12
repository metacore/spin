(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 24-Aug-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created
 *)

(* C interface to transaction functions *)
INTERFACE Extern;

<*EXTERNAL*>PROCEDURE bench_start(argc, argv: INTEGER);
(* RVM SOSP bench *)

<*EXTERNAL*>PROCEDURE rds_test_main(argc, argv: INTEGER);
(* Simple RDS sanity tester. *)
  
<*EXTERNAL*>PROCEDURE oo7_main(argc, argv: INTEGER);
(* OO7 *)

<*EXTERNAL*>PROCEDURE micro_start(argc, argv: INTEGER);
(* Microbench *)
  
<*EXTERNAL*>PROCEDURE perror(str: ADDRESS);
  
<*EXTERNAL*>PROCEDURE cancel_memory_limits();
  (* setrlimit to the maximum: we do this in C because M3 setrlimit is broken. *)
<*EXTERNAL*>PROCEDURE get_rpcc(): INTEGER;
<*EXTERNAL*>PROCEDURE get_mhz(): INTEGER;  
  
END Extern.
