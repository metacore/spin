(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 09-Sep-97  Yasushi Saito (yasushi) at the University of Washington
 *	Cleaned up.
 * 28-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Expanded signatures to Alpha specific memory management traps,
 *      alignment traps, instruction traps, and mcheck.
 *
 * 27-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Take extra arguments for Protection,Unaligned, and IllegalInst faults
 *	(so we know what happened)
 *
 * 15-Apr-96  Emin Gun Sirer (egs) at the University of Washington
 *	Moved preemption related code into Sched.
 *
 * 10-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *      Added extra argument to IOinterrupt for sal IO vector.
 *
 * 21-Sep-95  Stefan Savage (savage) at the University of Washington
 *	Added machine check support, nuked Protection
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Internal trap events.
 *) 
INTERFACE TrapPrivate;
IMPORT CPU;
CONST
  MMRefTypeExecute	= -1;
  MMRefTypeRead		= 0;
  MMRefTypeWrite	= 1;

  MMCsrTNV		= 0; (* Translation not valid *)
  MMCsrACV		= 1; (* Access violation *)
  MMCsrFOR		= 2; (* Fault on read *)
  MMCsrFOE		= 3; (* Fault on execute *)
  MMCsrFOW		= 4; (* Fault on write *)

CONST
  IFTypeBpt		= 0; (* Breakpoint *)
  IFTypeBugCheck	= 1; (* Bugcheck trap, pal call *)
  IFTypeGenTrap		= 2; (* Gentrap, pal call *)
  IFTypeFEN		= 3; (* FP Enable (eg issue fp inst with FEN = 0 *)
  IFTypeOpDec		= 4; (* Priviledged or illegal instruction *)

CONST
  ArithSWC	= 16_1;  (* Software completion *)
  ArithINV	= 16_2;  (* Invalid operation *)
  ArithDZE	= 16_4;  (* Division by zero *)
  ArithOVF	= 16_8;  (* Overflow *)
  ArithUNF	= 16_10; (* Underflow *)
  ArithINE	= 16_20; (* Ineact result *)
  ArithIOV	= 16_40; (* Integer overflow *)
  
PROCEDURE Init(verbose : BOOLEAN);
  PROCEDURE SyscallWrap(VAR ss: CPU.SavedState);

END TrapPrivate.
