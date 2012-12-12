(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 07-Aug-95  Stefan Savage (savage) at the University of Washington
 *	Created from old ThreadShell.m3 for SOSP paper
 *
 *)
INTERFACE PageFaultTest;

PROCEDURE FaultTest();

PROCEDURE SFaultTest();

PROCEDURE GetPTE();
PROCEDURE Prot1();
PROCEDURE Prot100();
PROCEDURE Unprot100();
PROCEDURE TrapTest();
PROCEDURE TrapClosure();
PROCEDURE Appel1();
PROCEDURE Appel2();

END PageFaultTest.









 
