(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 25-Sep-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed to use new Nameserver.  Removed the use of a linked list
 *	of init procedures for the encap domains.
 *
 *)

(* XXX out of date comment (mef) *)
(* BootEncap module

   Also the Encap main bodies cannot call Domain.Initialize since
   RTLinker.RunMainBodies is already active initializing the Encaps.
   So we make a list of boot domains that are initialized by the
   BootEncapThread. *)

MODULE BootEncap;
IMPORT Domain, NameServer, Fmt, IO, ThreadExtra;

PROCEDURE Lookup (domainname:TEXT; VAR d: Domain.T): BOOLEAN =
  BEGIN
    TRY
      d := NameServer.Lookup(NIL, "/../svc/domains/" & domainname);
      RETURN TRUE;
    EXCEPT
    | NameServer.Error(ec)  =>
      IF ec # NameServer.EC.NameNotFound THEN
	IO.PutError("Nameserver lookup FAILED " & domainname &
		    " (" & Fmt.Int(ORD(ec)) & ")\n");
      END;
    END;
    d := NIL;
    RETURN FALSE;
  END Lookup;

PROCEDURE BootEncapThread (<*UNUSED*> arg: ThreadExtra.ArgT): ThreadExtra.ResultT =
  BEGIN
    (* XXX should pass boot flag option for verbosity. *)
    Init(TRUE); (* implemented by generated start/$target/BootEncapInit module *)
    RETURN NIL;
  END BootEncapThread;

BEGIN
  EVAL ThreadExtra.PFork(BootEncapThread, NIL);
END BootEncap.
