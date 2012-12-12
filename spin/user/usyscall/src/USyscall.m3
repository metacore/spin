(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * Module Usyscall
 *
 * This file defines the minimal services that are available to user spaces.
 *
 * HISTORY
 * 17-May-97  Yasushi Saito (yasushi) at the University of Washington
 *	Added GetSpyInfo
 * 11-Feb-97  Robert Grimm (rgrimm) at the University of Washington
 *      Removed Identity
 *
 * 18-Jul-96  Brian Bershad (bershad) at the University of Washington
 *	New shell interface.
 *
 * 18-Jun-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed the dispatcher interface.
 *
 * 15-May-96  Brian Bershad (bershad) at the University of Washington
 *	Created.
 *
 *
 *)

MODULE USyscall;

IMPORT USyscallInterface, USyscallExtension, NameServer;
IMPORT UserSpaceThread, Space, AddressSpace, Strand, Text, Fmt, Shell, IO;
IMPORT Auth, Word, Debugger;
IMPORT Dispatcher, Domain;
IMPORT Trap;
IMPORT VMError;
IMPORT ProfileSupport;
IMPORT Translation;
IMPORT CPU;
IMPORT Spy;

PROCEDURE Null() =
  BEGIN
  END Null;

PROCEDURE SpaceSelf (): Space.T =
  BEGIN
    RETURN Translation.GetCurrent();
  END SpaceSelf;
  
PROCEDURE SpaceDestroy(s: Space.T) : ErrorCode =
  VAR me: BOOLEAN;
 BEGIN
   me := Translation.GetCurrent() = s;
   s.destroy();
   IF me THEN
     RETURN StrandDestroy(StrandSelf());
     (* we should never return from Destroy unless there's a failure *)
   END;

   RETURN Success;
 END SpaceDestroy;

(*
 * Return the USER space strand
 *)

PROCEDURE StrandSelf(): Strand.T =
  BEGIN
    RETURN UserSpaceThread.Self();
  END StrandSelf;


PROCEDURE StrandDestroy(<*UNUSED*>s: Strand.T) : ErrorCode =
  BEGIN
    Strand.Block(StrandSelf());
    RETURN Success;	(* Unreached *)
  END StrandDestroy;

PROCEDURE Close(xref : Word.T) : ErrorCode =
  VAR
    space := NARROW(Translation.GetCurrent(), AddressSpace.T);
  BEGIN
    space.getXrefTbl().delete(xref);
    RETURN Success;
  END Close;
  
PROCEDURE Putc(c: CHAR) =
  BEGIN
    IO.Put(Text.FromChar(c));
  END Putc;

PROCEDURE Putx(x: Word.T) =
  BEGIN
    IO.Put(Fmt.Unsigned(x));
  END Putx;


(* Should return the result in a text *)
PROCEDURE System (command: TEXT): ErrorCode =
  BEGIN
    IF Shell.OneShellCommand(command) THEN
      RETURN Success
    ELSE
      RETURN Failure;
    END;
  END System;




(******************************************************************)
(* Domain management *)
(******************************************************************)

VAR authAlways:= NEW(Auth.AuthAlways);
    loadedDomainIndex: CARDINAL := 1;	(* BEWARE WRAPAROUND! *)
    mu := NEW(MUTEX);

PROCEDURE NextDomain (): CARDINAL =
  VAR res: CARDINAL;
  BEGIN
    LOCK mu DO
      res := loadedDomainIndex;
      IF res = LAST(CARDINAL) THEN
        IO.Put("AXE AXE USyscall has run out of domain numbers\n");
        Debugger.Enter();
      END;
      INC(loadedDomainIndex);
      RETURN res;
    END;
  END NextDomain;
      

PROCEDURE DomainLookup(name : TEXT): Domain.T =
  VAR
    obj       : REFANY;
  BEGIN
    TRY
      obj := NameServer.Lookup(NIL, "/../svc/domains/" & name);
      RETURN NARROW(obj, Domain.T);
    EXCEPT
    | NameServer.Error(ec)  =>
      IF ec # NameServer.EC.NameNotFound THEN
	  IO.PutError("Nameserver Lookup FAILED " & name &
	  	" (" & Fmt.Int(ORD(ec)) & ")\n");
      END;
      RETURN NIL;
    END;
  END DomainLookup;


PROCEDURE DomainCreate(name : TEXT): Domain.T =
  VAR n1: TEXT;
  BEGIN
    n1 := name;
    IF name = NIL THEN
      n1 := "usyscall.anon." & Fmt.Int(NextDomain());
      IO.Put("USyscall.DomainCreate making anon domain " & n1 & "\n");
    END;
    RETURN Domain.Create(n1,NIL);
  END DomainCreate;



PROCEDURE DomainRegister(name : TEXT; d: Domain.T): ErrorCode =
  VAR
    domains   : NameServer.T;
  BEGIN
    TRY
      domains := NameServer.Lookup(NIL, "/../svc/domains");
      NameServer.Attach(domains, name, d);
      RETURN Success;
    EXCEPT
    | NameServer.Error(ec)  =>
	IO.PutError("Nameserver FAILED " & name &
	  	" (" & Fmt.Int(ORD(ec)) & ")\n");
      RETURN Failure;
    END;
  END DomainRegister;


PROCEDURE DomainLoad (d     : Domain.T;  
                      object: Word.T;
                      size  : INTEGER  ):  ErrorCode =
  VAR
    buf := NEW(REF ARRAY OF CHAR, size);
  BEGIN
    TRY
      Translation.Read(Translation.GetCurrent(), object, buf^);
      Domain.Add(d,Domain.Create("Usyscall." & Fmt.Int(NextDomain()), 
		 buf));
      RETURN Success;
    EXCEPT
    | VMError.E =>
      IO.Put("DomainLoad Space exception.\n");
      RETURN Failure;
    END;
  END DomainLoad;


PROCEDURE DomainLink (domain, extern: Domain.T): ErrorCode =
  BEGIN
    Domain.Resolve(domain, extern);
    IF Domain.FullyResolved(domain) THEN
      RETURN Success
    ELSE
      RETURN Failure;
    END;
  END DomainLink;



PROCEDURE DomainInitialize( domain: Domain.T): ErrorCode =
  BEGIN
   IF Domain.Initialize(domain) THEN RETURN Success ELSE RETURN Failure; END;
  END DomainInitialize;

  
    

PROCEDURE DomainDestroy (domain: Domain.T): ErrorCode =
  BEGIN
    IF Domain.Destroy(domain) THEN
      RETURN Success;
    ELSE
      RETURN Failure;
    END;
  END DomainDestroy;



(* The current brokennes of the dispatcher makes the following code
wrong. We only want to run this code in the event that there is no
handler for a particular rendezvous point. *)

PROCEDURE Rendezvous(<*UNUSED*>simpleName: TEXT; 
		     <*UNUSED*>key: REFANY; 
		     VAR reply: REFANY): ErrorCode =
  BEGIN
    reply := NIL;
    RETURN Failure;	(* BOGUS *)
  END Rendezvous;

PROCEDURE Profile (on : INTEGER) =
  BEGIN
    IF on > 1 THEN
      EVAL ProfileSupport.On(TRUE);
    ELSIF on = 1 THEN
      EVAL ProfileSupport.On(FALSE);
    ELSE
      EVAL ProfileSupport.Off();
    END;
  END Profile;

PROCEDURE GetSpyInfo (name: TEXT;
		      VAR info: SpyInfo;
		      sampleAddr: Word.T;
		      VAR nsamples: Word.T): ErrorCode =
  VAR
    spy: Spy.T;
    sinfo: Spy.Info;
  BEGIN
    info.mhz := CPU.Hertz() DIV 1000000;
    spy := Spy.Lookup(name);
    IF spy = NIL THEN RETURN Failure; END;
    Spy.GetInfo(spy, sinfo);
    info.hits := sinfo.hits;
    info.total := sinfo.total;
    info.max := sinfo.min;
    IF sinfo.samples # NIL THEN
      nsamples := MIN(nsamples, NUMBER(sinfo.samples^));
      Translation.Write(Translation.GetCurrent(),
			VIEW(sinfo.samples^, ARRAY OF CHAR),
			sampleAddr);
    ELSE
      nsamples := 0;
    END;
    RETURN Success;
  END GetSpyInfo;

BEGIN
  TRY
    EVAL Dispatcher.InstallHandler(Trap.Syscall, NIL, 
				   USyscallExtension.Syscall, 
		   key := NEW(Trap.AuthKey,
			      minProcID:=USyscallExtension.MinProcID,
			      maxProcID:=USyscallExtension.MaxProcID));
    EVAL USyscallInterface.Export(authAlways);
    IO.Put("USyscall handler ready.\n"); 
  EXCEPT
  | Dispatcher.Error =>
    IO.PutError("usyscall: dispatcher installation problem.\n");
  END;
END USyscall.



  
  

