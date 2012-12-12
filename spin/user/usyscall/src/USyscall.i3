(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * Interface Usyscall
 *
 * This file defines the minimal services that are available to user spaces.
 *
 * HISTORY
 * 19-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	Comments reorganized for m3tohtml.
 * 15-May-96  Brian Bershad (bershad) at the University of Washington
 *	Created.
 *
 * html
 *)

(*
   USyscall, <dfn>the universal system call extension</dfn>, provides
   primitive services that all user space programs might use.

   The usyscall services can be grouped into the below 6 categories.

   <ol>
   <li> Identity services, like getting the
        [memory:space] and thread ID, and destroying them.
   <li> [domain:Domain] services;
         creating and manipulating a domain.
   <li> Rendezvous service; bootstrap other systemcall extension by yourself.
   <li> Others.
   
   </ol>

*)

INTERFACE USyscall;

IMPORT Strand, AddressSpace, Word, Domain;
<*INTERFACE_PROC_BASE -1 *>
<*DESCENDING_PROC_IDS*>
<*NO_SMALL_VAR_OPTIMIZATION*>

TYPE ErrorCode = INTEGER;

CONST Success = 0;
      Failure = -1;

(*
  <h2>Identity services</h2>
 *)
  
PROCEDURE SpaceSelf(): AddressSpace.T;
(* Get the handle to the address space the caller is running on. *)
  
PROCEDURE SpaceDestroy(s: AddressSpace.T): ErrorCode;
(* Destroy the space "s". If the "s" is the caller's, then the caller will
   be terminated as well. *)
  
PROCEDURE StrandSelf(): Strand.T;
(* Get the [strand] of the caller. *)
  
PROCEDURE StrandDestroy(s: Strand.T): ErrorCode;
(* Destroy the strand "s". *)
  
PROCEDURE Close(xref: Word.T): ErrorCode;
  (* Erase the entry "xref" from the external ref table. *)
(*
  <h2>Domain system calls</h2>
 *)

PROCEDURE DomainLookup(<*CTEXT*>name: TEXT;): Domain.T;
(* Return a capability for the named domain.  NIL if name not found *)

PROCEDURE DomainCreate(<*CTEXT*>name: TEXT): Domain.T;
(* 
   Create a new domain with the given name.  
   Return NIL if domain not found. "DomainLookup(name)" will return NIL.
*)


PROCEDURE DomainRegister(<*CTEXT*>name: TEXT; d: Domain.T): ErrorCode;
(* Register "d" with "name". Success or fail only. *)

PROCEDURE DomainLoad (d    : Domain.T;  
		      object: Word.T;
                      size : INTEGER                                      ):
  ErrorCode;
(* Load domain "d" with the bytes representing the "object" file at the
   specified address. 
*)

PROCEDURE DomainLink( domain, extern: Domain.T): ErrorCode;
(* Link "domain" against "extern". Return Failure if any problems. *)


PROCEDURE DomainInitialize(domain: Domain.T): ErrorCode;
  (* Execute startup routines in the named, linked, domain. *)
  
PROCEDURE DomainDestroy(domain: Domain.T): ErrorCode;
(* Eliminate the named domain from existence. *)


(*
  <h2>Rendezvous</h2>
 *)
PROCEDURE Rendezvous(<*CTEXT*>simpleName: TEXT; 
		     key: REFANY;  (* Protect the rendezvous, NIL if any *)
		     VAR reply: REFANY): ErrorCode;
(*
   This service is used to autoload a systemcall extension.
   "simpleName" specifies the name of the extension, which
   is typically a "Brand" string defined in the extension.
   (ex. "Transaction" in case of Transaction extension).<br>

   I'm not sure about the use of "key" and "reply".
   At least it's not used currently. Ask Bershad about them.

   This service actually does <strong>nothing</strong>.
   Systemcall extensions should install its own handler to
   "USyscall.Rendezvous". The handler typically
   installs the system call handler for the caller.

   <p>
   Sounds confusing? Here's how a [Transaction] application starts up.

   <ol>
   <li> Sphinx user app starts up. At this point, USyscall is
          loaded and installed, but transaction is not loaded. 
   <li> The user app calls <code>USyscall_DomainLookup("Transaction")</code>
        This in turn calls the [ns:name server], and since
      trans isn't loaded, [nanny] is called to load it.
   <li> Transaction is loaded. Transaction <strong>does not</strong> installs
   the system call handler at startup. However, it installs
   a handler for the "USyscall.Rendezvous" event.
   <li> Control goes back to the user app, and the user app then
   calls "USyscall_Rendezvous(Transaction)".
   <li> The handler "Transaction.Rendezvous" is called in response to
   "USyscall.Rendezvous" event.
   This procedure installs a handler for the
   "Trap.Syscall" event. As a
   [auth:authorizer key], the triple

   <center>&\lt; user space thread id, min proc ID, max proc ID &\gt;</center>

   is passed to the [dispatcher].
   <li> Dispatcher installs the systemcall handler along with
   an imposed guard that checks the
   systemcall number range and thread
   identity upon each systemcall event.

   </ol>
   
   *)
  
(*
   <h2> Misc services.</h2>
*)
PROCEDURE Null();
(* The null procedure call. Get in. Get out. *)

PROCEDURE Putc(c: CHAR);
(* Put a character "c" on the console screen. *)
  
PROCEDURE Putx(x: Word.T);
(* Put an word "x" in hex on the console screen. *)

PROCEDURE System(<*CTEXT*>command: TEXT): ErrorCode;
(* Run "command" as the SPIN [shell] command. *)
  
PROCEDURE Profile(on: INTEGER);
(* If "on > 1", kernel profile is turned on, keeping old results.
   If "on = 1", the old results are erased, and kernel profile is turned on.
   Otherwise, profiling is turned off. *)

TYPE SpyInfo = RECORD mhz, min, max, hits, total: INTEGER; END;
    
PROCEDURE GetSpyInfo(<*CTEXT*>name: TEXT;
		     VAR info: SpyInfo;
		     sampleAddr: Word.T;
		     VAR nsamples: Word.T): ErrorCode;
END USyscall.
