(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 01-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 * html
 *)

(* 
   "TransGroup.T" is an abstraction of a <dfn>client</dfn>.
   A client is a (sphinx) process when
   that process is using the transaction service locally, or it is a
   remote host when the service is used via network connection.
   
   Its primary purpose is to concentrate the management of all client states
   into single place to make recovery from client failure easy.
   Client state includes open storages and outstanding transactions.
   By calling "ShutDown", all storages are closed and transactions are
   aborted.
   
*)

INTERFACE TransGroup;

CONST MaxGroup = 128;
(* Max # of clients allowed at a time. *)
  
TYPE
  (* Local host id will be used to implement a callback based buffer
     consistency protocol, ala AFS. It is not used now. *)
  ID = [0 .. MaxGroup-1]; (* Local host id. This is small enough to fit in
			    the range 0 .. HostSet.MaxHosts. *)
  T <: TPublic;
  TPublic = OBJECT
  METHODS
    init(): T;
    (* Create an group *)
    
    shutDown();
    (* Abort all the ongoing transactions that originated from
       the group "t", and also close all the
       storages opened in the name of "t". *)
  END;

VAR default: T;
  
END TransGroup.
