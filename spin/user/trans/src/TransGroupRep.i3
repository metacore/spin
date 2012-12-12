(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 04-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

INTERFACE TransGroupRep;
IMPORT TransGroup;
IMPORT TransQ;
IMPORT TransService;

CONST MaxServices = 16;
TYPE
  ServiceIdx = [0 .. MaxServices-1];
  
REVEAL
  TransGroup.T <: TPublic;
TYPE
  TPublic = TransGroup.TPublic OBJECT
    mu: MUTEX;
    trans: TransQ.T;
    (* List of transactions initiated in this host but which some storages
       in this site is participating. *)
    service: ARRAY ServiceIdx OF TransService.T;
    (* List of storages opened by the group. *)
    refCount: ARRAY ServiceIdx OF CARDINAL;
    (* How many times "storages[i]" is registered? This field is needed
       because we allow duplicate registering. *)
    gid: TransGroup.ID;
  END;

END TransGroupRep.
