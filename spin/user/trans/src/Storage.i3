(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 08-Feb-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *  html
 *)

(*
   <dfn>Storage</dfn> is an abstraction of a file.
   You can open, read, write, and close a storage.

   Internally, there are two types of storages,
   [StorageLocal:<dfn>local storage</dfn>]
   and [StorageProxy:<dfn>proxy</dfn>] storage.
   Local storages are storages who resides
   on a local device(ie, disk). Proxy storages are storages backed by
   remote hosts. Proxy storages are opened by passing the string of the form
   <var>host:path</var> when opening. Once opened, there is no difference
   between two.

*)

INTERFACE Storage;
IMPORT Error;
IMPORT CPU;
IMPORT TransT;
IMPORT TransGroup;
IMPORT TransService;

CONST
  PageSize = CPU.PAGESIZE;
  
TYPE
  T <: TPublic;
  TPublic = TransService.T OBJECT
  METHODS
    close(group: TransGroup.T);
    (* "close" closes the storage. As their signatures
       imply, open and close can be called without the interaction
       with the transaction manager.

       "group" specifies the [TransGroup:transaction group]
       that the caller belongs to.

       Note: this procedure is unsafe in a sense there is no protection
       from a client calling "close" many times. *)
    
    access(tr: TransT.T; from, len: CARDINAL;
	   callback: PROCEDURE (VAR x: ARRAY OF CHAR; pos: CARDINAL));
    (* Calls "callback", passing the contents of the region
       "from" to "from+len" into "c". "pos" will be the byte position
       of "c". "callback" may be called multiple times if the region
       "from" .. "from+len" spans multiple pages. This proc does not
       lock the storage. If the storage is site local, you might not
       have to pin the region beforehand, but when the storage is
       proxy, then the region <em>must</em> be pinned beforehand. *)
    accessRO(tr: TransT.T; from, len: CARDINAL;
	     callback: PROCEDURE (READONLY x: ARRAY OF CHAR; pos: CARDINAL));
  END;

PROCEDURE Open(path: TEXT; group: TransGroup.T): T RAISES {Error.E};
(* 
   "path" specifies the file name. By specifying a
   string of the form <var>hostname</var>:<var>port</var>:<var>filename</var>,
   you can connect to a remote storage manager running on a host
   <var>hostname</var>. <var>port</var>: part can be ommitted, in which case
   the default port number defined by TransRPC.m3 is used.

   "group" specifies the [TransGroup:transaction group]
   that the caller belongs to.
*)
   
END Storage.
