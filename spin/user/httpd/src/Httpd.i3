(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 31-May-97  David Becker at the University of Washington
 *      Replace Socket.Error with new Errno exception.
 *
 * 23-Aug-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. HTTP server.
 *
 *)

INTERFACE Httpd;
IMPORT FileCache, Wr;
IMPORT Errno, Error;
CONST Brand = "Httpd";

TYPE T = REF RECORD
  url : TEXT;
  offset: CARDINAL;
  size: CARDINAL;
  data: REF ARRAY OF CHAR;
  mutex: MUTEX;
END;

TYPE
  HttpCmd = {Get, Put};
  Request = RECORD
    op            : HttpCmd;   (* HTTP command *)
    proto         : TEXT;      (* protocol *)
    url           : TEXT;      (* url to access *)
    contentLength : INTEGER;   (* content-length supplied by client *)
    referer       : TEXT;      (* The link to us *)
    urltype       : TEXT;      (* mime type for the requested url (computed) *)
  END;

(*
 * Various internal exceptions that can be raised.
 *)
EXCEPTION
  BadRequest(TEXT);
  UrlForbidden(TEXT);
  FileNotFound(TEXT);
  InternalFault(TEXT);
  NotDoneYet(TEXT);

PROCEDURE ParseRequest(READONLY req: ARRAY OF CHAR; VAR request: Request)
  RAISES {BadRequest, NotDoneYet};

PROCEDURE ServeFromDisk(s: REFANY; req: Request; placeIntoCache: BOOLEAN)
  RAISES {FileNotFound, Error.E, Errno.E};

(*
 * We have a single cache, indexed by urls containing data blocks
 * either in memory or in fast secondary storage.
 *)
VAR
  filecache: FileCache.T;
  logWriter: Wr.T;
  useCaching: BOOLEAN;  (* cache files *)
  debug: BOOLEAN := FALSE; (* emit debugging output *)
  verbose: BOOLEAN := TRUE; (* print messages to the server output *)
  keeplog: BOOLEAN := TRUE; (* keep a log of hits and responses *)
  logfile: TEXT := "/wwwlog"; (* default logfilename *)
  serverShouldQuit: BOOLEAN := FALSE; (* set to true to stop server *)
  serverpriority: INTEGER := 0; (* thread priority of ServeClient threads *)

PROCEDURE GetFileCache() : FileCache.T;

(* Run server. Works only once *)
PROCEDURE Run(<*UNUSED*>arg: REFANY) : REFANY;

END Httpd.

