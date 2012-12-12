(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 04-Aug-97  Yasushi Saito (yasushi) at the University of Washington
 *	Changed Id from array of char to array of int.
 * 22-Jul-97  Yasushi Saito (yasushi) at the University of Washington
 *	Changed NameServer.Name representation.
 * 04-Oct-96  Yasushi Saito (yasushi) at the University of Washington
 *	Fed cookie to the iterators.
 *
 * 01-Oct-96  Marc Fiuczynski (mef) at the University of Washington
 *	Took out the getRd method.
 *
 * 18-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *)

(* "Nameserver" 
   
   This module implements a name service that can be used to associate
   any name with any object.  Arbitrary name spaces can be created and
   treated as first-class objects.  A global name space is provided
   and arbitrary clients may nest local name spaces inside the global
   space.

   FUTURE WORK: integrate access control. *)

INTERFACE NameServer;
IMPORT Word;
IMPORT NSName;

CONST SpinServices = "svc";

TYPE EC = { 
  NameNotFound, 
  NameExists, 
  InvalidName, 
  Unauthorized, 
  Panic
  };

EXCEPTION Error(EC);

TYPE
  Name = NSName.T;
    
  Alias = OBJECT 
  METHODS
    getObject(): REFANY;
  END;

TYPE
  T <: Public;
  Default <: T;
  
  Cookie = Word.T;
  (* "Cookie" is an opaque thing used to locate the seek position.
     If it is 0, it means seek to the beginning. 
     "getEntries" takes cookie as the start position, and each entry
     returned contains "cookie" field which indicates the cookie
     value to get the next entry.

     The idea of "cookie" is borrowed from NFS.
   
   Note: the reason the cookie is Word.T and not REFANY is that
   this value has to be passed into the user process through
   "getdirentries(2)" call, and I don't want to break the UNIX semantics.
   (beat me - yasushi).
   
   As such, implementors of getEntries have to check the internal
   consistency of the cookie by itself. They should not just
   raise an runtime exception. *)

  (*Id = ARRAY [0 .. (32 DIV BYTESIZE(INTEGER))-1] OF INTEGER; *)
  Id = ARRAY [0 .. 31] OF CHAR;
  (* "Id" is an opaque thing that is supposed to identify an object.
   This is used only as a field of "Entry", and that field does not have
   a meaning in the spincore.

   This field is mainly used in fscore module. *)
	      
  Entry = RECORD
    name: Name;
    cookie: Cookie;
    id: Id;
  END;
  
TYPE Public = OBJECT
METHODS
  init(): T;
  (* "init" initializes the internal state of a name space.  If "root"
     is NIL then the new name space is its own root name space;
     however, it may still be nested in other name spaces.  *)

  lookup((*INOUT*)VAR name: Name; 
	 dontFollowAlias: BOOLEAN := FALSE): REFANY RAISES {Error};
  (* "lookup" returns the object associated to "name" in the name space
     of this object *)

  attach(READONLY name: Name; obj: REFANY) RAISES {Error};
  (* "attach" will insert "obj" into the namespace using "name" *)

  detach(READONLY name: Name) RAISES {Error};
  (* "detach" an object called "name" from this name space *)

  rename(READONLY from, to: Name) RAISES {Error};
  (* "rename" a nameserver entry *)

  iterate(cookie: Word.T := 0): Iterator;
  (* "iterate" returns a sequence object to walk over the directory
     entries. Note: This is OBSOLETE!!. use getEntries *)

  size(): CARDINAL; 
  (* "size" returns the size of the directory *)

  getEntries(cookie: Cookie; VAR (*OUT*)ent: ARRAY OF Entry): CARDINAL;
  (* Get the list of entries in the object.
     Returns the number of entries stored in "ent".
     See the "Cookie" description also. *)
  
  create(name: Name): T RAISES {Error};
  (* XXX this MUST be renamed mkdir. The name "create" is so confusing. *)
END;

TYPE Iterator <: IteratorPublic;
  (* Iterator is built on top of "getEntries".
     I(yasushi) personally
     think the iterator should be removed from the interface. *)
  
TYPE IteratorPublic = OBJECT METHODS
  next(VAR k: Name; VAR v: REFANY): BOOLEAN;
  seek(cookie: Word.T);
END;

PROCEDURE Root(): T;
(* Return the root directory. *)
  
PROCEDURE Lookup(dir: T := NIL;
		 name: TEXT;
		 dontFollowalias := FALSE;
		 root: T := NIL): REFANY RAISES {Error};
(* A shorthand for "TextToName(name)", then "LookupName". *)

PROCEDURE LookupSoft(dir: T := NIL;
		     name: TEXT;
		     dontFollowalias := FALSE;
		     root: T := NIL): REFANY RAISES {Error};
(* Similar to "Lookup", but "LookupSoft"
   returns NIL if the file doesn't exist, rather than raising an exception. *)
  
PROCEDURE Attach(dir: T; name: TEXT; obj: REFANY) RAISES {Error};
  (* A shorthand for "TextToName(name)", then "dir.attach". *)
  
PROCEDURE Detach(dir: T; name: TEXT) RAISES {Error};
  (* A shorthand for "TextToName", then "dir.detach". *)

PROCEDURE LookupName(dir: T := NIL; READONLY name: Name;
		     dontFollowalias := FALSE;
		     root: T := NIL): REFANY RAISES {Error};
  (* "LookupName" returns the object associated to "name" in the name space
     based at "dir". If the "name" starts by "/", then "dir" is ignored and
     the name is searched from "root".
     If "root" is NIL, then it defaults to
     "Root()".  Names are interpreted similar to a
     conventional filesystem path using the "/" as the delimiter. *)

(* Name manipulation routines *)  
PROCEDURE GetComponent(VAR path: Name; VAR component: Name);
  (* Get next name from the "path". Eg, if name = "foo/bar/baz" on entry,m
     name will be "bar/baz" and component will be "foo" upon return. *)

PROCEDURE GetDirName(VAR path: Name; VAR lastname: Name);
  (* A sort of opposite of GetComponent. Eg, if name = "foo/bar/baz" on entry,m
     name will be "foo/bar" and lastname will be "baz" upon return. *)
		     
PROCEDURE IsNameValid(READONLY name: Name): BOOLEAN;

PROCEDURE Fmt(ec: EC): TEXT;
  (* Return the descripton of the error code "ec". *)
END NameServer.
