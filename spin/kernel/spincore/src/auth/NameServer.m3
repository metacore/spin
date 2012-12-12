(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 27-Jul-97  Yasushi Saito (yasushi) at the University of Washington
 *	The default attach raises an exception when the name exists.
 *	
 * 22-Jul-97  Yasushi Saito (yasushi) at the University of Washington
 *	Changed NameServer.Name representation.
 * 04-Oct-96  Yasushi Saito (yasushi) at the University of Washington
 *	Fed cookie to the iterators.
 *
 * 18-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *)

MODULE NameServer EXPORTS NameServer, NameServerPrivate;
IMPORT Word;
IMPORT IO;
IMPORT Debugger;
IMPORT NSName, NSNameRefTbl;

REVEAL T = Public BRANDED OBJECT
OVERRIDES
  init         := VoidInit;
  lookup       := VoidLookup;
  attach       := VoidAttach;
  detach       := VoidDetach;
  rename       := VoidRename;
  size         := VoidSize;
  getEntries   := VoidGetEntries;
  create       := VoidCreate;
END;

PROCEDURE VoidInit(<*UNUSED*>self: T): T =
  BEGIN
    Debugger.Enter();
    RETURN NIL;
  END VoidInit;
  
PROCEDURE VoidLookup(<*UNUSED*>self: T; <*UNUSED*>VAR name: Name; 
		     <*UNUSED*>dontFollowAlias: BOOLEAN := FALSE): REFANY RAISES {Error}=
  BEGIN
    IO.PutError("NameServer.Lookup not implemented.\n");
    RAISE Error(EC.NameNotFound);
  END VoidLookup;

PROCEDURE VoidAttach(<*UNUSED*>self: T;
		     <*UNUSED*>READONLY name: Name;
		     <*UNUSED*>obj: REFANY) =
  BEGIN
  END VoidAttach;

PROCEDURE VoidDetach(<*UNUSED*>self: T;
		     <*UNUSED*>READONLY name: Name)  =
  BEGIN
  END VoidDetach;

PROCEDURE VoidRename(<*UNUSED*>self: T;
		     <*UNUSED*>READONLY from, to: Name)  =
  BEGIN
  END VoidRename;

PROCEDURE VoidSize(<*UNUSED*>self: T): CARDINAL =
  BEGIN
    RETURN 0;
  END VoidSize;

PROCEDURE VoidGetEntries(<*UNUSED*>self: T;
			 <*UNUSED*>cookie: Cookie;
			 <*UNUSED*>VAR ent: ARRAY OF Entry): CARDINAL =
  BEGIN
    RETURN 0;
  END VoidGetEntries;

PROCEDURE VoidCreate (<*UNUSED*>self: T;
		      <*UNUSED*>name: Name): T =
  BEGIN
    IO.PutError("create:foofoo\n");
    RETURN NIL;
  END VoidCreate;

REVEAL Default = T BRANDED OBJECT
  items: NSNameRefTbl.Default;
  mutex: MUTEX;
OVERRIDES
  lookup       := MethodLookup;
  attach       := MethodAttach;
  detach       := MethodDetach;
  rename       := Rename;
  iterate      := Iterate;
  size         := Size;
  init         := Initialize;
  getEntries   := GetEntries;
  create := Create;
END;

PROCEDURE LookupName (self: T; 
		      READONLY orgName: Name;
		      dontFollowAlias: BOOLEAN;
		      root: T): REFANY RAISES {Error} =
  VAR
    entry: REFANY;
    name := orgName;
  BEGIN
    IF self = NIL THEN self := curRoot; END;
    IF name.str[name.from] = '/' THEN
      (* Absolute path specified. *)
      INC(name.from);
      self := root;
      IF root = NIL THEN
	self := curRoot;
      END;
      IF name.end = name.from THEN
	(* "/" passed in. *)
	RETURN self;
      END;
    END;
    
    entry := self;
    
    WHILE name.end > 0 DO
      entry := self.lookup(name, dontFollowAlias);
      TYPECASE entry OF
      | NULL =>
	IO.PutError("NameServer.Lookup: foo " & NSName.ToText(name));
	Debugger.Enter();
      | T(dir)  =>
	IF name.end > 0 THEN 
	  self := dir;
	END;
      | Alias(alias) =>
	(* if alias embedded in path, dereference alias *)
	IF name.end > 0 THEN
	  (* in the middle of the path *)
	  self := alias.getObject();
	  (* continue through loop to resolve name *)
	ELSIF NOT dontFollowAlias THEN
	  (* final component in path *)
	  entry := alias.getObject();
	END;
	(* fall through *)
      ELSE
	IF name.end > 0 THEN
	  (* middle of the path, but no directory. *)
	  RAISE Error(EC.NameNotFound);
	END;
      END;
    END;
    RETURN entry;
  END LookupName;

PROCEDURE Lookup (self: T; txt: TEXT;
		  dontFollowAlias: BOOLEAN;
		  root: T): REFANY RAISES {Error} =
  VAR
    name: Name;
  BEGIN
    name := NSName.FromText(txt);
    RETURN LookupName(self, name, dontFollowAlias, root);
  END Lookup;

PROCEDURE LookupSoft (self: T; txt: TEXT;
		      dontFollowAlias: BOOLEAN;
		      root: T): REFANY RAISES {Error} =
  BEGIN
    TRY
      RETURN Lookup(self, txt, dontFollowAlias, root);
    EXCEPT
    | Error(ec) =>
      IF ec = EC.NameNotFound THEN
	RETURN NIL;
      ELSE
	RAISE Error(ec);
      END;
    END;
  END LookupSoft;
		      
PROCEDURE MethodLookup (self: Default;
			VAR name: Name;
			<*UNUSED*>blah: BOOLEAN): REFANY RAISES {Error} =
  VAR
    component: Name;
    entry: REFANY;
    inTable: BOOLEAN;
  BEGIN
    GetComponent(name, component);
    LOCK self.mutex DO 
      inTable := self.items.get(component, entry);
    END;
    IF inTable THEN
      RETURN entry;
    END;
    RAISE Error(EC.NameNotFound);
  END MethodLookup;

PROCEDURE GetEntries (self: Default; cookie: Cookie;
		      VAR ent: ARRAY OF Entry): CARDINAL =
  VAR
    itr := self.items.iterate();
    nEntries: CARDINAL := 0;
    key: NSName.T;
    val: REFANY;
  BEGIN
    (* In the default nameserver implementation, the cookie is simply
       the order within the table, ie, 0 means first, 1 means second,
       so on.*)
    
    LOCK self.mutex DO 
      (* XXX this is extremely inefficient.
	 We either need to rewrite Table generic and add seek, or
	 we need to use a new table. I already have the former code, but
	 I am reluctant to bloat the Table further. Need more discussion.
	 -yasushi *)
      FOR i := 1 TO cookie DO
	EVAL itr.next(key, val);
      END;
      WHILE nEntries < NUMBER(ent) DO
	IF NOT itr.next(key, val) THEN EXIT; END;
	ent[nEntries].name := key;
	ent[nEntries].cookie := cookie + nEntries + 1;
	VIEW(ent[nEntries].id, INTEGER) := cookie + nEntries + 1;
	(* "id" has to be nonzero because some "readdir" implementations
	   (at least OSF/1 skips the entry if "id" is 0. *)
	INC(nEntries);
      END;
      itr := NIL;
    END;
    RETURN nEntries;
  END GetEntries;
    
PROCEDURE GetComponent (VAR name: Name; VAR component: Name) =
  VAR
    i: CARDINAL;
  BEGIN
    IF name.end = 0 THEN
      component.end := 0;
      RETURN;
    END;

    component.from := name.from;
    component.str := name.str;

    i := name.from;
    IF name.str[i] = '/' THEN
      (* "name" is an absolute path. Include the first "/" in
	 component. *)
      WHILE i < name.end AND name.str[i] = '/' DO INC(i); END;
    END;
    
    WHILE i < name.end AND name.str[i] # '/' DO INC(i); END;
    
    IF i < name.end THEN component.end := i;
    ELSE component.end := name.end;
    END;

    (* skip all consecutive '/'s. *)
    INC(i);
    WHILE i < name.end AND name.str[i] = '/' DO INC(i); END;
	
    IF i >= name.end THEN
      (* there's a trailing '/' at the end of the path. *)
      name.end := 0;
      RETURN;
    ELSE
      name.from := i;
    END;
  END GetComponent;
  
PROCEDURE GetDirName (VAR name: Name; VAR lastname: Name) =
  VAR
    i: INTEGER; (* may become -1 *)
  BEGIN
    IF name.end = 0 THEN
      lastname.end := 0;
      RETURN;
    END;

    lastname.end := name.end;
    lastname.str := name.str;

    i := name.end-1;
    WHILE i >= name.from AND name.str[i] # '/' DO DEC(i); END;
    
    lastname.from := i+1;

    (* see if we searched all strings *)
    IF i = name.from-1 THEN
      name.end := name.from;
      RETURN;
    END;

    (* skip all consecutive '/'s. *)
    DEC(i);
    WHILE i >= name.from AND name.str[i] = '/' DO DEC(i); END;
	
    IF i < name.from THEN
      (* there's a '/' at the start of the path. *)
      name.end := name.from+1;
      RETURN;
    ELSE
      name.end := i+1;
    END;
  END GetDirName;

PROCEDURE MethodAttach(self: Default; READONLY name: Name;
		       obj: REFANY) RAISES {Error} =
  VAR
    tmp := NSName.DeepCopy(name);
    crap: REFANY;
  BEGIN
    IF NOT IsNameValid(name) THEN
      RAISE Error(EC.Panic);
    END;

    LOCK self.mutex DO
      IF self.items.get(tmp, crap) THEN
	RAISE Error(EC.NameExists);
      ELSE
	EVAL self.items.put(tmp, obj);
      END;
    END;
  END MethodAttach;
  
PROCEDURE Attach(self: T; txt: TEXT; obj: REFANY) RAISES {Error} =
  VAR name: Name;
  BEGIN
    name := NSName.FromText(txt);
    self.attach(name, obj);
  END Attach;

PROCEDURE MethodDetach(self: Default; READONLY name: Name) RAISES {Error} =
  VAR obj: REFANY;
  BEGIN
    IF NOT IsNameValid(name) THEN
      RAISE Error(EC.Panic);
    END;

    LOCK self.mutex DO 
      IF NOT self.items.delete(name, obj) THEN
        RAISE Error(EC.NameNotFound);
      END;
    END;
  END MethodDetach;
  
PROCEDURE Detach(self: T; txt: TEXT) RAISES {Error} =
  VAR name: Name;
  BEGIN
    name := NSName.FromText(txt);
    self.detach(name);
  END Detach;

PROCEDURE Rename(self: Default; READONLY from,to: Name) RAISES {Error} = 
  VAR obj: REFANY;
  BEGIN
    IF NOT IsNameValid(from) OR NOT IsNameValid(to) THEN
      RAISE Error(EC.Panic);
    END;
    
    LOCK self.mutex DO
      IF NOT self.items.delete(from,obj) THEN
        RAISE Error(EC.NameNotFound);
      END;
      EVAL self.items.put(to,obj);
    END;
  END Rename;

PROCEDURE Size(self: Default) : CARDINAL = 
  BEGIN
    RETURN self.items.size();
  END Size;

PROCEDURE Create (self: Default; name: Name): T RAISES {Error} =
  VAR subdir := NEW(Default).init();
  BEGIN
    self.attach(name, subdir);
    RETURN subdir;
  END Create;
  
PROCEDURE Initialize (self: Default): T =
  BEGIN
    self.mutex := NEW(MUTEX);
    self.items := NEW(NSNameRefTbl.Default).init();
    RETURN self;
  END Initialize;

CONST CacheSize = 64;
REVEAL Iterator = IteratorPublic BRANDED OBJECT
    obj: T;
    ent: ARRAY [0 .. CacheSize-1] OF Entry;
    nextEnt: [0 .. CacheSize];
    nEnt: [0 .. CacheSize];
    cookie: Cookie;
  OVERRIDES
    next  := IterNext;
    seek  := IterSeek;
  END;

PROCEDURE Iterate(self: Default; cookie: Word.T): Iterator = 
  BEGIN
    RETURN NEW(Iterator, obj := self, cookie := cookie,
	       nextEnt := CacheSize);
  END Iterate;

PROCEDURE IterNext(self : Iterator;
		   VAR (*OUT*) k: Name; 
		   VAR (*OUT*) v: REFANY) : BOOLEAN =
  VAR
    tmp: Name;
  BEGIN
    IF self.nextEnt >= self.nEnt THEN
      self.nEnt := self.obj.getEntries(self.cookie, self.ent);
      IF self.nEnt = 0 THEN RETURN FALSE; END;
      self.cookie := self.ent[self.nEnt-1].cookie;
      self.nextEnt := 0;
    END;
    
    TRY
      k := self.ent[self.nextEnt].name;
      tmp := k;
      v := self.obj.lookup(tmp, TRUE);
      INC(self.nextEnt);
    EXCEPT
    | Error(ec) =>
      IO.Put("NameServer.IterNext: " & NSName.ToText(k)
	     & ": " & Fmt(ec) & ".\n");
      RETURN FALSE;
    END;
    
    RETURN TRUE;
  END IterNext;

PROCEDURE IterSeek(self: Iterator; cookie: Cookie) = 
  BEGIN
    self.cookie := cookie;
    self.nextEnt := CacheSize;
  END IterSeek;


PROCEDURE IsNameValid (<*UNUSED*>READONLY name: Name): BOOLEAN =
  BEGIN
    RETURN TRUE;
    (* XXX check if name contains "/" or something. *)
  END IsNameValid;

PROCEDURE SetRoot (dir: T) =
  BEGIN
    curRoot := dir;
  END SetRoot;
PROCEDURE Root (): T =
  BEGIN
    RETURN curRoot;
  END Root;

PROCEDURE Fmt(ec: EC): TEXT =
  CONST Messages = ARRAY EC OF TEXT
    {
     "Name not found",
     "Name already exists",
     "Invalid name",
     "Unauthorized operation",
     "Panic!!"
     };
  BEGIN
    RETURN Messages[ec];
  END Fmt;
  
PROCEDURE Init(verbose:BOOLEAN) = 
  BEGIN

    (* Special creation of the globalnamespace node. *)
    root := NEW(Default).init();
    curRoot := root;

    (* special creation of the spin services sub-namespace node *)
    svc := NEW(Default).init();

    TRY
      (* attach Svc node as a sub-namespace to the global namespace. *)
      Attach(root, "..", root);
      Attach(root, ".", root);
      Attach(root, "svc", svc);
    EXCEPT
    | Error =>
      IF verbose THEN END;
    END;
  END Init;

BEGIN
END NameServer.
