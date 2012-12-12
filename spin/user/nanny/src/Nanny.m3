(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 *
 * HISTORY
 * 18-Jul-96  Brian Bershad (bershad) at the University of Washington
 *	OneShellCommand returns BOOLEAN.
 *
 * 03-Jul-96  Yasushi Saito (yasushi) at the University of Washington
 *	-f option
 * 
 * 26-Apr-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added an argument to authorization. 
 *
 * 27-Mar-96  Brian Bershad (bershad) at the University of Washington
 *	Created.
 *)

(* "Nanny" watches over all the names. *)

MODULE Nanny;
IMPORT Debugger;<*NOWARN*>
IMPORT IO, Fmt, NameServer, Shell, ParseParams, NannyCmd;
IMPORT NSName;
IMPORT Text;
VAR nannyDir : NameServer.T;

TYPE LinkAlias = NameServer.Alias OBJECT 
  script: TEXT;
  parent: NameServer.T;
  name: NSName.T;
OVERRIDES
  getObject := LinkGetObject;
END;

PROCEDURE LinkGetObject(self: LinkAlias) : REFANY =
  VAR 
    res: REFANY;
    tmp: NSName.T;
  BEGIN
    TRY
      (* move ourselves out of the way *)
      self.parent.detach(self.name);
    
      (* execute the nanny script *)
      IF Shell.OneShellCommand("script " & self.script) THEN
	(* return the object that the script installed, if any *)
	tmp := self.name;
	res := self.parent.lookup(tmp);
      ELSE
	IO.Put("Error. Could not load script\n");
	(* reattach myself into the namespace *)
	self.parent.attach(self.name, self);
	(* already demand loaded *)
      END;
    EXCEPT
    | NameServer.Error(ec) =>
      IO.PutError("nanny: " & NSName.ToText(self.name)
		  & ": re-lookup failed " & NameServer.Fmt(ec) & ".\n");
    END;
    RETURN res;
  END LinkGetObject;

TYPE Alias = NameServer.Alias OBJECT
    namespace: TEXT;
    name: TEXT;
    script: TEXT;
    resolved := FALSE;
  OVERRIDES
    getObject := GetObject;
  END;

PROCEDURE GetObject (self: Alias): REFANY  =
  BEGIN
    TRY
      IF NOT self.resolved THEN
	IF self.namespace # NIL THEN
	  EVAL NameServer.Lookup(NIL, self.namespace);
	  self.resolved := TRUE;
	ELSE
	  IF Shell.OneShellCommand("script " & self.script) THEN
	    self.resolved := TRUE;
	  ELSE
	    IO.PutError("Nanny.GetObject: shell could not load script\n");
	  END;
	END;
      END;
    EXCEPT
    | NameServer.Error(ec) =>
      IO.PutError("Nanny.GetObject: " & self.name & ": "
		  & NameServer.Fmt(ec) & ".\n");
    END;
    RETURN "Nanny turd for " & self.name;
  END GetObject;

PROCEDURE NANNY (name, namespace, data: TEXT; force: BOOLEAN) =
  VAR
    alias: Alias;

  PROCEDURE Install () RAISES {NameServer.Error} = 
    VAR 
      linkAlias: LinkAlias;
      dirName, lastName: NameServer.Name;
      dir: TEXT;
      parent: NameServer.T;
    BEGIN
      (* Install nanny name. *)
      IF namespace # NIL THEN 
	dirName := NSName.FromText(namespace);
	NameServer.GetDirName(dirName, lastName);
	dir := NSName.ToText(dirName);
      
	(* Install a LinkAlias onto the "namespace" *)
	parent := NameServer.Lookup(NIL, "/.." & dir);
      
	linkAlias := NEW(LinkAlias,
			 parent := parent,
			 name := NSName.DeepCopy(lastName),
			 script := data);
	parent.attach(lastName, linkAlias);
      END;

      TRY
	NameServer.Detach(nannyDir, name);
      EXCEPT
      ELSE
      END;
      
      alias := NEW(Alias,
		   namespace := namespace,
		   script := data,
		   name := name);
      NameServer.Attach(nannyDir, name, alias);
    END Install;

  BEGIN
    TRY
      IF force THEN
	Install();
      ELSE
	alias := NameServer.LookupSoft(nannyDir, name, TRUE);
	IF alias = NIL THEN
	  IF namespace = NIL THEN
	    Install();
	  ELSIF NameServer.LookupSoft(NIL, "/.." & namespace) = NIL THEN
	    Install();
	  ELSE
	    IO.PutError("Nanny: " & namespace & "> already exists.\n");
	  END;
	END;
      END;
    EXCEPT
      NameServer.Error (ec) =>
	IO.Put("Nanny: " & name & ": " & NameServer.Fmt(ec) & ".\n");
    END;
  END NANNY;
    
(* "Touch" forces demand loaded name be loaded. *)
PROCEDURE TOUCH (name: TEXT)=
  BEGIN
    TRY
      EVAL NameServer.Lookup(nannyDir, name);
    EXCEPT
    | NameServer.Error (ec) =>
      IF ec = NameServer.EC.NameNotFound THEN
        IO.Put("nanny touch: \"" &name& "\" loaded but not entered in NameServer\n");
      ELSE
        IO.Put("Could not complete touch of " &name& ".  ec is " &
          Fmt.Int(ORD(ec)) & " \n");
      END;
    END;
  END TOUCH;

PROCEDURE RM (name: TEXT) =
  BEGIN
    TRY
      NameServer.Detach(nannyDir, name);
    EXCEPT
      NameServer.Error (ec) =>
        IO.Put("Nanny.RM namespace error " & Fmt.Int(ORD(ec)) & "\n");
    END;
  END RM;

PROCEDURE Run (pp: ParseParams.T): BOOLEAN =
  VAR
    name, namespace, init : TEXT;
    force := FALSE;
  BEGIN
    pp.reset();
    TRY
      pp.skipNext();             (* skip arg0 *)
      IF pp.testNext("zap") THEN
        NannyCmd.Uninstall();
        IO.Put("Nanny is uninstalled\n");
      ELSIF pp.testNext("touch") THEN
        name := pp.getNext();
        TOUCH(name); (* touch name *)
      ELSIF pp.testNext("delete") THEN
        name := pp.getNext();
        RM(name); (* rm name *)
      ELSIF pp.testNext("install") THEN
	IF pp.keywordPresent("-f") THEN
	  force := TRUE;
	END;
	IF pp.keywordPresent("-n") THEN
	  name := pp.getNext();
	END;

	IF NUMBER(pp.arg^) - pp.next >= 2 THEN
	  namespace := pp.getNext();
	  init := pp.getNext();
	  IF name = NIL THEN
	    WITH idx = Text.FindCharR(namespace, '/') DO
	      IF idx >= 0 THEN
		name := Text.Sub(namespace, idx+1);
	      ELSE
		(* pretty bogus. No absolute path name given. *)
		name := namespace;
	      END;
	    END;
	  END;
	ELSE
	  init := pp.getNext();
	  IF name = NIL THEN
	    WITH idx = Text.FindCharR(init, '/'),
	         idx2 = Text.FindCharR(init, '.') DO
	      IF idx >= 0 AND idx2 > idx THEN
		name := Text.Sub(init, idx+1, idx2-idx-1);
	      ELSE
		(* pretty bogus. No absolute path name given. *)
		name := init;
	      END;
	    END;
	  END;
	END;

        NANNY(name, namespace, init, force); (* name namespace init *)
      ELSE
        RAISE ParseParams.Error;
      END;
      pp.finish();
      RETURN TRUE;
    EXCEPT
    | ParseParams.Error =>
        IO.Put(CommandName & CommandHelp & "\n");
        RETURN FALSE;
    END;
  END Run;

BEGIN
  VAR
    dir: NameServer.T;
  BEGIN
    TRY 
      (* create the domain namespace *)
      dir := NameServer.Lookup(NIL, "/../svc");
      nannyDir := NEW(NameServer.Default).init();
      NameServer.Attach(dir, "nanny", nannyDir);
      IO.Put("Nanny namespace initialized\n"); 
    EXCEPT
    | NameServer.Error =>
      IO.PutError("Nanny namespace initialization failed\n"); 
    END;
  END;
END Nanny.
