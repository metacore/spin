(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 02-Aug-96  Tian Fung Lim (tian) at the University of Washington
 *	Moved Initialize to DomainRun, added a real Initialize and
 *	a Bind/Unbind.
 *
 * 15-Jul-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added destroy and list.
 *
 * 27-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	Name clean for code region.
 *
 * 13-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed output message to print DomainCmd instead of the old
 *	Download message.
 *
 * 22-Apr-96  Wilson Hsieh (whsieh) at the University of Washington
 *	changed output msg
 *
 * 28-Mar-96  Brian Bershad (bershad) at the University of Washington
 *	Try to be a bit more conservative with memory in Add.
 *
 * 26-Mar-96  Brian Bershad (bershad) at the University of Washington
 *	Catch exception from NameServer.Delete in Zap.
 *
 * 11-Mar-96  Emin Gun Sirer (egs) at the University of Washington
 *	Automatically mark dynamically linked C code as non-preemptible.
 *
 * 11-Mar-96  Emin Gun Sirer (egs) at the University of Washington
 *	Changed to output better debugging in case of link failure.
 *      Register only top level domains with the debugger.
 *
 * 09-Mar-96 David Becker (becker) at the University of Washington
 * 	Extend shell with domain manipulation command.
 *)
UNSAFE
MODULE DomainCmd;

IMPORT Domain, NameServer, Auth, File, FileSystem, FileStat,
       MachineLinker, KernelRegions, Error, Region, DomainRep,
       Debugger, DomainPrivate, DomainCmdCmd, Glob, IO, Fmt,
       ParseParams, Shell, Word, Text (*, TrackExtern*);
IMPORT NSName;

VAR 
  (* XXX auth:= NEW(Auth.AuthAlways); *) (* assures only the dom command can delete *)
  quiet:=FALSE;
  totalSize:INTEGER; (* just an estimate *)

PROCEDURE Lookup (domainname:TEXT; VAR d: Domain.T): BOOLEAN =
  VAR
    entry     : REFANY;
    key       : Auth.Key;
  BEGIN
    TRY
      key := NIL; (* No auth, so only find world readable domains *)
      entry := NameServer.Lookup(domainNS, domainname);
      TYPECASE entry OF
      | NULL =>
        (* SKIP *)
      | Domain.T(domain) =>
        d := domain; 
        RETURN TRUE;
      ELSE
        (* SKIP *)
      END;
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

TYPE 
  AT = Auth.T OBJECT
  OVERRIDES
    authorize := ATAuth;
  END;

PROCEDURE ATAuth(<*UNUSED*>at: AT;
		 <*UNUSED*>key: REFANY;
		 <*UNUSED*>arg: REFANY := NIL): BOOLEAN RAISES {Auth.Retry} =
  BEGIN
    IO.Put("Calling type authorizer\n");
    RETURN FALSE;
  END ATAuth;

PROCEDURE PlayWithTypes () =
  VAR d : Domain.T;
      auth: Auth.T;
  BEGIN
    IO.Put("Adding type authorizer\n");
    IF Lookup("FileSystem", d) THEN
      IO.Put("Got domain\n");
      Domain.AddTypeAuthorizer(d, "FileSystem.T", auth);
    END;
  END PlayWithTypes;


PROCEDURE Zap (domainname: TEXT) =
  VAR domain : Domain.T;
  BEGIN
    IF NOT Lookup(domainname, domain) THEN
      IO.Put("Cannot find domain " & domainname & "\n");
      RETURN;
    END;
    IF NOT Domain.FullyResolved(domain) THEN
      IO.Put("domain " & domainname & " not linked.\n");
      RETURN;
    END;
    IF Domain.Initialize(domain) = FALSE THEN
      IO.Put("domain " & domainname & " init failed.\n");
      RETURN;
    END;
    IO.Put("replacing domain command\n");
    DomainCmdCmd.Uninstall();
    TRY
      NameServer.Detach(domainNS, domainname);
    EXCEPT
      NameServer.Error  => IO.Put("Whoops. Domain not zappable\n");
    END;
  END Zap;


PROCEDURE Create (domainname:TEXT) =
  VAR
    domain : Domain.T;
  BEGIN
    TRY
      IO.Put(domainname & " ");
      IF Lookup(domainname, domain) THEN
	NameServer.Detach(domainNS, domainname);
        IF NOT quiet THEN IO.Put("re-created.\n") END;
      ELSE
        IF NOT quiet THEN IO.Put("created.\n") END;
      END;
      domain := Domain.Create(domainname,NIL);
      NameServer.Attach(domainNS, domainname, domain);
    EXCEPT
    | NameServer.Error(ec)  =>
	IO.PutError("DomainCmd.Create namespace failure " & domainname &
	  	" (" & Fmt.Int(ORD(ec)) & ")\n");
    END;
    totalSize := 0;
  END Create;

PROCEDURE Forget (domainname: TEXT) =
  VAR domain : Domain.T;
  BEGIN
    IF NOT Lookup(domainname, domain) THEN
      IO.Put("Cannot find domain " & domainname & "\n");
      RETURN;
    END;
    TRY
      NameServer.Detach(domainNS, domainname);
    EXCEPT
    | NameServer.Error (ec) =>
        IO.PutError("Nameserver FAILED " & domainname & " ("
                      & Fmt.Int(ORD(ec)) & ")\n");
        RETURN;
    END;
    IO.Put(domainname & " unregistered\n");
  END Forget;

PROCEDURE AddFile (domainname, filename: TEXT) =
  VAR
    fp               : File.T;
    stat             : FileStat.T;
    objectFile       : REF ARRAY OF CHAR;
    len              : CARDINAL;
    newDomain, domain: Domain.T;
    reg              : Region.T;
    from: CARDINAL;
  BEGIN
    IF NOT Lookup(domainname, domain) THEN
      IO.Put("Cannot find domain " & domainname & "\n");
      RETURN;
    END;

    IF quiet THEN
      IO.Put(".");
    ELSE
      IO.Put("<" & filename & ">");
    END;

    (* XXX
    add "bg" to the "domain" cmd
    INC(count_of_files_to_be_waited_for);   
    *)
    TRY
      TRY
        fp := FileSystem.Lookup(NIL, filename);
        fp.stat(stat);
	len := fp.readRef(objectFile, stat.size, 0, from);
      EXCEPT
        Error.E (e) =>
          IO.Put("DomainCmd.LoadFile: filename: " & filename & "\n");
          IO.Put("error: " & e.message() & "\n");
          RETURN;
      END;

      IF from # 0 OR len # stat.size THEN
	IO.Put(filename &": could not read all the file contents.\n");
	RETURN;
      END;
      IF NOT quiet THEN IO.Put(Fmt.Int(len) & " Kbytes\n"); END;
      INC(totalSize, len);
      
      newDomain := Domain.Create(filename, objectFile);
      Debugger.RegisterDomain(newDomain);

    FINALLY
      (* XXX
         DEC(count_of_files_to_be_waited_for);   
         SIGNAL();
      *)
      objectFile := NIL;
      TRY IF fp # NIL THEN fp.close() END EXCEPT ELSE END;
    END;


    IF NOT Domain.IsModula3(newDomain) THEN
      IF MachineLinker.TextInfo(newDomain.module, reg.begin, reg.length) THEN
        KernelRegions.NonPreemptibleRegions.add(reg);
      END;
    END;

    Domain.Add(domain, newDomain);
  END AddFile;

PROCEDURE Link (domainname, externname: TEXT) =
  VAR domain, extern: Domain.T;
  BEGIN
    IF NOT Lookup(domainname, domain) THEN
      IO.Put("Cannot find domain " & domainname & "\n");
      RETURN;
    END;

    IF NOT Lookup(externname, extern) THEN
      IO.Put("Cannot find domain " & externname & "\n");
      RETURN;
    END;

    (* XXX need to do Domain.Resolve()
    IF Domain.FullyResolved(domain) THEN
      IO.Put(domainname & " fully linked.\n");
      RETURN;
    END;
    *)
    
    (* XXX
    WHILE not_all_file_requests_completed DO
      WAIT();
    END;
    *)

    IF quiet AND totalSize # 0 THEN
      IO.Put(" " & Fmt.Int(totalSize DIV 1024) & " KB ");
    END;
    IF NOT quiet THEN
      IF totalSize # 0 THEN
        IO.Put("Total size " & Fmt.Int(totalSize DIV 1024) & " KB\n");
      END;
      IO.Put(domainname & " <- " & externname);
    END;
    totalSize := 0;
    IF extern # domain AND NOT Domain.FullyResolved(extern) THEN
      IO.Put(" (warning: " & externname & " not fully linked)");
      IF quiet THEN IO.Put("\n") END;
    END;
    IF NOT quiet THEN IO.Put("\n") END;

    Domain.Resolve(domain, extern);
    IF Domain.FullyResolved(domain) THEN
      IF NOT quiet THEN IO.Put(domainname) END;
      IO.Put(" link complete.\n");
    END;
  END Link;

PROCEDURE RunDomain (domainname: TEXT) =
  VAR 
    entry  : REFANY;
  BEGIN
    IF NOT quiet THEN
      IO.Put(domainname & " run M3 main bodies.\n");
    END;

    TRY
      entry := NameServer.Lookup(domainNS, domainname);
      NameServer.Detach(domainNS, domainname); (* Ack *)
      TYPECASE entry OF
      | Domain.T(domain) =>
        IF Domain.Initialize(domain) = FALSE THEN
          (* put our entry back in the nameserver so the user can fix it *)
	  NameServer.Attach(domainNS, domainname, domain);
          IO.PutError("Domain initialize failed...Link errors:\n");
          Link(domainname, "Debug");
          IO.Put("End of link errors.\n");
        END;
      ELSE
        IO.PutError("DomainCmd.RunDomain found non Domain.T REF for");
        IO.Put(domainname);
        IO.Put("\n");
      END;
    EXCEPT
    | NameServer.Error (ec) =>
      IO.PutError("DomainCmd.RunDomain: " & domainname & ": "
		  & NameServer.Fmt(ec) & ".\n");
    END;
  END RunDomain;


(* This simply does the initialize without touching the NameServer *)
(* This allows backward compatibility with old style templates that
   use domain run foo *)
PROCEDURE Initialize (domainName: TEXT) =
  VAR domain : Domain.T;
  BEGIN
    IF NOT Lookup(domainName, domain) THEN
      IO.Put ("Domain " & domainName & " does not exist\n");
      RETURN;
    END;
    
    IF Domain.Initialize(domain) = FALSE THEN
      IO.PutError("Domain initialize failed...Link errors:\n");
        Link(domainName, "Debug");
        IO.Put("End of link errors.\n");
    END;

  END Initialize;


PROCEDURE Bind (domainName1: TEXT; domainName2: TEXT) =
  VAR
    domain    : Domain.T;
  BEGIN
    IF Lookup(domainName2, domain) THEN
      IO.Put("Domain " & domainName2 & " already exists!\n");
      RETURN;
    END;

    IF NOT Lookup(domainName1, domain) THEN
      IO.Put("Cannot find domain " & domainName1 & "\n");
      RETURN;
    END;
    
    TRY
      NameServer.Attach(domainNS, domainName2, domain);
    EXCEPT
    | NameServer.Error (ec) =>
      IO.PutError("Nameserver FAILED adding " & domainName2 & " ("
      & Fmt.Int(ORD(ec)) & ")\n");
    END;
    
  END Bind;

PROCEDURE Unbind (domainName:TEXT) =
  VAR 
    domain    : Domain.T;
  BEGIN
    IF NOT Lookup(domainName, domain) THEN 
      IO.Put ("Domain " & domainName & " does not exist!\n");
      RETURN;
    END;

    IF NOT Domain.FullyResolved(domain) THEN
      IO.Put ("Cannot unbind "& domainName & " because it is not fully resolved\n");
      RETURN;
    END;

    TRY
      NameServer.Detach(domainNS, domainName);
    EXCEPT
    | NameServer.Error (ec) =>
      IO.PutError("Nameserver FAILED deleting " & domainName & " ("
      & Fmt.Int(ORD(ec)) & ")\n");
    END;
  END Unbind;

PROCEDURE Destroy (domainName: TEXT) = 
  VAR
    domain    : Domain.T;
  BEGIN
    IF NOT Lookup(domainName, domain) THEN
      IO.Put("Cannot find domain (" & domainName &")\n");
      RETURN;
    END;

    (* find the top-most domain for this name *)
    WHILE domain.parent # NIL DO
      domain := domain.parent;
    END;

    (* remove it from the name server *)
    TRY
      NameServer.Detach(domainNS, domainName);
    EXCEPT
    | NameServer.Error  => 
      IO.Put("Domain not be deleted from the name server\n");
      RETURN;
    END;

    (* try to destroy it *)
    IF NOT Domain.Destroy(domain) THEN
      IO.Put("Domain " & domainName & " could not be destroyed\n");
    ELSE
      (* IO.Put("Domain " & domainName & " destroyed\n");*)
    END;
  END Destroy;

CONST 
  protectionText = ARRAY BOOLEAN OF TEXT {"untrusted","trusted"};
  linkedAsText   = ARRAY BOOLEAN OF TEXT {"static","dynamic"};

PROCEDURE Nlist(name: TEXT) =
  VAR domain           : Domain.T;
      domainName       : TEXT;
      trusted, dynamic : BOOLEAN;
  BEGIN
    IF NOT Lookup(name, domain) THEN
      IO.Put("Cannot find domain (" & name &")\n");
      RETURN;
    END;
    DomainPrivate.GetState(domain, domainName, trusted, dynamic);
    IO.Put("domain : ");
    IO.Put(name);
    IO.Put(" internal name: ");
    IO.Put(domainName);
    IO.Put(" protection: ");
    IO.Put(protectionText[trusted]);
    IO.Put(" linked as: ");
    IO.Put(linkedAsText[dynamic]);
    IO.Put("\n");
  END Nlist;

PROCEDURE LoadMap(name: TEXT) = 
  VAR 
    domain           : Domain.T;
    domainName       : TEXT;
    trusted, dynamic : BOOLEAN;
    start            : Word.T;
    size             : INTEGER;
  BEGIN
    IF NOT Lookup(name, domain) THEN
      IO.Put("Cannot find domain (" & name &")\n");
      RETURN;
    END;
    DomainPrivate.GetState(domain, domainName, trusted, dynamic);
    IF DomainPrivate.TextInfo(domain, start, size) THEN
      IO.Put(name);
      IO.Put("\n");
      DomainPrivate.ShowModules(domain);
    END;
  END LoadMap;

TYPE
  DumpClosure = Domain.Closure OBJECT
    i     : INTEGER := 0;
    all   : BOOLEAN := FALSE;
    links : BOOLEAN := FALSE;
  OVERRIDES
    apply := DomainDump;
  END;

PROCEDURE DomainDump (cl: DumpClosure; domain: Domain.T): BOOLEAN =
  VAR
    name    : TEXT;
    trusted : BOOLEAN;
    dynamic : BOOLEAN;
  BEGIN
    DomainPrivate.GetState(domain, name, trusted, dynamic);
    IF NOT cl.all AND domain.dynamic THEN
      RETURN TRUE;      
    END;
    IF cl.links THEN
      DumpOne(domain, cl.all);
    ELSE
      INC(cl.i);
      IO.Put(Fmt.Int(cl.i) & " - " & Fmt.Int(domain.id) & 
        " domain : " & name & 
        " protection: " & protectionText[trusted] & 
        ", linked as: " & linkedAsText[dynamic] & " ");
      IF domain.parent # NIL THEN
        IO.Put("P: " & Fmt.Int(domain.parent.id) & " " & 
          domain.parent.name & "\n");
      ELSE
        IO.Put("\n");
      END;
    END;
    RETURN TRUE;
  END DomainDump;

PROCEDURE DumpOne (ptr: Domain.T; full: BOOLEAN) =

  PROCEDURE FindParent(ptr: Domain.T): Domain.T =
    BEGIN
      WHILE ptr.parent # NIL DO
        ptr := ptr.parent;
      END;
      RETURN ptr;
    END FindParent;
    
  PROCEDURE ReverseLookup(domain: Domain.T): TEXT =
    VAR
      iter   : NameServer.Iterator := domainNS.iterate();
      name   : NameServer.Name;
      ref    : REFANY;
    BEGIN
      WHILE iter.next(name, ref) DO
        IF ISTYPE(ref, Domain.T) THEN
          IF NARROW(ref, Domain.T) = domain THEN
            RETURN NSName.ToText(name);
          END;
        END;
      END;
      RETURN NIL;
    END ReverseLookup;

  PROCEDURE PrintLinks(subPtr: DomainRep.DList) =
    VAR
      id: INTEGER;
    BEGIN
      IF NOT full THEN
        FOR i := 0 TO DomainPrivate.nextID - 1 DO
          found[i] := FALSE;
        END;
        found[ptr.id] := TRUE;
      END;
      WHILE subPtr # NIL DO
        IO.Put(" ");
        IF full THEN
          id := subPtr.domain.id;
        ELSE
          id := FindParent(subPtr.domain).id;
          IF found[id] THEN
            id := -1;
          ELSE
            found[id] := TRUE;
            (* IO.Put("<" & FindParent(subPtr.domain).name & "> ");*)
          END;
        END;
        IF id # -1 THEN
          IO.PutInt(id);
        END;
        subPtr := subPtr.next;
      END;
    END PrintLinks;

  PROCEDURE PrintDomain(ptr: Domain.T) =
    BEGIN
      (* print its own ID and name *)
      IO.PutInt(ptr.id, 4); 
      IO.Put(" " & ptr.name);

      (* print the name from the name server *)
      IF NOT full THEN
        WITH nsName = ReverseLookup(ptr) DO
          IF nsName # NIL THEN
            IO.Put(" (" & nsName & ")"); 
          END;
        END;
      END;
    END PrintDomain;

  VAR
    found := NEW(REF ARRAY OF BOOLEAN, DomainPrivate.nextID);
    subPtr : DomainRep.DList;
    parent : Domain.T := FindParent(ptr);
  BEGIN
    PrintDomain(ptr);
    IF parent # ptr THEN
      IO.Put("\n\tparent: " & Fmt.Int(parent.id) & "\n");
      IF NOT full THEN
        RETURN;
      END;
    END;
    IO.Put(" : ");

    IF NOT full THEN IO.Put("\n\t"); END;
    
    (* print flags *)
    IF full THEN
      IF NOT ptr.collectable THEN IO.Put("NC "); ELSE IO.Put(" C "); END; 
      IF NOT ptr.modify THEN IO.Put("NM "); ELSE IO.Put(" M "); END; 
      IF NOT ptr.import THEN IO.Put("NI "); ELSE IO.Put(" I "); END; 
      IF NOT ptr.trusted THEN IO.Put("NT "); ELSE IO.Put(" T "); END; 
      IF NOT ptr.dynamic THEN IO.Put("ND "); ELSE IO.Put(" D "); END;
    END;

    (* print subdomains *)
    IF full THEN
      subPtr := ptr.subdlist;
      IO.Put ("[SUB: ");
      WHILE subPtr # NIL DO
        IO.Put(" ");
        IO.PutInt(subPtr.domain.id);
        subPtr := subPtr.next;
      END;
      IO.Put ("] ");
    END;

    (* print linked *)
    IO.Put ("LINK: ");
    PrintLinks(ptr.linked);
    IO.Put (" ");
    
    IF NOT full THEN IO.Put("\n\t"); END;

    (* print rev_linked *)
    IO.Put ("REV_LINK: ");
    PrintLinks(ptr.rev_linked);

    IO.Put("\n");
  END DumpOne;

TYPE
  NukeClosure = Domain.Closure OBJECT
    name : TEXT;
  OVERRIDES
    apply := DomainNuke;
  END;

PROCEDURE DomainNuke (cl: NukeClosure; domain: Domain.T): BOOLEAN =
  VAR
    name    : TEXT;
    trusted : BOOLEAN;
    dynamic : BOOLEAN;
  BEGIN
    DomainPrivate.GetState(domain, name, trusted, dynamic);
    IF Text.Equal(name, cl.name) THEN
      WHILE domain.parent # NIL DO
        domain := domain.parent;
      END;
      IF NOT Domain.Destroy(domain) THEN
        IO.Put("Domain " & name & " could not be destroyed\n");
      ELSE
        IO.Put("Domain " & name & " successfully destroyed\n");
      END;
      RETURN FALSE;
    END;
    RETURN TRUE;
  END DomainNuke;

(*
 * Shell extension
 *)
PROCEDURE Run (pp: ParseParams.T): BOOLEAN =
  VAR
    domainname, filename, externDomainName: TEXT;
  BEGIN
    TRY
      pp.reset();
      pp.skipNext();

      IF pp.testNext("zap") THEN  (* FIXME: what is this for ??? *)
        domainname := pp.getNext();
	Zap(domainname);
      ELSIF pp.testNext("create") THEN
        domainname := pp.getNext();
	Create(domainname);
      ELSIF pp.testNext("quiet") THEN  (* FIXME: make it verbose level ??? *)
        quiet := TRUE;
      ELSIF pp.testNext("verbose") THEN
        quiet := FALSE;
      ELSIF pp.testNext("play") THEN
        PlayWithTypes();
      ELSIF pp.testNext("stats") THEN
	VAR bytes, subdomains:INTEGER;
	BEGIN
	Domain.Stats(subdomains,bytes);
        IO.Put("Total Subdomains: " & Fmt.Int(subdomains) &
		". Total File Size: " &  Fmt.Int(bytes DIV 1000) &
		" KB\n");
	END;
      ELSIF pp.testNext("forget") THEN
        domainname := pp.getNext();
	Forget(domainname);
      ELSIF pp.testNext("link") THEN
        domainname := pp.getNext();
        externDomainName := pp.getNext();
	Link(domainname,externDomainName );
      ELSIF pp.testNext("check") THEN
        domainname := pp.getNext();
	Link(domainname,"Debug");
      ELSIF pp.testNext("run") THEN
        domainname := pp.getNext();
        RunDomain(domainname);
      ELSIF pp.testNext("addfile") THEN
        domainname := pp.getNext();
        filename := pp.getNext();
	AddFile(domainname, Glob.Translate(Shell.Vars(), filename) );
      ELSIF pp.testNext("dump") THEN
        VAR
          cl := NEW(DumpClosure);
        BEGIN
          IF pp.keywordPresent("-all") THEN
            IO.Put("all active domains:\n");
            cl.all := TRUE;
          ELSE
            IO.Put("all top-most domains:\n");
          END;
          IF pp.keywordPresent("-links") THEN
            cl.links := TRUE;
          END;
          EVAL DomainPrivate.ApplyToAll(cl);
        END;
      ELSIF pp.testNext("nuke") THEN
        VAR
          cl := NEW(NukeClosure);
        BEGIN
          cl.name := pp.getNext();
          EVAL DomainPrivate.ApplyToAll(cl);
        END;
      ELSIF pp.testNext("list") THEN
        IO.Put("domains registered with the name server:\n");
        VAR
          iter   : NameServer.Iterator := domainNS.iterate();
	  key    : NameServer.Name;
	  name: TEXT;
          ref    : REFANY;
          domain : Domain.T;
          i      : INTEGER := 0;
          links  : BOOLEAN;
        BEGIN
          links := pp.keywordPresent("-links");
          WHILE iter.next(key, ref) DO
	    name := NSName.ToText(key);
            IF ISTYPE(ref, Domain.T) THEN
              INC(i);
              IO.Put(Fmt.Int(i) & " : " & name);
              domain := NARROW(ref, Domain.T);
              IF links THEN
                IO.Put(" : ");
                DumpOne(domain, FALSE);
              ELSE
                IO.Put("\n");
              END;
            END;
          END;
        END;
      ELSIF pp.testNext("destroy") THEN
        domainname := pp.getNext();
        Destroy(domainname);
(*
      ELSIF pp.testNext("mem") THEN
        IO.Put("Malloc : " & Fmt.Int(TrackExtern.total_malloc_cnt) & 
          " " & Fmt.Int(TrackExtern.total_malloc_bytes) & 
          "\nFree   : " & Fmt.Int(TrackExtern.total_free_cnt) &
          " " & Fmt.Int(TrackExtern.total_free_bytes) & "\n");
*)
      ELSIF pp.testNext("initialize") THEN
        domainname := pp.getNext();
        Initialize(domainname);
      ELSIF pp.testNext("bind") THEN
        domainname := pp.getNext();
        externDomainName := pp.getNext();
        Bind(domainname, externDomainName);
      ELSIF pp.testNext("unbind") THEN
        domainname := pp.getNext();
        Unbind(domainname);
      ELSIF pp.testNext("nlist") THEN
        domainname := pp.getNext();
        Nlist(domainname);
      ELSIF pp.testNext("loadmap") THEN
        domainname := pp.getNext();
        LoadMap(domainname);
      ELSE
        RAISE ParseParams.Error;
      END;
    EXCEPT
      ParseParams.Error =>
        IO.Put("domain cmd parse error exception\n");
        IO.Put(CommandName & " " & CommandHelp & "\n");
        RETURN FALSE;
      | Glob.Error => IO.Put("link: bad filename " & filename & "\n");
        RETURN FALSE;
    END;
    RETURN TRUE;
  END Run;

VAR
  domainNS: NameServer.T;

PROCEDURE Init(<*UNUSED*> verbose: BOOLEAN) =
  <*FATAL NameServer.Error*>
  BEGIN
    domainNS := NameServer.Lookup(NIL, "/../svc/domains");
  END Init;

BEGIN
  Init(TRUE);
END DomainCmd.
