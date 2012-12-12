(*
 * Copyright 1996, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
	Init.m3

 	Init loads the initial extensions.
 *)
(* HISTORY
 * 31-May-97  David Becker at the University of Washington
 *      Added salnfs option
 *
 * 20-Mar-97  Tsutomu Owa at the University of Washington
 *	Changed FetchDep to Fetch as stcp works on both PC and alpha
 *
 * 03-Feb-97  Tsutomu Owa at the University of Washington
 *	Move Fetch() function to FetchDep module as it uses ALPHA_SPIN
 *	specific simple tcp engine. 
 *)

MODULE Init;
IMPORT BuildInfo, Text, IO, Fmt;
IMPORT Domain, Debugger, NameServer;
IMPORT Fetch;
(*
IMPORT Clock;
*)

PROCEDURE ScanLine (VAR (* IN *) obj: ARRAY OF CHAR;
		    VAR (* IN/OUT *) pos: INTEGER;
		    VAR (* OUT *) line: TEXT) =
 VAR
    cnt  : INTEGER := 0;
    start:= pos;
    end := NUMBER(obj);
  BEGIN
    WHILE pos < end AND obj[pos] # '\r' AND obj[pos] # '\n' DO
      INC(pos);
      INC(cnt);
    END;
    IF cnt # 0 THEN
      line := Text.FromChars(SUBARRAY(obj, start, pos - start));
    END;

    WHILE pos < end AND (obj[pos] = '\r' OR obj[pos] = '\n') DO
      INC(pos);
    END;
  END ScanLine;

PROCEDURE Lookup (domainname:TEXT; VAR d: Domain.T): BOOLEAN =
  BEGIN
    TRY
      d := NameServer.Lookup(NIL, "/../svc/domains/" & domainname);
      RETURN TRUE;
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

VAR
  home: TEXT;
  
PROCEDURE AddFile(extension:Domain.T; path:TEXT) =
  VAR
    buf : REF ARRAY OF CHAR;
    domain:Domain.T;
    bytes : INTEGER;
  BEGIN
    bytes := Fetch.Fetch(path,buf);
    IF bytes=0 THEN RETURN END;

    domain := Domain.Create("/spin/"&path,buf);
    Domain.Add(extension,domain);
    Debugger.RegisterDomain(domain);
  END AddFile;

(* pidgin script interpretter *)
PROCEDURE RunScript(VAR (* IN *) obj: ARRAY OF CHAR ) =
  VAR
    pos := 0;
    line: TEXT;
    cmd: TEXT;
    extension, domain: Domain.T;
    extName: TEXT;
    linkName: TEXT;
    end := NUMBER(obj);
    verbose := FALSE;
  BEGIN
    extension:=NIL;
    WHILE pos#end DO
      ScanLine(obj,pos,line);

      cmd := "boot tftp";
      IF Text.Equal(cmd,Text.Sub(line,0,Text.Length(cmd))) THEN
        Fetch.Config(line);
      END;

      cmd := "boot nfs";
      IF Text.Equal(cmd,Text.Sub(line,0,Text.Length(cmd))) THEN
        Fetch.Config(line);
      END;

      cmd := "boot http";
      IF Text.Equal(cmd,Text.Sub(line,0,Text.Length(cmd))) THEN
        Fetch.Config(line);
      END;

      cmd := "script ~";
      IF Text.Equal(cmd,Text.Sub(line,0,Text.Length(cmd))) THEN
	Script(home&Text.Sub(line, Text.Length(cmd),
			Text.Length(line)-Text.Length(cmd)));
      END;

      cmd := "domain create ";
      IF Text.Equal(cmd,Text.Sub(line,0,Text.Length(cmd))) THEN
	extName := Text.Sub(line, Text.Length(cmd),
			Text.Length(line)-Text.Length(cmd));
        extension := Domain.Create(extName,NIL);
	IO.Put(extName&" ");
      END;

      IF extName # NIL THEN
        cmd := "domain addfile "&extName&" ~";
        IF Text.Equal(cmd,Text.Sub(line,0,Text.Length(cmd))) THEN
          AddFile(extension, home&Text.Sub(line, Text.Length(cmd),
			Text.Length(line)-Text.Length(cmd)));
	  IO.Put(".");
        END;

        cmd := "domain link "&extName&" ";
        IF Text.Equal(cmd,Text.Sub(line,0,Text.Length(cmd))) THEN
	  linkName := Text.Sub(line, Text.Length(cmd),
			Text.Length(line)-Text.Length(cmd));
	  IF Text.Equal(linkName,extName) THEN
            Domain.Resolve(extension,extension);
	  ELSE
            IF NOT Lookup(linkName,domain) THEN
              IO.Put("Domain " & linkName & " not found.\n");
            END;
            Domain.Resolve(extension,domain);
	  END;
        END;

        cmd := "domain run "&extName;
        IF Text.Equal(cmd,Text.Sub(line,0,Text.Length(cmd))) THEN
          IF Domain.FullyResolved(extension) THEN
	    IO.Put(" link complete.\n");
            IF verbose THEN IO.Put("fully linked\n") END;
            EVAL Domain.Initialize(extension);
          ELSE
            IF verbose THEN IO.Put("linked failed\n") END;
            Domain.Resolve(extension,Domain.debug);
          END;
        END;
      END;
    END;
  END RunScript;

PROCEDURE Script(path:TEXT) =
  VAR
    script : REF ARRAY OF CHAR;
    bytes : INTEGER;
  BEGIN
    (*
      IO.Put("Init.Script: fetching "&path&"\n");
    *)
    bytes := Fetch.Fetch(path,script);
    IF bytes=0 THEN RETURN END;

    RunScript(SUBARRAY(script^, 0, bytes));
  END Script;

PROCEDURE LoadExtensions() =
  VAR
    version       : TEXT;
    buildDate     : TEXT;
    builder       : TEXT;
    thisTree      : TEXT;
    target        : TEXT;
    path	  : TEXT;
    spinpath	  : TEXT;
    mount_point   : TEXT;
    mount_pad     : TEXT;
    (*
    tick          : INTEGER;
    *)
  BEGIN
    BuildInfo.GetInfo(version, target, buildDate, builder, thisTree);
    mount_point := BuildInfo.GetMountPoint();
    mount_pad := BuildInfo.GetMountPad();
    Fetch.Init();
    spinpath := mount_point & "/" & mount_pad;
    IF Text.Equal(spinpath,Text.Sub(thisTree,0,Text.Length(spinpath))) THEN
      WITH rest = Text.Sub(thisTree, Text.Length(spinpath)),
           end = Text.FindCharR(rest, '/') DO
           IF end < 1 THEN
             home := "";
           ELSE
             home := Text.Sub(rest, 0, end); 
           END;
      END;
    ELSE
      home := builder;
    END;

    path := home&"/spin/user/scripts/init."&target;
    IO.Put("Init.Script: fetching "&path&"\n");
    
    (*
    tick := Clock.ReadTicks();
    *)
   

    Script(path);
    Fetch.FreeMemory();

    (* 1024 ticks/sec
    tick := Clock.ReadTicks() - tick;
    IO.Put("Init.Script: took " & Fmt.Int(tick DIV 1024) & " sec\n");
    *)

  END LoadExtensions;

BEGIN
  LoadExtensions();
END Init.
