(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 25-Jul-97  Tsutomu Owa (owa) at the University of Washington
 *	Quick fix for Create() when file.id is <0,,,>
 *
 * 14-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

MODULE InodePager;
IMPORT InodePagerInterface;
IMPORT IO;
IMPORT Fmt;
IMPORT File;
IMPORT Error;
IMPORT PagerObject;
IMPORT MemoryObject;
IMPORT CacheObject;
IMPORT VMTypes;
IMPORT VMError;
IMPORT CPU;
IMPORT RefRefTbl;
IMPORT FileStat;
IMPORT NameServer;
IMPORT NSName;
IMPORT Auth;
IMPORT VMDebug;
IMPORT Debugger;
IMPORT PhysAddr;
IMPORT Protection;
IMPORT Mutex;
IMPORT Clock;
IMPORT Word;

TYPE T = PagerObject.T OBJECT
  f: File.T;
  mObj: MemoryObject.T;
OVERRIDES
  destroy := Destroy;
  pageIn := PageIn;
  pageOut := PageOut;
  unmapNotify := UnmapNotify;
END;

VAR
  tbl := NEW(RefRefTbl.Default).init();

  (* Keys are all the File.Ts that currently have
     associated memory objects. Values are memory objects
     that back up the file. *)
  mu := NEW(MUTEX); (* Guards "tbl" *)

  lastgtick: INTEGER;
PROCEDURE DestroyOldMemoryObjects () =
  VAR
    r1, r2: REFANY;
    gtick := Clock.ReadTicks();
    itr: RefRefTbl.Iterator;
    mObj: MemoryObject.T;
  BEGIN
    <*ASSERT Mutex.IsLocked(mu)*>
    IF Word.GT(gtick - lastgtick, 10000) THEN
      (* XXX change 10000 to some decent value. *)
      itr := tbl.iterate();
      WHILE itr.next(r1, r2) DO
	mObj := r2;
	IF mObj.stat().residentSize = 0 AND NOT mObj.isMapped() THEN
	  mObj.destroy();
	  (* the pager is deleted from the table later by the
	     MemoryObject.DestroyThread. *)
	END;
      END;
    END;
    lastgtick := gtick;
  END DestroyOldMemoryObjects;
  
PROCEDURE Create (file: File.T; cache: CacheObject.T;
		  READONLY xname: NameServer.Name): MemoryObject.T =
  VAR
    r: REFANY;
    mObj: MemoryObject.T;
    pager: PagerObject.T;
    stat: FileStat.T;
    name: TEXT;
  BEGIN
    LOCK mu DO 
      IF tbl.get(file, r) THEN
	RETURN r;
      END;

      DestroyOldMemoryObjects();
      
      name := NSName.ToText(xname);
      TRY
	IF VMDebug.DebugMessage THEN
	  IO.Put("inode pager create: " & name & ".\n");
	END;
	EVAL file.open(0); (* Keep the file from being closed while it is
			      still used by the memory object. *)
	file.stat(stat);

	mObj := NEW(MemoryObject.T);
	pager := NEW(T, f := file, mObj := mObj);
	EVAL mObj.init(stat.size DIV CPU.PAGESIZE, pager, cache, name);
	EVAL tbl.put(file, mObj);
      EXCEPT
      | Error.E(e) =>
	IO.PutError("InodePager.Create: " & e.message() & ".\n");
      | VMError.E(e) =>
	IO.PutError("InodePager.Create: " & VMError.Message(e) & ".\n");
      END;
      RETURN mObj;
    END;
  END Create;

PROCEDURE Print (t: T): TEXT =
  BEGIN
    RETURN NARROW(t.mObj, MemoryObject.T).print();
  END Print;
  
PROCEDURE PageIn (t: T; off: VMTypes.PageNumber;
		  <*UNUSED*>type: INTEGER; frame: PhysAddr.T;
		  VAR prot: Protection.T): PagerObject.ResultCode =
  VAR
    nRead: CARDINAL;
    rc := PagerObject.ResultCode.Success;
  PROCEDURE Callback (VAR data: PhysAddr.Content) =
    BEGIN
      IF VMDebug.DebugMessage THEN
	IO.Put("ipager pagein:" & Print(t) & "," & Fmt.Int(off));
      END;
      
      TRY
	nRead := t.f.read(data, off * CPU.PAGESIZE);
      EXCEPT
      | Error.E(e) =>
	IO.Put("inodepager pagein: " & e.message() & ".\n");
	rc :=  PagerObject.ResultCode.NotImplemented; (* XXX *)
      END;
	
      IF VMDebug.DebugMessage THEN
	IO.Put(".done\n");
      END;
    
      IF nRead < NUMBER(data) THEN
	IF VMDebug.DebugMessage THEN
	  IO.Put("InodePager.pagein eof.req " & Fmt.Int(NUMBER(data)) & ","
		 & "read " & Fmt.Int(nRead) & ".\n");
	END;
	(* XXX this is very inefficient. We need a fast array clear library. *)
	FOR i := nRead TO LAST(data) DO
	  data[i] := '\000';
	END;
      END;
    END Callback;
  BEGIN
    TRY
      prot := Protection.All;
      PhysAddr.Access(frame, Callback);
    EXCEPT
    | VMError.E(e) =>
      IO.Put("inodepager pagein: " & Fmt.Int(e) & ".\n");
      rc := PagerObject.ResultCode.NotImplemented; (* XXX *)
    END;
      
    RETURN rc;
  END PageIn;


PROCEDURE PageOut (t: T; off: VMTypes.PageNumber;
		   <*UNUSED*>frame: PhysAddr.T;
		   <*UNUSED*>dirty: BOOLEAN): PagerObject.ResultCode =
  BEGIN
    IF VMDebug.DebugMessage THEN
      IO.Put("ipager pageout:" & Print(t) & "," & Fmt.Int(off) & ".\n");
    END;

    (* Note: In theory the below call may deadlock, but it doesn't because
       all the method calls doesn't lock self. *)
    RETURN PagerObject.ResultCode.Success;
  END PageOut;

PROCEDURE Destroy (t: T) =
  VAR
    r: REFANY;
    status: BOOLEAN;
  BEGIN
    IF VMDebug.DebugMessage THEN
      IO.Put("inodepager destroy:" & Print(t) & ".\n");
    END;
    TRY
      t.f.close();
    EXCEPT
    | Error.E(e) =>
      IO.Put("inode pager close: " & e.message() & ".\n");
    END;
    LOCK mu DO
      status := tbl.delete(t.f, r);
      IF NOT status THEN
	IO.Put("something wrong in Inode pager destroy.\n");
	Debugger.Enter();
      END;
    END;
  END Destroy;

PROCEDURE UnmapNotify (<*UNUSED*>t: T;
		       <*UNUSED*>from: VMTypes.PageNumber;
		       <*UNUSED*>len: VMTypes.PageCount;
		       <*UNUSED*>space: REFANY;
		       <*UNUSED*>virtAddr: VMTypes.PageNumber) =
  BEGIN
  END UnmapNotify;

BEGIN
  EVAL InodePagerInterface.Export(NEW(Auth.AuthAlways));
  IO.Put("Inode pager installed\n");
END InodePager.
