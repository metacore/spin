(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 04-Mar-96  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Shell interface to change the web servers parameters.
 *)
MODULE WWW;
IMPORT Httpd, FileCache, Text, IO, ParseParams;
IMPORT BlockList, Buffer;
IMPORT File, FileSystem;
IMPORT WWWCmd;
IMPORT Spy;

(* XXX (mef) *)
IMPORT TcpStuff;


PROCEDURE ShowBool(x: BOOLEAN) = 
  BEGIN
    IF x THEN
      IO.Put(" enabled.\n");
    ELSE
      IO.Put(" disabled.\n");
    END;
  END ShowBool;

PROCEDURE DisplayStatus() = 
  BEGIN
    IO.Put("Server caching is ");
    ShowBool(Httpd.useCaching);
    IO.Put("Server verbosity is ");
    ShowBool(Httpd.verbose);
    IO.Put("Server logging is ");
    ShowBool(Httpd.keeplog);
    IO.Put("Server debugging is ");
    ShowBool(Httpd.debug);
    IO.Put("Server thread priority is "); IO.PutInt(Httpd.serverpriority);
    IO.Put("\n");
  END DisplayStatus;

PROCEDURE FetchFromCFS(filename: TEXT; timer: Spy.T) =
  VAR
    filecache: FileCache.T;
    bl: BlockList.T;
    response: Buffer.T;
    blockno : CARDINAL := 0;
    size : CARDINAL;
  BEGIN
    filecache := Httpd.GetFileCache();
    Spy.Enter(timer);
    IF filecache.get(filename, bl) THEN
      REPEAT
        response := bl.retrieveBlock(blockno, size);
        bl.unlockBlock(blockno);
        INC(blockno);
      UNTIL response = NIL;
    ELSE
      IO.Put("This file was never in the cachefs\n");
    END;
    Spy.Exit(timer);
  END FetchFromCFS;

PROCEDURE FetchFromUFS(filename: TEXT; timer: Spy.T) =
  VAR
    fp: File.T;
    nread, offset: CARDINAL;
    response: Buffer.T;
  CONST
    BigBlockSize = 8000;
  BEGIN
    response := Buffer.Allocate(BigBlockSize);
    Spy.Enter(timer);
    TRY
      TRY
        fp := FileSystem.Open((* XXX 0, taken out by mef *) "/mnt" & filename);
        offset := 0;
        REPEAT
          (*
           * We place the buffers in the cache when CacheBuffer is called.
           *)
	  nread := fp.read(response.data^, offset);
          offset := offset + nread;
        UNTIL nread # BigBlockSize; (* short read from disk signals the end *)
      FINALLY
        fp.close();
      END;
    EXCEPT
    ELSE
      IO.Put("Some exception happened\n");
    END;
    Spy.Exit(timer);
  END FetchFromUFS;

PROCEDURE Run (pp: ParseParams.T): BOOLEAN =
  VAR
    option     : TEXT;
    swapdevice : TEXT;
    spy        : Spy.T;
  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* skip command name *)
      option := pp.getNext();
      IF Text.Equal(option, "showconf") THEN
        DisplayStatus();
      ELSIF Text.Equal(option, "debug") THEN
        Httpd.debug := NOT Httpd.debug;
        (* XXX taken out by mef ExtentPrivate.debug := Httpd.debug; *)
        DisplayStatus();
      ELSIF Text.Equal(option, "shutdown") THEN
        IO.Put("Signalling server and uninstalling shell command\n");
        IO.Put("You may have to send a final request to the server to ");
        IO.Put("wake it up\n");
        Httpd.serverShouldQuit := TRUE;
        WWWCmd.Uninstall();
      ELSIF Text.Equal(option, "swapoff") THEN
        Httpd.useCaching := FALSE;
        DisplayStatus();
      ELSIF Text.Equal(option, "swapon") THEN
        swapdevice := pp.getNext();
        IO.Put("Swap device is " & swapdevice & "\n");
        TRY
          Httpd.GetFileCache().registerSwapDevice(swapdevice);
          Httpd.useCaching := TRUE;
        EXCEPT
          FileCache.SwapNotFound => 
             IO.Put("Swap error, caching is disabled.\n");
             Httpd.useCaching := FALSE;
        END;
      ELSIF Text.Equal(option, "pageout") THEN
        Httpd.GetFileCache().pack();
      ELSIF Text.Equal(option, "flush") THEN
        Httpd.GetFileCache().flush();
      ELSIF Text.Equal(option, "verbose") THEN
        Httpd.verbose := TRUE;
      ELSIF Text.Equal(option, "quiet") THEN
        Httpd.verbose := FALSE;
      (*
       * Log related commands 
       *)
      ELSIF Text.Equal(option, "log") THEN
        Httpd.logfile := pp.getNext();
        Httpd.keeplog := TRUE;
      ELSIF Text.Equal(option, "logoff") THEN
        Httpd.keeplog := FALSE;
      (*
       * Timing related commands
       *)
      ELSIF Text.Equal(option, "fetchcfs") THEN
        spy := Spy.Create("CFS");
        FetchFromCFS(pp.getNext(), spy);
      ELSIF Text.Equal(option, "fetchufs") THEN
        spy := Spy.Create("UFS");
        FetchFromUFS(pp.getNext(), spy);
      ELSIF Text.Equal(option, "priority") THEN
        Httpd.serverpriority := pp.getNextInt();

      ELSIF Text.Equal(option, "srv") THEN
        (* XXX (mef) *)
        TcpStuff.CreateListen(pp.getNextInt());

      ELSE
        IO.Put("Command unknown.\n");
        IO.Put(CommandName & CommandHelp & "\n");
        RETURN FALSE;
      END;
      RETURN TRUE;
    EXCEPT
      ParseParams.Error => IO.Put(CommandName & CommandHelp & "\n");
      RETURN FALSE;
    END;
  END Run;

BEGIN
END WWW.
