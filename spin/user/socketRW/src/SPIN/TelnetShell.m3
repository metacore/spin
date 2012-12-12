(* HISTORY
 *
 * 07-Aug-97  Robert Grimm (rgrimm) at the University of Washington
 *      Switched over to new security manager.
 *
 * 11-Feb-97  Robert Grimm (rgrimm) at the University of Washington
 *      Replaced Identity with SecurityContext
 *
 * 18-Jul-96  Brian Bershad (bershad) at the University of Washington
 *	Use SpinShell command loop.
 *
 * 26-Mar-96  Charles Garrett (garrett) at the University of Washington
 *	This command listens for connections on the specified port 
 *      and then spawns a shell which reads from and writes to the
 *      connection. This allows you to telnet to the machine and talk
 *      to the spin shell.
 *
 *)

MODULE TelnetShell;

IMPORT IO, Rd, Wr, TCP, IP, ConnRW;
IMPORT ParseParams, ThreadExtra, Shell, Fingerprint;
IMPORT SecurityManager, Glob;
(* IMPORT Fmt; *)

VAR telnetCount: CARDINAL := 0;

PROCEDURE Run(pp: ParseParams.T): BOOLEAN =
  VAR
    port: INTEGER;
  BEGIN
    TRY
      pp.reset();
      pp.skipNext();
      port := pp.getNextInt();
    EXCEPT
    | ParseParams.Error => 
      IO.Put(CommandName & CommandHelp & "\n"); 
      RETURN FALSE;
    END;

    WITH arg = NEW(REF INTEGER) DO
      arg^ := port;
      EVAL ThreadExtra.PFork(ListenForConnections, arg);
    END;
    
    RETURN TRUE;
  END Run;

PROCEDURE ListenForConnections(arg: REFANY) : REFANY =
  VAR
    listener: TCP.Connector;
    socket: TCP.T;
    port := NARROW(arg, REF INTEGER)^;
  BEGIN
    (* Every time a connection is made on this socket, spawn a shell
       which reads and writes to whoever made the connection.  Each
       new shell runs as a separate identity with its own set of
       shell variables initialized to the default system variables.
       (Starting to understand what we need in  terms of contexts.
     *)
    TRY
      listener := TCP.NewConnector(IP.Endpoint{IP.NullAddress, port});
      TRY
        LOOP
          socket := TCP.Accept(listener);
          (* XXX *)
          IO.Put("Got connetion... starting a shell.\n");

          (* Security Change Me : init new context here *)

          INC(telnetCount);	(* LOCK LOCK *)
          EVAL SecurityManager.SetCurrentProperty( Shell.SHELLVARS,
                                                   Glob.New(Shell.SysVars()));
          EVAL ThreadExtra.PFork(StartAShell, socket);
        END;
      FINALLY
        TCP.CloseConnector(listener);
      END;
    EXCEPT
    | IP.Error =>
      IO.PutError("Failure while listening for connections.\n");
    END;

    RETURN NIL;
  END ListenForConnections; 

(* Remember the password as a fingerprint so that casual browsers cannot
   read it. *)
(*
CONST
  ShellPassword = Fingerprint.T{byte := ARRAY [0..7] OF BITS 8 FOR [0..255] 
                                          {16_d3, 16_21, 16_9c, 16_3d, 
                                           16_f7, 16_0c, 16_dc, 16_60}};
*)
CONST
  TelnetPrompt : TEXT = "telnet-spin>";

PROCEDURE StartAShell(arg: REFANY) : REFANY =
  VAR
    socket: TCP.T;
    oldRd: Rd.T;
    oldWr: Wr.T;
    password : TEXT;
  BEGIN
    (* XXX *)
    IO.Put("In StartAShell\n");


    TRY
      socket := NARROW(arg, TCP.T);
      oldRd := ThreadExtra.SetRdSelf(ConnRW.NewRd(socket));
      oldWr := ThreadExtra.SetWrSelf(ConnRW.NewWr(socket));

      (* Request a password from the user before proceding *)
      REPEAT
        TRY
          IO.Put("Password:");
          password := IO.ReadLine(echo := FALSE);
        EXCEPT
          (* Careful, this return will go to the finally block which
             will then return for real. *)
        | IO.Error => RETURN NIL;
        END;
      UNTIL TRUE; (*Fingerprint.Equal(Fingerprint.FromText(password), ShellPassword);*)


      Glob.SetVariable(Shell.Vars(), "prompt",TelnetPrompt);	
      Shell.CommandLoop(showPrompt := TRUE, echoChars := TRUE);
    FINALLY
      TCP.Close(socket);
    END;
    EVAL ThreadExtra.SetRdSelf(oldRd);
    EVAL ThreadExtra.SetWrSelf(oldWr);

    (* XXX *)
    IO.Put("Exiting StartAShell\n");
    RETURN NIL;
  END StartAShell;


BEGIN
END TelnetShell. 

