(* HISTORY
 * 26-Mar-96  Charles Garrett (garrett) at the University of Washington
 *	A command to test the use of socket readers and writers.
 *
 *)

MODULE SocketTest;

IMPORT IO, Rd, Wr, Fmt, TCP, IP, ConnRW, Text;
IMPORT ParseParams, Thread, ThreadExtra;

PROCEDURE Run(pp: ParseParams.T): BOOLEAN =
  VAR
    type: TEXT;
    hostName: TEXT;
    port: INTEGER;
  BEGIN
    TRY
      pp.reset();
      pp.skipNext();
      type := pp.getNext();
      
      IF Text.Equal(type, "-client") THEN
        hostName := pp.getNext();
      END;
      
      port := pp.getNextInt();
    EXCEPT
    | ParseParams.Error => 
      IO.Put(CommandName & CommandHelp & "\n"); 
      RETURN FALSE;
    END;

    IF Text.Equal(type, "-server") THEN
      WITH arg = NEW(REF INTEGER) DO
        arg^ := port;
        EVAL ThreadExtra.PFork(ServerTest, arg);
      END;
    ELSIF Text.Equal(type, "-client") THEN
      IO.Put("Do Client " & hostName & "\n");
      WITH arg = NEW(REF IP.Endpoint) DO
        TRY
          arg^ := IP.NewEndpoint(hostName, port);
          EVAL ThreadExtra.PFork(ClientTest, arg);
        EXCEPT
        | IP.Error =>
          IO.PutError("IP.NewEndpoint failed. Could not create socket.\n");
        END;
      END;
    ELSE
      IO.Put(CommandName & CommandHelp & "\n"); 
    END;
    RETURN TRUE;
  END Run;

PROCEDURE ServerTest(arg: REFANY) : REFANY =
  VAR
    listener: TCP.Connector;
    socket: TCP.T;
    rd: Rd.T;
    wr: Wr.T;
    port := NARROW(arg, REF INTEGER)^;
  BEGIN
    (* First read some data from the socket and then write to it *)
    TRY
      listener := TCP.NewConnector(IP.Endpoint{IP.NullAddress, port});
      TRY
        socket := TCP.Accept(listener);
        TRY
          TRY
            rd := ConnRW.NewRd(socket);
            WHILE NOT Rd.EOF(rd) DO
              IO.Put(Fmt.Char(Rd.GetChar(rd)));
            END;
            socket.shutdownIn();
          EXCEPT
          | Rd.Failure =>
            IO.PutError("TCP server socket test failed with read failure.\n");
          | Rd.EndOfFile =>
            IO.PutError("TCP server socket test failed with EOF.\n");
          | Thread.Alerted =>
            IO.PutError("TCP server socket test failed with thread alerted.\n");
          END;
          
          TRY
            wr := ConnRW.NewWr(socket);
            FOR i := 0 TO 10 DO
              IO.Put("right back at you\n", wr);
            END;
            socket.shutdownOut();
          EXCEPT
          | Wr.Failure =>
            IO.PutError("TCP server socket test failed with write failure.\n");
          END;
        FINALLY
          TCP.Close(socket);
        END;
      FINALLY
        TCP.CloseConnector(listener);
      END;
    EXCEPT
    | IP.Error =>
      IO.PutError("TCP server socket test failed with IP error.\n");
    END;

    RETURN NIL;
  END ServerTest;

PROCEDURE ClientTest(arg: REFANY) : REFANY =
  VAR
    socket: TCP.T;
    rd: Rd.T;
    wr: Wr.T;
  BEGIN
    TRY
      socket := TCP.Connect(NARROW(arg, REF IP.Endpoint)^);
      TRY
        TRY
          wr := ConnRW.NewWr(socket);
          FOR i := 1 TO 100 DO
            IO.Put("speak up\n", wr);
          END;

          socket.shutdownOut();
        EXCEPT
        | Wr.Failure =>
          IO.PutError("TCP client socket test failed with write failure.\n");
        END;
        
        TRY
          rd := ConnRW.NewRd(socket);
          WHILE NOT Rd.EOF(rd) DO
            IO.Put(Fmt.Char(Rd.GetChar(rd)));
          END;
          socket.shutdownIn();
        EXCEPT
        | Rd.Failure =>
          IO.PutError("TCP client socket test failed with read failure.\n");
        | Rd.EndOfFile =>
          IO.PutError("TCP client socket test failed with EOF.\n");
        | Thread.Alerted =>
          IO.PutError("TCP client socket test failed with thread alerted.\n");
        END;
      FINALLY
        TCP.Close(socket);
      END;
    EXCEPT
    | IP.Error =>
      IO.PutError("TCP client socket test failed with IP error.\n");
    END;

    IO.Put("SocketTest -client done\n");

    RETURN NIL;
  END ClientTest;

BEGIN
END SocketTest.

