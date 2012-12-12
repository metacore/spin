(* 
 * Copyright 1994 - 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)

MODULE Ether6Client;
IMPORT Dispatcher;

IMPORT IO, ParseParams, Commands, Net, EtherPktFormat, Mbuf, Ip6,
       Ether, Ip6PktFormat;
IMPORT Ether6Gen, Ether6ClientLink; <* NOWARN *>

CONST eth_hdr_len = BYTESIZE(EtherPktFormat.T);

PROCEDURE ETHER6CLIENT(
    closure: REFANY; 
    pp: ParseParams.T): BOOLEAN =
  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* etherclassification *)
      IF pp.testNext("debug") THEN
      ELSIF pp.testNext("uninstall") THEN
        Uninit(FALSE);
      ELSE
        Commands.ParseError(closure);
      END;
    EXCEPT
      ParseParams.Error => 
      Commands.ParseError(closure);
    END;
    RETURN TRUE;
  END ETHER6CLIENT; 

PROCEDURE ETHER6GEN(
    closure: REFANY; 
    pp: ParseParams.T): BOOLEAN =
  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* Ether6Gen *)
      IF pp.testNext("-debug") THEN
        Ether6Gen.debug_level := Net.MapDebug(pp.getNextInt());
      ELSE
        Commands.ParseError(closure);
      END;
    EXCEPT
      ParseParams.Error => Commands.ParseError(closure);
    END;
    RETURN TRUE;
  END ETHER6GEN;

FUNCTIONAL
PROCEDURE Guard_IP6(
    <*UNUSED*> 
    packet : Mbuf.T; 
    curr   : Mbuf.T; 
    offset : CARDINAL):BOOLEAN =
  BEGIN
    WITH etherHeaderBuf = SUBARRAY(Mbuf.Array(curr)^,offset, eth_hdr_len),
         etherHeader = VIEW(etherHeaderBuf,EtherPktFormat.T) 
     DO
      RETURN etherHeader.type = EtherPktFormat.ETHERTYPE_IP6;
    END;
  END Guard_IP6;

PROCEDURE PacketArrived_IP6(
    <*UNUSED*>
    packet : Mbuf.T; 
    curr   : Mbuf.T; 
    offset : CARDINAL):BOOLEAN =
  BEGIN
    INC(offset,BYTESIZE(EtherPktFormat.T));
    IF offset >= curr.mh_hdr.mh_len THEN
      curr := curr.mh_hdr.mh_next;
      EVAL Ip6.PacketArrived(curr, curr, 0);
    ELSE
      (* XXX illegal hack !!! FUCK! *)
      INC(curr.mh_hdr.mh_data,BYTESIZE(EtherPktFormat.T));
      DEC(curr.mh_hdr.mh_len,BYTESIZE(EtherPktFormat.T));      
      EVAL Ip6.PacketArrived(curr, curr, 0);
      DEC(curr.mh_hdr.mh_data,BYTESIZE(EtherPktFormat.T));
      INC(curr.mh_hdr.mh_len,BYTESIZE(EtherPktFormat.T));      
    END;

    RETURN FALSE;
  END PacketArrived_IP6;

VAR
  bind  : ARRAY [0..0] OF REFANY;
  shell : ARRAY [0..1] OF REFANY;

PROCEDURE Init (verbose:BOOLEAN) = 
  BEGIN
    Ether6ClientLink.Init();
    Ether6Gen.Init(verbose);
    bind[0] := Ether.Install(Ether.PacketArrived,
                             Guard_IP6,
                             PacketArrived_IP6);
    shell[0] := Commands.Install(ETHER6CLIENT,
                                 Ether6ClientCommandName,
                                 Ether6ClientCommandHelp);
    shell[1] := Commands.Install(ETHER6GEN,
                                 Ether6GenCommandName,
                                 Ether6GenCommandHelp);
    IF verbose THEN IO.Put("EtherClient module initialized.\n"); END;
  END Init;

PROCEDURE ExceptionPrint(ec:Dispatcher.ErrorCode) = 
  BEGIN
    CASE ec OF
    | Dispatcher.ErrorCode.InvalidProcedure =>
      IO.Put("Trusted invalid procedure installed.\n");
    ELSE
      IO.Put("Trusted dispatcher install error.\n");
    END;
  END ExceptionPrint;

PROCEDURE Uninstall(binding: REFANY) =
  BEGIN
    WITH s = NARROW(binding,Dispatcher.Binding) DO
      TRY
        Dispatcher.Uninstall(s);
      EXCEPT
      | Dispatcher.Error(ec) => ExceptionPrint(ec);
      END;
    END;
  END Uninstall;

PROCEDURE Uninit(<*UNUSED*>verbose:BOOLEAN) = 
  BEGIN
    FOR i := FIRST(bind) TO LAST(bind) DO
      IF bind[i] # NIL THEN
        Uninstall(bind[i]);
        bind[i] := NIL;
      END;
    END;
    FOR i := FIRST(shell) TO LAST(shell) DO
      IF shell[i] # NIL THEN
        Commands.Uninstall(shell[i]);        
        shell[i] := NIL;
      END;
    END;
  END Uninit; 

CONST verbose = TRUE;
BEGIN
  Init(verbose);
END Ether6Client.
