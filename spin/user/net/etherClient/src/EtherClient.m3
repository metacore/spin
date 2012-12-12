(* 
 * Copyright 1994 - 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 10-Dec-97  Marc Fiuczynski (mef) at the University of Washington
 *	Added support to uninstall.
 *
 * 28-Jun-95  Marc Fiuczynski (mef) at the University of Washington
 *	Initialization module called started by the dynamic linker after
 *	the M3 code has been initialized.
 *)

MODULE EtherClient;

IMPORT IO, ParseParams, Commands, Net, IpPktFormat, EtherPktFormat,
       Ctypes;

IMPORT EtherArp, EtherClassification, EtherDefault,
       EtherGen, EtherClientLink; <* NOWARN *>

PROCEDURE ETHERCLASSIFICATION(
    closure: REFANY; 
    pp: ParseParams.T): BOOLEAN =
  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* etherclassification *)
      IF pp.testNext("-debug") THEN
        EtherClassification.debug_level := Net.MapDebug(pp.getNextInt());
      ELSIF pp.testNext("uninstall") THEN
        EtherClassification.Uninit(FALSE);
      ELSE
        Commands.ParseError(closure);
      END;
    EXCEPT
      ParseParams.Error => Commands.ParseError(closure);
    END;
    RETURN TRUE;
  END ETHERCLASSIFICATION;

PROCEDURE ETHERGEN(
    closure: REFANY; 
    pp: ParseParams.T): BOOLEAN =
  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* EtherGen *)
      IF pp.testNext("-debug") THEN
        EtherGen.debug_level := Net.MapDebug(pp.getNextInt());
      ELSE
        Commands.ParseError(closure);
      END;
    EXCEPT
      ParseParams.Error => Commands.ParseError(closure);
    END;
    RETURN TRUE;
  END ETHERGEN;

PROCEDURE ETHERDEFAULT(
    closure: REFANY; 
    pp: ParseParams.T): BOOLEAN =
  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* EtherDefault *)
      IF pp.testNext("-debug") THEN
        EtherDefault.debug_level := Net.MapDebug(pp.getNextInt());
      ELSE
        Commands.ParseError(closure);
      END;
    EXCEPT
      ParseParams.Error => Commands.ParseError(closure);
    END;
    RETURN TRUE;
  END ETHERDEFAULT;

PROCEDURE ETHERARP(
    closure: REFANY; 
    pp: ParseParams.T): BOOLEAN =

    CONST stat = ARRAY[FIRST(EtherArp.Status)..LAST(EtherArp.Status)] OF TEXT
      {"nascent","permanent","resolved","stale","duplicate"};

  PROCEDURE PrintEntry(READONLY entry: EtherArp.AddressMapping) = 
    VAR first : BOOLEAN;
    BEGIN
      IO.PutInt(VIEW(entry.protocolAddress[0],Ctypes.unsigned_char));
      IO.Put(".");
      IO.PutInt(VIEW(entry.protocolAddress[1],Ctypes.unsigned_char));
      IO.Put(".");
      IO.PutInt(VIEW(entry.protocolAddress[2],Ctypes.unsigned_char));
      IO.Put(".");
      IO.PutInt(VIEW(entry.protocolAddress[3],Ctypes.unsigned_char));
      IO.Put("\tat ");
      IF EtherArp.Status.Resolved IN entry.status THEN 
        first := TRUE;
        FOR i := FIRST(entry.hardwareAddress) TO LAST(entry.hardwareAddress) DO
          IF NOT first THEN IO.Put("-"); ELSE first := FALSE; END;
          IO.Putx(VIEW(entry.hardwareAddress[i],Ctypes.unsigned_char)); 
        END;
        IO.Put(" ");
      ELSE
        IO.Put("(incomplete) ");
      END;
      first := TRUE;
      FOR i := FIRST(EtherArp.Status) TO LAST(EtherArp.Status) DO
        IF i IN entry.status THEN
          IF NOT first THEN IO.Put("|"); ELSE first := FALSE; END;
          IO.Put(stat[i]);
        END;
      END;
      IO.Put(" pkt queue depth = ");
      IO.PutInt(entry.ifq.ifq_len);
      IO.Put("\n");
    END PrintEntry;

  BEGIN
    pp.reset();
    TRY 
      pp.skipNext(); (* ether *)
      IF pp.testNext("-debug") THEN
        (* set debug level *)
        EtherArp.debug_level := Net.MapDebug(pp.getNextInt());
      ELSIF pp.testNext("-dump") THEN
        EtherArp.CacheDump(PrintEntry);
      ELSIF pp.testNext("-del") THEN
        VAR 
            key: EtherArp.AddressMapping;
        BEGIN
          GetAddr(pp,key.protocolAddress);
          key.proType := Net.htons(EtherPktFormat.ETHERTYPE_IP);
          EtherArp.CacheDelete(key);
        END;
      ELSIF pp.testNext("uninstall") THEN
        EtherArp.Uninit(FALSE);
      ELSE
        Commands.ParseError(closure);
      END;
    EXCEPT
      ParseParams.Error => Commands.ParseError(closure);
    END;
   RETURN TRUE;
 END ETHERARP;

PROCEDURE GetAddr (
    pp       : ParseParams.T; 
    VAR addr : IpPktFormat.AddressArray)
  RAISES {ParseParams.Error} =
  BEGIN
    FOR i := FIRST(addr) TO LAST(addr) DO
      addr[i] := VAL(pp.getNextInt(), Net.BYTE);
    END;
  END GetAddr;

<*UNUSED*>
PROCEDURE GetEthAddr (
    pp       : ParseParams.T; 
    VAR addr : EtherPktFormat.Address)
  RAISES {ParseParams.Error} =
  BEGIN
    FOR i := FIRST(addr) TO LAST(addr) DO
      addr[i] := VAL(pp.getNextInt(), Ctypes.unsigned_char);
    END;
  END GetEthAddr;

PROCEDURE ETHERCLIENT(
    closure: REFANY; 
    pp: ParseParams.T): BOOLEAN =
  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* etherclient *)
      IF pp.testNext("uninstall") THEN
        Uninit(FALSE);
      ELSE
        Commands.ParseError(closure);
      END;
    EXCEPT
      ParseParams.Error => Commands.ParseError(closure);
    END;
    RETURN TRUE;
  END ETHERCLIENT;

VAR shell : ARRAY [0..4] OF REFANY;

PROCEDURE Init (verbose:BOOLEAN) = 
  BEGIN
    EtherClientLink.Init();
    EtherGen.Init(verbose);
    EtherClassification.Init(verbose);
    EtherArp.Init(verbose);
    (* EtherDefault.Init(verbose);*)

    shell[0] := Commands.Install(ETHERCLASSIFICATION,
                     EtherClassificationCommandName,
                     EtherClassificationCommandHelp);
    shell[1] := Commands.Install(ETHERGEN,
                     EtherGenCommandName,
                     EtherGenCommandHelp);
    shell[2] := Commands.Install(ETHERDEFAULT,
                     EtherDefaultCommandName,
                     EtherDefaultCommandHelp);
    shell[3] := Commands.Install(ETHERARP,
                     EtherArpCommandName,
                     EtherArpCommandHelp);
    shell[4] := Commands.Install(ETHERCLIENT,
                     "etherclient",
                     " uninstall");
    IF verbose THEN IO.Put("EtherClient initialized.\n"); END;
  END Init;

PROCEDURE Uninit(verbose:BOOLEAN) = 
  BEGIN
    FOR i := FIRST(shell) TO LAST(shell) DO
      IF shell[i] # NIL THEN
        Commands.Uninstall(shell[i]);
        shell[i] := NIL;
      END;
    END;
    EtherClassification.Uninit(FALSE);
    EtherArp.Uninit(FALSE);
    IF verbose THEN IO.Put("EtherClient unloaded.\n"); END;
  END Uninit;

CONST verbose = TRUE;
BEGIN
  Init(verbose);
END EtherClient.
