(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 28-Jul-97  Yasushi Saito (yasushi) at the University of Washington
 *	Changed NS interface.
 * 19-Oct-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed to use a table and allow host ip addresses to be added
 *	from the shell.
 *
 * 20-Jul-96  Brian Bershad (bershad) at the University of Washington
 *	Handle exception from Glob.Lookup.
 *
 * 02-Jul-96  Marc Fiuczynski (mef) at the University of Washington
 *	Added wigeon.
 *
 * 02-Jul-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed 'gethostname' to get localhostname from the Glob
 *	environment.
 *
 * 21-Jul-95  Marc Fiuczynski (mef) at the University of Washington
 *	This implementation of DNS for hostname -> ip address conversion
 *	either should communicate directly with a DNS server, or should
 *	maintain a table of hostname/ipaddr mapping.  
 *)

MODULE NetDb; 

IMPORT NetDbInterface, Net, IO, IpPktFormat, Glob, Shell, NameServer,
       ParseParams;

PROCEDURE PrintAddr(addr: IpPktFormat.Address) =
  BEGIN
    WITH ip = VIEW(addr,IpPktFormat.AddressArray) DO 
      IO.PutInt(ORD(ip[0]));
      IO.Put(".");
      IO.PutInt(ORD(ip[1]));
      IO.Put(".");
      IO.PutInt(ORD(ip[2]));
      IO.Put(".");
      IO.PutInt(ORD(ip[3]));
    END;
  END PrintAddr;

PROCEDURE GetAddr (
    pp: ParseParams.T; 
    VAR addr: IpPktFormat.Address)
  RAISES {ParseParams.Error} =
  BEGIN
    WITH tmp = VIEW(addr, IpPktFormat.AddressArray) DO
      tmp[0] := VAL(pp.getNextInt(), Net.BYTE);
      tmp[1] := VAL(pp.getNextInt(), Net.BYTE);
      tmp[2] := VAL(pp.getNextInt(), Net.BYTE);
      tmp[3] := VAL(pp.getNextInt(), Net.BYTE);
    END;
  END GetAddr;

PROCEDURE GetHostByName(machinename: TEXT):IpPktFormat.Address RAISES {HostNotFound} = 
  BEGIN
    TRY    
      WITH entry = NameServer.Lookup(hostNS, machinename) DO
	TYPECASE entry OF
	| NULL => 
	| REF IpPktFormat.Address(ip) => RETURN ip^;
	ELSE
	END;
      END;
    EXCEPT
    | NameServer.Error(ec) =>
      IF ec # NameServer.EC.NameNotFound THEN
        IO.PutError("NetDb.GetHostByName nameserver lookup returned ");
        IO.PutInt(ORD(ec)); 
        IO.Put(" unexpected error\n");
      END;
    END;
    RAISE HostNotFound;
  END GetHostByName;

(* XXX FIXME *)
PROCEDURE HostName(<*UNUSED*>name:TEXT) =
  BEGIN
    <* ASSERT FALSE *>
  END HostName;

PROCEDURE GetHostName (): TEXT =
  BEGIN
    TRY
      RETURN Glob.Lookup(Shell.SysVars(), "hostnamefull");
    EXCEPT
      Glob.Error => IO.Put("Where am I??\n"); RETURN NIL;
    END;
  END GetHostName;

PROCEDURE Add(pp: ParseParams.T) 
  RAISES{ParseParams.Error} =
  VAR 
    name: TEXT;
    addr: REF IpPktFormat.Address;
  BEGIN
    addr := NEW(REF IpPktFormat.Address);
    name := pp.getNext();
    GetAddr(pp,addr^);

    TRY
      NameServer.Attach(hostNS, name, addr);
    EXCEPT
    | NameServer.Error(ec) =>
      IO.PutError("NetDb.Add: " & name & ": " & NameServer.Fmt(ec) & ".\n");
    END;
  END Add;

PROCEDURE Del(pp: ParseParams.T) 
  RAISES{ParseParams.Error} =
  VAR 
    name: TEXT;
  BEGIN
    name := pp.getNext();
    TRY
      NameServer.Detach(hostNS, name);
    EXCEPT
    | NameServer.Error(ec) =>
      IO.PutError("NetDb.Del: " & name & ": " & NameServer.Fmt(ec) & ".\n");
    END;
  END Del;

PROCEDURE Lookup(pp: ParseParams.T) 
  RAISES{ParseParams.Error} =
  VAR 
    name : TEXT;
    addr : IpPktFormat.Address;
  BEGIN
    name := pp.getNext();
    TRY
      addr := GetHostByName(name);
      IO.Put(name & " = ");
      PrintAddr(addr);
      IO.Put("\n");
    EXCEPT
    | HostNotFound =>
      IO.PutError(name & " not found\n");
    END;
  END Lookup;

PROCEDURE Run(pp: ParseParams.T): BOOLEAN =
  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* route *)
      IF pp.testNext("-add") THEN
        Add(pp);
      ELSIF pp.testNext("-del") THEN
        Del(pp);
      ELSIF pp.testNext("-lookup") THEN
        Lookup(pp);
      ELSE
        Usage();
      END;
    EXCEPT
      ParseParams.Error => Usage();
    END;
   RETURN TRUE;
 END Run;

PROCEDURE Usage() = 
  BEGIN
    IO.Put("Usage: " & CommandName & ":" & CommandHelp & "\n");
  END Usage;

VAR hostNS : NameServer.T;
PROCEDURE Init(verbose:BOOLEAN) = 
  VAR
    svcNS: NameServer.T;
  BEGIN

    TRY 
      (* create the domain namespace *)
      svcNS := NameServer.Lookup(NIL,"/../svc");
      hostNS := NEW(NameServer.Default).init();
      NameServer.Attach(svcNS, "hosts", hostNS);
      IF verbose THEN IO.Put("DNS namespace initialized\n"); END;
    EXCEPT
    | NameServer.Error =>
      IF verbose THEN IO.PutError("DNS namespace initialization failed\n"); END;
      RETURN;
    END;

    EVAL NetDbInterface.Export(NIL);
  END Init;


BEGIN
  Init(TRUE);
END NetDb. 
