MODULE NetText;
IMPORT Salnet;
IMPORT Fmt, IO, Lex, Scan, Text;

PROCEDURE FmtIp(ip:Salnet.IpAddr): TEXT =
  BEGIN
    WITH ipChars = VIEW(ip, ARRAY[0..3] OF CHAR) DO 
      RETURN  Fmt.Int(ORD(ipChars[0]))& "."&
	      Fmt.Int(ORD(ipChars[1]))& "."&
	      Fmt.Int(ORD(ipChars[2]))& "."&
	      Fmt.Int(ORD(ipChars[3]));
    END;
  END FmtIp;

PROCEDURE FmtEther(ether:Salnet.EtherAddr): TEXT =
  BEGIN
    WITH etherChars = VIEW(ether, ARRAY[0..5] OF CHAR) DO 
      RETURN  Fmt.Unsigned(ORD(etherChars[0]))& ":"&
	      Fmt.Unsigned(ORD(etherChars[1]))& ":"&
	      Fmt.Unsigned(ORD(etherChars[2]))& ":"&
	      Fmt.Unsigned(ORD(etherChars[3]))& ":"&
	      Fmt.Unsigned(ORD(etherChars[4]))& ":"&
	      Fmt.Unsigned(ORD(etherChars[5]));
    END;
  END FmtEther;

PROCEDURE TextToEther(etherText: TEXT;VAR (*OUT*) ether: Salnet.EtherAddr) =
  VAR
    offset,dot:=0;
  BEGIN
    WITH etherChars = VIEW(ether, ARRAY[0..5] OF CHAR) DO 
      FOR i := 0 TO 4 DO
	dot := Text.FindChar(etherText,':',offset);
	TRY
	  etherChars[i]:=VAL(Scan.Unsigned(Text.Sub(etherText,offset,dot-offset)),CHAR);
	EXCEPT
	  Lex.Error => IO.Put("Not an ether addr: "&etherText);
	  RETURN;
	END;
	offset:=dot+1;
      END;
      TRY
	etherChars[5]:=VAL(Scan.Unsigned(Text.Sub(etherText,offset,offset)),CHAR);
      EXCEPT
	Lex.Error => IO.Put("Not an ether addr: "&Text.Sub(etherText,offset)&"[\n");
	RETURN;
      END;
    END;
  END TextToEther;

PROCEDURE TextToIp(ipText: TEXT): Salnet.IpAddr =
  VAR
    ip: Salnet.IpAddr;
    offset,dot:=0;
  BEGIN
    WITH ipChars = VIEW(ip, ARRAY[0..3] OF CHAR) DO 
      FOR i := 0 TO 2 DO
        dot := Text.FindChar(ipText,'.',offset);
        TRY
          ipChars[i]:=VAL(Scan.Int(Text.Sub(ipText,offset,dot-offset)),CHAR);
        EXCEPT
          Lex.Error => IO.Put("Not an ip addr: "&ipText);
	  RETURN 0;
        END;
	offset:=dot+1;
      END;
      TRY
        ipChars[3]:=VAL(Scan.Int(Text.Sub(ipText,offset)),CHAR);
      EXCEPT
        Lex.Error => IO.Put("Not an ip addr: "&ipText);
        RETURN 0;
      END;
    END;
    RETURN ip;
  END TextToIp;

BEGIN
END NetText.
