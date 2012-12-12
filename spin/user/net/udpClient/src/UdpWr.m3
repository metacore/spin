(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 12-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Converted to using Net.BYTE rather than CHAR.
 *	This CHAR != BYTE business is really a headache.
 *
 * 13-Dec-95  Charlie Garrett (garrett) at the University of Washington
 *      Created.
 *)

MODULE UdpWr;

IMPORT WrClass, IpPktFormat, UdpPktFormat, Net, NetDb, Mbuf, UdpGen;

REVEAL
  T = Public BRANDED OBJECT
    conn: Connection;
  OVERRIDES
    seek      := Seek;
    putString := PutString;
    flush     := Flush;
    init      := ObjInit;
  END;

PROCEDURE ObjInit (wr: T; d: Destination): T =
  BEGIN
    WrClass.Lock(wr);
    TRY
      wr.conn.ip.saddr := NetDb.GetHostByName(NetDb.GetHostName());
      wr.conn.ip.daddr := d.ipaddr;
      wr.conn.ip.tot_len := 0;
      wr.conn.ip.protocol := IpPktFormat.IPPROTO_UDP;

      wr.conn.udp.sport := Net.htons(8181);
      wr.conn.udp.dport := d.port;

      wr.buff := NEW(REF ARRAY OF CHAR, 1);
      wr.st := 0;
      wr.cur := 0;
      wr.lo := 0;
      wr.hi := 1;
      wr.closed := FALSE;
      wr.seekable := FALSE;
      wr.buffered := FALSE;
    FINALLY
      WrClass.Unlock(wr);
    END;
    RETURN wr;
  END ObjInit;

PROCEDURE New(d: Destination): T = BEGIN RETURN NEW(T).init(d); END New;

PROCEDURE Seek(wr: T; n: CARDINAL) RAISES {} =
  BEGIN
    (* Only the PutChar procedure uses the buffer and it will
       increment wr.cur by 1. We adjust the lo and hi values
       to make the wr.cur offset always equal wr.lo so that 
       PutChar always write to wr.buff[0]. *)
    wr.lo := n;
    wr.hi := n + 1;
  END Seek;

PROCEDURE GetMbuf(totlen:CARDINAL):Mbuf.T = 
  VAR m: Mbuf.T;
  BEGIN
    <* ASSERT FALSE *>
(*
    m := Mbuf.m_get(Mbuf.M_WAIT, Mbuf.MT_DATA);
    m.mh_hdr.mh_len := totlen;
    IF totlen < Mbuf.MLEN THEN
      (* do nothing *)
    ELSIF totlen < Mbuf.MCL2KBYTES THEN
      Mbuf.MCLGET2(m,Mbuf.M_WAIT);
    ELSIF totlen < Mbuf.MCLBYTES THEN
      Mbuf.MCLGET(m,Mbuf.M_WAIT);
    ELSE
      IO.Put("PANIC UdpRpc.getmbuf size request too large.\n");
      RETURN NIL;
    END;
*)
    Mbuf.M_ALIGN(m,totlen);
    RETURN m;
  END GetMbuf; 

PROCEDURE PutString(wr: T; READONLY a: ARRAY OF CHAR) RAISES {} =
  VAR
    data: Mbuf.T;
  BEGIN
    (* Send straight to output *)
    (* Because the UdpWr.T is unbuffered, there won't be any unflushed
       characters in the buffer, so we can ignore it. *)
    wr.conn.ip.tot_len := 0;
    wr.conn.udp.len := BYTESIZE(UdpPktFormat.Header) + NUMBER(a);

    data := GetMbuf(NUMBER(a));
    WITH payload = Mbuf.Array(data) DO
      IF NUMBER(payload^) < NUMBER(a) THEN RETURN; END;
      FOR i := FIRST(a) TO LAST(a) DO
        payload[i] := a[i];
      END;
      (* SUBARRAY(payload^, 0, NUMBER(a)) := aBytes;*)
    END;

    UdpGen.PacketSend(wr.conn.ip, wr.conn.udp, data);
  END PutString;

PROCEDURE Flush(wr: T) RAISES {} =
  VAR
    data : Mbuf.T;
  BEGIN
    (* Send straight to output *)
    IF wr.cur > wr.lo THEN
      wr.conn.ip.tot_len := 0;
      wr.conn.udp.len := BYTESIZE(UdpPktFormat.Header) + NUMBER(wr.buff^);

      data := GetMbuf(NUMBER(wr.buff^));
      WITH payload = Mbuf.Array(data) DO
        IF NUMBER(payload^) < NUMBER(wr.buff^) THEN RETURN; END;
        FOR i := FIRST(wr.buff^) TO LAST(wr.buff^) DO
          payload[i] := wr.buff[i];
        END;
      END;

      UdpGen.PacketSend(wr.conn.ip, wr.conn.udp, data);
      wr.lo := wr.cur;
    END;
  END Flush;

BEGIN
END UdpWr. 
