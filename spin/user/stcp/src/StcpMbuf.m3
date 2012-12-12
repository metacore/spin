(*
 * Copyright 1995-1996, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 17-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	Don't know why.  But unalignedAccess errors went away
 *	by changing pktdat.  This looks like uncovered by m_gethdr().
 *
 * 09-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	Change interface names so that these don't conflict w/
 *	those used by user/net.
 *
 * 18-Mar-97  Tsutomu Owa (owa) at the University of Washington
 *	Reverted If structure defined as urt/urtcore/src.
 *
 * 03-Feb-97  Tsutomu Owa at the University of Washington
 *	Copied from user/urt/urtcore/src and added overflow check to
 *	mbstat.m_mbufs and mbstat.m_mtypes.
 *
 * 22-Nov-96  becker at the University of Washington
 *	Change to TrackStrand from Spy
 *
 * 24-Oct-96  Marc Fiuczynski (mef) at the University of Washington
 *	verify in checksum that an mbuf array is not zero length.
 *
 * 19-Oct-96  Marc Fiuczynski (mef) at the University of Washington
 *	Updated to be SAL independent.
 *
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	make Array FUNCTIONAL so that net guards can call it
 *
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	New interface to interrupt handling.
 *
 * 13-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	Added M_LEADINGSPACE procedure.
 *
 * 15-May-96  Marc Fiuczynski (mef) at the University of Washington
 *	Add Spy name to the mbuf isr thread.
 *
 * 26-Apr-96  Marc Fiuczynski (mef) at the University of Washington
 *	Restructured StcpMbuf.AsyncDeliver to more precisely capture exceptions
 *	generated by the upcall to the clients free method.
 *
 * 24-Apr-96  Marc Fiuczynski (mef) at the University of Washington
 *	Passing offset to the checksum method.
 *
 * 20-Apr-96  Marc Fiuczynski (mef) at the University of Washington
 *	Added support for mbuf method upcalls. Right now there is the
 *	"free" and "csum" upcall.  The plan for the future is for the
 *	StcpMbuf.T to become an object which can be extended to support
 *	arbitrary, application-specific upcalls.  This will happen when
 *	uncollectable and collectable heaps have been merged into one.
 *      M_M3FREE flag is now called M_M3METHODS.
 *
 * 12-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Setting M_M3FREE flag in mbuf so that mbuf free upcalls have
 *	correct parameters.
 *
 * 02-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Eliminated the need for some of the dependence on C code.
 *	
 * 31-Jan-96  Marc Fiuczynski (mef) at the University of Washington
 *	Made the Isr() function safe wrt ThreadExtra.
 *
 * 09-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Made it possible to use large arrays as containers of smaller
 *	amounts of information to be wrapped in an StcpMbuf.
 *
 * 04-Jan-96  Marc Fiuczynski (mef) at the University of Washington
 *	Added support to handle async mbuf free upcalls.
 *
 * 25-Jul-95  Marc Fiuczynski (mef) at the University of Washington
 *	Created. To cooperate with BSD style uipc_mbuf.c.
 *)

UNSAFE (* to import StcpMbufExtern *)
MODULE StcpMbuf EXPORTS StcpMbuf, StcpMbufPublic;
IMPORT TrackStrand;

IMPORT Sema, IO, CPU, CPUPrivate, SpinException, Thread,
       ThreadExtra, Word, Ctypes, StcpMbufExtern, StcpNet, StcpMbufDep;
IMPORT StrongRef; <*NOWARN*>
IMPORT StcpEtherDev;

CONST useStrongRef = TRUE;

(* DEC-OSF REF COUNTing *)
TYPE ext_refqT = RECORD
  forw : UNTRACED REF ext_refqT;
  back : UNTRACED REF ext_refqT;
END;

(* description of external storage mapped into mbuf, valid if M_EXT set *)
TYPE m_extT = RECORD
  ext_buf     : Word.T; 
  (* pointer to buffer *)

  ext_methods : Word.T;
  (* pointer to method table or to free handler *)

  ext_size    : Word.T;
  (* size of the buffer *)

  ext_m3oa    : Word.T;
  (* pointer to m3 open array header *)

  ext_arg     : Word.T; 
  (* pointer additional argument passed in by user. *)

  ext_ref     : ext_refqT;
  (* reference count linked list (DEC-OSF REF COUNTing) *)

  (* XXX on the x86 there is another field here. 
     See sal/i386_freebsd/kern/uipc_mbuf.c (ulbright)
     Fix later when we put Mbufs into spincore/src/sal *)
END;

CONST extOffset = BYTESIZE(pkthdrT);
      (* offset of ext in the mbuf M_Dat area *)
(* XXX. why pktdat points to pkthdrT+ext_refqT?
	why do I need to align pktdat? 
CONST pktdat = BYTESIZE(pkthdrT) + BYTESIZE(ext_refqT);
 *)
CONST pktdat = BYTESIZE(pkthdrT);
      (* offset to data portion of mbuf after pkthdr and ext *)

CONST M_MBUF = StcpMbufDep.M_MBUF;                       (* malloc mbuf type *)
CONST MALLOCBUCKETINDEX = StcpMbufDep.MALLOCBUCKETINDEX; (* based on 256bytes mbuf size *)


<*INLINE*>
FUNCTIONAL PROCEDURE Array(m: T): UNTRACED REF ARRAY OF CHAR =
  TYPE OpenArrayHeader = UNTRACED REF RECORD
    start: ADDRESS;
    len  : INTEGER;
  END;
  VAR oah : OpenArrayHeader;
  BEGIN
    (* sanity check *)
    IF m = NIL THEN RETURN NIL; END;

    (* look at the mbuf mh_data and mh_len field as an OpenArrayHeader *)
    oah := LOOPHOLE(ADR(m.mh_hdr.mh_data),OpenArrayHeader);

    (* return the openarrayheader as m3 untraced array *)
    RETURN LOOPHOLE(oah,UNTRACED REF ARRAY OF CHAR);
  END Array;

<* INLINE *>
PROCEDURE MclGetx(
    methods :Word.T;
    arg1    :Word.T;
    arg2    :Word.T;
    addr    :Word.T;
    len     :CARDINAL;
    wait    :HowT): T =
  VAR mbuf: T;
  BEGIN
    (* allocate an mbuf header *)
    mbuf := m_gethdr(wait,MT_DATA);
    IF mbuf = NIL THEN RETURN NIL; END;
    
    (* set the EXT flag bit *)
    mbuf.mh_hdr.mh_flags := Word.Or(mbuf.mh_hdr.mh_flags, M_EXT);

    WITH ext = LOOPHOLE(ADR(mbuf.M_dat[FIRST(mbuf.M_dat)])+extOffset,UNTRACED REF m_extT) DO

      mbuf.mh_hdr.mh_data := LOOPHOLE(addr, Word.T);
      mbuf.mh_hdr.mh_len  := len;

      ext.ext_buf      := addr;
      ext.ext_size     := len;
      ext.ext_methods  := methods;
      ext.ext_arg      := arg1;
      ext.ext_m3oa     := arg2;
      ext.ext_ref.forw := ADR(ext.ext_ref);
      ext.ext_ref.back := ADR(ext.ext_ref);
    END;
    RETURN mbuf;
  END MclGetx;

PROCEDURE MclGetOa(
    buffer: REF ARRAY OF CHAR; 
    length: CARDINAL; 
    methods: Methods;
    desc: REFANY) : T 
  RAISES {LengthMismatch} =
  VAR 
    mbuf : T;
    userMethods: BOOLEAN := TRUE;
  BEGIN
    (* sanity checks *)
    IF buffer = NIL THEN RETURN NIL; END;
    IF NUMBER(buffer^) < length THEN RAISE LengthMismatch; END;

    IF methods = NIL THEN
      methods := globalMethods; 
      userMethods := FALSE;
    END;

    mbuf := MclGetx(
                LOOPHOLE(methods,Word.T), (* SafeConvert.RefAnyToWord(methods) *)
                LOOPHOLE(desc,Word.T),    (* SafeConvert.RefAnyToWord(desc) *)
                LOOPHOLE(buffer,Word.T),  (* SafeConvert.RefAnyToWord(buffer) *)
                LOOPHOLE(ADR(buffer[FIRST(buffer^)]), Word.T),
                length, 
                M_WAIT);

    IF mbuf = NIL THEN RETURN NIL; END;

    (* assumes GC will be fixed to unstrongref *)
    IF useStrongRef THEN 
      StrongRef.Add(buffer);
      StrongRef.Add(desc);
      StrongRef.Add(methods);
    END;

    (* set length field *)
    SetPktHdrLen(mbuf, length);

    (* set M3METHODS flag to free mbuf data area properly *)
    mbuf.mh_hdr.mh_flags := Word.Or(mbuf.mh_hdr.mh_flags,M_M3METHODS);
    mbuf.mh_hdr.mh_flags := Word.Or(mbuf.mh_hdr.mh_flags,M_FASTFREE);
    (* XXX not yet
    IF NOT userMethods THEN 
      mbuf.mh_hdr.mh_flags := Word.Or(mbuf.mh_hdr.mh_flags,M_FASTFREE);
    END;
    *)
    RETURN mbuf;
  END MclGetOa; 

(* Compute the amount of space available before the current start of
  data in an mbuf.  *)
<*UNUSED*>
PROCEDURE M_LEADINGSPACE(m:T): CARDINAL =
  BEGIN
    IF Word.And(m.mh_hdr.mh_flags, M_EXT) # 0 THEN
      <* ASSERT FALSE *>

    END;
    
    IF Word.And(m.mh_hdr.mh_flags, M_PKTHDR) # 0 THEN
      <* ASSERT FALSE *>      

    END;

    RETURN m.mh_hdr.mh_data - LOOPHOLE(ADR(m.M_dat[FIRST(m.M_dat)]),Word.T);
  END M_LEADINGSPACE;

<*INLINE*>
PROCEDURE M_PREPEND(m:T; plen:CARDINAL; how:HowT):T =
  BEGIN
    IF StcpMbufExtern.M_LEADINGSPACE(m) >= plen THEN
      (* pointer arithmetic *)
      m.mh_hdr.mh_data := m.mh_hdr.mh_data - plen;
      m.mh_hdr.mh_len  := m.mh_hdr.mh_len + plen;
    ELSE
      m := StcpMbufExtern.m_prepend(m,plen,how)
    END;
    IF ((m # NIL) AND (Word.And(m.mh_hdr.mh_flags, M_PKTHDR) = M_PKTHDR)) THEN
      SetPktHdrLen(m,GetPktHdrLen(m)+plen);
    END;
    RETURN m;
  END M_PREPEND;

<*INLINE*>
PROCEDURE M_ALIGN(m:T;len:CARDINAL) =
  BEGIN
    m.mh_hdr.mh_data := m.mh_hdr.mh_data + Word.And((MLEN - len), Word.Not(BYTESIZE(ADDRESS)-1));
  END M_ALIGN;

<*INLINE*>
PROCEDURE MH_ALIGN(m:T;len:CARDINAL) =
  BEGIN
    m.mh_hdr.mh_data := m.mh_hdr.mh_data + Word.And((MHLEN - len), Word.Not(BYTESIZE(ADDRESS)-1));
  END MH_ALIGN;

<*INLINE*>
PROCEDURE m_length(m:T): CARDINAL = 
  VAR 
    t   :T;
    len :CARDINAL := 0;
  BEGIN
    t := m;
    WHILE t # NIL DO 
      INC(len, t.mh_hdr.mh_len);
      t := t.mh_hdr.mh_next;
    END;
    RETURN len;
  END m_length; 

PROCEDURE SetPktHdrLen(m:T; len: CARDINAL) = 
  BEGIN
    (* XXX need to verify that this mbuf has the pkthdr flag set. *)
    WITH pkthdr = LOOPHOLE(ADR(m.M_dat[FIRST(m.M_dat)]),UNTRACED REF pkthdrT) DO
      pkthdr.len := len;
    END;
  END SetPktHdrLen;

PROCEDURE SetPktHdrRcvIf(m:T; dev: StcpEtherDev.T) = 
  BEGIN
    (* XXX need to verify that this mbuf has the pkthdr flag set. *)
    WITH pkthdr = LOOPHOLE(ADR(m.M_dat[FIRST(m.M_dat)]),UNTRACED REF pkthdrT) DO
      pkthdr.rcvif := LOOPHOLE(dev,ADDRESS);
    END;
  END SetPktHdrRcvIf;

PROCEDURE GetPktHdrLen(m:T) : CARDINAL = 
  BEGIN 
    (* XXX need to verify that this mbuf has the pkthdr flag set. *)
    WITH pkthdr = LOOPHOLE(ADR(m.M_dat[FIRST(m.M_dat)]),UNTRACED REF pkthdrT) DO
      RETURN pkthdr.len;
    END;
  END GetPktHdrLen;

PROCEDURE GetPktHdrRcvIf(m:T): StcpEtherDev.T =
  BEGIN
    (* XXX need to verify that this mbuf has the pkthdr flag set. *)
    WITH pkthdr = LOOPHOLE(ADR(m.M_dat[FIRST(m.M_dat)]),UNTRACED REF pkthdrT) DO
      RETURN LOOPHOLE(pkthdr.rcvif, StcpEtherDev.T );
    END;
  END GetPktHdrRcvIf;


PROCEDURE Deliver(mbuf: T) =
  VAR ext : UNTRACED REF m_extT;
  BEGIN
    TRY 
      IF Word.And(mbuf.mh_hdr.mh_flags,M_EXT) # 0 THEN
        ext := LOOPHOLE(ADR(mbuf.M_dat[FIRST(mbuf.M_dat)])+extOffset,UNTRACED REF m_extT);
        TRY
          IF FALSE (* MCLREFERENCED *) THEN
            IO.Put("StcpMbuf.Isr() mbuf ext without free function.\n");
            (* PANIC *)
          ELSIF Word.And(mbuf.mh_hdr.mh_flags,M_M3METHODS) # 0 THEN 
            TRY
              WITH methods = LOOPHOLE(ext.ext_methods, Methods) DO
                IF methods # NIL AND methods.free # NIL THEN
                  TRY
                    methods.free(LOOPHOLE(ext.ext_m3oa,REF ARRAY OF CHAR),
                                 ext.ext_size,
                                 LOOPHOLE(ext.ext_arg, REFANY));
                  EXCEPT
                  | SpinException.Exception =>
                    IO.Put("Exception fault in StcpMbuf.AsyncDeliver calling free method.\n");
                  END;
                END;
              END;
            FINALLY
              IF useStrongRef THEN
                StrongRef.Remove(LOOPHOLE(ext.ext_methods,REFANY));
                StrongRef.Remove(LOOPHOLE(ext.ext_arg,REFANY));
                StrongRef.Remove(LOOPHOLE(ext.ext_m3oa,REFANY));
              END;
            END;
          ELSE
            TRY
              WITH free = LOOPHOLE(ext.ext_methods, freeprocT) DO
                free(LOOPHOLE(ext.ext_buf,REF ARRAY OF CHAR),
                     ext.ext_size,
                     LOOPHOLE(ext.ext_arg, REFANY));
              END;
            EXCEPT
            | SpinException.Exception =>
              IO.Put("Exception fault in StcpMbuf.AsyncDeliver calling standard free upcall.\n");
            END;
          END;
        FINALLY

          (* Be nice to GC and get rid of ambiguous roots in 
             untraced heap by NILing out the references to 
             traced data.
          *)

          ext.ext_buf         := 0;
          ext.ext_arg         := 0;
          ext.ext_m3oa        := 0;
          ext.ext_methods     := 0;
          mbuf.mh_hdr.mh_data := 0;
        END;
      END
    FINALLY
      TRY
        (* turn off the EXT bit, so that m_free wont upcall. *)
        mbuf.mh_hdr.mh_flags := 0; (* Word.And(mbuf.mh_hdr.mh_flags,Word.Not(M_EXT)); *)
        EVAL m_free(mbuf);
      EXCEPT
        SpinException.Exception =>
        IO.Put("Exception fault in StcpMbuf.AsyncDeliver finally handler.\n");
      END;
    END;
  END Deliver;


(* internal service functions *)
PROCEDURE AsyncDeliver(arg: ThreadExtra.ArgT) : ThreadExtra.ResultT =
  VAR mbuf: T;
  BEGIN
    mbuf := NARROW(arg,REF T)^;
    Deliver(mbuf);
    RETURN NIL;
  END AsyncDeliver;

PROCEDURE Isr(<* UNUSED *> r: ThreadExtra.ArgT) : ThreadExtra.ResultT = 
  CONST High = CPUPrivate.InterruptClass.High;
  VAR mbuf, m: T;
      spl: CPU.InterruptLevel;
  BEGIN
    LOOP
      (* NIL out stack variable to be GC nice. *)
      mbuf        := NIL;
      m           := NIL;

      Sema.P(processStcpMbufs);

      (* dequeue operations has to be atomic wrt to interrupt handlers *)
      BEGIN spl:= CPUPrivate.SetInterruptMask(High);
        mbuf                  := StcpMbufExtern.mfreelater;
        StcpMbufExtern.mfreelater := NIL;
      END; CPUPrivate.RestoreInterruptMask(spl);        

      (* walk the mbuf list and call free *)
      WHILE mbuf # NIL DO
        m                := mbuf;
        mbuf             := mbuf.mh_hdr.mh_next;
        m.mh_hdr.mh_next := NIL;
        IF Word.And(m.mh_hdr.mh_flags,M_FASTFREE) = 0 THEN
          VAR mbufCarrier: REF T;
          BEGIN
            mbufCarrier := NIL;
            mbufCarrier := NEW(REF T);
            mbufCarrier^ := m;
            EVAL ThreadExtra.PFork(AsyncDeliver,mbufCarrier);
          END;
        ELSE
          Deliver(m);
        END;
      END;
    END;
  END Isr;

(* SchedIsr() 
 * Kick Isr thread. 
 * Called possible from interrupt handler in uipc_mbuf.c m_free().
 *)
PROCEDURE SchedIsr() = 
  BEGIN
    Sema.V(processStcpMbufs);
  END SchedIsr;

VAR
  processStcpMbufs : Sema.T;
  mbufThread   : Thread.T;

(* defined in bsd/uipc_mbuf.c *)
<* INLINE *>
PROCEDURE m_copydata(m:T; off:CARDINAL; len:CARDINAL; cp:T) =
  BEGIN
    StcpMbufExtern.m_copydata(m,off,len, cp.mh_hdr.mh_data);
  END m_copydata;

<*INLINE*>
PROCEDURE m_copym(m:T; off0:CARDINAL; len:CARDINAL; wait:HowT):T =
  BEGIN
    RETURN StcpMbufExtern.m_copym(m,off0,len,wait);
  END m_copym;

PROCEDURE m_free(m:T):T =
  BEGIN
    RETURN StcpMbufExtern.m_free(m);
  END m_free;

<*INLINE*>
PROCEDURE m_freem(m:T) =
  BEGIN
    StcpMbufExtern.m_freem(m);
  END m_freem;

<*INLINE*>
PROCEDURE m_getclr(canwait: HowT; type: Ctypes.int):T =
  BEGIN
    RETURN StcpMbufExtern.m_getclr(canwait,type);
  END m_getclr;

<* INLINE *>
PROCEDURE m_get_internal(canwait: HowT; type: Ctypes.int): T =
  BEGIN
    WITH addr = StcpMbufExtern.malloc(MSIZE,MALLOCBUCKETINDEX,M_MBUF,canwait),
         m = LOOPHOLE(addr,T),
         mh = m.mh_hdr
     DO
      IF m # NIL THEN
        mh.mh_next    := NIL;
        mh.mh_nextpkt := NIL;
        mh.mh_data    := LOOPHOLE(ADR(m.M_dat[FIRST(m.M_dat)]),Word.T);
        mh.mh_len     := 0;
        mh.mh_type    := type;
        mh.mh_flags   := 0;
      END;
      RETURN m;
    END;
  END m_get_internal;

<*INLINE*>
PROCEDURE m_get   (canwait: HowT; type: Ctypes.int):T =
  VAR m : T;
  BEGIN
    m := m_get_internal(canwait,type);
    IF m # NIL THEN
      (* XXX Do I really need to check range? *)
      (*     What the right way to handle this? *)
      (*     Is error caused by memory leak? *)
      IF StcpMbufExtern.mbstat.m_mbufs = LAST(Ctypes.unsigned_long) THEN
	StcpMbufExtern.mbstat.m_mbufs := 0;
      ELSE
        StcpMbufExtern.mbstat.m_mbufs := StcpMbufExtern.mbstat.m_mbufs + 1;
      END;
(* XXX 
      IF StcpMbufExtern.mbstat.m_mtypes[type] = LAST(Ctypes.unsigned_int) THEN
        StcpMbufExtern.mbstat.m_mtypes[type] := 0;
      ELSE
        StcpMbufExtern.mbstat.m_mtypes[type] :=StcpMbufExtern.mbstat.m_mtypes[type] + 1;
      END;
 *)
     (*
      StcpMbufExtern.mbstat.m_mbufs := StcpMbufExtern.mbstat.m_mbufs + 1;
      StcpMbufExtern.mbstat.m_mtypes[type] := StcpMbufExtern.mbstat.m_mtypes[type] + 1;
      *)
    ELSE
      m := m_retry(canwait,type);
    END;
    RETURN m;
    (* RETURN StcpMbufExtern.m_get   (canwait,type);*)
  END m_get;

<* INLINE *>
PROCEDURE m_gethdr_internal(canwait: HowT; type: Ctypes.int):T =
  BEGIN
    WITH addr = StcpMbufExtern.malloc(MSIZE,MALLOCBUCKETINDEX,M_MBUF,canwait),
         m = LOOPHOLE(addr,T),
         mh = m.mh_hdr
     DO
      IF m # NIL THEN
        mh.mh_next    := NIL;
        mh.mh_nextpkt := NIL;
        mh.mh_data    := LOOPHOLE(ADR(m.M_dat[FIRST(m.M_dat)+pktdat]),Word.T);
        mh.mh_len     := 0;
        mh.mh_type    := type;
        mh.mh_flags   := M_PKTHDR;
      END;
      RETURN m;
    END;
    (* <*NOWARN *> RETURN StcpMbufExtern.m_gethdr(canwait,type); *)
  END m_gethdr_internal;

<*INLINE*>
PROCEDURE m_gethdr(canwait: HowT; type: Ctypes.int):T =
  VAR m: T;
  BEGIN
    m := m_gethdr_internal(canwait,type);
    IF m # NIL THEN
      (* XXX Do I really need to check range? *)
      (*     What the right way to handle this? *)
      (*     Is error caused by memory leak? *)
      IF StcpMbufExtern.mbstat.m_mbufs = LAST(Ctypes.unsigned_long) THEN
	StcpMbufExtern.mbstat.m_mbufs := 0;
      ELSE
        StcpMbufExtern.mbstat.m_mbufs := StcpMbufExtern.mbstat.m_mbufs + 1;
      END;
(* XXX
      IF StcpMbufExtern.mbstat.m_mtypes[type] = LAST(Ctypes.unsigned_int) THEN
        StcpMbufExtern.mbstat.m_mtypes[type] := 0;
      ELSE
        StcpMbufExtern.mbstat.m_mtypes[type] :=StcpMbufExtern.mbstat.m_mtypes[type] + 1;
      END;
 *)
     (*
      StcpMbufExtern.mbstat.m_mbufs := StcpMbufExtern.mbstat.m_mbufs + 1;
      StcpMbufExtern.mbstat.m_mtypes[type] := StcpMbufExtern.mbstat.m_mtypes[type] + 1;
      *)
    ELSE
      m := m_retryhdr(canwait,type);
    END;
    RETURN m;
    (* <*NOWARN *> RETURN StcpMbufExtern.m_gethdr(canwait,type); *)
  END m_gethdr;

(* When MGET fails, ask someone to free space when short of memory
   and then re-attempt to allocate an mbuf. *)
PROCEDURE m_retry(canwait: HowT; type: Ctypes.int):T = 
  VAR m: T := NIL;
  BEGIN
    IF canwait # M_DONTWAIT THEN

      (* RECLAIM MBUFS and maintain stats that we reclaimed
         something. *)

      m := m_get_internal(canwait,type);

      (* IF m = NIL THEN maintain stats on dropped mbuf. END; *)

    END;
    RETURN m;
  END m_retry;

PROCEDURE m_retryhdr(canwait: HowT; type: Ctypes.int):T =
  VAR m: T;
  BEGIN
    m := m_retry(canwait,type);
    IF m # NIL THEN
      m.mh_hdr.mh_flags := Word.Or(m.mh_hdr.mh_flags, M_PKTHDR);
      m.mh_hdr.mh_data  := LOOPHOLE(ADR(m.M_dat[FIRST(m.M_dat)+pktdat]),Word.T);
    END;
    RETURN m;
  END m_retryhdr;

<*INLINE*>
PROCEDURE m_prepend(m:T; len:CARDINAL; how:HowT):T =
  BEGIN
    RETURN StcpMbufExtern.m_prepend(m,len,how);
  END m_prepend;

PROCEDURE m_pullup(n:T; len: CARDINAL):T =
  BEGIN
    RETURN StcpMbufExtern.m_pullup(n,len);
  END m_pullup;

EPHEMERAL
PROCEDURE FreeProc(
    <* UNUSED *> ext_buf  : REF ARRAY OF CHAR; 
    <* UNUSED *> ext_size : CARDINAL; 
    <* UNUSED *> ext_arg  : REFANY) = 
  BEGIN
    (* DOES NOTHING *)
  END FreeProc;

(* EPHEMERAL *)
<*UNUSED*>
PROCEDURE CsumProc(
    buf  : REF ARRAY OF CHAR; 
    size : CARDINAL; 
    <* UNUSED *> arg  : REFANY;
    csum : Ctypes.unsigned_short): Ctypes.unsigned_short = 
  BEGIN
    RETURN StcpNet.checksum(buf^,size,csum);
  END CsumProc;

PROCEDURE Checksum(
    m:T; 
    csum:Ctypes.unsigned_short; 
    len:CARDINAL): Ctypes.unsigned_short = 
  CONST checkflags = Word.Or(M_EXT,M_M3METHODS);
  VAR ext: UNTRACED REF m_extT;
      s: CARDINAL;
      offset:CARDINAL;
      m0:T := m;
  BEGIN
    WHILE m0 # NIL AND len > 0 DO
      WITH data_buf = Array(m0) DO
        IF NUMBER(data_buf^) > 0 THEN
          IF Word.And(m0.mh_hdr.mh_flags,checkflags) = checkflags THEN 
            ext := LOOPHOLE(ADR(m0.M_dat[FIRST(m0.M_dat)])+extOffset,UNTRACED REF m_extT);
            s   := MIN(len,BYTESIZE(data_buf^));
            WITH methods = LOOPHOLE(ext.ext_methods, Methods) DO
              IF methods # NIL AND methods.csum # NIL THEN
                (* compute offset into the external buffer *)
                offset := m0.mh_hdr.mh_data - ext.ext_buf;

                (* XXX this is a EPHEMERAL upcall and we need to put
                   something here to terminate the user's procedure *)
                csum := methods.csum(
                            LOOPHOLE(ext.ext_m3oa,REF ARRAY OF CHAR),
                            s,
                            LOOPHOLE(ext.ext_arg, REFANY),
                            csum, offset);
              ELSE
                csum := StcpNet.checksum(data_buf^,s,csum);
              END;
            END;
          ELSE
            (* standard checksum on mbuf *)
            s := MIN(len, BYTESIZE(data_buf^));
            csum := StcpNet.checksum(data_buf^,s,csum);
          END;
          DEC(len,s);
        END;
      END;
      m0 := m0.mh_hdr.mh_next;
    END;
    RETURN csum;
  END Checksum;


VAR globalMethods : Methods;
PROCEDURE Init() = 
  BEGIN
    processStcpMbufs := Sema.Alloc();
    mbufThread := ThreadExtra.PFork(Isr, NIL);
    TrackStrand.SetName(ThreadExtra.GetTracker(mbufThread),"StcpMbufReaper");
    
    StcpMbufExtern.sal_mbuf_free := SchedIsr;
    globalMethods := NEW(Methods);
    globalMethods.free := FreeProc;
    globalMethods.csum := NIL;  (* LOOPHOLE(CsumProc,csumprocT);  *)
  END Init;

BEGIN
END StcpMbuf.
