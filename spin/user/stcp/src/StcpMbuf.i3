(*
 * Copyright 1995-1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 09-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	Change interface names so that these don't conflict w/
 *	those used by user/net.
 *
 * 18-Mar-97  Tsutomu Owa (owa) at the University of Washington
 *	Reverted If structure defined as urt/urtcore/src.
 *
 * 03-Feb-97  Tsutomu Owa (owa) at the University of Washington
 *	Copied from user/urt/urtcore/src and changed If.ifnet to Ifnet
 *
 * 19-Oct-96  Marc Fiuczynski (mef) at the University of Washington
 *	Updated to be SAL independent.
 *
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	make Array FUNCTIONAL so that net guards can call it
 *
 * 20-Apr-96  Marc Fiuczynski (mef) at the University of Washington
 *	Added support for mbuf method upcalls. Right now there is the
 *	"free" and "csum" upcall.  The plan for the future is for the
 *	StcpMbuf.T to become an object which can be extended to support
 *	arbitrary, application-specific upcalls.  This will happen when
 *	uncollectable and collectable heaps have been merged into one.
 *
 * 19-Apr-96  Marc Fiuczynski (mef) at the University of Washington
 *	Got rid of dependency on Net module.  Added lots of comments
 *	describing the various types, procedures, and constants.
 *
 * 10-Apr-96  Marc Fiuczynski (mef) at the University of Washington
 *	Added M3 SET for flags.
 *
 * 20-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Corrected M_DONTWAIT and M_WAIT constants.  Their values were
 *	flipped around.
 *
 * 09-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Made it possible to use large arrays as containers of smaller
 *	amounts of information to be wrapped in an StcpMbuf. Fix for bugs
 *	36 and 37.
 *
 * 03-Oct-95  Marc Fiuczynski (mef) at the University of Washington
 *      Created.  An interface to the BSD memory buffer.
 *)
INTERFACE StcpMbuf;
IMPORT Ctypes, Word, StcpMbufDep;

(* StcpMbufs are of a single size, MSIZE, which includes overhead.  An
   mbuf may add a single "mbuf cluster" of size MCLBYTES (from CLBYTES
   in sys/param.h), which has no additional overhead and is used
   instead of the internal data area; this is usually done when at
   least MINCLSIZE of data must be stored.  *)

CONST
  MSIZE         = StcpMbufDep.MSIZE;
  (* size of an mbuf *)

  MLEN          = StcpMbufDep.MLEN;
  (* normal data len *)

  MHLEN         = (MLEN - BYTESIZE(pkthdrT));
  (* data len with a packet header *)

  MINCLSIZE     = (MHLEN + MLEN);
  (* smallest amount to put in cluster *)

  M_MAXCOMPRESS = (MHLEN DIV 2);
  (* max amount to copy for compression *)

  CLBYTES       = StcpMbufDep.CLBYTES;
  MCLBYTES      = CLBYTES;            (* cluster page size *)
  MCL2KBYTES    = StcpMbufDep.MCL2KBYTES; (* special cluster size for ethernet *)

(* m3 style mbuf flags *)
TYPE StcpMbufFlags = {EXT,      (* has associated storage            *)
                  PKTHDR,   (* start of record                   *)
                  EOR,      (* end of record                     *)
                  FASTFREE, (* free external storage asap        *)
                  M3METHODS,  (* has associate methods table       *)
                  BCAST,    (* send/recv as link-level broadcast *)
                  MCAST,    (* send/recv as link-level multicast *)
                  WCARD,    (* recvd as network-level broadcast  *) 

                  (* XXX turds to make the set representation complete
                     for the compiler. Wilson is looking into changing
                     the compiler so that we don't need this.  MEF 4/8/96. *)
                  x9, 
                  x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,
                  x20,x21,x22,x23,x24,x25,x26,x27,x28,x29,
                  x30,x31,x32};
TYPE StcpMbufFlagSet = BITS 32 FOR SET OF StcpMbufFlags;

(* c style mbuf flags *)
CONST
  M_EXT         = 16_0001; (* has associated external storage           *)
  M_PKTHDR      = 16_0002; (* start of record                           *)
  M_EOR         = 16_0004; (* end of record                             *)
  M_FASTFREE    = 16_0008; (* free external storage asap                *)
  M_M3METHODS   = 16_0010; (* free external open array                  *)

                           (* mbuf pkthdr flags, also in m_flags        *)
  M_BCAST       = 16_0100; (* send/received as link-level broadcast     *)
  M_MCAST       = 16_0200; (* send/received as link-level multicast     *)
  M_WCARD       = 16_0400; (* received as network-level broadcast       *)

                           (* flags copied when copying m_pkthdr        *)
  M_COPYFLAGS   = 
    Word.Or(M_PKTHDR, 
            Word.Or(M_EOR, 
                    Word.Or(M_BCAST, Word.Or(M_MCAST,M_WCARD))));
  (* (M_PKTHDR|M_EOR|M_BCAST|M_MCAST|M_WCARD); *)


TYPE StcpMbufTypes = StcpMbufDep.MbufTypes;
(* mbuf types *)
CONST
  MT_FREE       : StcpMbufTypes = 0;  (* should be on free list      *)
  MT_DATA       : StcpMbufTypes = 1;  (* dynamic (data) allocation   *)
  MT_HEADER     : StcpMbufTypes = 2;  (* packet header               *)
  MT_SOCKET     : StcpMbufTypes = 3;  (* socket structure            *)
  MT_PCB        : StcpMbufTypes = 4;  (* protocol control block      *)
  MT_RTABLE     : StcpMbufTypes = 5;  (* routing tables              *)
  MT_HTABLE     : StcpMbufTypes = 6;  (* IMP host tables             *)
  MT_ATABLE     : StcpMbufTypes = 7;  (* address resolution tables   *)
  MT_SONAME     : StcpMbufTypes = 8;  (* socket name                 *)
  MT_SOOPTS     : StcpMbufTypes = 10; (* socket options              *)
  MT_FTABLE     : StcpMbufTypes = 11; (* fragment reassembly header  *)
  MT_RIGHTS     : StcpMbufTypes = 12; (* access rights               *)
  MT_IFADDR     : StcpMbufTypes = 13; (* interface address           *)
  MT_CONTROL    : StcpMbufTypes = 14; (* extra-data protocol message *)
  MT_OOBDATA    : StcpMbufTypes = 15; (* expedited data              *)
  MT_MAX        : StcpMbufTypes = MT_OOBDATA+1;

  (* flags to mbuf space allocation routines. *)
  M_WAITOK      = 16_0000;
  M_NOWAIT      = 16_0001;
  M_DONTWAIT    = M_NOWAIT;
  M_WAIT        = M_WAITOK;

  (* length to m_copy to copy all. *)
  M_COPYALL     = 1000000000;

(* header at beginning of each mbuf: *)
TYPE mh_hdrT = StcpMbufDep.mh_hdrT;

(* record/packet header in first mbuf of chain; valid if M_PKTHDR set *)
TYPE pkthdrT = RECORD
  len   : INTEGER; (* used to be Ctypes.int; *)
  rcvif : ADDRESS; (* UNTRACED REF If.ifnet; *)
END;

TYPE StcpMbufT = StcpMbufDep.MbufT;
TYPE T = StcpMbufDep.T;

TYPE HowT = [M_WAIT .. M_DONTWAIT];

<* INLINE *>
PROCEDURE m_copydata(
    mbuf : T; 
    off  : CARDINAL; 
    len  : CARDINAL; 
    cp   : T);
(* Copy data from an mbuf chain starting "off" bytes from the
   beginning, continuing for "len" bytes, into the indicated
   buffer. *)

<* INLINE *>
PROCEDURE m_copym(
    mbuf : T; 
    off  : CARDINAL; 
    len  : CARDINAL; 
    wait : HowT):T;
(* Make a copy of an mbuf chain starting "off0" bytes from the
   beginning, continuing for "len" bytes.  If len is M_COPYALL, copy
   to end of mbuf.  The wait parameter is a choice of
   M_WAIT/M_DONTWAIT from caller.  *)

PROCEDURE m_free(mbuf : T):T;
(* Free an allocated mbuf, freeing associated cluster if present.  If
   cluster requires special action, place whole mbuf on mfreelater and
   schedule later freeing (so as not to free from interrupt level).  *)

<* INLINE *>
PROCEDURE m_freem(mbuf : T);
(* Free an allocated mbuf chain.  Calls m_free for each mbuf in the
   chain. *)

<* INLINE *>
PROCEDURE m_get(
    canwait : HowT; 
    type    : Ctypes.int):T;
(* Space allocation routines.  Will get a mbuf. *)

<* INLINE *>
PROCEDURE m_getclr(
    canwait : HowT; 
    type    : Ctypes.int):T;
(* Space allocation routines.  Will get a mbuf zero'd mbuf data area. *)

<* INLINE *>
PROCEDURE m_gethdr(
    canwait : HowT; 
    type    : Ctypes.int):T;
(* Space allocation routines.  Will get a mbuf with pkt header. *)

<* INLINE *>
PROCEDURE m_length(mbuf : T):CARDINAL;
(* get the length of an mbuf chain.  *)

<* INLINE *>
PROCEDURE m_prepend(
    mbuf : T; 
    len  : CARDINAL; 
    how  : HowT):T;
(* Lesser-used path for M_PREPEND: allocate new mbuf to prepend to
   chain, copy junk along.  *)

PROCEDURE m_pullup(
    mbuf : T; 
    len  : CARDINAL):T;
(* Rearange an mbuf chain so that len bytes are contiguous and in the
   data area of an mbuf (so that mtod and dtom will work for a
   structure of size len).  Returns the resulting mbuf chain on
   success, frees it and returns null on failure.  If there is room,
   it will add up to max_protohdr-len extra bytes to the contiguous
   region in an attempt to avoid being called next time.  *)

<* INLINE *>
PROCEDURE M_PREPEND(
    mbuf : T; 
    plen : CARDINAL; 
    how  : HowT):T;
(* Arrange to prepend space of size plen to mbuf m.  If a new mbuf
   must be allocated, how specifies whether to wait.  If how is
   M_DONTWAIT and allocation fails, the original mbuf chain is freed
   and m is set to NULL.  *)

<* INLINE *>
PROCEDURE M_ALIGN(
    mbuf : T;
    len  : CARDINAL);
(* Set the m_data pointer of a newly-allocated mbuf (m_get/MGET) to
   place an object of the specified size at the aligned end of the
   mbuf. *)

<* INLINE *>
PROCEDURE MH_ALIGN(
    mbuf : T;
    len  : CARDINAL);
(* As above, for mbufs allocated with m_gethdr/MGETHDR or initialized
  by M_COPY_PKTHDR.  *)

(* StcpMbuf statistics *)
TYPE Mbstat = RECORD
  m_mbufs: Ctypes.unsigned_long;
  (* mbufs obtained from page pool     *)

  m_clusters: Ctypes.unsigned_long;
  (* clusters obtained from page pool  *)

  m_drops: Ctypes.unsigned_long;
  (* times failed to find space        *)

  m_drain: Ctypes.unsigned_long;
  (* times drained protocols for space *)

  m_mtypes: ARRAY [FIRST(StcpMbufTypes)..LAST(StcpMbufTypes)] OF Ctypes.unsigned_int;
  (* type specific mbuf allocation     *)
END;

(*  
 * Routines specific to SPIN implementation.
 *) 

EXCEPTION LengthMismatch;

<* INLINE *>
FUNCTIONAL PROCEDURE Array(m: T): UNTRACED REF ARRAY OF CHAR;

TYPE freeprocT = (* EPHEMERAL *) PROCEDURE (
                     ext_buf  : REF ARRAY OF CHAR; 
                     ext_size : CARDINAL; 
                     ext_arg  : REFANY);

TYPE csumprocT = (* EPHEMERAL *) PROCEDURE (
                     ext_buf  : REF ARRAY OF CHAR; 
                     ext_size : CARDINAL; 
                     ext_arg  : REFANY;
                     offset   : CARDINAL;
                     csum     : Ctypes.unsigned_short): Ctypes.unsigned_short;

(* XXX These methods needs to become extensible by the user.  The
   conflict is that we want the upcall to be fast for checksum, which
   requires that I make it EPHEMERAL, but general enough for the user
   to do anything they please, which requires that I don't make it
   EPHEMERAL.
 *)
TYPE Methods = REF RECORD
  free: freeprocT;   (* XXX MUST BE FIRST IN THIS RECORD *)
  csum: csumprocT;
END;

PROCEDURE MclGetOa(
    array   : REF ARRAY OF CHAR; 
    length  : CARDINAL; 
    methods : Methods := NIL;
    desc    : REFANY  := NIL):T
  RAISES {LengthMismatch};
(* Allocate an mbuf that wraps up the user supplied buffer.  The user
   can provide a method suite that act on the mbuf.  *)

PROCEDURE Checksum(
    mbuf     : T; 
    prevcsum : Ctypes.unsigned_short := 0; 
    len      : CARDINAL := LAST(Ctypes.unsigned_int)): Ctypes.unsigned_short;

(* Computes a 16-bit csum value over the mbuf chain, over at most len
   bytes.  By default len is set to maxint, which will compute an mbuf
   over the entire mbuf chain.

   XXX offset value is currently not used to index into the mbuf
   chain.  Maybe it should. *)


PROCEDURE Init();
(* Module initialization routine.  *)

END StcpMbuf. 
