(*
 * Copyright 1995, 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 12-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	Change interface names so that these don't conflict w/
 *	those used by user/net.
 *
 * 03-Mar-97  Tsutomu Owa (owa) at the University of Washington
 *	Copied from user/urt/urtcore/src/IX86_SPIN.
 *
 * 19-Oct-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.  FreeBSD dependent constants and types.
 *
 *)

INTERFACE StcpMbufDep;
IMPORT Ctypes, Word;

(* Mbufs are of a single size, MSIZE, which includes overhead.  An
   mbuf may add a single "mbuf cluster" of size MCLBYTES (from CLBYTES
   in sys/param.h), which has no additional overhead and is used
   instead of the internal data area; this is usually done when at
   least MINCLSIZE of data must be stored.  *)

CONST
  MSIZE         = 256;
  MLEN          = (MSIZE - BYTESIZE(mh_hdrT));
  CLBYTES       = 2*1024; (* sys/param.h  *)
  MCLBYTES      = 2*1024; (* sys/param.h 1 << MCLSHIFT *)
  MCL2KBYTES    = 2*1024;

CONST M_MBUF = 1;                       (* malloc mbuf type *)
CONST MALLOCBUCKETINDEX = 8; (* based on 256bytes mbuf size *)

(* header at beginning of each mbuf: *)
TYPE mh_hdrT = RECORD
  mh_next    : T;
  mh_nextpkt : T;
  mh_data    : Word.T;
  mh_len     : Word.T;
  mh_type    : Ctypes.short;
  mh_flags   : Ctypes.short;
END;

TYPE MbufT = RECORD
  mh_hdr : mh_hdrT;
  M_dat  : ARRAY [1..MLEN] OF CHAR;
END;
TYPE T = UNTRACED REF MbufT;

TYPE MbufTypes = [0..255];

(* Mbuf statistics *)
TYPE Mbstat = RECORD
  m_mbufs: Ctypes.unsigned_long;                   (* mbufs obtained from page pool     *)
  m_clusters: Ctypes.unsigned_long;                (* clusters obtained from page pool  *)
  m_space: Ctypes.unsigned_long;                   (* spare field                       *)
  m_clfree: Ctypes.unsigned_long;                  (* free clusters                     *)
  m_drops: Ctypes.unsigned_long;                   (* times failed to find space        *)
  m_wait: Ctypes.unsigned_long;                    (* times waited for space            *)
  m_drain: Ctypes.unsigned_long;                   (* times drained protocols for space *)
  m_mtypes: ARRAY [FIRST(MbufTypes)..LAST(MbufTypes)] OF Ctypes.unsigned_int;
  (* type specific mbuf allocation     *)
END;

END StcpMbufDep.
