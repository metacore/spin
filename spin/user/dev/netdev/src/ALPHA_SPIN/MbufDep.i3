(*
 * Copyright 1995, 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 19-Oct-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created. SAL dependent component for mbuf support.
 *
 *)

INTERFACE MbufDep;
IMPORT Ctypes, Word;
(* Mbufs are of a single size, MSIZE, which includes overhead.  An
   mbuf may add a single "mbuf cluster" of size MCLBYTES (from CLBYTES
   in sys/param.h), which has no additional overhead and is used
   instead of the internal data area; this is usually done when at
   least MINCLSIZE of data must be stored.  *)

CONST
  MSIZE         = 256;
  MLEN          = (MSIZE - BYTESIZE(mh_hdrT));
  CLBYTES       = 8*1024;
  MCL2KBYTES    = 2*1024;

CONST M_MBUF = 1;                       (* malloc mbuf type *)
CONST MALLOCBUCKETINDEX = 4; (* based on 256bytes mbuf size *)

(* header at beginning of each mbuf: *)
TYPE mh_hdrT = RECORD
  mh_next    : T;
  mh_nextpkt : T;
  mh_data    : Word.T;
  mh_len     : Word.T;
  mh_type    : Ctypes.int;
  mh_flags   : Ctypes.int;
  mh_union   : ARRAY [0..3] OF ARRAY [1..BYTESIZE(Ctypes.long)] OF CHAR;
END;

TYPE MbufT = RECORD
  mh_hdr : mh_hdrT;
  M_dat  : ARRAY [1..MLEN] OF CHAR;
END;

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
  (* reference count linked list *)
END;

TYPE T = UNTRACED REF MbufT;

TYPE MbufTypes = [0..255];

(* Mbuf statistics *)
TYPE Mbstat = RECORD
  m_mbufs: Ctypes.unsigned_long; (* mbufs obtained from page pool     *)
  m_clusters: Ctypes.unsigned_long; (* clusters obtained from page pool  *)
  m_drops: Ctypes.unsigned_long; (* times failed to find space        *)
  m_drain: Ctypes.unsigned_long; (* times drained protocols for space *)
  m_mtypes: ARRAY [FIRST(MbufTypes)..LAST(MbufTypes)] OF Ctypes.unsigned_int;
  (* type specific mbuf allocation     *)
END;

END MbufDep.
