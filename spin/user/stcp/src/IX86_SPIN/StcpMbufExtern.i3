(*
 * Copyright 1996 University of Washington
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
 *	Updated to use SAL dependent types.
 *
 * 15-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Got rid of obsolete external functions, which are now
 *	reimplemented in M3 by the Mbuf module.
 *
 * 03-Jan-96  Marc Fiuczynski (mef) at the University of Washington
 *	Fixed mclgetoa and mclgetountraced to have correct parameter
 *	passing argument for the array.
 *
 *)

UNSAFE (* for externals *)
INTERFACE StcpMbufExtern;

IMPORT StcpMbuf, StcpMbufDep, Ctypes, Word;

<* EXTERNAL *>
PROCEDURE malloc(
    size: Ctypes.unsigned_long;
    indx: Ctypes.int;
    type: Ctypes.int;
    nowait: Ctypes.int): ADDRESS;

(* defined in bsd/uipc_mbuf.c *)
<* EXTERNAL *> VAR mbstat: StcpMbufDep.Mbstat;            
<* EXTERNAL *> VAR sal_mbuf_free: PROCEDURE();
<* EXTERNAL *> VAR mfreelater: StcpMbuf.T;

<* EXTERNAL *>
PROCEDURE m_free         (m: StcpMbuf.T): StcpMbuf.T;
<* EXTERNAL *>
PROCEDURE m_freem        (m: StcpMbuf.T);

<* EXTERNAL *>
PROCEDURE m_copydata(
        m        : StcpMbuf.T; 
        off      : CARDINAL; 
        len      : CARDINAL; 
        cp       : Word.T);

<* EXTERNAL *>
PROCEDURE m_copym(
        m        : StcpMbuf.T; 
        off0     : CARDINAL; 
        len      : CARDINAL; 
        wait     : StcpMbuf.HowT): StcpMbuf.T;

<* EXTERNAL *>
PROCEDURE m_get   (
        canwait  : StcpMbuf.HowT; 
        type     : Ctypes.int): StcpMbuf.T;

<* EXTERNAL *>
PROCEDURE m_getclr(
        canwait  : StcpMbuf.HowT; 
        type     : Ctypes.int): StcpMbuf.T;

<* EXTERNAL *>
PROCEDURE m_gethdr(
        canwait  : StcpMbuf.HowT; 
        type     : Ctypes.int): StcpMbuf.T;

<* EXTERNAL *>
PROCEDURE m_prepend(
        m        : StcpMbuf.T; 
        len      : CARDINAL; 
        how      : StcpMbuf.HowT): StcpMbuf.T;

<* EXTERNAL *>
PROCEDURE m_pullup(
        n        : StcpMbuf.T; 
        len      : Ctypes.int): StcpMbuf.T;

<* EXTERNAL m_leadingspace *>
PROCEDURE M_LEADINGSPACE(m: StcpMbuf.T): CARDINAL;

END StcpMbufExtern.
