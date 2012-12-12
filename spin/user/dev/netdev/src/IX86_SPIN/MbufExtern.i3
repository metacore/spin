(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 09-Dec-97  David Becker at the University of Washington
 *	Add m_adj(). 
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
INTERFACE MbufExtern;

IMPORT Mbuf, MbufDep, Ctypes, Word;

<* EXTERNAL *>
PROCEDURE malloc(
    size: Ctypes.unsigned_long;
    type: Ctypes.int;
    nowait: Ctypes.int): ADDRESS;

(* defined in bsd/uipc_mbuf.c *)
<* EXTERNAL *> VAR mbstat: MbufDep.Mbstat;            
<* EXTERNAL *> VAR sal_mbuf_free: PROCEDURE();
<* EXTERNAL *> VAR mfreelater: Mbuf.T;

<* EXTERNAL *>
PROCEDURE m_free         (m: Mbuf.T): Mbuf.T;
<* EXTERNAL *>
PROCEDURE m_freem        (m: Mbuf.T);

<* EXTERNAL *>
PROCEDURE m_copydata(
        m        : Mbuf.T; 
        off      : CARDINAL; 
        len      : CARDINAL; 
        cp       : Word.T);

<* EXTERNAL *>
PROCEDURE m_copym(
        m        : Mbuf.T; 
        off0     : CARDINAL; 
        len      : CARDINAL; 
        wait     : Mbuf.HowT): Mbuf.T;

<* EXTERNAL *>
PROCEDURE m_get   (
        canwait  : Mbuf.HowT; 
        type     : Ctypes.int): Mbuf.T;

<* EXTERNAL *>
PROCEDURE m_getclr(
        canwait  : Mbuf.HowT; 
        type     : Ctypes.int): Mbuf.T;

<* EXTERNAL *>
PROCEDURE m_gethdr(
        canwait  : Mbuf.HowT; 
        type     : Ctypes.int): Mbuf.T;

<* EXTERNAL *>
PROCEDURE m_prepend(
        m        : Mbuf.T; 
        len      : CARDINAL; 
        how      : Mbuf.HowT): Mbuf.T;

<* EXTERNAL *>
PROCEDURE m_adj(
        n        : Mbuf.T; 
        len      : Ctypes.int);

<* EXTERNAL *>
PROCEDURE m_pullup(
        n        : Mbuf.T; 
        len      : Ctypes.int): Mbuf.T;

<* EXTERNAL m_leadingspace *>
PROCEDURE M_LEADINGSPACE(m: Mbuf.T): CARDINAL;

<* EXTERNAL byte_swap_word *> 
EPHEMERAL FUNCTIONAL
PROCEDURE nstoh(x:Ctypes.unsigned_short):Ctypes.unsigned_short;

<* EXTERNAL *> 
EPHEMERAL 
PROCEDURE in_checksum(
    packet   : ADDRESS; 
    len      : Ctypes.unsigned_int; 
    prevcsum : Ctypes.unsigned_short := 16_FFFF):Ctypes.unsigned_short;
END MbufExtern.
