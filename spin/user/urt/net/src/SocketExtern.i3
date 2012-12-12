(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 13-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	Added EXTERNAL to soo_select which is implemented in
 *	urt/net/socksupport.c.
 *
 * 20-Mar-96  Marc Fiuczynski (mef) at the University of Washington
 *	Added sosblock and sosbwait procedures.
 *
 * 02-Mar-96  Marc Fiuczynski (mef) at the University of Washington
 *	Added sounlock and solock procedures.
 *
 * 15-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Switched to using CVAR.  CVAR is closer to the semantics used
 *	when interfacing with C code. Use CVAR sparingly.
 *
 * 08-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Added sosetopt.
 *
 * 03-Jan-96  Marc Fiuczynski (mef) at the University of Washington
 *	Cleaned up interface to C code.
 *      Both soreceive() and sosend() use REF T parameters where we
 *	want to use VAR parameters.  However, as it is impossible to
 *	specify a NIL VAR parameter, we resort to using REF and rely
 *      on the clients to simply pass NIL in those cases.
 *)

UNSAFE (* for externals *)
INTERFACE SocketExtern;
IMPORT Ctypes;
IMPORT Errno;
IMPORT Mbuf;
IMPORT SocketRep;
IMPORT Uio;

TYPE T = UNTRACED REF SocketRep.socketT;

<* EXTERNAL *> PROCEDURE socreate(
	dom     : Ctypes.int; 
	VAR aso : T;
        type    : Ctypes.int; 
        proto   : Ctypes.int) : Errno.T;

<* EXTERNAL *> PROCEDURE sobind(
	so       : T; 
	namembuf : Mbuf.T): Errno.T;

<* EXTERNAL *> PROCEDURE solisten(
	so      : T; 
	backlog : Ctypes.int): Errno.T;

<* EXTERNAL *> PROCEDURE sosleep(
        so       : T; 
        addr     : ADDRESS; 
        pri, tmo : Ctypes.int): Ctypes.int;

<* EXTERNAL *> PROCEDURE soconnect(
        so  : T; 
        nam : Mbuf.T): Ctypes.int;

<* EXTERNAL *> PROCEDURE sodequeue(
        head: T;
        VAR so: T;
        CVAR mbuf: Mbuf.T;
        compat_43: Ctypes.int): Ctypes.int;

<* OBSOLETE *>
<* EXTERNAL *> PROCEDURE sosend(
        so       : T;
        addr     : Mbuf.T;
        uio : REF Uio.uioT; (* XXX should be CVAR, but compiler is broken. *)
        top      : Mbuf.T;
        control  : Mbuf.T; 
        flags    : Ctypes.int) : Errno.T;

<* EXTERNAL *> PROCEDURE soreceive(
        so            : T;
        CVAR paddr    : Mbuf.T;
        VAR uio       : Uio.uioT;
        VAR mp0       : Mbuf.T;
        CVAR controlp : Mbuf.T;
        CVAR flagsp   : Ctypes.int) : Errno.T;


<* EXTERNAL *> PROCEDURE soshutdown(
	so: T; 
	how: Ctypes.int) : Errno.T;

<* EXTERNAL *> PROCEDURE soclose(so: T) : Errno.T;

<* EXTERNAL *> PROCEDURE sosetopt(so: T; level, optname: CARDINAL; m0: Mbuf.T) : Errno.T;

<* EXTERNAL solock_ext *> PROCEDURE solock(so: T);
<* EXTERNAL sounlock_ext *> PROCEDURE sounlock(so: T);

<* EXTERNAL sosblock *> PROCEDURE sosblock(CVAR sb: SocketRep.sockbufT; so: T) : Errno.T;
<* EXTERNAL sosbwait *> PROCEDURE sosbwait(CVAR sb: SocketRep.sockbufT; so: T) : Errno.T;
<* EXTERNAL sbunlock *> PROCEDURE sbunlock(CVAR sb: SocketRep.sockbufT);

<* EXTERNAL soo_select *> PROCEDURE soo_select(
        so          : T; 
        VAR events  : Ctypes.short_int;
        VAR revents : Ctypes.short_int;
        scanning    : INTEGER ) : Errno.T;

END SocketExtern.
