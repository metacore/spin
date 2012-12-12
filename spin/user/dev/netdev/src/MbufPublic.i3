(*
 * HISTORY
 * 13-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *
 *)

INTERFACE MbufPublic;
IMPORT Mbuf, NetDev;

PROCEDURE SetPktHdrLen   (m:Mbuf.T; len: CARDINAL (* used to be Ctypes.int *));
PROCEDURE SetPktHdrRcvIf (m:Mbuf.T; rcvif: NetDev.T);
PROCEDURE GetPktHdrLen   (m:Mbuf.T) : CARDINAL (* used to be Ctypes.int *);
PROCEDURE GetPktHdrRcvIf (m:Mbuf.T): NetDev.T;

END MbufPublic.
