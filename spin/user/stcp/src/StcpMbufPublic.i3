(*
 * HISTORY
 * 09-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	Change interface names so that these don't conflict w/
 *	those used by user/net.
 *
 * 18-Mar-97  Tsutomu Owa (owa) at the University of Washington
 *	Reverted If structure defined as urt/urtcore/src.
 *
 * 03-Feb-97  Tsutomu Owa (owa) at the University of Washington
 *	Copied from user/urt/urtcore/src.
 *
 * 13-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *
 *)

INTERFACE StcpMbufPublic;
IMPORT StcpMbuf;
IMPORT StcpEtherDev;

PROCEDURE SetPktHdrLen   (m:StcpMbuf.T; len: CARDINAL (* used to be Ctypes.int *));
PROCEDURE SetPktHdrRcvIf (m:StcpMbuf.T; dev: StcpEtherDev.T);
PROCEDURE GetPktHdrLen   (m:StcpMbuf.T) : CARDINAL (* used to be Ctypes.int *);
PROCEDURE GetPktHdrRcvIf (m:StcpMbuf.T): StcpEtherDev.T;

END StcpMbufPublic.


