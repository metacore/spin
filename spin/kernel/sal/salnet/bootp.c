/*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 */
/*
	bootp.c

	bootp protocol to discover our IP addr by broadcasting over the
	ethernet.

 	created by David Becker Fri Jun 23 08:37:49 PDT 1995
*/

#include <sal/salnet.h>


#include <sys/param.h>
#include <sys/types.h>
#include <sys/kernel.h>
#include <sys/systm.h>
#define SALNET
#include <sys/mbuf.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <net/if.h>
#include <netinet/if_ether.h>
#include <machine/spl.h>
#include <sal/salnet/bootp.h>

#define DB if (0) printf

/*
 * Generate a ``random'' transaction id number.  It builds it out of the
 * hardware ethernet id.  It's not unique -- XXX
 */
static unsigned short
build_xid(char *hwaddr)
{
	unsigned int ret;
	ret= ((hwaddr[0] <<12) | (hwaddr[5] <<8) | (hwaddr[1] <<4) | hwaddr[4]);
	ret ^= ((hwaddr[2] <<12) | (hwaddr[3] <<4));
	return ret;
}

salnet_err
salnet_bootpmine(salnet_route *route, struct mbuf *m)
{
	struct bootp *bp;

	bp = mtod(m,struct bootp*);
	if (m->m_pkthdr.len<160) {
		printf("bootp: reply too short %d < %d\n",m->m_pkthdr.len,
			sizeof(struct bootp)) ;
		}
	if (bp->bp_op != BOOTREPLY) {
		printf("bootp: reply not BOOTREPLY %d %d\n",
			bp->bp_op,BOOTREPLY); 
		return SALNET_BADREPLY;
		}

	if (bp->bp_xid != build_xid(route->localether)) {
		printf("bootp: xid %x != %x\n",bp->bp_xid,
			build_xid(route->localether));
		return SALNET_NOTMINE;
		}

	if (bp->bp_htype != HTYPE_ETHERNET) {
		printf("bootp: type not HTYPE_ETHERNET %x %x\n",
			bp->bp_htype ,HTYPE_ETHERNET); 
		return SALNET_BADREPLY;
		}
	if (bp->bp_hlen != 6) {
		printf("bootp: len not 6 %d %d\n",
			bp->bp_hlen ,6); 
		return SALNET_BADREPLY;
		}
	if (route->remotip != bp->bp_siaddr.s_addr)
		printf("bootp: serve ip not sender %x %x\n",
			route->remotip, bp->bp_siaddr.s_addr);

	return SALNET_SUCCESS;
}

#define HDRBYTES (sizeof(struct bootp)-BP_SNAME_LEN-BP_FILE_LEN-BP_VEND_LEN)

salnet_err
salnet_bootp(/*OUT*/ ipaddr_t *localip, ipaddr_t *serverip,
		char *servername, char *bootfile)
{
	salnet_route bootpd;
	struct mbuf *m;
	struct bootp request, *bp;
	salnet_err err;

	if (!localip || !serverip || !servername || !bootfile)
		return SALNET_BADARGS;
	*localip=*serverip=0;

	salnet_getlocalroute(&bootpd);
	bootpd.localudpport=IPPORT_BOOTPC;
	bootpd.localip=INADDR_ANY;
	bootpd.remotudpport=IPPORT_BOOTPS;
	bootpd.remotip=INADDR_BROADCAST;
	bootpd.remotether=etherbroadcastaddr;

	bp = &request;
	bzero(bp, sizeof(struct bootp));
	bp->bp_op = BOOTREQUEST;
	bp->bp_htype = HTYPE_ETHERNET;
	bp->bp_hlen = ETHERADDRBYTES;
	bp->bp_xid = build_xid(bootpd.localether);
	bcopy(bootpd.localether, bp->bp_chaddr, bp->bp_hlen);

	err = salnet_udploop(&bootpd, bp, sizeof(struct bootp),
			10, 500, HDRBYTES, &m);
	if (err) return err;
	if (err = salnet_bootpmine(&bootpd, m)) return err;

	bp = mtod(m,struct bootp*);
	*localip = bp->bp_yiaddr.s_addr; /* Your ip addr */
	*serverip = bp->bp_siaddr.s_addr; /* Serv ip addr */

	m_copydata(m, HDRBYTES, BP_SNAME_LEN, servername); 
	m_copydata(m, HDRBYTES+BP_SNAME_LEN, BP_FILE_LEN, bootfile);
	m_freem(m);

	return SALNET_SUCCESS;
}


void
bootp_init()
{
	ipaddr_t localip;
	ipaddr_t remotip;
	char servername[BP_SNAME_LEN];
	char filename[BP_FILE_LEN];
	int err;
	unsigned char *l = &localip, *r = &remotip;

	err = salnet_bootp(&localip, &remotip, servername, filename);
	if (err) {
		salnet_perror("bootp",err);
		return;
		}

	salnet_setipaddr(localip);
	salnet_setbootserver(remotip);

	printf("bootp %d.%d.%d.%d.  Boot server %s(%d.%d.%d.%d) default file %s\n",
		l[0],l[1],l[2],l[3],
		servername,
		r[0],r[1],r[2],r[3],
		filename);
}
