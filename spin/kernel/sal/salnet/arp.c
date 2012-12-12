
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
#ifdef __FreeBSD__
#include <vm/vm_param.h> /* for TRUE/FALSE */
#endif

int arpcacheip=0;
char arpcacheether[ETHERADDRBYTES];

salnet_err
salnet_arp(ipaddr_t ip, /*OUT*/ etheraddr_t ether)
{
	salnet_route sendroute, recvroute;
        struct ether_arp request, *arp;
	struct mbuf *m;
	int resend, retry;

	if (!ether) return SALNET_BADARGS;

	if (ip==arpcacheip) {
		bcopy(arpcacheether, ether, ETHERADDRBYTES);
		return SALNET_SUCCESS;
		}

	salnet_getlocalroute(&sendroute);
	sendroute.ether_type = ETHERTYPE_ARP;
	sendroute.remotether=etherbroadcastaddr;
	recvroute = sendroute; /* structure copy */

	arp = &request;
	arp->arp_hrd = htons(ARPHRD_ETHER);
	arp->arp_pro = htons(ETHERTYPE_IP);
	arp->arp_hln = ETHERADDRBYTES;	/* length of hardware address */
	arp->arp_pln = 4;	/* length of protocol address */
	arp->arp_op  = htons(ARPOP_REQUEST);
	bcopy(sendroute.localether, arp->arp_sha, ETHERADDRBYTES);
	bzero(arp->arp_tha, ETHERADDRBYTES);
	*(int*)arp->arp_spa = sendroute.localip;
	*(int*)arp->arp_tpa = ip;

	
	salnet_begin();
	for(resend=3,m=0; m==0 && resend; --resend) {
		MGETHDR(m,M_DONTWAIT, MT_DATA);
		if (!m) {
			salnet_perror("arp",SALNET_NOMBUF);
			return SALNET_NOMBUF;
			}
		m->m_pkthdr.len = m->m_len = sizeof(struct ether_arp);
		m->m_data += sizeof(struct ether_header);
		bcopy(&request, m->m_data, sizeof(struct ether_arp));

		salnet_etherfill(&sendroute, m);
		salnet_devsend(&sendroute, m);

		for(retry=10; retry; --retry) {
			m = salnet_etherrecv(&recvroute, 300);
			if (!m) continue;

			arp = mtod(m,struct ether_arp*);
			if (arp->arp_op==htons(ARPOP_REPLY) &&
			    bcmp(arp->arp_spa , &ip, 4)==0)
			    /*
			    (*(int*)arp->arp_spa == ip))
			    */
				break;
			m_freem(m);
			m=0;
			}
		}
	salnet_end();

	if (!m) {
		salnet_perror("arp",SALNET_TIMEOUT);
		return SALNET_TIMEOUT;
		}


	if (salnet_debug)
		printf("arp: good reply src %x target %x\n",
			*(int*)arp->arp_spa, *(int*)arp->arp_tpa);

	bcopy(arp->arp_sha, ether, ETHERADDRBYTES);
	arpcacheip=ip;
	bcopy(arp->arp_sha, arpcacheether, ETHERADDRBYTES);
	m_freem(m);

	return SALNET_SUCCESS;
}

int
arpd_check(struct mbuf *m)
{
	/* must not alter packet that is not for arpd */

	struct ether_header *ehp;
        struct ether_arp *arp;

	if (m->m_len < sizeof(struct ether_header) + sizeof(struct ether_arp))
		return 0;

	ehp = mtod(m,struct ether_header*);
	m->m_data +=  sizeof(struct ether_header);
	arp = mtod(m, struct ether_arp *);

	m->m_data -= sizeof(struct ether_header);

	return (ehp->ether_type == ETHERTYPE_ARP) &&
		(arp->arp_op  == htons(ARPOP_REQUEST)) &&
		(*(int*)arp->arp_tpa == salnet_getipaddr());
}

int
arpd_ether_packet(struct mbuf *m) 
{
	salnet_route route;
	struct ether_header *ehp = mtod(m,struct ether_header*);
        struct ether_arp reply, *arp;

	if (!arpd_check(m)) return FALSE;
	m_adj(m, sizeof(struct ether_header));

	arp = mtod(m, struct ether_arp *);

	salnet_getlocalroute(&route);
	route.ether_type = ETHERTYPE_ARP;
	route.remotether = reply.arp_tha;  /* NOTE: remotether is a ptr */

	bcopy(route.localether, reply.arp_sha, ETHERADDRBYTES);
	bcopy(arp->arp_sha, reply.arp_tha, ETHERADDRBYTES);
	*(int*)reply.arp_spa = route.localip;
	*(int*)reply.arp_tpa = *(int*)reply.arp_spa;

	m_freem(m);

	arp = &reply;
	arp->arp_hrd = htons(ARPHRD_ETHER);
	arp->arp_pro = htons(ETHERTYPE_IP);
	arp->arp_hln = ETHERADDRBYTES;	/* length of hardware address */
	arp->arp_pln = 4;	/* length of protocol address */
	arp->arp_op  = htons(ARPOP_REPLY);

	MGETHDR(m,M_DONTWAIT, MT_DATA);
	if (!m) {
		salnet_perror("arpd",SALNET_NOMBUF);
		return TRUE;
		}

	m->m_pkthdr.len = m->m_len = sizeof(struct ether_arp);
	m->m_data += sizeof(struct ether_header);
	bcopy(&reply, m->m_data, sizeof(struct ether_arp));
	salnet_etherfill(&route, m);
	salnet_devsend(&route, m);

	return TRUE;
}
