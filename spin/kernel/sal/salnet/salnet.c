
#include <sal/salnet.h>
#include <sal/cyclecount.h>

#include <sys/param.h>
#include <sys/types.h>
#include <sys/kernel.h>
#include <sys/systm.h>
#include <sys/socket.h>
#define SALNET
#include <sys/mbuf.h>

#ifdef __FreeBSD__
#include <sys/queue.h>
#endif

#include <net/if.h>
#include <netinet/in.h>
#include <netinet/if_ether.h>
#include <netinet/in_systm.h>
#include <netinet/ip.h>
#include <net/route.h>
#include <net/if_types.h>
#include <netinet/in_pcb.h>
#include <netinet/ip_var.h>
#include <netinet/udp.h>
#include <netinet/udp_var.h>
#ifdef __FreeBSD__
#include <vm/vm_param.h> /* for TRUE/FALSE */
#endif

/*****
 from OSF net/if_ethersubr.c
 */
u_char	etherbroadcastaddr[6] = { 0xff, 0xff, 0xff, 0xff, 0xff, 0xff };

int salnet_debug=0;



static ipaddr_t bootserver=0, my_ip=0;

#ifndef SALNETDEVEL
struct ifqueue salnet_pollq ={(struct mbuf*)0,(struct mbuf*)0,0,IFQ_MAXLEN,0};
struct ifnet *salnet_ifp=0;
void (*salnet_ifintr)()=0;
static char *salnet_etheraddr=0;

/* The lance driver init code insits an if_addrlist must exist */
struct sockaddr_in sin_spoof;
struct ifaddr ifa_spoof;

void
salnet_bootdev(struct ifnet *ifp,void (*ifintr)(),char *hwaddr)
{
	max_linkhdr = 16;       /* usually happens in bsd/uipc_domain.c */

	if (salnet_ifp) {
		printf("salnet_bootdev: salnet_ifp already set %x\n",
			salnet_ifp);
		return;
		}
	
	printf("salnet_bootdev: %s%d\n", ifp->if_name, ifp->if_unit);
	salnet_ifp = ifp;
	salnet_ifintr = ifintr;
	salnet_etheraddr = hwaddr;

	max_linkhdr = 16;	/* usually happens in bsd/uipc_domain.c */
	ifp->if_flags |= IFF_UP; /* usually done by ifconfig             */

	if (salnet_debug) ifp->if_flags |= IFF_DEBUG;

	/* invent something valid for if_addrlist */
	sin_spoof.sin_family = AF_INET;
	ifa_spoof.ifa_addr = &sin_spoof;
	ifp->if_addrlist = &ifa_spoof;

	(*ifp->if_init)(ifp->if_unit);
}

void
salnet_getlocalroute(salnet_route *route)
{
	route->localip = my_ip;
	route->localether = salnet_etheraddr;
	route->ifp = salnet_ifp;
	route->ifintr = salnet_ifintr;
	
	if (!salnet_ifp || !salnet_ifintr)
		printf("salnet_getlocalroute: warning no if  %x %x\n",
			salnet_ifp, salnet_ifintr);
}

void
salnet_getetheraddr(/*OUT*/ etheraddr_t ether)
{
	bcopy(salnet_etheraddr, ether, ETHERADDRBYTES);
}

#endif

ipaddr_t
salnet_getipaddr()
{
	return my_ip;
}

void
salnet_setipaddr(ipaddr_t ipaddr)
{
	my_ip=ipaddr;
}

ipaddr_t
salnet_getbootserver()
{
	return bootserver;
}

void
salnet_setbootserver(ipaddr_t ipaddr)
{
	bootserver=ipaddr;
}

static int s;
int pollmode=0;

void
salnet_begin()
{
	if (!pollmode)
		s = splhigh();
	else if (salnet_debug)
		printf("salnet_begin: warning already pollmode %d\n", pollmode);
	++pollmode;
	if (salnet_debug>1) printf("salnet_begin %d\n",pollmode);
}

void
salnet_end()
{
	if (!pollmode) {
		printf("salnet_end: warning not pollmode %d\n", pollmode);
		return;
		}
	--pollmode;
	if (salnet_debug>1) printf("salnet_end %d\n",pollmode);
	if (pollmode==0) {
		struct mbuf *m;
		IF_DEQUEUE(&salnet_pollq,m);
		while(m) {
			m_freem(m);
			IF_DEQUEUE(&salnet_pollq,m);
			}

		splx(s);
		}
}


void
salnet_udpfill(salnet_route *route, struct mbuf *m)
{
	struct udphdr *uh;

	M_PREPEND(m,sizeof(struct udphdr),M_DONTWAIT);
	uh = mtod(m,struct udphdr*);
	uh->uh_sport= htons(route->localudpport);
	uh->uh_dport= htons(route->remotudpport);
	uh->uh_ulen = htons(m_length(m));  /* length of hdr and msg */
	uh->uh_sum = 0;
}

void
salnet_ipfill(salnet_route *route, struct mbuf *m)
{
	struct ip *ip;
	static short id=1;

	M_PREPEND(m,sizeof(struct ip),M_DONTWAIT);
	ip = mtod(m,struct ip*);

#ifdef __FreeBSD__
	ip->ip_v = 4;
	ip->ip_hl = sizeof(struct ip) >> 2;
#else
        ip->ip_vhl = (IPVERSION << 4) | (sizeof(struct ip) >> 2);
#endif
	ip->ip_id = ntohs(id++);
	ip->ip_off = 0;
	ip->ip_ttl = 0xff;
	ip->ip_p = route->ip_protocol;
	ip->ip_sum = 0;
	ip->ip_tos = 0;
	ip->ip_len = htons(m_length(m));
	ip->ip_src.s_addr = route->localip;
	ip->ip_dst.s_addr = route->remotip;

	/* now calculate checksum of the constructed ip header */
	ip->ip_sum = in_cksum(m, sizeof(struct ip));
}

void
salnet_etherfill(salnet_route *route, struct mbuf *m)
{
	struct ether_header *ehp;

	M_PREPEND(m,sizeof(struct ether_header),M_DONTWAIT);
	ehp = mtod(m,struct ether_header*);

	bcopy(route->localether,ehp->ether_shost,sizeof(ehp->ether_shost));
	bcopy(route->remotether,ehp->ether_dhost,sizeof(ehp->ether_dhost));
	ehp->ether_type = htons(route->ether_type);
}

void
salnet_devsend(salnet_route *route, struct mbuf *m)
{
	struct ifnet *ifp;

	if (!route || !route->ifp || !m) {
		printf("salnet_devsend: bad args %x %x %x\n",
			route, route->ifp, m);
		return;
		}

	ifp=route->ifp;
	if (salnet_debug)
		printf("salnet_devsend: %s%d enq %d bytes\n",
			route->ifp->if_name, route->ifp->if_unit,m_length(m));
        IF_ENQUEUE(&ifp->if_snd, m);
	(*ifp->if_start)(ifp);
}

void
salnet_udpsend(salnet_route *route, struct mbuf *msg)
{
	struct mbuf *m;

	if (!msg || !route) {
		printf("salnet_udpsend bad args: route %x msg %x\n",
			route, msg);
		return;
		}
	if (msg->m_len<=0) printf("salnet_udpsend msg len %d\n",msg->m_len);

	MGETHDR(m,M_DONTWAIT, MT_DATA);
	m->m_data += MHLEN;
	m->m_len = 0;

	m->m_pkthdr.len = m_length(msg);
	m->m_next = msg;

	route->ip_protocol = IPPROTO_UDP;
	route->ether_type = ETHERTYPE_IP;

	salnet_udpfill(route,m);
	salnet_ipfill(route,m);
	salnet_etherfill(route,m);

	if (salnet_debug>1)
		printf("salnet_udpsend: %d %d\n",m->m_pkthdr.len,m->m_len);
	salnet_devsend(route,m);
}

static int in_devrecv=0;

unsigned int
salnet_devrecv(salnet_route *route, int msecmax)
{
	cycle_t start;
	int khz = (hertz()/1000);
	cycle_t maxcycles=msecmax*khz;

	if (!route || !route->ifintr || !route->ifp) {
		printf("salnet_devrecv: bad args %x %x %x\n",
			route ,route->ifintr ,route->ifp);
		return msecmax;
		}
	if (!pollmode)
		printf("salnet_devrecv: warning not pollmode %d\n", pollmode);
	in_devrecv++;

	start=cyclecounter();
	while(cycleminus(cyclecounter(),start) < maxcycles) {
		(*route->ifintr)(route->ifp->if_unit);
		if (route->ifq->ifq_len) break;
		}
	in_devrecv--;

	return cycleminus(cyclecounter(),start)/khz;
}

boolean_t
#ifndef SALNETDEVEL
salnet_ether_packet(struct mbuf *m)
#else
devel_ether_packet(struct mbuf *m)
#endif
{
	if (!in_devrecv) return FALSE;
	IF_ENQUEUE(&salnet_pollq, m);
	return TRUE;
}

void
salnet_bootrecv(struct ifnet *ifp)
{
	struct mbuf *m;

	IF_DEQUEUE(&ifp->if_rcv,m);
	while(m) {
		if (!ttd_ether_packet(m) &&
		    !arpd_ether_packet(m) &&
#ifdef SALNETDEVEL
		    !devel_ether_packet(m) &&
#endif
		    !salnet_ether_packet(m) )
			m_freem(m);

		IF_DEQUEUE(&ifp->if_rcv,m);
		}
}

boolean_t
salnet_udpmine(salnet_route *route, struct mbuf *m)
{
	struct udphdr *uh;
	int bytes = sizeof(struct udphdr);
	if (m->m_len<bytes) {
		printf("salnet_udpmine: short mbuf. %d < %d\n",m->m_len,bytes);
		return 0;
		}

        uh = mtod(m, struct udphdr *);
	m_adj(m, bytes);

	/* salnet_ipmine checks udp length and cksum
		since those rely on ip layer */

	route->remotudpport=ntohs(uh->uh_sport);

	if (salnet_debug)
		printf("salnet_udpmine: remote %x local port %x %x\n",
		  route->remotudpport, ntohs(uh->uh_dport),route->localudpport);
	return (uh->uh_dport==htons(route->localudpport));
}


void
salnet_m_cat(m, n)
	register struct mbuf *m, *n;
{
	while (m->m_next)
		m = m->m_next;
	m->m_next = n;
}

boolean_t
salnet_ipfrag(salnet_route *route, struct mbuf *m, struct mbuf **fullpkt)
{
	struct ip *ip;
	int bytes = sizeof(struct ip);
	int hlen;

	static struct mbuf *pkt=0;
	static unsigned short pktid, nextoffset, fragrevorder, pktlen, lastfrag;

	*fullpkt=0;

	if (m->m_len<bytes) {
		printf("salnet_ipfrag: short mbuf. %d < %d\n",m->m_len,bytes);
		return 0;
		}
	ip = mtod(m, struct ip *);
#ifdef __FreeBSD__
        hlen = ip->ip_hl << 2;
	if (hlen < sizeof(struct ip)) {
		printf("salnet_ip: bad len, 0x%x is less than 0x%x\n",
			hlen,sizeof(struct ip));
		return FALSE;
		}
	if (ip->ip_v != IPVERSION) {
		printf("salnet_ip: bad version, 0x%x is not 0x%x\n",
			ip->ip_v,IPVERSION);
		return FALSE;
		}
#else
	if ((long)ip & (sizeof(int)-1))
		printf("salnet_ipfrag: unaligned mbuf. %x\n",ip);
        hlen = (ip->ip_vhl & 0x0f) << 2;
	if (hlen < sizeof(struct ip)) {
		printf("salnet_ip: bad len, 0x%x is less than 0x%x\n",
			hlen,sizeof(struct ip));
		return FALSE;
		}
	if ((ip->ip_vhl & 0xf0) != IPVERSION << 4) {
		printf("salnet_ip: bad version, 0x%x is not 0x%x\n",
			ip->ip_vhl, IPVERSION << 4);
		return FALSE;
		}
#endif
	if (in_cksum(m, hlen)) {
		printf("salnet_ip: bad ip cksum, got 0x%x cksum is 0x%x\n",
			ip->ip_sum, in_cksum(m, sizeof(struct ip)));
		return FALSE;
		}

	ip->ip_len = ntohs(ip->ip_len);
        if (m->m_pkthdr.len < ip->ip_len) {
		printf("salnet_ip: ip_len %d longer than pkt %d\n",
			ip->ip_len, m->m_pkthdr.len);
		return FALSE;
		}

	ip->ip_off = ntohs(ip->ip_off);
#ifndef IP_OFFMASK 
#define	IP_OFFMASK 0x1fff		/* mask for fragmenting bits */
#endif
	if ((ip->ip_off&IP_MF) || ((ip->ip_off&IP_OFFMASK)>0)){
		int offset = (ip->ip_off&IP_OFFMASK)<<3;
		if (salnet_debug>1) {
			printf("salnet_ip: frag 0x%x ip_off 0x%x %d@%d of %d",
			 ip->ip_id, ip->ip_off,ip->ip_len-hlen, offset, pktlen);
			if (ip->ip_off&IP_MF) printf(" more frag");
			printf("\n");
			}
		if (pktid!=ip->ip_id) {
			if (salnet_debug)
				printf("new frag %d %d\n",ntohs(pktid),
					ntohs(ip->ip_id)) ;
			if (pkt) m_freem(pkt);
			pktid = ip->ip_id;
			fragrevorder= (offset!=0);
			if (fragrevorder) {
				pktlen = offset+ip->ip_len;
				pkt=0;
				}
			else	pkt=m;
			nextoffset = offset;
			}

		if (fragrevorder) lastfrag = (offset==0);
		else lastfrag = ((ip->ip_off&IP_MF)==0);

		if (salnet_debug)
			printf("off %d next %d total %d last %d\n",nextoffset, offset, pktlen, lastfrag);
		if (nextoffset==offset) {
			if (fragrevorder) {
				if (!lastfrag) m_adj(m, hlen);
				if (pkt) {
					salnet_m_cat(m, pkt);
					m->m_pkthdr.len += pkt->m_pkthdr.len;
					}
				pkt=m;
				nextoffset -= 1480; /* XXXassuming ether */ 
			} else {
				nextoffset += ip->ip_len-hlen;
				if (pkt!=m) {
					m_adj(m, hlen);
					salnet_m_cat(pkt, m);
					pkt->m_pkthdr.len += m->m_pkthdr.len;
					/* The ip ptr may not be valid now */
					}
				}
			}
		else	{
			if (salnet_debug)
				printf("ip frag out of order %d %d\n",
					nextoffset,offset) ;
			salnet_m_cat(m, pkt);
			pkt = 0;
			pktid = 0;
			*fullpkt=m;
			return FALSE;
			}

		if (!lastfrag) return 0;

		/* now have full ip datagram */
		if (!fragrevorder) {
			m = pkt;	
			ip = mtod(m, struct ip *);
			ip->ip_len = nextoffset+hlen;
			}
		else 	ip->ip_len = pktlen;

		if (salnet_debug)
			printf("frag done %d %d %d\n",m_length(m),ip->ip_len, nextoffset);

		pkt=0;
		pktid=0;
		}
	*fullpkt=m;
	return TRUE;
}



boolean_t
salnet_ipmine(salnet_route *route, struct mbuf *m)
{
	struct ip *ip;
	int bytes = sizeof(struct ip);
	if (m->m_len<bytes) {
		printf("salnet_ipmine: short mbuf. %d < %d\n",m->m_len,bytes);
		return 0;
		}
	ip = mtod(m, struct ip *);

	if (salnet_debug)
		printf("salnet_ipmine: proto %x %x,  ip addr %x %x\n", ip->ip_p,route->ip_protocol, route->localip , ip->ip_dst.s_addr);

	if (route->localip && route->localip != ip->ip_dst.s_addr)
		return FALSE;
	route->localip=ip->ip_dst.s_addr;
	route->remotip=ip->ip_src.s_addr;

	/* udp/ip observes poor layering, so we have to do some udp here */
	if (ip->ip_p == IPPROTO_UDP) {
		int len;
		struct udphdr *uh;
		struct udpiphdr *ui;
		uh = (struct udphdr *)((caddr_t)ip + sizeof (struct ip));
		len = ntohs((u_short)uh->uh_ulen);
		if (len > ip->ip_len) {
			printf("salnet_ip: udp len %d longer than ip len %d\n",
				len, ip->ip_len);
			return FALSE;
			}
		ui = (struct udpiphdr *) ip;
		if (ui->ui_sum) {
#ifdef __FreeBSD__
			ui->ui_i.ih_next = ui->ui_i.ih_prev = 0;
#else
			ui->ui_i.fill[0] = ui->ui_i.fill[1] = 0;
#endif
			ui->ui_x1 = 0;
			ui->ui_len = ui->ui_ulen;
			if (in_cksum(m, len + sizeof (struct ip))) {
				printf("salnet_ip: bad udp cksum, got 0x%x cksum is 0x%x\n", ui->ui_sum, in_cksum(m, len + sizeof (struct ip)));
				return FALSE;
				}
			}
		}
	/* end of udp section */

	m_adj(m, sizeof(struct ip) /* XXX does not account for options */);
	return (ip->ip_p==route->ip_protocol);
}

boolean_t
salnet_ethermine(salnet_route *route, struct mbuf *m)
{
	struct ether_header *ehp;
	int bytes = sizeof(struct ether_header);
	if (m->m_len<bytes) {
		printf("salnet_ethermine: short mbuf %d < %d\n",m->m_len,bytes);
		return 0;
		}
	ehp = mtod(m,struct ether_header*);

	m_adj(m, sizeof(struct ether_header));

	/* we just dup the ether addr ptrs.  This means they are NOT VALID
	after the mbuf is freed.  The alternative is mallocing new space.
	Problem there is then salnet_routes have a field that may need
	to be freed.  Since the idea here is just to get the
	info up to the caller, a ptr dup will meet our needs.
	*/
	route->localether = ehp->ether_dhost;
	route->remotether = ehp->ether_shost;

	if (salnet_debug)
		printf("ethermine %x %x\n",ehp->ether_type, route->ether_type);
	return (ehp->ether_type == route->ether_type);
}

static struct mbuf *
lookforudp(salnet_route *route)
{
	struct mbuf *m;
	IF_DEQUEUE(route->ifq,m);
	while(m) {
		struct mbuf *fullpkt=m;
		if (salnet_ethermine(route,m) &&
			salnet_ipfrag(route,m, &fullpkt) &&
			salnet_ipmine(route, fullpkt) &&
			salnet_udpmine(route, fullpkt))
			return fullpkt;

		if (fullpkt) m_freem(fullpkt);
		IF_DEQUEUE(route->ifq,m);
		}
	return 0;
}


salnet_err
salnet_udprecv(salnet_route *route, int msecmax, /*OUT*/ struct mbuf **reply)
{
	struct mbuf *m;

	if (!reply) return SALNET_BADARGS;
	*reply = 0;
	if (!route) return SALNET_BADARGS;

	route->ip_protocol = IPPROTO_UDP;
	route->ether_type = ETHERTYPE_IP;
	route->ifq = &salnet_pollq;

	if (m=lookforudp(route)) {
		*reply=m;
		return SALNET_SUCCESS;
		}
	while(msecmax>0) {
		msecmax -= salnet_devrecv(route, msecmax);
		if (m=lookforudp(route)) {
			*reply=m;
			return SALNET_SUCCESS;
			}
		}

	if (salnet_debug)
		printf("salnet_udprecv: timeout\n");
	return SALNET_TIMEOUT;
}

static struct mbuf *
lookforether(salnet_route *route)
{
	struct mbuf *m;
	IF_DEQUEUE(route->ifq,m);
	while(m) {
		if (salnet_ethermine(route,m))
			return m;

		m_freem(m);
		IF_DEQUEUE(route->ifq,m);
		}
	return 0;
}

struct mbuf*
salnet_etherrecv(salnet_route *route, int msecmax)
{
	struct mbuf *m;

	route->ifq = &salnet_pollq;

	if (m=lookforether(route)) return m;
	while(msecmax>0) {
		msecmax -= salnet_devrecv(route, msecmax);
		if (m=lookforether(route)) return m;
		}
	if (salnet_debug)
		printf("salnet_etherrecv: timeout\n");
	return 0;
}


salnet_err
salnet_udploop(salnet_route *route, void *msg, long msgbytes, long retries,
	long maxmsec, long replyhdrbytes, /*OUT*/ struct mbuf **reply)
{
	salnet_err err=SALNET_SUCCESS;
	struct mbuf *m;
	salnet_route originalroute=*route;

	if (!reply) return SALNET_BADARGS;
	*reply = 0;
	if (!route) return SALNET_BADARGS;

	if (msgbytes>MCLBYTES) return SALNET_BADARGS;

	while(retries--) {
		*route = originalroute;

		MGET(m,M_DONTWAIT, MT_DATA);
		if (m == 0) return SALNET_NOMBUF;
		if (msgbytes>MLEN) {
			MCLGET(m,M_DONTWAIT);
			if ((m->m_flags & M_EXT) == 0) {
				m_freem(m);
				return SALNET_NOMBUF;
				}
			}
		m->m_len = msgbytes;
		bcopy(msg, mtod(m,char*), msgbytes);

		salnet_begin();
		salnet_udpsend(route, m);
		err = salnet_udprecv(route, maxmsec, reply);
		salnet_end();

		if (err) continue;

		m=*reply;
		if (m_length(m)!=m->m_pkthdr.len) {
			printf("len mismatch %d %d\n",
					m_length(m),m->m_pkthdr.len);
			m_freem(m);
			err = SALNET_BADREPLY;
			}
		else if (m->m_pkthdr.len<replyhdrbytes) {
			printf("len short %d %d\n",
					m->m_pkthdr.len,replyhdrbytes);
			m_freem(m);
			err = SALNET_BADREPLY;
			}
		else if (m->m_len<replyhdrbytes) {
			m = m_pullup(m, replyhdrbytes);
			*reply = m;
			if (!m) {
				printf("pullup null %d %d\n",
						m->m_len,replyhdrbytes);
				m_freem(m);
				err = SALNET_NOMBUF;
				}
			}

		if (!err) return SALNET_SUCCESS;
		}

	if (salnet_debug) salnet_perror("salnet_udploop",err);
	return err;
}


char *msgs[] = {
	"SALNET_SUCCESS",
	"SALNET_TIMEOUT",
	"SALNET_NOMBUF",
	"SALNET_BADREPLY",
	"SALNET_BADARGS",
	"SALNET_NOTMINE",
	"SALNET_NFSERROR",
	};

void
salnet_perror(char *msg, salnet_err err)
{
	if (err>SALNET_MAXERROR) {
		printf("%s: unknown error %d\n",msg,err);
		return;
		}
	printf("%s: %s\n",msg, msgs[err]);
}



boolean_t
salnet_udp_check(salnet_route *route, struct mbuf *m)
{
	/* must not alter packet that is not for this route */

	struct ether_header *ehp;
	struct ip *ip;
	struct udphdr *uh;
	int bytes = sizeof(struct ether_header) + sizeof(struct ip) +
			sizeof(struct udphdr);

	if (m->m_len < bytes) {
		printf("udp_check: short mbuf. %d < %d\n",m->m_len,bytes);
		return 0;
		}
	ehp = mtod(m,struct ether_header*);
	m->m_data +=  sizeof(struct ether_header);
	ip = mtod(m, struct ip *);
	m->m_data +=  sizeof(struct ip);
	uh = mtod(m, struct udphdr *);

	m->m_data -= (sizeof(struct ether_header) + sizeof(struct ip));

	if (salnet_debug>1)
		printf("salnet_udp_check %x %x, %x %x, %x %x\n",
			ehp->ether_type , route->ether_type,
			ip->ip_p,route->ip_protocol,
			uh->uh_dport,htons(route->localudpport));

	return (ehp->ether_type == route->ether_type) &&
		(ip->ip_p==route->ip_protocol) &&
		(uh->uh_dport==htons(route->localudpport));
}
