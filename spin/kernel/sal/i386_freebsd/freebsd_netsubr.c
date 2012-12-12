
#include <sys/param.h>
#include <sys/kernel.h>
#include <sys/systm.h>

/*****
 salnet support
 */
#include <sys/mbuf.h>
#include <net/if.h>
#include <net/if_types.h>
#include <netinet/in.h>
#include <netinet/if_ether.h>

struct ifnet *ifnet = 0;
unsigned long if_index = 0;


/*****
 from net/if.c
 */

struct	ifqueue ipintrq;



/*****
 from net/if.c
 */

/*
 * Attach an interface to the
 * list of "active" interfaces.
 */
void
if_attach(ifp)
	struct ifnet *ifp;
{
	struct ifnet **p;

	/* XXX needs to be moved into networking startup code */
	max_linkhdr = 16;	/* usually happens in bsd/uipc_domain.c */

	/*
	printf("  %s\n",ifp->if_version);
	printf("  %s%d mtu %d mtu %d baudrate %d\n",
	       ifp->if_name, 
	       ifp->if_unit, 
	       ifp->if_mtu, 
	       ifp->if_mtu, 
	       ifp->if_baudrate);
	*/

	/* attach this ifp to the end of all configured interface list */
	ifp->if_next = ifnet;
	ifnet = ifp;
	ifp->if_index = ++if_index; /* assign next index */
}



/*****
 from net/if_ethersubr.c
 */

/*
 * Process a received Ethernet packet;
 * the packet is in the mbuf chain m without
 * the ether header, which is provided separately.
 */
void
ether_input(ifp, eh, m)
	struct ifnet *ifp;
	register struct ether_header *eh;
	struct mbuf *m;
{
	/*
	   Unlike the regular ether_input stack, we throw the ether header on
	   top right away.
	 */
	struct mbuf *n = m;
	eh->ether_type = ntohs(eh->ether_type);

	M_PREPEND(m,sizeof(struct ether_header),M_DONTWAIT);
	n->m_flags |= M_PKTHDR; /* XXX making sure that pkthdr flag stays set */

	bcopy(eh, mtod(m,struct ether_header*),
	      sizeof(struct ether_header));

	/* queue packet for handling by ifp->if_recv() at the end of 
	the interrupt. ether_input is call mid-interrupt, when it may be
	ok to transmit a packet out this interface.
	if_rcv and if_recv are spin specific */
	IF_ENQUEUE(&ifp->if_rcv,m);
}

/*
 * Ethernet output routine.
 * Encapsulate a packet of type family for the local net.
 * Use trailer local net encapsulation if enough data in first
 * packet leaves a multiple of 512 bytes of data in remainder.
 * Assumes that ifp is actually pointer to arpcom structure.
 */
int
ether_output(ifp, m0, dst, rt0)
	register struct ifnet *ifp;
	struct mbuf *m0;
	struct sockaddr *dst;
	struct rtentry *rt0;
{
	printf("ether_output: stub called.\n");
	if (m0) m_freem(m0);
	return 0;
}

/*
 * Perform common duties while attaching to interface list
 */
void
ether_ifattach(ifp)
	register struct ifnet *ifp;
{
	register struct ifaddr *ifa;
	register struct sockaddr_dl *sdl;

	ifp->if_type = IFT_ETHER;
	ifp->if_addrlen = 6;
	ifp->if_hdrlen = 14;
	ifp->if_mtu = ETHERMTU;
	if (ifp->if_baudrate == 0)
	    ifp->if_baudrate = 10000000;
}

/*
 * Add an Ethernet multicast address or range of addresses to the list for a
 * given interface.
 */
int
ether_addmulti(ifr, ac)
        struct ifreq *ifr;
        register struct arpcom *ac;
{
  return (EAFNOSUPPORT);
}

/*
 * Delete a multicast address record.
 */
int
ether_delmulti(ifr, ac)
        struct ifreq *ifr;
        register struct arpcom *ac;
{
  return (EAFNOSUPPORT);
}

/*
 * Convert Ethernet address to printable (loggable) representation.
 */
static char digits[] = "0123456789abcdef";
char *
ether_sprintf(ap)
        register u_char *ap;
{
        register i;
        static char etherbuf[18];
        register char *cp = etherbuf;

        for (i = 0; i < 6; i++) {
                *cp++ = digits[*ap >> 4];
                *cp++ = digits[*ap++ & 0xf];
                *cp++ = ':';
        }
        *--cp = 0;
        return (etherbuf);
}

/*****
 from netinet/if_ether.c
 */

void
arp_ifinit(ac, ifa)
        struct arpcom *ac;
        struct ifaddr *ifa;
{
}
