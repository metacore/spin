

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/types.h>
#include <net/if.h>
#include <netinet/in.h>
#include <sys/mbuf.h>

#include <sal/salnet.h>

#ifdef OSF
#include "../ALPHA_SPIN/EtherDev.h"
#endif
#ifdef __FreeBSD__
#include "../IX86_SPIN/EtherDev.h"
#endif

static void *devarg;

/* called at splio in ether intr handler */
void
bootlisten(struct ifnet *ifp)
{
	struct mbuf *m;

	IF_DEQUEUE(&ifp->if_rcv,m);
	while(m) {
		if (!ttd_ether_packet(m) &&
		    !arpd_ether_packet(m) &&
		    !salnet_ether_packet(m))
			(*EtherDev__Receive)(devarg, m);

		IF_DEQUEUE(&ifp->if_rcv,m);
		}
}

install_bootlisten(void *dev)
{
	salnet_route bootroute;
	salnet_getlocalroute(&bootroute);

	devarg = dev;
	bootroute.ifp->if_recv = bootlisten;
}

bootether(char *etheraddr)
{
	salnet_getetheraddr(etheraddr);
}

bootname(char *name)
{
	salnet_route bootroute;
	salnet_getlocalroute(&bootroute);

	sprintf(name,"%s%d",bootroute.ifp->if_name, bootroute.ifp->if_unit);
}

void*
bootifp()
{
	salnet_route bootroute;
	salnet_getlocalroute(&bootroute);

	return bootroute.ifp;
}

int
ipaddr(struct ifnet *ifp)
{
	if (!ifp || !ifp->if_addrlist || !ifp->if_addrlist->ifa_addr)
		return 0;
		
	if (ifp->if_addrlist->ifa_addr->sa_family != AF_INET) return 0;
	return ((struct sockaddr_in *) (ifp->if_addrlist->ifa_addr))->sin_addr.s_addr;

}

ifpsend(struct ifnet *ifp, struct mbuf *m)
{
	long s = splhigh();
        IF_ENQUEUE(&ifp->if_snd, m);
	(*ifp->if_start)(ifp);
	splx(s);
}
