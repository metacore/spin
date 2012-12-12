/*
 * Copyright 1994, 1995, 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 */

/*
 * HISTORY
 * 25-Jul-97  Tsutomu Owa (owa) at the University of Washington
 *	Added pffindproto and dummy icmp_error.
 *
 * 31-May-97  David Becker at the University of Washington
 *      Add addifp/RegisterInterface to keep bsd net code informed of interfaces
 *
 * 09-Apr-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed ip_output_upcall to pass route information.
 *
 * 16-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	The inetsw structure is now allocated and initialized by
 *	Modula-3.
 *
 * 13-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Cleaning out unnecessary C code to get minimum required
 *      support to spoof networking that we borrowed from OSF/1.
 *
 * 11-Nov-95  Marc Fiuczynski (mef) at the University of Washington
 *	Adding more debugging output to determine if the TCP making calls
 *	to unimplemented functions that it depends on.
 *
 * 18-Sep-95  David Becker (becker) at the University of Washington
 *	 Created the network interface to Digital Unix (DEC OSF/1 3.2)
 */

#include "sys/types.h"
#include "sys/errno.h"
#include "sys/ioctl.h"
#include "sys/mbuf.h"
#include "sys/socket.h"
#include "sys/socketvar.h"
#include "sys/domain.h"
#include "sys/protosw.h"
#include "net/if.h"
#include "net/netisr.h"
#include "net/route.h"
#include "netinet/in.h"
#include "netinet/in_var.h"
#include "netinet/if_ether.h"
#include "netinet/ip.h"
#include "netinet/ip_var.h"
#include "netinet/udp.h"
#include "netinet/udp_var.h"
#include "machine/pcb.h"
#include "netinet/if_ether.h"
#include "machine/cpu.h"


/*************************************************************************
 * verbose printing while spoofing networking C code.
 */
#define SpinOsfNetworkVerbose 0
#define SpinOsfNetworkReallyVerbose 0

/*************************************************************************
 * Globals used by networking C code.
 */


/***** from ip_input.c */
struct in_ifaddr *in_ifaddr = 0;

/***** from netinet/ip_input.c */
struct  ifqueue ipintrq;                        /* ip packet input queue */
/* extern CONST struct protosw inetsw[]; */
CONST u_char inetctlerrmap[24]; /* netinet/in_input.c */

/***** from net/if_loop.c*/
struct ifnet loif;

/***** from netinet/in_proto.c */
#define TCPINETSWOFFSET 0
/*
 #define UDPINETSWOFFSET sizeof(struct protosw)
 */
#define UDPINETSWOFFSET 1
/* inetsw will be given a newly allocated address by OsfTcp.m3.
 * Need to define it this way to keep compiler quiet about 
 * netinet/in.h
 */
/* XXX 
 * struct protosw inetsw[1];
 */
struct protosw inetsw[2];
#if 0 /* no implemented by OsfTcpExtern and OsfTcp module */
struct protosw inetsw[1] = {
{ 0,	0,	0, 0,
  0,	0,		0,	0,
  0,
  0,	0,	0,	0
},
};

#define inet_funnel 0
struct domain inetdomain =
    { AF_INET, "internet", 0, 0, 0, 
      inetsw, &inetsw[sizeof(inetsw)/sizeof(inetsw[0])],
      0, 0, inet_funnel, 0 };

#endif

addifp (struct ifnet *ifp, struct in_addr in_addr)
{
	struct socket s;
	register struct ifreq ifr;

        ifr.ifr_addr.sa_family = AF_INET;
	((struct sockaddr_in *)&ifr.ifr_addr)->sin_addr = in_addr;
	s.so_state = SS_PRIV;
	in_control(&s,SIOCSIFADDR,(void*)&ifr,ifp);
}


#ifdef MEF

struct openarray
{
    void *start;
    unsigned long size;
};

addmultiaddr (struct ifnet *ifp, struct openarray* addr)
{
	register struct ifreq ifr;
	unsigned char * multiaddr = (unsigned char*)addr->start;
	int i, err = 0;

	ifp->if_flags = ifp->if_flags | IFF_MULTICAST;

	ifr.ifr_addr.sa_len = 6;
        ifr.ifr_addr.sa_family = AF_UNSPEC;

	for (i=0; i<6; i++) {
#if 0
	  printf("0x%x ", multiaddr[i]);
#endif	
	  (unsigned char)(ifr.ifr_addr.sa_data[i]) = multiaddr[i];
	}
#if 0
	printf("done!\n");
#endif

	err = (*ifp->if_ioctl)(ifp, SIOCADDMULTI, (caddr_t)&ifr);
	printf("err: %d\n", err);
}

#else

struct etheraddr {
  unsigned char addr[6];
};

addmultiaddr (struct ifnet *ifp, unsigned char *multiaddr)
{
	register struct ifreq ifr;
	int i, err = 0;
	/*unsigned char *addr2 = *(&multiaddr+7);*/

#ifdef FOO
	ifp->if_flags = ifp->if_flags | IFF_MULTICAST | IFF_ALLMULTI | IFF_PROMISC;
#else
	ifp->if_flags = ifp->if_flags | IFF_MULTICAST;
#endif

	ifr.ifr_addr.sa_len = 6;
        ifr.ifr_addr.sa_family = AF_UNSPEC;

	for (i=0; i<6; i++) {
	  printf("0x%x ", multiaddr[i]);
	  (unsigned char)(ifr.ifr_addr.sa_data[i]) = multiaddr[i];
	}

/*
	(unsigned char)(ifr.ifr_addr.sa_data[0]) = 0x33;
	(unsigned char)(ifr.ifr_addr.sa_data[1]) = 0x33;
	(unsigned char)(ifr.ifr_addr.sa_data[2]) = 0x2b;
	(unsigned char)(ifr.ifr_addr.sa_data[3]) = 0xbd;
	(unsigned char)(ifr.ifr_addr.sa_data[4]) = 0xde;
	(unsigned char)(ifr.ifr_addr.sa_data[5]) = 0xb4;
*/
	for (i=0; i<6; i++) {
	  printf("0x%x ", (unsigned char)(ifr.ifr_addr.sa_data[i]));
	}

	printf("done!\n");

	err = (*ifp->if_ioctl)(ifp, SIOCADDMULTI, (caddr_t)&ifr);
	printf("err: %d\n", err);
}

#endif

/*************************************************************************
 * upcall support for ip output.  tcp C code calls ip_output directly, 
 * which gets redirected to plexus IpGen.m3.
 */

/* XXX this stub called only while SPIN ip is not defined. */
static void 
ip_output_stub(struct mbuf **m, struct route* ro)
{
	printf("{ip_output_stub(%lx,%lx)}",*m,ro);
	if(*m) m_freem(*m);
}

/* XXX initialize ip_output_upcall to the above stub.  This way we
 * don't need to check in ip_ouput if the function pointer has been
 * initialized.  The M3 portion of this module is responsible for
 * resetting the function pointer to the actual SPIN event handler.
 */
void (*tcpip_output_upcall)(struct mbuf **m,
			    struct route* ro) = ip_output_stub;


/* netinet/ip_output.c */
int tcpip_output(struct mbuf* m0, 
	      struct mbuf* opt,
	      struct route* ro,
	      int flags,
	      struct ip_moptions* imo
	      )
{
	(*tcpip_output_upcall)(&m0,0 /* ro */);
	return 0;
}

/* XXX initialize ip_output_upcall to the above stub.  This way we
 * don't need to check in ip_ouput if the function pointer has been
 * initialized.  The M3 portion of this module is responsible for
 * resetting the function pointer to the actual SPIN event handler.
 */
void (*udpip_output_upcall)(struct mbuf **m,
			    struct route* ro) = ip_output_stub;


/* netinet/ip_output.c */
int udpip_output(struct mbuf* m0, 
	      struct mbuf* opt,
	      struct route* ro,
	      int flags,
	      struct ip_moptions* imo
	      )
{
	(*udpip_output_upcall)(&m0,0 /* ro */);
	return 0;
}

/*************************************************************************
 * upcall support for pffindtype.
 */

/* XXX this stub called only while SPIN ip is not defined. */
static struct protosw *
pffindtype_stub(int family, int type)
{
	printf("WARNING: [pffindtype_stub() called.}");
	return (struct protosw *)0;
}

/* XXX initialize pffindtype_upcall to the above stub.  
 * The M3 portion of this module is responsible for
 * resetting the function pointer to the actual SPIN event handler.
 */
struct protosw *(*pffindtype_upcall)(int family, int type) = pffindtype_stub;

struct protosw *
pffindtype(family, type)
        int family, type;
{
	return pffindtype_upcall(family,type);
	/* return (struct protosw *)inetsw+TCPINETSWOFFSET; /* 0 is tcp */
}

/*************************************************************************
 * hardwired to return inetsw for TCP.  Needs to be rewriten in Modula-3.
 */

/* XXX should this stub be called only while SPIN ip is not defined too? */
static struct protosw *
pffindproto_stub(int family, int protocol, int type)
{
	printf("WARNING: [pffindtype_stub() called.}");
	return (struct protosw *) inetsw+TCPINETSWOFFSET;
}

/* XXX initialize pffindproto_upcall to the above stub.  
 * The M3 portion of this module is responsible for
 * resetting the function pointer to the actual SPIN event handler.
 */
struct protosw *(*pffindproto_upcall)(int family, int protocol, int type) = pffindproto_stub;

struct protosw *
pffindproto(family, protocol, type)
        int family, protocol, type;
{
	return pffindproto_upcall(family, protocol, type);
}

#if 0
struct protosw *
pffindproto(family, protocol, type)
        int family, protocol, type;
{
	GET_CALLER(caller);
	printf("WARNING: [pffindproto () ra = %lx] not implemented\n",caller);
	return (struct protosw *) inetsw+TCPINETSWOFFSET;
}
#endif


/*************************************************************************
 * upcall support for tcp_URT_bind and tcp_URT_unbind.
 */


/* XXX this stub called only while SPIN ip is not defined. */
static void
tcp_URT_unbind_stub(unsigned short port)
{
	printf("WARNING: [tcp_URT_unbind_stub() called.}");
}

void (*tcp_URT_unbind_upcall)(unsigned short port) = tcp_URT_unbind_stub;

void tcp_URT_unbind(unsigned short port)
{
	tcp_URT_unbind_upcall(port);
}

/* XXX this stub called only while SPIN ip is not defined. */
static void
tcp_URT_bind_stub(unsigned short port)
{
	printf("WARNING: [tcp_URT_bind_stub() called.}");
}

void (*tcp_URT_bind_upcall)(unsigned short port) = tcp_URT_bind_stub;

void tcp_URT_bind(unsigned short port)
{
	tcp_URT_bind_upcall(port);
}

/*************************************************************************
 * upcall support for udp_URT_bind and udp_URT_unbind.
 */


/* XXX this stub called only while SPIN ip is not defined. */
static void
udp_URT_unbind_stub(unsigned short port)
{
	printf("WARNING: [udp_URT_unbind_stub() called.}");
}

void (*udp_URT_unbind_upcall)(unsigned short port) = udp_URT_unbind_stub;

void udp_URT_unbind(unsigned short port)
{
	udp_URT_unbind_upcall(port);
}

/* XXX this stub called only while SPIN ip is not defined. */
static void
udp_URT_bind_stub(unsigned short port)
{
	printf("WARNING: [udp_URT_bind_stub() called.}");
}

void (*udp_URT_bind_upcall)(unsigned short port) = udp_URT_bind_stub;

void udp_URT_bind(unsigned short port)
{
	udp_URT_bind_upcall(port);
}

/*************************************************************************
 * dummy functions for the networking C code.
 */

int
ip_ctloutput (int i0, struct socket *s , int i1, int i2, struct mbuf **m)
{
#if SpinOsfNetworkVerbose > 0
	GET_CALLER(caller);
	printf("WARNING: [ip_ctloutput () ra = %lx] not implemented\n",caller);
#endif
	return 0;
}

struct mbuf *ip_srcroute() /* netinet/ip_input.c */
{
#if SpinOsfNetworkReallyVerbose > 0
#if SpinOsfNetworkVerbose > 0
	GET_CALLER(caller);
	printf("WARNING: service [ip_srcroute() ra=%lx] not implemented\n",caller);
#endif /*  SpinOsfNetworkVerbose > 0 */
#endif
	return (struct mbuf*)0;
} 

n_time iptime() /* netinet/ip_icmp.c */
{
#if SpinOsfNetworkVerbose > 0
	GET_CALLER(caller);
	printf("WARNING: service [iptime() ra=%lx] not implemented\n",caller);
#endif
	return 0;
} 

/*
 * Strip out IP options, at higher
 * level protocol in the kernel.
 * Third argument is buffer to which options
 * will be moved.
 */
void
ip_stripoptions(m, mopt, ipopt )
	register struct mbuf *m;
	struct mbuf *mopt;
	struct ipoption *ipopt;
{
#if SpinOsfNetworkVerbose > 0
	GET_CALLER(caller);
	printf("WARNING: service [ip_stripoptions() ra=%lx] not implemented\n",caller);
#endif
}

void
ip_freemoptions()
{
#if SpinOsfNetworkReallyVerbose > 0	
#if SpinOsfNetworkVerbose > 0
	/* gets called by in_pcb.c: in_pcbfree() */
	GET_CALLER(caller);
	printf("WARNING: service [ip_freemoptions() ra=%lx] not implemented.\n",caller);
#endif /* SpinOsfNetworkVerbose > 0 */
#endif
}

void
sockaddr_old (struct mbuf *m)
{
#if SpinOsfNetworkVerbose > 0
	GET_CALLER(caller);
	printf("WARNING: [sockaddr_old () ra = %lx] not implemented\n",caller);
#endif
}

arpaliaswhohas()
{
#if SpinOsfNetworkVerbose > 0
	GET_CALLER(caller);
	printf("WARNING: [arpaliaswhohas() ra = %lx] not implemented\n",caller);
#endif
}

int
if_addmulti (struct ifnet *ifp, struct ifreq *ifr)
{
#if SpinOsfNetworkVerbose > 0
	GET_CALLER(caller);
	printf("WARNING: [if_addmulti () ra = %lx] not implemented\n",caller);
#endif
	return 0;
}

int
if_delmulti (struct ifnet *ifp, struct ifreq *ifr)
{
#if SpinOsfNetworkVerbose > 0
	GET_CALLER(caller);
	printf("WARNING: [if_delmulti () ra = %lx] not implemented\n",caller);
#endif
	return 0;
}

/* called by in_pcbconnect during soconnect */

struct ifaddr *
ifa_ifwithaddr (struct sockaddr *s)
{
#if SpinOsfNetworkVerbose > 0
	GET_CALLER(caller);
	printf("WARNING: [ifa_ifwithaddr () ra = %lx] not implemented\n",caller);
#endif
	return 0;
}


struct in_ifaddr localaddr;
struct ifaddr *
ifa_ifwithdstaddr (struct sockaddr *s)
{
#if SpinOsfNetworkVerbose > 0
	GET_CALLER(caller);
	printf("WARNING: [ifa_ifwithdstaddr () ra = %lx] not implemented\n",caller);
#endif
	localaddr.ia_addr.sin_addr.s_addr = salnet_getipaddr();
	return (void*)&localaddr;
}

igmp_joingroup  ()
{
#if SpinOsfNetworkVerbose > 0
	GET_CALLER(caller);
	printf("WARNING: [igmp_joingroup () ra = %lx] not implemented\n",caller);
#endif
}

igmp_leavegroup ()
{
#if SpinOsfNetworkVerbose > 0
	GET_CALLER(caller);
	printf("WARNING: [igmp_leavegroup () ra = %lx] not implemented\n",caller);
#endif
}

int
rtinit(struct ifaddr *ifa, int i1, int i2)
{
#if SpinOsfNetworkVerbose > 0
	GET_CALLER(caller);
	printf("WARNING: [rtinit() ra = %lx] not implemented\n",caller);
#endif
	return 0;
}

int
rtrequest  (int i1, struct sockaddr *s1, 
		 struct sockaddr *s2, struct sockaddr *s3,
		 int i2, struct rtentry **r)
{
#if SpinOsfNetworkVerbose > 0
	GET_CALLER(caller);
	printf("WARNING: [rtrequest  () ra = %lx] not implemented\n",caller);
#endif
	return 0;
}

void
rt_missmsg (int i1, struct sockaddr *s1, 
		 struct sockaddr *s2,struct sockaddr *s3, 
		 struct sockaddr *s4, int i2, int i3)
{
#if SpinOsfNetworkVerbose > 0
	GET_CALLER(caller);
	printf("WARNING: [rt_missmsg () ra = %lx] not implemented\n",caller);
#endif
}

void
rtalloc    (struct route *rt)
{
#if SpinOsfNetworkReallyVerbose > 0
#if SpinOsfNetworkVerbose > 0
	GET_CALLER(caller);
	printf("WARNING: [rtalloc    () ra = %lx] not implemented\n",caller);
#endif
#endif
}

void
rtfree     (struct rtentry *rte)
{
#if SpinOsfNetworkVerbose > 0
	GET_CALLER(caller);
	printf("WARNING: [rtfree     () ra = %lx] not implemented\n",caller);
#endif
}

/*************************************************************************
 * This code will probably never get used.
 */

gsignal(){
#if SpinOsfNetworkVerbose > 0
	GET_CALLER(caller);
	printf("WARNING: [gsignal() ra = %lx] not implemented\n",caller);
#endif
}


void * /* struct proc * */
pfind(){
#if SpinOsfNetworkVerbose > 0
	GET_CALLER(caller);
	printf("WARNING: [pfind() ra = %lx] not implemented\n",caller);
#endif
	return (void*)0;
}

#if NETISR_THREAD
void
psignal_inthread(){
#else
void
psignal() {
#endif
#if SpinOsfNetworkVerbose > 0
	GET_CALLER(caller);
	printf("WARNING: [psignal_inthread() ra = %lx] not implemented\n",caller);
#endif
}

P_UNREF(){
#if SpinOsfNetworkVerbose > 0
	GET_CALLER(caller);
	printf("WARNING: [P_UNREF() ra = %lx] not implemented\n",caller);
#endif
}

/*************************************************************************
 * uiomove used by uipc_socket code.
 */
uiomove()
{
#if SpinOsfNetworkVerbose > 0
	GET_CALLER(caller);
	printf("WARNING: [uiomove() ra = %lx] not implemented\n",caller);
#endif
}

/*************************************************************************
 * socket locking upcall.
 */

extern void solock_ext(struct socket *);
extern void sounlock_ext(struct socket *);
void solock(struct socket *so){
	solock_ext(so);
}

void sounlock(struct socket *so){
	sounlock_ext(so);
}

/*************************************************************************
 * net synchronization lock support.
 */

ndecl_lock_data(,route_lock)
ndecl_lock_data(,inifaddr_lock)
ndecl_lock_data(,igmp_lock)
ndecl_lock_data(,inp_udp_li)
long lockmode = 1;

void udp_ctloutput() {
#if SpinOsfNetworkVerbose > 0
	GET_CALLER(caller);
	printf("WARNING: [udp_ctloutput() ra = %lx] not implemented\n",caller);
#endif
}

#undef  SpinOsfNetworkVerbose 
#define SpinOsfNetworkVerbose 1

void icmp_error_stub(struct mbuf *m0, int i0, int i1, u_int ia0)
{
#if SpinOsfNetworkVerbose > 0
	GET_CALLER(caller);
	printf("WARNING: [icmp_error() ra = %lx] not implemented\n",caller);
#endif
}

void (*icmp_error_upcall)(struct mbuf *m0, int i0, int i1, u_int ia0)  = icmp_error_stub;

void icmp_error (struct mbuf *m0, int i0, int i1, struct in_addr ia0)
{
/* in_addr defined in netinet/in.h.
 struct in_addr {
	u_int  s_addr;
 };
 */
	icmp_error_upcall(m0, i0, i1, ia0.s_addr);
	if(m0) m_freem(m0);
}

#if 0
void icmp_error (struct mbuf *m0, int i0, int i1, struct in_addr ia0)
{
#if SpinOsfNetworkVerbose > 0
	GET_CALLER(caller);
	printf("WARNING: [icmp_error() ra = %lx] not implemented\n",caller);
#endif
}
#endif

#if	NETISR_THREAD
/* This is a template for other local sockaddr_in's (ip, icmp, udp, etc) */
/* Global versions would need to be locked between threads. */
CONST struct sockaddr_in in_zeroaddr = { sizeof (struct sockaddr_in), AF_INET };
#endif

/*************************************************************************
 * timing support used by networking.
 */

static void *
SpyCreate_stub(char *name)
{
	printf("WARNING: [SpyCreate_stub() called.}");
	return (void*) 0;
}

void* (*SpyCreate_upcall)(char *) = SpyCreate_stub;

void* SpyCreate(char *name)
{
	return SpyCreate_upcall(name);
}

static void
SpyEnter_stub(void* timer)
{
	printf("WARNING: [SpyEnter_stub() called.}");
}

void (*SpyEnter_upcall)(void *) = SpyEnter_stub;

void SpyEnter(void* timer)
{
	SpyEnter_upcall(timer);
}


static void
SpyExit_stub(void* timer)
{
	printf("WARNING: [SpyExit_stub() called.}");
}

void (*SpyExit_upcall)(void *) = SpyExit_stub;

void SpyExit(void* timer)
{
	SpyExit_upcall(timer);
}

static void *
SpyChain_stub(void* timer)
{
	printf("WARNING: [SpyChain_stub() called.}");
	return (void*)0;
}

void* (*SpyChain_upcall)(void *) = SpyChain_stub;

void* SpyChain(void* timer)
{
	return SpyChain_upcall(timer);
}

/* 
 *  tcp timers
 */

void * tcp_output_checksum;
void * tcp_output_presend;
void * tcp_output_send;
void * tcp_input_checksum;


/* 
 * user specific checksum upcall.
 */
unsigned short
mbuf_URT_csum_stub(struct mbuf *m,
		   unsigned short csum,
		   long len)
{
	printf("mbuf_csum_stub called.\n");
	return (unsigned short)-1;
}

unsigned short
(*mbuf_URT_csum_upcall)(struct mbuf *m,
			unsigned short csum,
			long len)  = mbuf_URT_csum_stub;

unsigned short
mbuf_URT_csum(void *mbuf, 
	      unsigned short csum, 
	      long len)
{
	return mbuf_URT_csum_upcall(mbuf, csum, len);
}

extern int tcp_mssdflt;

tcp_URT_mss_stub(struct tcpcb *tp, u_short offer)
{
	return tcp_mssdflt;
}

(*tcp_URT_mss)(struct tcpcb *tp, 
	       u_short offer) = tcp_URT_mss_stub;



/*
 * from net/if.c
 */

/*
 * Attach an interface to the
 * list of "active" interfaces.
 */
struct ifnet *ifnet = 0;
unsigned long if_index = 0;

void if_attach_stub(struct ifnet *ifp)
{
	struct ifnet **p;
	/*printf("  %s\n",ifp->if_version);*/
	printf("  %s%d mtu %d mtu %d baudrate %d\n",
	       ifp->if_name, 
	       ifp->if_unit, 
	       ifp->if_mtu, 
	       ifp->if_mtu, 
	       ifp->if_baudrate);

	/* attach this ifp to the end of all configured interfaces */
	for(p=&(ifnet);
	    *p!=(struct ifnet*)0 && (*p)->if_next!=(struct ifnet*)0;
	    p=&((*p)->if_next));

	if(*p!=(struct ifnet*)0) {
	  (*p)->if_next = ifp;
	} else {
	  *p=ifp;
	}
	(*p)->if_index = ++if_index;
}

/* XXX initialize if_attach_upcall to the above stub.  This way we
 * don't need to check in if_attach if the function pointer has been
 * initialized.  The M3 portion of this module is responsible for
 * resetting the function pointer to the actual SPIN event handler.
 */
void (*if_attach_upcall)(struct ifnet *ifp) = if_attach_stub;

void if_attach (struct ifnet *ifp)
{
	/* XXX needs to be moved into networking startup code */
	extern int max_linkhdr;
	max_linkhdr = 16;	/* usually happens in bsd/uipc_domain.c */

printf("if_attach_upcall %x: %x %x\n",&if_attach_upcall,if_attach_upcall,if_attach_stub);
	(*if_attach_upcall)(ifp);
}
