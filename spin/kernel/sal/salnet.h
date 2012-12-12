
typedef unsigned char* etheraddr_t;
typedef unsigned char* nfshandle_t;
typedef unsigned int ipaddr_t;
typedef unsigned int salnet_err;

typedef struct {
	unsigned short	localudpport;
	unsigned short	remotudpport;

	unsigned char	ip_protocol;
	ipaddr_t localip;
	ipaddr_t remotip;

	unsigned short	ether_type;
	etheraddr_t localether;
	etheraddr_t remotether;

	struct ifnet *ifp;
	void (*ifintr)(int);
	struct ifqueue *ifq;
	} salnet_route;

void salnet_getlocalroute(/*OUT*/ salnet_route *route);
void salnet_getetheraddr(/*OUT*/ etheraddr_t ether);

ipaddr_t salnet_getipaddr();
void salnet_setipaddr(ipaddr_t ip);
ipaddr_t salnet_getbootserver();
void salnet_setbootserver(ipaddr_t ipaddr);

salnet_err salnet_arp(ipaddr_t ip, /*OUT*/ etheraddr_t ether);

salnet_err salnet_bootp(/*OUT*/ ipaddr_t *localip, ipaddr_t *serverip,
		char *servername, char *bootfile);

salnet_err saldns_query(char *hostname, ipaddr_t *ip);

salnet_err saltftp_fetch(ipaddr_t server, char *filename, long maxbytes,
		/*OUT*/ void *buf, long *bytes);

salnet_err salnfs_mount(ipaddr_t server, char *dir,
		/*OUT*/ nfshandle_t mount);

salnet_err salnfs_fetch(ipaddr_t server, nfshandle_t mount, char *path,
		long maxbytes, /*OUT*/ void *buf, long *bytes);

void salnet_begin();
void salnet_end();


#define ETHERADDRBYTES 6
/*****
 from OSF netinet/if_ether.h
 */
extern unsigned char etherbroadcastaddr[6];

extern struct ifqueue salnet_pollq;

struct mbuf;
void salnet_bootrecv(struct ifnet *ifp);
int salnet_udp_check(salnet_route *route, struct mbuf *m);
salnet_err salnet_udploop(salnet_route *route, void *msg, long msgbytes, long retries, long maxmsec, long replyhdrbytes, /*OUT*/ struct mbuf **reply);
salnet_err salnet_udprecv(salnet_route *route, int msecmax, /*OUT*/ struct mbuf **reply);
void salnet_udpsend(salnet_route *route, struct mbuf *msg);
void salnet_perror(char *msg, salnet_err err);
struct mbuf* salnet_etherrecv(salnet_route *route, int msecmax);
void salnet_devsend(salnet_route *route, struct mbuf *msg);

#define SALNET_SUCCESS	0
#define SALNET_TIMEOUT	1
#define SALNET_NOMBUF	2
#define SALNET_BADREPLY	3
#define SALNET_BADARGS	4
#define SALNET_NOTMINE	5
#define SALNET_NFSERROR	6

#define SALNET_MAXERROR	6

extern int salnet_debug;
