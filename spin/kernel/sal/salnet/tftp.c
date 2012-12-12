/* 
 * HISTORY
 * 20-Feb-97  becker at the University of Washington
 *	Marc and myself altered to make valid freebsd mbufs
 *
 * 11-Jul-96  becker at the University of Washington
 *	Use TFTPD_ syms instead of hardcoded values.
 *
 * 01-Jul-96  Marc Fiuczynski (mef) at the University of Washington
 *	Return TRUE in tftp_input when we get a straggler tftp packet.
 *
 * 03-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	Return TRUE in tftp_input to indicate that tftp will consume
 *	packet, even if it is rejecting it due to not being at
 *	splextreme.
 *
 *  9-May-96  David Becker (becker) at the University of Washington
 *	return ENOSPACE on buffer overflow.  checking for blockno 1 not
 *	always valid.  increased timeout for rerequest.
 *
 *  6-May-96  David Becker (becker) at the University of Washington
 *	tftp_input was not returning anything for the straggler case
 *
 * 25-Apr-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed tftp_get to timeout and retry the tftp RRQ if it is hung
 *	for a long time. This also required a change to net_poll.c.
 *	
 *
 */

#include <sal/salnet.h>

#include <sys/param.h>
#include <sys/types.h>
#include <sys/kernel.h>
#define SALNET
#include <sys/mbuf.h>
#include <netinet/in.h>
#include <net/if.h>
#include <netinet/if_ether.h>
#include <machine/spl.h>
#include <arpa/tftp.h>
#ifdef __FreeBSD__
#include <vm/vm_param.h> /* for TRUE/FALSE */
#endif

#define DB if(0) printf

#define max(a,b) ((a>b) ? a : b)
#define min(a,b) ((a>b) ? b : a)

enum {
    TFTPC_PORT = 1901,
    TFTPS_PORT = 69
};
#define TOOBIG          "file longer than allocated buffer"
#define BAD_IP          "cannot lookup server ip addr"
#define NO_IP           "client ip addr unknown"

#define TFTPHDRBYTES 4	/* struct tftphdr is odd len so sizeof may
			return unexpected result */

static char octet[]="octet";

tftp_ack(salnet_route *tftpd, int block)
{
	struct mbuf *m;
	struct tftphdr *tp;

	int reqlen =  TFTPHDRBYTES;

	/* alloc 2k mbuf and align data so protocol headers can be prepended */
	MGETHDR(m,M_DONTWAIT, MT_DATA);
	MCLGET(m,M_DONTWAIT);

	reqlen = max(ETHERMIN,reqlen); /* get to min size */
	reqlen = (reqlen + sizeof(unsigned long)) & ~(sizeof(unsigned long)-1); 
	m->m_data += MCLBYTES;
	m->m_len = 0;
	m->m_pkthdr.len = 0;
	M_PREPEND(m,reqlen,M_DONTWAIT);

	tp = mtod(m, struct tftphdr*);
	tp->th_opcode = htons(ACK);
	tp->th_u.tu_block = htons(block);

	salnet_udpsend(tftpd, m);
}



int tftpd_ip = 0;

tftp_req(salnet_route *tftpd, char *filename)
{
	struct mbuf *m;
	struct tftphdr *tp;
	int bytes, namelen=strlen(filename)+1;

	bytes = TFTPHDRBYTES + namelen + strlen(octet)+1;

	MGET(m,M_DONTWAIT, MT_DATA);
	if (bytes>MLEN) MCLGET(m,M_DONTWAIT);
	m->m_len=bytes;

	tp = mtod(m, struct tftphdr*);
	tp->th_opcode = htons(RRQ);
	strcpy(tp->th_u.tu_stuff, filename);
	strcpy(tp->th_u.tu_stuff+namelen, octet);

	salnet_udpsend(tftpd, m);
}

#define RETRYREQUEST 4  /* re-request after roughly 2 seconds */
/*
	tftp_get 
		get a file from a server running tftpd

	Return  0 on success, file is data and datalen is byte length of file.
	Return server error code (positive number) and error message in data
	Return -1 on arp failure. data is invalid.
*/
static struct mbuf *tftp_reply = 0;
static int straggler_port=0;

salnet_err
saltftp_fetch(ipaddr_t serve_ip, char *filename, long maxlen,
		/*OUT*/ void *data, long *datalen)
{
	int nextblock = 1, errcode=0;
	int serve_port = 0, udpport;
	int retry; 
	salnet_route tftpd_route, recv_route;
	u_char tftpd_ether[ETHERADDRBYTES];
	salnet_err err;

	*datalen = 0;

	if (salnet_getipaddr()==0) {
		*datalen=min(maxlen, strlen(NO_IP)+1);
		strncpy(data, NO_IP, *datalen);
		return EBADID;
		}

	err = salnet_arp(serve_ip,tftpd_ether);
	if (err) return err;
	salnet_getlocalroute(&tftpd_route);
	tftpd_route.localudpport = TFTPC_PORT;
	tftpd_route.remotudpport = TFTPS_PORT;
	tftpd_route.remotip = serve_ip;
	tftpd_route.remotether = &tftpd_ether[0];

	salnet_begin();
	tftp_req(&tftpd_route, filename);

	while(TRUE) {
		long  blocklen;
		struct tftphdr *tp;
		struct mbuf *m;

		/* poll for tftp packet */

		retry = RETRYREQUEST;

		bcopy(&tftpd_route,&recv_route,sizeof(salnet_route));
		err = salnet_udprecv(&recv_route, 2000, &tftp_reply);
		if (err) salnet_perror("tftp", err);
		while (!tftp_reply) {
			if (nextblock-1)
				tftp_ack(&tftpd_route, nextblock-1);
			else {
				--retry;
				if (retry==0) {
					printf("rerequest %s ",filename);
					tftp_req(&tftpd_route, filename);
					retry = RETRYREQUEST;
					*datalen=0;
					nextblock = 1;
					straggler_port = serve_port;
					serve_port = 0;
					}
				}
			bcopy(&tftpd_route,&recv_route,sizeof(salnet_route));
			err=salnet_udprecv(&recv_route, 2000, &tftp_reply);
			if (err) salnet_perror("tftp", err);
			}

		m=tftp_reply;
		while (m->m_len==0 && m->m_next) m = m->m_next;

		/* check what we got */
		tp = mtod(m, struct tftphdr*);

		udpport = recv_route.remotudpport;
		if (!serve_port) {
			/* look for block 1 from a new server port */
			if (udpport == straggler_port) {
				m_freem(tftp_reply);
				continue;
				}
			serve_port = udpport;
			tftpd_route.remotudpport = recv_route.remotudpport;
			}
		if (serve_port != udpport) {
			printf("tftp: from udp port %d block %d expecting port %d. discarding pkt.\n", udpport,ntohs(tp->th_u.tu_block), serve_port);
			m_freem(tftp_reply);
			continue;
			}
			
		if (tftp_reply->m_pkthdr.len<TFTPHDRBYTES) {
DB("tftp: bad len %d < %d\n",tftp_reply->m_len,TFTPHDRBYTES);
			m_freem(tftp_reply);
			continue;
			}
		blocklen = tftp_reply->m_pkthdr.len-TFTPHDRBYTES;

		if (ntohs(tp->th_opcode)== ERROR) {
			strncpy(data,tp->th_data,blocklen);
			((char*)data)[blocklen] = '\0';
			*datalen=blocklen;
			errcode= ntohs(tp->th_u.tu_code);
			m_freem(tftp_reply);
			break;
			}

		if (ntohs(tp->th_opcode)!= DATA ||
		   (ntohs(tp->th_u.tu_block)>nextblock)) {
DB("tftp: bad op %d. block %d not next %d\n",ntohs(tp->th_opcode),
	   ntohs(tp->th_u.tu_block),nextblock); 
			m_freem(tftp_reply);
			continue;
			}
		if (ntohs(tp->th_u.tu_block)<nextblock) {
DB("tftp: repeat block %d<%d blocklen %d\n",ntohs(tp->th_u.tu_block), nextblock, blocklen);
			m_freem(tftp_reply);
			tftp_ack(&tftpd_route,nextblock-1);
			continue;
			}

		if(errcode) {
			/*
			After overflow, errcode is 2 and we have to recv and
			junk the rest of the packets
			*/
			}
		else if (*datalen + blocklen > maxlen) {
			/* buffer overflow */
			*datalen=min(maxlen, strlen(TOOBIG)+1);
			strncpy(data, TOOBIG, *datalen);
			errcode = ENOSPACE;
			}
		else {
			/* Good tftp data packet. take the data and ack */
			m_copydata(m, TFTPHDRBYTES, blocklen, (char*)data+*datalen);
			*datalen+=blocklen;
			}

DB("tftp ack %d\n",nextblock);
		m_freem(tftp_reply);
		tftp_ack(&tftpd_route,nextblock);

		++nextblock;

		if (blocklen<SEGSIZE)
			break;  /* end of file */
		}


	straggler_port = serve_port;
	salnet_end();
	return errcode;
}
