
#include <sal/salnet.h>

#include <sys/param.h>
#include <sys/types.h>
#include <sys/kernel.h>
#define SALNET
#include <sys/mbuf.h>
#include <netinet/in.h>
#include <net/if.h>
#include <netinet/if_ether.h>
#include <netinet/if_ether.h>
#include <machine/spl.h>
#include <arpa/nameser.h>
#include <sal/salnet/resolv.h>

char mydomain[]="cs.washington.edu";
int dnsip = 0x04025f80; /* XXX june */

struct __res_state _res;

static char *ns_ops[] = {
	"", " inv_q", " stat", " op3", " op4", " op5", " op6", " op7",
	" op8", " updataA", " updateD", " updateDA",
	" updateM", " updateMA", " zoneInit", " zoneRef",
};

static char *ns_resp[] = {
	"", " FormErr", " ServFail", " NXDomain",
	" NotImp", " Refused", " Resp6", " Resp7",
	" Resp8", " Resp9", " Resp10", " Resp11",
	" Resp12", " Resp13", " Resp14", " NoChange",
};


/* skip over a domain name */
static const u_char *
ns_nskip(register const u_char *cp)
{
	register u_char i;

	if (((i = *cp++) & 0xc0) == 0xc0)
		return (cp + 1);
	while (i) {
		cp += i;
		i = *cp++;
	}
	return (cp);
}

/* print a reply */
static void
ns_rprint(register const u_char *cp, register const u_char *bp,
	  register const u_char *ep)
{
	register u_int i;
	u_short typ;

	cp = ns_nskip(cp);

	if (cp + 10 > ep)
		return;

	/* print the type/qtype and class (if it's not IN) */
	typ = *cp++ << 8;
	typ |= *cp++;
	i = *cp++ << 8;
	if ((i |= *cp++) != C_IN)
		if (i == C_ANY)
			printf("(c_any)");
		else
			printf("(Class %d)", i);

	/* ignore ttl & len */
	cp += 6;
	/*
	printf(" %s", tok2str(type2str, "Type%d", typ));
	*/
	switch (typ) {

	case T_A:
		/*
		printf(" %s", ipaddr_string(cp));
		*/
		printf(" %d.%d.%d.%d\n", cp[0],cp[1],cp[2],cp[3]);
		break;

	case T_NS:
	case T_CNAME:
	case T_PTR:
		/*
		ns_nprint(cp, bp, ep);
		break;

	case T_MX:
		ns_nprint(cp+2, bp, ep);
		*/
#ifndef TCPDUMP_ALIGN
		printf(" %d", *(short *)cp);
#else
		{
		    u_short x = *cp | cp[1] << 8;
		    printf(" %d", ntohs(x));
		}
#endif
		break;
	}
}

print_answer(HEADER *np, int len)
{
	int qdcount, ancount, nscount, arcount;

	/* get the byte-order right */
	qdcount = ntohs(np->qdcount);
	ancount = ntohs(np->ancount);
	nscount = ntohs(np->nscount);
	arcount = ntohs(np->arcount);

	printf(" %d%s%s%s%s%s",
		ntohs(np->id),
		ns_ops[np->opcode],
		ns_resp[np->rcode],
		np->aa? "*" : "",
		np->ra? "" : "-",
		np->tc? "|" : "");
	if (qdcount != 1)
		printf(" [%dq]", qdcount);
	printf(" %d/%d/%d", ancount, nscount, arcount);
	if (ancount)
		ns_rprint(ns_nskip((const u_char *)(np + 1)) + 4,
			  (const u_char *)np, (const u_char *)np+len);
}

salnet_err
saldns_answer(struct mbuf *m, ipaddr_t *ip)
{
	HEADER *np;
	const char *cp;
	int ancount;

	np = mtod(m, HEADER*);

	if (!np->qr) return SALNET_BADREPLY;

	/*
	print_answer(np, m->m_len);
	*/

	ancount = ntohs(np->ancount);
	cp = ns_nskip((const u_char *)(np + 1)) + 4;

	while(ancount--) {
		u_short type, dlen, class;
		int ttl;
		cp = ns_nskip(cp);

		GETSHORT(type, cp)
		GETSHORT(class, cp)
		GETLONG(ttl, cp)
		GETSHORT(dlen, cp)

		if (type==T_A) {
			*ip = *(int*)cp;
			return SALNET_SUCCESS;
			}

		cp += dlen;
		}
		
	return SALNET_BADREPLY;
}

salnet_err
saldns_query(char *hostname, ipaddr_t *ip)
{
	salnet_route dnsroute;
	struct mbuf *m;
	char dnsether[6];
	char nbuf[2*MAXDNAME+2];
	int err, reqbytes;
	char request[256];

	if (!ip) return SALNET_BADARGS;
	*ip = 0;
	if (!hostname) return SALNET_BADARGS;

	err = salnet_arp(dnsip, dnsether);
	if (err) return err;
	salnet_getlocalroute(&dnsroute);

	dnsroute.localudpport=3333;
	dnsroute.remotudpport=NAMESERVER_PORT;
	dnsroute.remotip=dnsip;
	dnsroute.remotether=dnsether;

	if (!index(nbuf,'.')) sprintf(nbuf,"%s.%s",hostname,mydomain);

	reqbytes = res_mkquery(QUERY,nbuf,C_IN,T_A,0,0,0,request,256);

	err = salnet_udploop(&dnsroute, request, reqbytes,
			3, 500, sizeof(HEADER), &m);
	if (err) return err;

	err = saldns_answer(m,ip);
	m_freem(m);
	return err;
}
