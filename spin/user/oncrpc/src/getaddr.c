/**
   getaddr.c -- Routine to find local Internet address.
   David Nichols, Xerox PARC
   October, 1991 

   $Id: getaddr.c,v 1.1 1996/02/09 18:08:21 mef Exp $
*/
/* Copyright (c) 1991, 1992 Xerox Corporation.  All rights reserved.

   Use and copying of this software and preparation of derivative works
   based upon this software are permitted.  Any distribution of this
   software or derivative works must comply with all applicable United
   States export control laws.  This software is made available AS IS, and
   Xerox Corporation makes no warranty about the software, its performance
   or its conformity to any specification. */

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>		/* needed on AIX, at least */
#include <sys/ioctl.h>
#include <net/if.h>
#include <netinet/in.h>

#define NRECS	10
static struct ifreq ifr[NRECS];

/* Return a local Internet address in network byte order or 0 if failed. */
long GetLocalInetAddr()
{
    struct ifconf ifc;
    int nInts;
    int i;
    struct sockaddr_in addr;
    struct ifreq *ifp;
    int s;

    s = socket(AF_INET, SOCK_DGRAM, 0);
    if (s < 0)
	return 0;
    /* First get a list of all interfaces on this machine. */
    ifc.ifc_len = sizeof ifr;
    ifc.ifc_req = ifr;
    if (ioctl(s, SIOCGIFCONF, (char *) &ifc) < 0) {
	close(s);
	return 0;
    }
    nInts = ifc.ifc_len / sizeof(struct ifreq);
    /* Now check each one out. */
    for (i = 0; i < nInts; ++i) {
	ifp = &ifr[i];
	/* Only Internet interfaces. */
	if (ifp->ifr_addr.sa_family != AF_INET)
	    continue;
	if (ioctl(s, SIOCGIFFLAGS, (char *) ifp) < 0)
	    continue;
	/* Skip down interfaces, loopback interfaces or interfaces that
	   aren't either broadcast or point-to-point. */
	if ((ifp->ifr_flags & IFF_UP) == 0 ||
	    (ifp->ifr_flags & IFF_LOOPBACK) != 0 ||
	    (ifp->ifr_flags & (IFF_BROADCAST | IFF_POINTOPOINT)) == 0)
	    continue;
	if (ioctl(s, SIOCGIFADDR, (char *) ifp) < 0)
	    continue;
	bcopy((char *) &ifp->ifr_addr, (char *) &addr, sizeof ifp->ifr_addr);
	/* addr has the answer. */
	close(s);
	return addr.sin_addr.s_addr;
    }
    close(s);
    return 0;
}
