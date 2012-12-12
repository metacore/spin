

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/types.h>
#include <net/if.h>
#include <sys/mbuf.h>


#include <sal/salnet.h>

#ifdef OSF
#include "../ALPHA_SPIN/StcpEtherDev.h"
#endif
#ifdef __FreeBSD__
#include "../IX86_SPIN/StcpEtherDev.h"
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
			(*StcpEtherDev__Receive)(0,m);

		IF_DEQUEUE(&ifp->if_rcv,m);
		}
}

install_bootlisten()
{
	salnet_route bootroute;
	salnet_getlocalroute(&bootroute);

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

ifpsend(struct ifnet *ifp, struct mbuf *m)
{
	long s = splhigh();
        IF_ENQUEUE(&ifp->if_snd, m);
	(*ifp->if_start)(ifp);
	splx(s);
}

#ifdef __FreeBSD__
/*
 * This routine does all the checksum computations that don't
 * require anything special (like copying or special headers).
 */

unsigned short in_checksum(unsigned char * buff, int len, unsigned long sum)
{
	/* Do the first multiple of 4 bytes and convert to 16 bits. */
	if (len > 3)
	{
		__asm__("clc\n"
		"1:\t"
		"lodsl\n\t"
		"adcl %%eax, %%ebx\n\t"
		"loop 1b\n\t"
		"adcl $0, %%ebx\n\t"
		"movl %%ebx, %%eax\n\t"
		"shrl $16, %%eax\n\t"
		"addw %%ax, %%bx\n\t"
		"adcw $0, %%bx"
		: "=b" (sum) , "=S" (buff)
		: "0" (sum), "c" (len >> 2) ,"1" (buff)
		: "ax", "cx", "si", "bx" );
	}
	if (len & 2)
	{
		__asm__("lodsw\n\t"
		"addw %%ax, %%bx\n\t"
		"adcw $0, %%bx"
		: "=b" (sum), "=S" (buff)
		: "0" (sum), "1" (buff)
		: "bx", "ax", "si");
	}
	if (len & 1)
	{
		__asm__("lodsb\n\t"
		"movb $0, %%ah\n\t"
		"addw %%ax, %%bx\n\t"
		"adcw $0, %%bx"
		: "=b" (sum), "=S" (buff)
		: "0" (sum), "1" (buff)
		: "bx", "ax", "si");
	}
	sum =~sum;
	return(sum & 0xffff);
}

/* Refer to i386_freebsd/machine/endian.h */
uint byte_swap_long(uint i)
{
	return __byte_swap_long(i);
}

ushort byte_swap_word(ushort s)
{
	return __byte_swap_word(s);
}

#endif
