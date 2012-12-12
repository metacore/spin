/*
  	stub_subr.c

	These subroutine stubs exist so the driver code salboot will link
	without extensive ifdef'ing.

	created by David Becker Wed Jun 25 09:34:50 PDT 1997
 */

#include <sys/param.h>
#include <sys/types.h>
#include <sys/kernel.h>
#include <sys/systm.h>
#include <sys/mbuf.h>
#include <sys/queue.h>
#include <net/if.h>
#include <net/if_arp.h>
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

#define LOCORE
#include <machine/spl.h>


void timeout(timeout_func_t func, void *arg, int time)
{
}

void untimeout(timeout_func_t func, void *arg)
{
}

void
dev_attach(void){}

void
dev_detach(void){}


void
panic(const char *s, ...)
{
	printf("PANIC %s\n",s);
	exit();
}

/* a surrogate */
int
DELAY(int val)
{
	int c;
	int i;
	for(i=0,c=0; c<val*10000; c++, i++);
	return i;
}

unsigned long
kvtop(void* x){return x;};

void
log(int i, const char *c, ...){printf("log|");};

/* kdc_externalize_t devconf.h */
int
isa_generic_externalize(void){printf("isa_generic_externalize|");return 0;};
/* kdc_internalize_t devconf.h */
long kdc_isa0;

void
splz(void){printf("splz|");};
#define SPLEXTREME (HWI_MASK|SWI_MASK)
unsigned cpl = 0;
unsigned ipending = 0;
unsigned net_imask = 0;
unsigned bio_imask = 0;
unsigned tty_imask = 0;
unsigned atdevbase = 0;


/* misc */
int bootverbose = 0;

int	copyout(void *kaddr, void *udaddr, u_int len)
{
	printf("copyout()|");
	return -1;
}

unsigned long 
pmap_mapdev(void)
{
	printf("pmap_mapdev()|");
	return 0;
}

int ttd_run_status=0;

ttd_ether_packet()
{
	return 0; /* no packets are ttd packets as far as salboot goes */
}
