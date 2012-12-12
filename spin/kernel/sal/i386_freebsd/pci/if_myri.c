/*
 * The basis of the FreeBSD myrinet driver is the BSDi driver of the
 * MOSIX project. 
 * The port was done in the Spring/summer of 1997.
 *
 * Contact: hutton@isi.edu (Anne Hutton) 
 *	    atomic-2@isi.edu
 */


/*
 * Copyright (c) 1997 University of Southern California.
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation, advertising
 * materials, and other materials related to such distribution and use
 * acknowledge that the software was developed by the University of
 * Southern California, Information Sciences Institute.  The name of the
 * University may not be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 * 
 */

/*
 * $Id: if_myri.c,v 1.2 1998/03/14 05:30:04 ulbright Exp $
 */
/*
 * Copyright (c) 1995, The Hebrew University of Jerusalem.
 * All rights reserved.
 *
 * Author(s): Ilia Gilderman
 *
 * Permission to use, copy, modify and distribute this software and
 * its documentation  is  hereby granted,  provided  that  both the
 * copyright notice and this permission notice appear in all copies
 * of the software, derivative works or modified versions,  and any
 * portions  thereof,  and that  both notices appear in  supporting
 * documentation.
 *
 * HEBREW UNIVERSITY ALLOWS FREE USE OF THIS SOFTWARE IN ITS "AS IS"
 * CONDITION.  HEBREW UNIVERSITY DISCLAIMS ANY LIABILITY OF ANY KIND
 * FOR ANY DAMAGES WHATSOEVER RESULTING FROM THE USE OF THIS SOFTWARE.
 *
 * Hebrew University requests users of this software to return to:
 *
 *  MOSIX software distribution, mosix@CS.huji.ac.il
 *  Institute of Computer Science
 *  The Hebrew University
 *  Jerusalem 91904, Israel
 *
 * any improvements or extensions that they make and grant Hebrew
 * University the rights to redistribute these changes.
 *
 */

#define MULTICAST
#define MYRI_DEBUG

#include <sys/param.h>			/* ALWAYS included */
#include <sys/device.h>			/* generic device definitions */
#include <sys/mbuf.h>		
#include <sys/conf.h>
#include <sys/systm.h>
#include <sys/socket.h>
#include <sys/errno.h>
#include <sys/ioctl.h>
#include <sys/proc.h>

#include <vm/vm.h>

#include <net/if.h>
#include <net/netisr.h>

#include <netinet/in.h>
#include <netinet/in_systm.h>
#include <netinet/in_var.h>
#include <netinet/ip.h>
#include <netinet/if_ether.h>

#include <pci/pcireg.h>
#include <pci/pcivar.h>
#include <vm/vm_extern.h>
#include <vm/pmap.h>
#include <sys/kernel.h>
#include "bpfilter.h"
#include "myri.h"
#include "pci.h"
#if !defined(IFF_NOTRAILERS)
#define IFF_NOTRAILERS          0
#endif

#if NBPFILTER > 0
#include <net/bpf.h>
#include <net/bpfdesc.h>
#endif /* NBPFILTER > 0 */

#include <MyrinetPCI.h>
#include <mcpConstants.h>
#include <lanai_device.h>
#include <myriInterface.h>
#include <myriApi.h>
#include <myriApiExternal.h>

#include <pci/myrivar.h>

#define MYRI_WATCH_TIMEOUT 	2
#define MYRI_FUNCTION	  	0
/*
 * PCI configuration registers offset and masks
 */
#define PCI_COMMAND_MEMORY      0x0002    	/* Enable Memory access */
#define PCI_COMMAND_MASTER      0x0004    	/* Enable BUS master  */
#define PCI_CBMA                0x10


static char *myri_pci_probe __P((pcici_t, pcidi_t));
static void myri_pci_attach __P((pcici_t, int));
static int myriattach __P((struct myri_softc *,int unit));
static int myriioctl __P((register struct ifnet *, int, caddr_t));
static void myristart __P((struct ifnet *));
#ifdef SPIN
static int myriintr();
#else
static void myriintr __P((void *));
#endif
static int myri_check_receive __P((struct myri_softc *, int));

static u_long myri_count;
struct pci_device myridevice = {
    "myri",
    myri_pci_probe,
    myri_pci_attach,
    &myri_count,
    NULL,
};
DATA_SET (pcidevice_set, myridevice);

static int myri_stuffbuffer __P((struct myri_softc *, int, struct mbuf *));
static int myriinit __P((struct myri_softc *));
static void myristop __P((struct myri_softc *));
static void myrishutdown __P((int, void *));
static void myriwatchdog __P((struct ifnet *));
static void myribanner __P((struct myri_softc *));
#ifndef SPIN
static char *ether_sprintf __P((u_char *)); 
#endif

#ifdef SPIN
#define ETHER_ADDR_LEN     6
#define MYRI_UNIT_TO_SOFTC(unit)       (myris[unit])
#endif

struct myri_softc *myris[NMYRI];

#ifdef	DMA
#ifndef SPIN
static
#endif
void myrifree(caddr_t, u_int);
static void myriref (caddr_t b, u_int n);
#endif

static char *
myri_pci_probe(config_id, device_id)
     pcici_t config_id;
     pcidi_t device_id;
{
	if ((PCI_VENDORID(device_id) == MYRINET_PCI_VENDOR_ID)
			&& (PCI_CHIPID(device_id) == MYRINET_PCI_DEVICE_ID))
		return "Myrinet-LAN/PCI interface (M2F-PCI32)";
	return NULL;
}

static void 
myri_pci_attach(config_id, unit)
     pcici_t config_id;
     int unit;
{
	vm_offset_t pa;
	struct myri_softc *sc;
	int s;
		
	if (unit >= NMYRI) {
		printf("myri%d: not configured; kernel is built for only %d 
			device%s.\n", unit, NMYRI, NMYRI == 1 ? "" : "s");
		return;
	}
	sc = malloc(sizeof(struct myri_softc), M_DEVBUF, M_NOWAIT);
	if (sc == NULL)
		return;
	bzero(sc, sizeof(struct myri_softc));               
	
	s = splimp();
	if (!pci_map_mem(config_id, PCI_CBMA,
			(vm_offset_t *) &sc->myri_base, &pa)) {
		printf("myri%d: couldn't map memory\n", unit);
		goto fail;
	}
	if (!pci_map_int(config_id, myriintr, (void *)sc, &net_imask)) {
		printf("myri%d: couldn't map interrupt\n", unit);
		goto fail;
	}
	else {
		printf("myri%d: interrupt attached softc %x\n",unit,sc);
	}
	if (myriattach(sc,unit)) {
		splx(s);
		return;
	}
	printf("myri%d: couldn't attach interface\n", unit);
	(void) pci_unmap_int(config_id);
fail:
	if (sc)
		free(sc, M_DEVBUF);
	splx(s);
}

static
int
myriattach(sc, unit)
	struct myri_softc *sc;
	int unit;
{
	struct ifnet *ifp = &sc->myri_if;
	struct	 MYRINET_BOARD  *mb;
	int i;

	/*
	 * allocate kernel buffer for a copy block
	 */
#ifdef SPIN
	sc->myri_copy_block = (u_int *)spin_malloc(round_page(COPY_BLOCK_SIZE));
	printf("Use spin_malloc on myri_copy_block. %x at %x.\n",round_page(COPY_BLOCK_SIZE),sc->myri_copy_block );
#else
	sc->myri_copy_block = (u_int *)vm_page_alloc_contig(
		round_page(COPY_BLOCK_SIZE), 0x100000, 0xffffffff,
		PAGE_SIZE);
#endif
	if (sc->myri_copy_block == NULL) {
		printf("myri%d: fails to allocate api copy block\n",unit);
		return(0);
	}
#ifdef DMA
#ifdef SPIN
	sc->recv_vaddr  = (caddr_t)spin_malloc(round_page(RECVBUFFER));
	printf("Use spin_malloc on recv_block. %x at %x.\n",round_page(RECVBUFFER),sc->recv_vaddr  );
#else
	sc->recv_vaddr  = (caddr_t)vm_page_alloc_contig(
		round_page(RECVBUFFER), 0x100000, 0xffffffff,
		PAGE_SIZE);
#endif
	if (sc->recv_vaddr == NULL) {
		printf("myri%d: fails to allocate dma recv block\n",unit);
		/* FIX: free myri_copy_block! */
		return(0);
	}
	sc->recv_paddr = (caddr_t) vtophys(sc->recv_vaddr);
#endif

	sc->myri_reset_count=0;
#ifndef	DMA
	for (i = 0; i < MYRI_NUM_RECEIVES; i++)
		sc->myri_rbuff[i] = NULL;
#endif
        /*
         * initialize the sent packets queue
	 */
	sc->myri_sifq.ifq_maxlen = MYRI_NUM_SENDS;
	sc->myri_sifq.ifq_len = 0;
	sc->myri_sifq.ifq_drops = 0;

	/*
	 * initialize ifnet structure 
	 */
#ifndef SPIN
	ifp->if_softc = sc;
#endif
	ifp->if_unit = unit;
	ifp->if_name = "myri";

#ifdef MULTICAST
	ifp->if_flags = IFF_BROADCAST|IFF_SIMPLEX|IFF_NOTRAILERS|IFF_MULTICAST;
#else
	ifp->if_flags = IFF_BROADCAST|IFF_SIMPLEX|IFF_NOTRAILERS;
#endif /* MULTICAST */
	ifp->if_ioctl = myriioctl;
	ifp->if_output = ether_output;
	ifp->if_start = myristart;
	ifp->if_watchdog = myriwatchdog;
	ifp->if_baudrate = 1280000000L;
	ifp->if_mtu = MYRINET_MTU;	 /* should be MYRINET_MTU for Myrinet */
	ifp->if_timer = 0;
	ifp->if_ipackets = 0;
	ifp->if_opackets = 0;
	ifp->if_ierrors = 0;
	ifp->if_oerrors = 0;
	if_attach(ifp);

#if NBPFILTER > 0
	bpfattach(ifp, DLT_EN10MB, sizeof(struct ether_header));
#endif /* NBPFILTER > 0 */

	mb = (struct MYRINET_BOARD *)sc->myri_base;
	myrinet_init_pointers(unit, mb);

	/*
	 * get the board memory address and set eeprom, reg, memory
	 * (/dev/mlanai support)
	 */
	sc->myri_eeprom = (struct MYRINET_EEPROM *)&mb->lanai_eeprom[0];
	sc->myri_lanai_reg = &mb->lanai_registers[0];
	sc->myri_lanai_ctl = &mb->lanai_control[0];
	sc->myri_memory = &mb->lanai_memory[0];
        sc->myri_open = 0;
	myris[unit]=sc;
	/*
	 *	initialize the interface with interrupts off
	 */
	lanai_get_board_id(unit, sc->myri_addr);
	bcopy(sc->myri_addr, sc->myri_addr8 + MYRI_PAD_LEN, ETHER_ADDR_LEN);
	myribanner(sc);
	myriApiLoadLanai(unit, MYRI_BURST, MYRI_RECEIVE_INTERRUPT_MASK, (u_char *)sc->myri_addr8);
#ifndef SPIN
	at_shutdown(myrishutdown, sc, SHUTDOWN_POST_SYNC);
#endif
	return(1);
}

static
void 
myribanner(sc)
	struct myri_softc *sc;
{
	struct MYRINET_EEPROM *me = sc->myri_eeprom;
	struct ifnet *ifp = &sc->myri_if;
	char buf[33];
	/*
	 * Banner...
	 */
	printf("myri%d: Myrinet PCI-1.1 LANai %d.%d address %s\n",
		ifp->if_unit, 
		(me->lanai_cpu_version & 0x00ff),
		(me->lanai_cpu_version & 0xff00) >> 8,
		ether_sprintf(sc->myri_addr));

#ifdef MYRI_DEBUG
	bzero(buf, 33);
	bcopy(me->fpga_version, buf, 32);
	printf("FPGA version %s\n", buf);
#endif MYRI_DEBUG
}

#ifndef SPIN
static
char *
ether_sprintf(ap)
        register u_char *ap;
{
        register i;
        static char etherbuf[18];
        register char *cp = etherbuf;
	static char digits[] = "0123456789abcdef";

        for (i = 0; i < 6; i++) {
                *cp++ = digits[*ap >> 4];
                *cp++ = digits[*ap++ & 0xf];
                *cp++ = ':';
        }
        *--cp = 0;
        return (etherbuf);
}
#endif

/*
 * initialize the board, reload the MCP ....
 */
static
int
myriinit(sc)
	struct myri_softc *sc;
{
	struct ifnet *ifp = &sc->myri_if;
	int unit = ifp->if_unit;
        register int      i;
	struct mbuf *m;

printf("myri%d: myriinit()\n",unit);
	/*
	 *	reset the interface with interrupts on
	 */
	myriApiLoadLanai(unit, MYRI_BURST, MYRI_RECEIVE_INTERRUPT_MASK,
		(u_char *)sc->myri_addr8);
	myriApiReset(unit, KERNEL_CHANNEL);
	if (!myriApiHandshake(unit, KERNEL_CHANNEL)) {
		printf("myri%d: failure to handshake\n",unit);
		if (sc->myri_reset_count++ > 3) {
			printf("myri%d: failure to reset\n",unit);
			myristop(sc);
		}
		return (0);
	}
	myriApiInterruptEnable(unit);
#ifdef	DMA
	for (i = 0; i < DMA_BUF_SIZE; i++) {
		sc->freelist[i] = i;
		sc->refs[i] = 0;
	}
	sc->nfree = DMA_BUF_SIZE;
	while (myri_stuffbuffer(sc,0,NULL))
		;
#else
	/*
	 * stuffs buffers to the MCP and free all
	 * sent packets from the send queue
	 */ 
	for (i = 0; i < MYRI_NUM_RECEIVES; i++) {
		m = sc->myri_rbuff[i];
		m_freem(m);
		(void)myri_stuffbuffer(sc, i, (struct mbuf *)0);
	}
#endif
	do {
		IF_DEQUEUE(&sc->myri_sifq, m);
		m_freem(m);
	} while(m);
        /*
         * XXX set an interface queue length to be 5000 and not 50
	 * to avoid busy waiting on the queue by ether_output
	 */
	sc->myri_if.if_snd.ifq_maxlen = 5000;
        sc->myri_reset_count = 0;
	ifp->if_flags |= IFF_RUNNING;
	ifp->if_flags &= ~IFF_OACTIVE;
	ifp->if_timer = 0;
        return (1);
}

static
void 
myristop(sc)
	struct myri_softc *sc;
{
	struct ifnet *ifp = &sc->myri_if;
	register int unit = ifp->if_unit;
	struct mbuf *m;
	int i;

	/*
	 *	reset the interface with interrupts off
	 */
	myriApiLoadLanai(unit, MYRI_BURST, 0, (u_char *)sc->myri_addr8);
	ifp->if_flags &= ~(IFF_RUNNING | IFF_OACTIVE);
	ifp->if_timer = 0;
#ifndef	DMA
	for (i = 0; i < MYRI_NUM_RECEIVES; i++) {
		m_freem(sc->myri_rbuff[i]);
		sc->myri_rbuff[i] = NULL;
	}
#endif
	do {
		IF_DEQUEUE(&sc->myri_sifq, m);
		m_freem(m);
	} while(m);
}

static void
myrishutdown(howto, sc)
int howto;
void *sc;
{
	myristop((struct myri_softc *) sc);
}       

static
void myriwatchdog(ifp)
       struct ifnet *ifp;
{
#if 0
	printf("myri%d: device timeout\n", ifp->if_unit);
	ifp->if_oerrors++;
	myriinit(ifp->if_softc);
#endif
}

/*
 * Device interrupt handler
 */
static
#ifdef SPIN
int
myriintr(sc0)
void *sc0;
#else
void
myriintr(sc0)
	void *sc0;
#endif
{
	register struct myri_softc *sc = (struct myri_softc *)sc0;
	register struct ifnet *ifp = &sc->myri_if;
	register int unit = ifp->if_unit;
	register int i;
	static unsigned long count = 0;

	if (!(ifp->if_flags & IFF_RUNNING)) {
		if ((count++ % 10000) == 0)
			printf("myri%d: another 10000 stray interrupts\n",unit);
		return;
	}
	myriApiInterruptDisable(unit);
	for (i = 0; i < 3; i++) 
                if (!myri_check_receive(sc, unit))
                        break;
	myriApiInterruptEnable(unit);
#ifdef SPIN
    /* we call if_recv here where it is always ok to send packets out dev */
    if (ifp->if_rcv.ifq_len) (*ifp->if_recv)(ifp);
#endif
}

/*
 * Get the received packet from the MCP buffers
 */
static
int
myri_check_receive(sc, unit)
	struct myri_softc *sc;
	int unit;
{
	struct ifnet *ifp = &sc->myri_if;
	struct ether_header *eh;
	register struct mbuf *top=NULL;
	register struct mbuf *m, *mp;
	int checksum, ind, num;
	caddr_t addr;
	int len=0;
	int i=0;
	static struct MyriGather scatter[MYRI_MAX_SCATTERS];

	num = myriApiGetScatter(unit, KERNEL_CHANNEL,
		&ind, &checksum, scatter);
	if (num == 0){     /* try to recover some buffers that we've lost */
		return (0);                 
		printf("myri%d: another stray interrupt\n",unit);
	}
	if (num < 0) { 
		myriinit(sc);
		return (0);
	}

#ifdef DMA
	(void) myri_stuffbuffer(sc, 0, NULL);
	MGETHDR(top, M_DONTWAIT, MT_DATA);
	addr = sc->recv_vaddr + MYRI_DATA_ADDR(ind);
	len = scatter[0].length;
	if (!top) {
		myrifree(addr,len);
		return(0);
	}
	top->m_data = top->m_ext.ext_buf = addr;
	top->m_len = top->m_ext.ext_size = len;
	top->m_flags |= M_EXT;
	top->m_ext.ext_free = myrifree;
	top->m_ext.ext_ref = myriref;
#if defined (SPIN) || defined (NETPOLL)
	top->m_ext.ext_refcount.forw = top->m_ext.ext_refcount.back =
	    &top->m_ext.ext_refcount;
#endif /* SPIN || NETPOLL */
	myriref (addr, len);
#else
	top = m = sc->myri_rbuff[ind];
        for (i = 0, len = 0; i < num; i++) {
                m->m_len = scatter[i].length;
                len += scatter[i].length;
                mp = m;
                m = m->m_next;
        }
        /*      
         * if myri_stuffbuffer fault e.i. no more mbufs available
         * we will stuff to MCP the received packet to avoid the MCP
         * sturvation. The packet, of course, will be droped.
         */
        if (!myri_stuffbuffer(sc, ind, (struct mbuf *)0)) {
                (void)myri_stuffbuffer(sc, ind, top);
                return (0);
	}
        mp->m_next = 0;
        m_freem(m);
#endif
        if (*mtod(top, u_short *) != mx_ntohs(MYRI_PAD)) {
#ifdef MYRI_DEBUG
#ifndef SPIN
                printf("bad pad=0x%x len=%d ind=%d rbuf=0x%x top=0x%x\n",
                        *mtod(top, u_short *), len, ind, sc->myri_rbuff, top);
#endif
#endif MYRI_DEBUG
                sc->myri_if.if_ierrors++;
                goto nombufs;
        }
        top->m_data += MYRI_PAD_LEN;
        top->m_len -= MYRI_PAD_LEN;
        top->m_pkthdr.rcvif = &sc->myri_if;
        top->m_pkthdr.len = len - MYRI_PAD_LEN;

        eh = mtod(top, struct ether_header *);
#if NBPFILTER > 0
        if (ifp->if_bpf) {
                bpf_mtap(ifp, top);

                if ((ifp->if_flags & IFF_PROMISC) &&
                        bcmp(eh->ether_dhost,sc->myri_addr,
                                sizeof(eh->ether_dhost)) != 0 &&
#ifdef MULTICAST
			(eh->ether_dhost[0] & 1) == 0)
#else
                        bcmp(eh->ether_dhost, etherbroadcastaddr,
                                sizeof(eh->ether_dhost)) != 0)
#endif MULTICAST
                                {
                        goto nombufs;
                }
        }
#endif /* NBPFILTER > 0 */

        m_adj(top, sizeof(struct ether_header));
	/*
	eh->ether_type = ntohs((u_short) eh->ether_type);
	*/
        ifp->if_ipackets++;
        ether_input(ifp, eh, top);              /* pass to upper layer*/
        return (len);

nombufs :
        if (top)
		m_freem(top);
        return (0);
}

#ifdef DMA
static
void myriref(addr, len)
	caddr_t addr;
	u_int len;
{
	unsigned int n;
	struct myri_softc *sc;

	addr -= MYRI_HDR_LEN;
	sc = *((struct myri_softc **) addr);
	n = (addr - sc->recv_vaddr) / MAX_RBUF_SIZE;
	sc->refs[n]++;
}

#ifndef SPIN
static
#endif
void myrifree(addr, len)
	caddr_t addr;
	u_int len;
{
	unsigned int n;
	struct myri_softc *sc;

	addr -= MYRI_HDR_LEN;
	sc = *((struct myri_softc **) addr);
	n = (addr - sc->recv_vaddr) / MAX_RBUF_SIZE;
	if (--sc->refs[n] != 0)
		return;
	sc->freelist[sc->nfree++] = n;
	(void) myri_stuffbuffer(sc, 0, NULL);
}
#endif

static
int myri_stuffbuffer(sc, ind, m0)
	struct myri_softc *sc;
	int ind;
	register struct mbuf *m0;
{
	register int unit = sc->myri_if.if_unit;
	static struct MyriGather scatter[MYRI_MAX_SCATTERS];

#ifdef	DMA
	if (sc->nfree == 0)
		return(0);
	ind = sc->freelist[sc->nfree - 1];
	*((struct myri_softc **) (sc->recv_vaddr + MYRI_HDR_ADDR(ind))) = sc;
	scatter[0].pointer = sc->recv_paddr + MYRI_DATA_ADDR(ind);
	scatter[0].length = MYRI_DATA_LEN;
	if (myriApiAddScatter(unit,KERNEL_CHANNEL,ind,1,scatter) <= 0)
		return(0);
	sc->nfree--;
	return(1);
#else
	register int i;
        register struct mbuf *m, *n;

        if (m0 == (struct mbuf *)0) {
                MGETHDR(m0, M_DONTWAIT, MT_DATA);
                if (m0 == (struct mbuf *)0)
                        goto nombufs;
                MCLGET(m0, M_DONTWAIT);
                if ((m0->m_flags & M_EXT) == 0)
                        goto nombufs;
                n = m0;
                for (i = 0; i < MYRI_CLUSTER_NUM - 1; i++) {
                        MGET(m, M_DONTWAIT, MT_DATA);
                        if (m == (struct mbuf *)0)
                                goto nombufs;
                        MCLGET(m, M_DONTWAIT);
                        n->m_next = m;
                        if ((m->m_flags & M_EXT) == 0)
                                goto nombufs;
                        n = m;
                }
        }

        for (i = 0, m = m0; m; m = m->m_next, i++) {
                scatter[i].length = m->m_ext.ext_size;
                scatter[i].pointer = (void *)pmap_extract(kernel_pmap,
                        mtod(m, vm_offset_t));
        }
        sc->myri_rbuff[ind] = m0;
        if (myriApiAddScatter(unit, KERNEL_CHANNEL, ind,
                MYRI_CLUSTER_NUM, scatter) <= 0) {
#ifdef MYRI_DEBUG
                printf("myriApiAddScatter() fault");
#endif MYRI_DEBUG
                goto nombufs;
        }

        return (1);

nombufs :
        sc->myri_rbuff[ind] = (struct mbuf *)0;
        m_freem(m0);
        return (0);
#endif
}

/*
 * frees the packets already transmitted by MCP
 */
#define MYRI_SENDFREE() {					\
	register struct mbuf *m;				\
	register int pend;					\
	pend = myriApiGetNumSendsPending(unit, KERNEL_CHANNEL);	\
	if (pend >= 0) {					\
		while (sc->myri_sifq.ifq_len > pend+1) {	\
			IF_DEQUEUE(&sc->myri_sifq, m);		\
			m_freem(m);				\
		}						\
	}							\
}

/* 
 * transmit the packet
 */
static
void
myristart(ifp)
	register struct ifnet *ifp;
{
#ifdef SPIN
	register struct myri_softc *sc = MYRI_UNIT_TO_SOFTC(ifp->if_unit);
#else
	register struct myri_softc *sc = (struct myri_softc *) ifp->if_softc;
#endif
	int unit = ifp->if_unit;
	register struct mbuf *m0, *m, *mm = (struct mbuf *)0;
	int iovcnt, len, ret;
	static struct MyriGather gather[UIO_MAXIOV];  /* XXX */
	u_char *p;
	u_int32_t address[2];


again :
        m0 = ifp->if_snd.ifq_head;
	if (m0 == (struct mbuf *)0) {
		/*
		 * XXX nothing to do so it is good chance that DMA
		 * is not working now, we will try to free all
		 * packets that are already sent by MCP
		 */
		 MYRI_SENDFREE();
		 return;
	}
	IF_DEQUEUE(&ifp->if_snd, m0);
	M_PREPEND(m0, MYRI_PAD_LEN, M_DONTWAIT);

	if (m0 == (struct mbuf *)0) {
#ifdef MYRI_DEBUG
		printf("send fault no mbufs\n");
#endif MYRI_DEBUG
        	return;
	}
#ifdef NEWCOMPRESS
resend:
#endif NEWCOMPRESS
	/*
	* pass over the mbuf chain and rearrange the chain so that all
	* mbufs will be 4 byte length aligned and 4 byte pointer aligned
	*/
	for (iovcnt = 0, m = m0, len = m0->m_pkthdr.len; m && len; ) {
		if (m->m_len & 0x3) {
			register int pad = MYRI_ALIGN - (m->m_len & 0x3);
			if (M_TRAILINGSPACE(m) < pad) {
				register u_char *p0 = mtod(m, u_char *);
				if(M_LEADINGSPACE(m)<(pad-M_TRAILINGSPACE(m))) {
					printf("No leading space\n");
					m_freem(m0);
					return;
					}
				m->m_data -= (pad-M_TRAILINGSPACE(m));
				ovbcopy(p0, mtod(m, caddr_t), m->m_len);
			}
			if (mm == (struct mbuf *)0)
				mm = m->m_next;
				if (mm) {
				register int lpad = MIN(pad, mm->m_len);
				bcopy(mtod(mm, caddr_t),
				mtod(m, caddr_t)+m->m_len, lpad);
				m->m_len += lpad;
				mm->m_data += lpad;
				mm->m_len -= lpad;
				mm = mm->m_next;
				continue;
			}
			else {
				m->m_len += pad;
			}
		}
		else
			mm = (struct mbuf *)0;
		/*
		* XXX ext. buf may be misaligned from its beginning
		* so we must not copy backwards in that case.
		*/
		if ((int)mtod(m, char *) & 0x3) {
			register int over = (int)mtod(m, char *) & 0x3;
			ovbcopy(mtod(m, char *), mtod(m,char *)-over, m->m_len);
			m->m_data -= over;
		}
		if (m->m_len) {
			gather[iovcnt].pointer =
		  		(myriApiDmaAddr_t) vtophys(mtod(m,vm_offset_t));
			gather[iovcnt].length = m->m_len;
			len -= m->m_len;
			iovcnt++;
		}
		m = m->m_next;
	}
#if NBPFILTER > 0
	if (ifp->if_bpf) {
		/*
	       	 * XXX before passing the pakcet to the bpf
		 * remove Myrinet padding
		 */
		m0->m_len -= MYRI_PAD_LEN;
		m0->m_pkthdr.len -= MYRI_PAD_LEN;
		m0->m_data += MYRI_PAD_LEN;
		bpf_mtap(ifp, m0);

		m0->m_len += MYRI_PAD_LEN;
		m0->m_pkthdr.len += MYRI_PAD_LEN;
		m0->m_data -= MYRI_PAD_LEN;
	}
#endif /* NBPFILTER > 0 */


#ifdef NEWCOMPRESS
        if (iovcnt > MYRI_MAX_GATHERS) {
                struct mbuf *mt0, *mt1, *m2;
                int len, mlen, space, soff, doff;

                MGETHDR(mt0, M_DONTWAIT, MT_DATA);
                if (mt0 == (struct mbuf *) 0)
                        goto nombufs;
                mt0->m_pkthdr.len = len = m0->m_pkthdr.len;
                mt0->m_pkthdr.rcvif = (struct ifnet *) 0;
                mt0->m_len = 0;
                mt1 = mt0;
                m2 = m0;
                space = MHLEN;
                soff = doff = 0;

                while (len > 0) {
                        mlen = MIN(space, m2->m_len);
                        bcopy(mtod(m2, char *) + soff, mtod(mt1, char *) + doff,
 mlen);
                        space -= mlen;
                        soff += mlen;
                        doff += mlen;
                        m2->m_len -= mlen;
                        mt1->m_len += mlen;
                        len -= mlen;
                        if (len == 0)
                                break;
                        if (space == 0) {
                                MGET(mt1->m_next, M_DONTWAIT, MT_DATA);
                                if (mt1->m_next == (struct mbuf *) 0) {
                                        m_freem(mt0);
                                        goto nombufs;
                                }
                                mt1 = mt1->m_next;
                                mt1->m_len = 0;
                                if (len > MINCLSIZE) {
                                        MCLGET(mt1, M_DONTWAIT);
                                        if ((mt1->m_flags & M_EXT) == 0) {
                                                m_freem(mt0);
                                                goto nombufs;
                                        }
                                }
                                space = mt1->m_flags & M_EXT ?
                                        mt1->m_ext.ext_size : MLEN;
                                doff = 0;
                        }
                        if (m2->m_len == 0) {
                                soff = 0;
                                m2 = m2->m_next;
                        }
                }
                m_freem(m0);
                m0 = mt0;
                goto resend;
        }

#else
        /*
         * if the chain was longer than MAX_MYRI_GATHER
         * we will try to compress it
         */
        if (iovcnt > MYRI_MAX_GATHERS) {
                register int lead, trail;
                for (iovcnt = 0, m = m0; m; ) {
                        if ((lead = M_LEADINGSPACE(m)) > 0) {
                                register char *p0 = mtod(m, char *);
                                printf("leading space\n");
                                if (lead & 0x3) {
                                        printf("not alligned leading space\n");
                        }
                                m->m_data -= lead;
                                ovbcopy(p0, mtod(m, caddr_t), m->m_len);
                        }
                        if ((trail = M_TRAILINGSPACE(m)) > 0 && m->m_next) {
                                register int pull = min(trail,m->m_next->m_len);
                                bcopy(mtod(m->m_next, char *),
                                        mtod(m, char *) + m->m_len, trail);
                                m->m_next->m_len -= pull;
                                m->m_next->m_data += pull;
                                m->m_len += pull;
                                trail -= pull;
                        }
                         if (m->m_next && m->m_next->m_len == 0) {
                                struct mbuf *mm = m->m_next;
                                m->m_next = mm->m_next;
                                m_free(mm);
                                continue;
                        }
                        gather[iovcnt].pointer =
                                (void *)pmap_extract(kernel_pmap,
                                        mtod(m, vm_offset_t)); 
                        gather[iovcnt++].length = m->m_len;
                        m = m->m_next;
                }
                if (iovcnt > MYRI_MAX_GATHERS) {
                        printf("long chain %d...len = %d ..dropped\n",iovcnt);
                        m_freem(m0);
                        return;
                }
        
#ifdef MYRI_DEBUG 
                {
                register int total = 0;
                for (m = m0; m; m = m->m_next) { 
                        if (m->m_len & 0x3)
                                printf("not aligned length\n");
                        if ((int)mtod(m, char *) & 0x3)
                                printf("not aligned pointer\n");
                                total += m->m_len;
                }
                if (total != m0->m_pkthdr.len)
                        printf("wrong total in compression total=%d len=%d\n",
                                total, m0->m_pkthdr.len);
                }
#endif MYRI_DEBUG
        }       

#endif /* NEWCOMPRESS */

	/* set the destination address */
        p = mtod(m0, u_char *);
        if (p[MYRI_PAD_LEN] & 0x1) {
		/* multicast or broadcast packet are broadcast destinations */
		address[0] = htonl(0xFFFFFFFF);
		address[1] = htonl(0xFFFFFFFF);
	}
        else {
		address[0] = *((u_int32_t *) p) & htonl(0x0000FFFF);
		address[1] = *((u_int32_t *) &p[sizeof(u_int32_t)]);
	}

	/* lets do this MyriPad stuff right */
	*((u_short *) p) = htons(MYRI_PAD);


	/*
	 * pass the chain to the MCP
	 */
	if ((ret = myriApiSend(unit, KERNEL_CHANNEL, (char *) &address,
		KERNEL_CHANNEL, iovcnt, gather)) <= 0) {
		if (ret < 0) {                  /* Fault */
#ifdef MYRI_DEBUG
			printf("myristart() send fault\n");
#endif MYRI_DEBUG
			sc->myri_if.if_oerrors++;
		}
		m_freem(m0);
		return;
	}
	else  {
		/*
		 * everything is OK so we will put this mbuf on the
		 * queue and later will try to free all mbufs that
		 * were actualy sent by the MCP
		 */
		sc->myri_if.if_opackets++;
		IF_ENQUEUE(&sc->myri_sifq, m0);
		if (IF_QFULL(&sc->myri_sifq))
			 MYRI_SENDFREE();
		sc->myri_if.if_timer = MYRI_WATCH_TIMEOUT;
	}
	goto again;
#ifdef NEWCOMPRESS
  nombufs:
        m_freem(m0);
	printf("send fault no mbufs\n");
	return;
#endif NEWCOMPRESS
}

/*
 * Process an ioctl request
 */
static
int
myriioctl(ifp, cmd, data)
	register struct ifnet *ifp;
	int cmd;
	caddr_t data;
{
#ifdef SPIN
	register struct myri_softc *sc = MYRI_UNIT_TO_SOFTC(ifp->if_unit);
#else
	register struct myri_softc *sc = (struct myri_softc *) ifp->if_softc;
#endif
	register struct ifaddr *ifa = (struct ifaddr *)data;
	register struct ifreq *ifr = (struct ifreq *)data;
	int s, error = 0;

	s = splimp();
	switch (cmd) {
		case SIOCSIFADDR:
			switch (ifa->ifa_addr->sa_family) {
#ifdef INET
				case AF_INET:
					arp_ifinit((struct arpcom *)ifp, ifa);
					break;
#endif /* INET */
				default:
					break;
			}
			break;
		case SIOCGIFADDR:
			{
				struct sockaddr *sa;

				sa = (struct sockaddr *) & ifr->ifr_data;
				bcopy((caddr_t) sc->myri_ac.ac_enaddr,
					(caddr_t) sa->sa_data,
					sizeof(sc->myri_ac.ac_enaddr));
			}
			break;
		case SIOCSIFFLAGS:
			if (ifp->if_flags & IFF_UP) {
				if (!(ifp->if_flags & IFF_RUNNING))
					myriinit(sc); 
			}
			else {
				if (ifp->if_flags & IFF_RUNNING)
					myristop(sc);
			}
			break;
#ifdef MULTICAST
		/*
		 * Update our multicast list.
		 */
		case SIOCADDMULTI:
			error = ether_addmulti(ifr, &sc->myri_ac);
			goto reset;

		case SIOCDELMULTI:
			error = ether_delmulti(ifr, &sc->myri_ac);
reset:
			if (error == ENETRESET) {
				myriinit(sc);
				error = 0;
			}
			break;
#endif /* MULTICAST */
		default:
			error = EINVAL;
	}
	splx(s);
	return (error);
}

/*
 *	The rest of this file is for /dev/mlanai support.
 */

static int myriopen  __P((dev_t, int, int, struct proc *));
static int myriclose __P((dev_t, int, int, struct proc *));
static int myrilanaiioctl __P((dev_t, int, caddr_t, int, struct proc *));
static int myrimmap __P((dev_t, int, int));

#define CDEV_MAJOR 85
#ifndef SPIN
static struct cdevsw myri_cdevsw = 
        { myriopen,  myriclose,   NULL,    NULL,   /*85*/
          myrilanaiioctl, nostop,         nullreset,   nodevtotty,/* Myri */
          seltrue,      myrimmap, NULL,      "mlanai",       NULL,   -1 };
#endif

/*
 * open /dev/mlanai0 device
 */
static
int
myriopen(dev, flag, fmt, p)
        dev_t dev;
        int flag, fmt;
        struct proc *p;
{
        int unit = minor(dev);
        struct myri_softc *sc = MYRI_UNIT_TO_SOFTC(unit);

        if (unit >= NMYRI || (sc == NULL))
                return (ENXIO);

        if (sc->myri_open == 0) {
        	sc->myri_open = 1;
        }
        return (0);
} 

/*
 * close /dev/mlanai0 device
 */
static
int
myriclose(dev, flag, fmt, p)
        dev_t dev;
        int flag, fmt;
        struct proc *p;
{  
        struct myri_softc *sc;
	int unit = minor(dev);

	if (unit >= NMYRI || (sc = MYRI_UNIT_TO_SOFTC(unit) ) == NULL)
		return (ENXIO);

        sc->myri_open = 0;
        return(0);
}

/*
 * ioctls /dev/mlanai0 device
 */
static
int
myrilanaiioctl(dev, cmd, data, flag, p)
        dev_t dev;
        int cmd;
        caddr_t data;
        int flag;
        struct proc *p;
{
        struct myri_softc *sc;
	int	s = splimp(), error = 0, unit = minor(dev);


	if (unit >= NMYRI || (sc = MYRI_UNIT_TO_SOFTC(unit)) == NULL)
		return (ENXIO);

        switch (cmd) {
	case MLANAI_GET_INFO:
	case 0x40186d00:
	{
		struct board_info *mbi = (struct board_info *)data;

		mbi->lanai_control = (u_long)sc->myri_lanai_ctl;
		mbi->lanai_eeprom = (u_long)sc->myri_eeprom;
		mbi->lanai_registers = (u_long)sc->myri_lanai_reg;
		mbi->lanai_memory = (u_long)sc->myri_memory;
		mbi->copy_blockK = (u_long)sc->myri_copy_block;
		mbi->copy_blockD = vtophys(sc->myri_copy_block);
		mbi->copy_blockU = 0;
	        mbi->copy_block_size = COPY_BLOCK_SIZE;
	}
		break;
        default:
                error = ENOTTY;
        }
	splx(s);
        return (error);
}

/*
 * maps the board memory to be accessible by user
 */
static
int
myrimmap(dev, off, prot)
        dev_t dev;
        int   off, prot;
{
        int unit = minor(dev);
        struct myri_softc *sc = MYRI_UNIT_TO_SOFTC(unit);

        u_int retval;

        if (unit >= NMYRI || (sc  == NULL))
                return (-1);

        if (off < 0 || off > sizeof(struct MYRINET_BOARD)+COPY_BLOCK_SIZE)
                return (-1);

        if (off < sizeof(struct MYRINET_BOARD)){
          retval = i386_btop(pmap_extract(kernel_pmap,
                                          sc->myri_base+off));
        }
        else{
          retval = i386_btop(vtophys(sc->myri_copy_block)
                             + off - sizeof(struct MYRINET_BOARD));
        }
        return(retval);
}

#ifndef SPIN
static void     myri_drvinit(void *unused)
{
        dev_t dev;
	static myri_devsw_installed = 0;

        if( ! myri_devsw_installed ) {
                dev = makedev(CDEV_MAJOR, 0);
                cdevsw_add(&dev,&myri_cdevsw, NULL);
                myri_devsw_installed = 1;
        }
}
SYSINIT(myridevice,SI_SUB_DRIVERS,SI_ORDER_MIDDLE+CDEV_MAJOR,myri_drvinit,NULL)
#endif
