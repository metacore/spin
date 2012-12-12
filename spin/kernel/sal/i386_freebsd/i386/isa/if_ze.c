/*-
 * TODO:
 * [1] integrate into current if_ed.c
 * [2] parse tuples to find out where to map the shared memory buffer,
 *     and what to write into the configuration register
 * [3] move pcic-specific code into a separate module.
 *
 * Device driver for IBM PCMCIA Credit Card Adapter for Ethernet,
 * if_ze.c
 *
 * Based on the Device driver for National Semiconductor DS8390 ethernet
 * adapters by David Greenman.  Modifications for PCMCIA by Keith Moore.
 * Adapted for FreeBSD 1.1.5 by Jordan Hubbard.
 *
 * Currently supports only the IBM Credit Card Adapter for Ethernet, but
 * could probably work with other PCMCIA cards also, if it were modified
 * to get the locations of the PCMCIA configuration option register (COR)
 * by parsing the configuration tuples, rather than by hard-coding in
 * the value expected by IBM's card.
 *
 * Sources for data on the PCMCIA/IBM CCAE specific portions of the driver:
 *
 * [1] _Local Area Network Credit Card Adapters Technical Reference_,
 *     IBM Corp., SC30-3585-00, part # 33G9243.
 * [2] "pre-alpha" PCMCIA support code for Linux by Barry Jaspan.
 * [3] Intel 82536SL PC Card Interface Controller Data Sheet, Intel
 *     Order Number 290423-002
 * [4] National Semiconductor DP83902A ST-NIC (tm) Serial Network
 *     Interface Controller for Twisted Pair data sheet.
 *
 *
 * Copyright (C) 1993, David Greenman. This software may be used, modified,
 *   copied, distributed, and sold, in both source and binary form provided
 *   that the above copyright and these terms are retained. Under no
 *   circumstances is the author responsible for the proper functioning
 *   of this software, nor does the author assume any responsibility
 *   for damages incurred with its use.
 */
/*
 * I doubled delay loops in this file because it is not enough for some
 * laptop machines' PCIC (especially, on my Chaplet ILFA 350 ^^;).
 *                        HOSOKAWA, Tatsumi <hosokawa@mt.cs.keio.ac.jp>
 */
/*
 * Very small patch for IBM Ethernet PCMCIA Card II and IBM ThinkPad230Cs.
 *			ETO, Toshihisa <eto@osl.fujitsu.co.jp>
 */

/*
 * $Id: if_ze.c,v 1.1.1.1 1996/08/15 03:23:01 fgray Exp $
 */

#include "ze.h"
#if	NZE > 0
#include "bpfilter.h"

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/errno.h>
#include <sys/ioctl.h>
#include <sys/mbuf.h>
#include <sys/socket.h>
#include <sys/syslog.h>

#include <net/if.h>
#include <net/if_dl.h>
#include <net/if_types.h>
#include <net/netisr.h>

#ifdef INET
#include <netinet/in.h>
#include <netinet/in_systm.h>
#include <netinet/in_var.h>
#include <netinet/ip.h>
#include <netinet/if_ether.h>
#endif

#ifdef NS
#include <netns/ns.h>
#include <netns/ns_if.h>
#endif

#if NBPFILTER > 0
#include <net/bpf.h>
#include <net/bpfdesc.h>
#endif

#include <machine/clock.h>

#include <i386/isa/isa.h>
#include <i386/isa/isa_device.h>
#include <i386/isa/icu.h>
#include <i386/isa/if_zereg.h>
#include <i386/isa/pcic.h>

#include "apm.h"
#if NAPM > 0
#include <machine/apm_bios.h>
#endif /* NAPM > 0 */


/*****************************************************************************
 *                       Driver for Ethernet Adapter                         *
 *****************************************************************************/
/*
 * ze_softc: per line info and status
 */
struct	ze_softc {

	caddr_t  maddr;
	u_long  iobase, irq;

	struct	arpcom arpcom;	/* ethernet common */

	char	*type_str;	/* pointer to type string */
	char    *mau;		/* type of media access unit */
	u_short	nic_addr;	/* NIC (DS8390) I/O bus address */

	caddr_t	smem_start;	/* shared memory start address */
	caddr_t	smem_end;	/* shared memory end address */
	u_long	smem_size;	/* total shared memory size */
	caddr_t	smem_ring;	/* start of RX ring-buffer (in smem) */

	caddr_t	bpf;		/* BPF "magic cookie" */

	u_char	memwidth;	/* width of access to card mem 8 or 16 */
	u_char	xmit_busy;	/* transmitter is busy */
	u_char	txb_cnt;	/* Number of transmit buffers */
	u_char	txb_next;	/* Pointer to next buffer ready to xmit */
	u_short	txb_next_len;	/* next xmit buffer length */
	u_char	data_buffered;	/* data has been buffered in interface memory */
	u_char	tx_page_start;	/* first page of TX buffer area */

	u_char	rec_page_start;	/* first page of RX ring-buffer */
	u_char	rec_page_stop;	/* last page of RX ring-buffer */
	u_char	next_packet;	/* pointer to next unread RX packet */
	int	slot;		/* information for reconfiguration */
	u_char	last_alive;	/* information for reconfiguration */
	u_char	last_up;	/* information for reconfiguration */
#if NAPM > 0
	struct apmhook s_hook;	/* reconfiguration support */
	struct apmhook r_hook;	/* reconfiguration support */
#endif /* NAPM > 0 */
} ze_softc[NZE];

int	ze_attach(), ze_ioctl(), ze_probe();
void	ze_init(), ze_start(), ze_stop(), ze_intr();
void	ze_reset(), ze_watchdog(), ze_get_packet();

void 	ze_setup __P((struct ze_softc *sc));
static inline void ze_rint();
static inline void ze_xmit();
static inline char *ze_ring_copy();

struct isa_driver zedriver = {
	ze_probe,
	ze_attach,
	"ze"
};

#define	ETHER_MIN_LEN	64
#define ETHER_MAX_LEN	1518
#define	ETHER_ADDR_LEN	6
#define	ETHER_HDR_SIZE	14

static unsigned char enet_addr[6];
static unsigned char card_info[256];

#define CARD_INFO  "IBM Corp.~Ethernet~0933495"

/*
 * IBM Ethernet PCMCIA Card II returns following info.
 */
#define CARD2_INFO  "IBM Corp.~Ethernet~0934214"

/* */

#define CARD3_INFO  "National Semiconductor~InfoMover NE4"

/*
 * scan the card information structure looking for the version/product info
 * tuple.  when we find it, compare it to the string we are looking for.
 * return 1 if we find it, 0 otherwise.
 */

static int
ze_check_cis (unsigned char *scratch)
{
    int i,j,k;

    card_info[0] = '\0';
    i = 0;
    while (scratch[i] != 0xff && i < 1024) {
	unsigned char link = scratch[i+2];

#if 0
	printf ("[%02x] %02x ", i, link);
	for (j = 4; j < 2 * link + 4 && j < 32; j += 2)
	    printf ("%02x ", scratch[j + i]);
	printf ("\n");
#endif
	if (scratch[i] == 0x15) {
	    /*
	     * level 1 version/product info
	     * copy to card_info, translating '\0' to '~'
	     */
	    k = 0;
	    for (j = i+8; scratch[j] != 0xff; j += 2)
		card_info[k++] = scratch[j] == '\0' ? '~' : scratch[j];
	    card_info[k++] = '\0';
#if 0
	    return (memcmp (card_info, CARD_INFO, sizeof(CARD_INFO)-1) == 0);
#else
	    if ((memcmp (card_info, CARD_INFO, sizeof(CARD_INFO)-1) == 0) ||
		(memcmp (card_info, CARD2_INFO, sizeof(CARD2_INFO)-1) == 0) ||
		(memcmp (card_info, CARD3_INFO, sizeof(CARD3_INFO)-1) == 0)) {
		return 1;
	    }
	    return 0;
#endif
	}
	i += 4 + 2 * link;
    }
    return 0;
}

/*
 * Probe each slot looking for an IBM Credit Card Adapter for Ethernet
 * For each card that we find, map its card information structure
 * into system memory at 'scratch' and see whether it's one of ours.
 * Return the slot number if we find a card, or -1 otherwise.
 *
 * Side effects:
 * + On success, leaves CIS mapped into memory at 'scratch';
 *   caller must free it.
 * + On success, leaves ethernet address in enet_addr.
 * + Leaves product/vendor id of last card probed in 'card_info'
 */

static int prev_slot = 0;

static int
ze_find_adapter (unsigned char *scratch, int reconfig)
{
    int slot;

    for (slot = prev_slot; slot < MAXSLOT; ++slot) {
	/*
	 * see if there's a PCMCIA controller here
	 * Intel PCMCIA controllers use 0x82 and 0x83
	 * IBM clone chips use 0x88 and 0x89, apparently
	 */
	/*
	 * IBM ThinkPad230Cs use 0x84.
	 */
	unsigned char idbyte = pcic_getb (slot, PCIC_ID_REV);

	if (idbyte != 0x82 && idbyte != 0x83 &&
	    idbyte != 0x84 &&			/* for IBM ThinkPad 230Cs */
	    idbyte != 0x88 && idbyte != 0x89) {
#if 0
	    printf ("ibmccae: pcic slot %d: wierd id/rev code 0x%02x\n",
		    slot, idbyte);
#endif
	    continue;
	}
	if ((pcic_getb (slot, PCIC_STATUS) & PCIC_CD) != PCIC_CD) {
	    if (!reconfig) {
		printf ("ze: slot %d: no card in slot\n", slot);
	    }
	    else {
		log (LOG_NOTICE, "ze: slot %d: no card in slot\n", slot);
	    }
	    /* no card in slot */
	    continue;
	}
	pcic_power_on (slot);
	pcic_reset (slot);
	/*
	 * map the card's attribute memory and examine its
	 * card information structure tuples for something
	 * we recognize.
	 */
	pcic_map_memory (slot, 0, kvtop (scratch), 0L,
			 0xFFFL, ATTRIBUTE, 1);

	if ((ze_check_cis (scratch)) > 0) {
	    /* found it */
	    if (!reconfig) {
		printf ("ze: found card in slot %d\n", slot);
	    }
	    else {
		log (LOG_NOTICE, "ze: found card in slot %d\n", slot);
	    }
	    prev_slot = (prev_slot == MAXSLOT - 1) ? 0 : prev_slot+1;

	    return slot;
	}
	else {
	    if (!reconfig) {
		printf ("ze: pcmcia slot %d: %s\n", slot, card_info);
	    }
	    else {
		log (LOG_NOTICE, "ze: pcmcia slot %d: %s\n", slot, card_info);
	    }
	}
	pcic_unmap_memory (slot, 0);
    }
    prev_slot = 0;
    return -1;
}


/*
 * macros to handle casting unsigned long to (char *) so we can
 * read/write into physical memory space.
 */

#define PEEK(addr) (*((unsigned char *)(addr)))
#define POKE(addr,val) do { PEEK(addr) = (val); } while (0)

/*
 * Determine if the device is present
 *
 *   on entry:
 * 	a pointer to an isa_device struct
 *   on exit:
 *	NULL if device not found
 *	or # of i/o addresses used (if found)
	pcic(
 */
int
ze_probe(isa_dev)
	struct isa_device *isa_dev;
{
	struct ze_softc *sc = &ze_softc[isa_dev->id_unit];
	int i, x, re_init_flag;
	u_int memsize;
	u_char iptr, memwidth, sum, tmp;
	int slot;

        if ((slot = ze_find_adapter (isa_dev->id_maddr, isa_dev->id_reconfig)) < 0)
	    return NULL;

	/*
	 * okay, we found a card, so set it up
	 */
	/*
	 * Inhibit 16 bit memory delay.
	 * POINTETH.SYS apparently does this, for what reason I don't know.
	 */
	pcic_putb (slot, PCIC_CDGC,
		   pcic_getb (slot, PCIC_CDGC) | PCIC_16_DL_INH);
	/*
	 * things to map
	 * (1) card's EEPROM is already mapped by the find_adapter routine
	 *     but we still need to get the card's ethernet address.
	 *     after that we unmap that part of attribute memory.
	 * (2) card configuration registers need to be mapped in so we
	 *     can set the configuration and socket # registers.
	 * (3) shared memory packet buffer
	 * (4) i/o ports
	 * (5) IRQ
	 */
	/*
	 * Sigh.  Location of the ethernet address isn't documented in [1].
	 * It was derived by doing a hex dump of all of attribute memory
	 * and looking for the IBM vendor prefix.
	 */
	enet_addr[0] = PEEK(isa_dev->id_maddr+0xff0);
	enet_addr[1] = PEEK(isa_dev->id_maddr+0xff2);
	enet_addr[2] = PEEK(isa_dev->id_maddr+0xff4);
	enet_addr[3] = PEEK(isa_dev->id_maddr+0xff6);
	enet_addr[4] = PEEK(isa_dev->id_maddr+0xff8);
	enet_addr[5] = PEEK(isa_dev->id_maddr+0xffa);
	pcic_unmap_memory (slot, 0);

	sc->maddr = isa_dev->id_maddr;
	sc->irq = isa_dev->id_irq;
	sc->iobase = isa_dev->id_iobase;
	sc->slot = slot;
	/*
	 * Setup i/o addresses
	 */
	sc->nic_addr = sc->iobase;
	sc->smem_start = (caddr_t)sc->maddr;

	ze_setup(sc);

	tmp = inb (sc->iobase + ZE_RESET);
	sc->mau = tmp & 0x09 ? "10base2" : "10baseT";

	/* set width/size */
	sc->type_str = "IBM PCMCIA";
	memsize = 16*1024;
	sc->memwidth = 16;

	/* allocate 1 xmit buffer */
	sc->smem_ring = sc->smem_start + (ZE_PAGE_SIZE * ZE_TXBUF_SIZE);
	sc->txb_cnt = 1;
	sc->rec_page_start = ZE_TXBUF_SIZE + ZE_PAGE_OFFSET;
	sc->smem_size = memsize;
	sc->smem_end = sc->smem_start + memsize;
	sc->rec_page_stop = memsize / ZE_PAGE_SIZE + ZE_PAGE_OFFSET;
	sc->tx_page_start = ZE_PAGE_OFFSET;

	/* get station address */
	for (i = 0; i < ETHER_ADDR_LEN; ++i)
		sc->arpcom.ac_enaddr[i] = enet_addr[i];

	isa_dev->id_msize = memsize;


	/* information for reconfiguration */
	sc->last_alive = 0;
	sc->last_up = 0;

	return 32;
}


void
ze_setup(struct ze_softc *sc)
{
	int re_init_flag = 0,tmp,slot = sc->slot;

re_init:
	/*
	 * (2) map card configuration registers.  these are offset
	 * in card memory space by 0x20000.  normally we could get
	 * this offset from the card information structure, but I'm
	 * too lazy and am not quite sure if I understand the CIS anyway.
	 *
	 * XXX IF YOU'RE TRYING TO PORT THIS DRIVER FOR A DIFFERENT
	 * PCMCIA CARD, the most likely thing to change is the constant
	 * 0x20000 in the next statement.  Oh yes, also change the
	 * card id string that we probe for.
	 */
	pcic_map_memory (slot, 0, kvtop (sc->maddr), 0x20000, 8L,
			 ATTRIBUTE, 1);
	POKE(sc->maddr, 0x80);	/* reset the card (how long?) */
	DELAY (40000);
	/*
	 * Set the configuration index.  According to [1], the adapter won't
	 * respond to any i/o signals until we do this; it uses the
	 * Memory Only interface (whatever that is; it's not documented).
	 * Also turn on "level" (not pulse) interrupts.
	 *
	 * XXX probably should init the socket and copy register also,
	 * so that we can deal with multiple instances of the same card.
	 */
	POKE(sc->maddr, 0x41);
	pcic_unmap_memory (slot, 0);

	/*
	 * (3) now map in the shared memory buffer.  This has to be mapped
	 * as words, not bytes, and on a 16k boundary.  The offset value
	 * was derived by installing IBM's POINTETH.SYS under DOS and
	 * looking at the PCIC registers; it's not documented in IBM's
	 * tech ref manual ([1]).
	 */
	pcic_map_memory (slot, 0, kvtop (sc->maddr), 0x4000L, 0x4000L,
			 COMMON, 2);

	/*
	 * (4) map i/o ports.
	 *
	 * XXX is it possible that the config file leaves this unspecified,
	 * in which case we have to pick one?
	 *
	 * At least one PCMCIA device driver I'v seen maps a block
	 * of 32 consecutive i/o ports as two windows of 16 ports each.
	 * Maybe some other pcic chips are restricted to 16-port windows;
	 * the 82365SL doesn't seem to have that problem.  But since
	 * we have an extra window anyway...
	 */
#ifdef SHARED_MEMORY
	pcic_map_io (slot, 0, sc->iobase, 32, 1);
#else
	pcic_map_io (slot, 0, sc->iobase, 16, 1);
	pcic_map_io (slot, 1, sc->iobase+16, 16, 2);
#endif /* SHARED_MEMORY */

	/*
	 * (5) configure the card for the desired interrupt
	 *
	 * XXX is it possible that the config file leaves this unspecified?
	 */
	pcic_map_irq (slot, ffs (sc->irq) - 1);

	/* tell the PCIC that this is an I/O card (not memory) */
	pcic_putb (slot, PCIC_INT_GEN,
		   pcic_getb (slot, PCIC_INT_GEN) | PCIC_CARDTYPE);

#if 0
	/* tell the PCIC to use level-mode interrupts */
	/* XXX this register may not be present on all controllers */
	pcic_putb (slot, PCIC_GLO_CTRL,
		   pcic_getb (slot, PCIC_GLO_CTRL) | PCIC_LVL_MODE);
#endif

#if 0
	pcic_print_regs (slot);
#endif

	/* reset card to force it into a known state */
	tmp = inb (sc->iobase + ZE_RESET);
	DELAY(20000);
	outb (sc->iobase + ZE_RESET, tmp);
	DELAY(20000);

#if 0
	tmp = inb(sc->iobase);
	printf("CR = 0x%x\n", tmp);
#endif
	/*
	 * query MAM bit in misc register for 10base2
	 */
	tmp = inb (sc->iobase + ZE_MISC);

	/*
	 * Some Intel-compatible PCICs of Cirrus Logic fails in
	 * initializing them.  This is a quick hack to fix this
	 * problem.
	 *        HOSOKAWA, Tatsumi <hosokawa@mt.cs.keio.ac.jp>
	 */
	if (!tmp && !re_init_flag) {
		re_init_flag++;
		goto re_init;
	}
}

#if NAPM > 0
static int
ze_suspend(isa_dev)
	struct isa_device *isa_dev;
{
	struct ze_softc *sc = &ze_softc[isa_dev->id_unit];

	pcic_power_off(sc->slot);
	return 0;
}

static int
ze_resume(isa_dev)
	struct isa_device *isa_dev;
{
#if 0
	printf("Resume ze:\n");
#endif
	prev_slot = 0;
	reconfig_isadev(isa_dev, &net_imask);
	return 0;
}
#endif /* NAPM > 0 */

/*
 * Install interface into kernel networking data structures
 */

int
ze_attach(isa_dev)
	struct isa_device *isa_dev;
{
	struct ze_softc *sc = &ze_softc[isa_dev->id_unit];
	struct ifnet *ifp = &sc->arpcom.ac_if;
	struct ifaddr *ifa;
	struct sockaddr_dl *sdl;
	int pl;

	/* PCMCIA card can be offlined. Reconfiguration is required */
	if (isa_dev->id_reconfig) {
		ze_reset(isa_dev->id_unit);
		if (!isa_dev->id_alive && sc->last_alive) {
			pl = splimp();
			sc->last_up = (ifp->if_flags & IFF_UP);
			if_down(ifp);
			splx(pl);
			sc->last_alive = 0;
		}
		if (isa_dev->id_alive && !sc->last_alive) {
			if (sc->last_up) {
				pl = splimp();
				if_up(ifp);
				splx(pl);
			}
			sc->last_alive = 1;
		}
		return 1;
	}
	else {
		sc->last_alive = 1;
	}

	/*
	 * Set interface to stopped condition (reset)
	 */
	ze_stop(isa_dev->id_unit);

	/*
	 * Initialize ifnet structure
	 */
	ifp->if_unit = isa_dev->id_unit;
	ifp->if_name = "ze" ;
	ifp->if_mtu = ETHERMTU;
	ifp->if_init = ze_init;
	ifp->if_output = ether_output;
	ifp->if_start = ze_start;
	ifp->if_ioctl = ze_ioctl;
	ifp->if_reset = ze_reset;
	ifp->if_watchdog = ze_watchdog;

	/*
	 * Set default state for IIF_LINK0 flag (used to disable the tranceiver
	 *	for AUI operation), based on compile-time config option.
	 */
	if (isa_dev->id_flags & ZE_FLAGS_DISABLE_TRANCEIVER)
		ifp->if_flags = (IFF_BROADCAST | IFF_SIMPLEX | IFF_NOTRAILERS
			| IFF_LINK0);
	else
		ifp->if_flags = (IFF_BROADCAST | IFF_SIMPLEX | IFF_NOTRAILERS);

	/*
	 * Attach the interface
	 */
	if_attach(ifp);

	/*
	 * Search down the ifa address list looking for the AF_LINK type entry
	 */
 	ifa = ifp->if_addrlist;
	while ((ifa != 0) && (ifa->ifa_addr != 0) &&
	    (ifa->ifa_addr->sa_family != AF_LINK))
		ifa = ifa->ifa_next;
	/*
	 * If we find an AF_LINK type entry we fill in the hardware address.
	 *	This is useful for netstat(1) to keep track of which interface
	 *	is which.
	 */
	if ((ifa != 0) && (ifa->ifa_addr != 0)) {
		/*
		 * Fill in the link-level address for this interface
		 */
		sdl = (struct sockaddr_dl *)ifa->ifa_addr;
		sdl->sdl_type = IFT_ETHER;
		sdl->sdl_alen = ETHER_ADDR_LEN;
		sdl->sdl_slen = 0;
		bcopy(sc->arpcom.ac_enaddr, LLADDR(sdl), ETHER_ADDR_LEN);
	}

	/*
	 * Print additional info when attached
	 */
	printf("ze%d: address %s, type %s (%dbit)%s, MAU %s\n",
	       isa_dev->id_unit,
	       ether_sprintf(sc->arpcom.ac_enaddr), sc->type_str,
	       sc->memwidth,
	       (ifp->if_flags & IFF_LINK0 ? " [tranceiver disabled]" : ""),
	       sc->mau);

	/*
	 * If BPF is in the kernel, call the attach for it
	 */
#if NBPFILTER > 0
	bpfattach(&sc->bpf, ifp, DLT_EN10MB, sizeof(struct ether_header));
#endif

#if NAPM > 0
	sc->s_hook.ah_fun = ze_suspend;
	sc->s_hook.ah_arg = (void *)isa_dev;
	sc->s_hook.ah_name = "IBM PCMCIA Ethernet I/II";
	sc->s_hook.ah_order = APM_MID_ORDER;
	apm_hook_establish(APM_HOOK_SUSPEND , &sc->s_hook);
	sc->r_hook.ah_fun = ze_resume;
	sc->r_hook.ah_arg = (void *)isa_dev;
	sc->r_hook.ah_name = "IBM PCMCIA Ethernet I/II";
	sc->r_hook.ah_order = APM_MID_ORDER;
	apm_hook_establish(APM_HOOK_RESUME , &sc->r_hook);
#endif /* NAPM > 0 */

	return 1;
}

/*
 * Reset interface.
 */
void
ze_reset(unit)
	int unit;
{
	int s;

	s = splnet();

	/*
	 * Stop interface and re-initialize.
	 */
	ze_stop(unit);
	ze_init(unit);

	(void) splx(s);
}

/*
 * Take interface offline.
 */
void
ze_stop(unit)
	int unit;
{
	struct ze_softc *sc = &ze_softc[unit];
	int n = 5000;

	/*
	 * Stop everything on the interface, and select page 0 registers.
	 */
	outb(sc->nic_addr + ZE_P0_CR, ZE_CR_RD2|ZE_CR_STP);

	/*
	 * Wait for interface to enter stopped state, but limit # of checks
	 *	to 'n' (about 5ms). It shouldn't even take 5us on modern
	 *	DS8390's, but just in case it's an old one.
	 */
	while (((inb(sc->nic_addr + ZE_P0_ISR) & ZE_ISR_RST) == 0) && --n);
	pcic_power_off(sc->slot);

}

/*
 * Device timeout/watchdog routine. Entered if the device neglects to
 *	generate an interrupt after a transmit has been started on it.
 */
void
ze_watchdog(unit)
	int unit;
{
#if 1
    struct ze_softc *sc = &ze_softc[unit];
    u_char isr, imr;
    u_short imask;

    if(!(sc->arpcom.ac_if.if_flags & IFF_UP))
	return;
    /* select page zero */
    outb (sc->nic_addr + ZE_P0_CR,
	  (inb (sc->nic_addr + ZE_P0_CR) & 0x3f) | ZE_CR_PAGE_0);

    /* read interrupt status register */
    isr = inb (sc->nic_addr + ZE_P0_ISR) & 0xff;

    /* select page two */
    outb (sc->nic_addr + ZE_P0_CR,
	  (inb (sc->nic_addr + ZE_P0_CR) & 0x3f) | ZE_CR_PAGE_2);

    /* read interrupt mask register */
    imr = inb (sc->nic_addr + ZE_P2_IMR) & 0xff;

    imask = inb(IO_ICU2) << 8 | inb(IO_ICU1);

    log (LOG_ERR, "ze%d: device timeout, isr=%02x, imr=%02x, imask=%04x\n",
	 unit, isr, imr, imask);
#else
    log(LOG_ERR, "ze%d: device timeout\n", unit);
#endif

    ze_reset(unit);
}

/*
 * Initialize device.
 */
void
ze_init(unit)
	int unit;
{
	struct ze_softc *sc = &ze_softc[unit];
	struct ifnet *ifp = &sc->arpcom.ac_if;
	int i, s;
	u_char	command;


	pcic_power_on(sc->slot);
	pcic_reset(sc->slot);
        if(!(sc->arpcom.ac_if.if_flags & IFF_UP))
		Debugger("here!!");
	ze_setup(sc);
	/* address not known */
	if (ifp->if_addrlist == (struct ifaddr *)0) return;

	/*
	 * Initialize the NIC in the exact order outlined in the NS manual.
	 *	This init procedure is "mandatory"...don't change what or when
	 *	things happen.
	 */
	s = splnet();

	/* reset transmitter flags */
	sc->data_buffered = 0;
	sc->xmit_busy = 0;
	sc->arpcom.ac_if.if_timer = 0;

	sc->txb_next = 0;

	/* This variable is used below - don't move this assignment */
	sc->next_packet = sc->rec_page_start + 1;

	/*
	 * Set interface for page 0, Remote DMA complete, Stopped
	 */
	outb(sc->nic_addr + ZE_P0_CR, ZE_CR_RD2|ZE_CR_STP);

	if (sc->memwidth == 16) {
		/*
		 * Set FIFO threshold to 8, No auto-init Remote DMA,
		 *	byte order=80x86, word-wide DMA xfers
		 */
		outb(sc->nic_addr + ZE_P0_DCR, ZE_DCR_FT1|ZE_DCR_WTS);
	} else {
		/*
		 * Same as above, but byte-wide DMA xfers
		 */
		outb(sc->nic_addr + ZE_P0_DCR, ZE_DCR_FT1);
	}

	/*
	 * Clear Remote Byte Count Registers
	 */
	outb(sc->nic_addr + ZE_P0_RBCR0, 0);
	outb(sc->nic_addr + ZE_P0_RBCR1, 0);

	/*
	 * Enable reception of broadcast packets
	 */
	outb(sc->nic_addr + ZE_P0_RCR, ZE_RCR_AB);

	/*
	 * Place NIC in internal loopback mode
	 */
	outb(sc->nic_addr + ZE_P0_TCR, ZE_TCR_LB0);

	/*
	 * Initialize transmit/receive (ring-buffer) Page Start
	 */
	outb(sc->nic_addr + ZE_P0_TPSR, sc->tx_page_start);
	outb(sc->nic_addr + ZE_P0_PSTART, sc->rec_page_start);

	/*
	 * Initialize Receiver (ring-buffer) Page Stop and Boundry
	 */
	outb(sc->nic_addr + ZE_P0_PSTOP, sc->rec_page_stop);
	outb(sc->nic_addr + ZE_P0_BNRY, sc->rec_page_start);

	/*
	 * Clear all interrupts. A '1' in each bit position clears the
	 *	corresponding flag.
	 */
	outb(sc->nic_addr + ZE_P0_ISR, 0xff);

	/*
	 * Enable the following interrupts: receive/transmit complete,
	 *	receive/transmit error, and Receiver OverWrite.
	 *
	 * Counter overflow and Remote DMA complete are *not* enabled.
	 */
	outb(sc->nic_addr + ZE_P0_IMR,
		ZE_IMR_PRXE|ZE_IMR_PTXE|ZE_IMR_RXEE|ZE_IMR_TXEE|ZE_IMR_OVWE);

	/*
	 * Program Command Register for page 1
	 */
	outb(sc->nic_addr + ZE_P0_CR, ZE_CR_PAGE_1|ZE_CR_RD2|ZE_CR_STP);

	/*
	 * Copy out our station address
	 */
	for (i = 0; i < ETHER_ADDR_LEN; ++i)
		outb(sc->nic_addr + ZE_P1_PAR0 + i, sc->arpcom.ac_enaddr[i]);

#if NBPFILTER > 0
	/*
	 * Initialize multicast address hashing registers to accept
	 *	 all multicasts (only used when in promiscuous mode)
	 */
	for (i = 0; i < 8; ++i)
		outb(sc->nic_addr + ZE_P1_MAR0 + i, 0xff);
#endif

	/*
	 * Set Current Page pointer to next_packet (initialized above)
	 */
	outb(sc->nic_addr + ZE_P1_CURR, sc->next_packet);

	/*
	 * Set Command Register for page 0, Remote DMA complete,
	 * 	and interface Start.
	 */
	outb(sc->nic_addr + ZE_P1_CR, ZE_CR_RD2|ZE_CR_STA);

	/*
	 * Take interface out of loopback
	 */
	outb(sc->nic_addr + ZE_P0_TCR, 0);

	/*
	 * Set 'running' flag, and clear output active flag.
	 */
	ifp->if_flags |= IFF_RUNNING;
	ifp->if_flags &= ~IFF_OACTIVE;

	/*
	 * ...and attempt to start output
	 */
	ze_start(ifp);

	(void) splx(s);
}

/*
 * This routine actually starts the transmission on the interface
 */
static inline void
ze_xmit(ifp)
	struct ifnet *ifp;
{
	struct ze_softc *sc = &ze_softc[ifp->if_unit];
	u_short len = sc->txb_next_len;

	/*
	 * Set NIC for page 0 register access
	 */
	outb(sc->nic_addr + ZE_P0_CR, ZE_CR_RD2|ZE_CR_STA);

	/*
	 * Set TX buffer start page
	 */
	outb(sc->nic_addr + ZE_P0_TPSR, sc->tx_page_start +
		sc->txb_next * ZE_TXBUF_SIZE);

	/*
	 * Set TX length
	 */
	outb(sc->nic_addr + ZE_P0_TBCR0, len & 0xff);
	outb(sc->nic_addr + ZE_P0_TBCR1, len >> 8);

	/*
	 * Set page 0, Remote DMA complete, Transmit Packet, and *Start*
	 */
	outb(sc->nic_addr + ZE_P0_CR, ZE_CR_RD2|ZE_CR_TXP|ZE_CR_STA);

	sc->xmit_busy = 1;
	sc->data_buffered = 0;

	/*
	 * Switch buffers if we are doing double-buffered transmits
	 */
	if ((sc->txb_next == 0) && (sc->txb_cnt > 1))
		sc->txb_next = 1;
	else
		sc->txb_next = 0;

	/*
	 * Set a timer just in case we never hear from the board again
	 */
	ifp->if_timer = 2;
}

/*
 * Start output on interface.
 * We make two assumptions here:
 *  1) that the current priority is set to splnet _before_ this code
 *     is called *and* is returned to the appropriate priority after
 *     return
 *  2) that the IFF_OACTIVE flag is checked before this code is called
 *     (i.e. that the output part of the interface is idle)
 */
void
ze_start(ifp)
	struct ifnet *ifp;
{
	struct ze_softc *sc = &ze_softc[ifp->if_unit];
	struct mbuf *m0, *m;
	caddr_t buffer;
	int len;
	u_char laar_tmp;

outloop:
	/*
	 * See if there is room to send more data (i.e. one or both of the
	 *	buffers is empty).
	 */
	if (sc->data_buffered)
		if (sc->xmit_busy) {
			/*
			 * No room. Indicate this to the outside world
			 *	and exit.
			 */
			ifp->if_flags |= IFF_OACTIVE;
			return;
		} else {
			/*
			 * Data is buffered, but we're not transmitting, so
			 *	start the xmit on the buffered data.
			 * Note that ze_xmit() resets the data_buffered flag
			 *	before returning.
			 */
			ze_xmit(ifp);
		}

	IF_DEQUEUE(&sc->arpcom.ac_if.if_snd, m);
	if (m == NULL) {
	/*
	 * The following isn't pretty; we are using the !OACTIVE flag to
	 * indicate to the outside world that we can accept an additional
	 * packet rather than that the transmitter is _actually_
	 * active. Indeed, the transmitter may be active, but if we haven't
	 * filled the secondary buffer with data then we still want to
	 * accept more.
	 * Note that it isn't necessary to test the data_buffered flag -
	 * we wouldn't have tried to de-queue the packet in the first place
	 * if it was set.
	 */
		ifp->if_flags &= ~IFF_OACTIVE;
		return;
	}

	/*
	 * Copy the mbuf chain into the transmit buffer
	 */

	buffer = sc->smem_start + (sc->txb_next * ZE_TXBUF_SIZE * ZE_PAGE_SIZE);
	len = 0;
	for (m0 = m; m != 0; m = m->m_next) {
		bcopy(mtod(m, caddr_t), buffer, m->m_len);
		buffer += m->m_len;
        	len += m->m_len;
	}

	sc->txb_next_len = max(len, ETHER_MIN_LEN);

	if (sc->txb_cnt > 1)
		/*
		 * only set 'buffered' flag if doing multiple buffers
		 */
		sc->data_buffered = 1;

	if (sc->xmit_busy == 0)
		ze_xmit(ifp);
	/*
	 * If there is BPF support in the configuration, tap off here.
	 */
#if NBPFILTER > 0
	if (sc->bpf) {
		bpf_mtap(sc->bpf, m0);
	}
#endif

	m_freem(m0);

	/*
	 * If we are doing double-buffering, a buffer might be free to
	 *	fill with another packet, so loop back to the top.
	 */
	if (sc->txb_cnt > 1)
		goto outloop;
	else {
		ifp->if_flags |= IFF_OACTIVE;
		return;
	}
}

/*
 * Ethernet interface receiver interrupt.
 */
static inline void /* only called from one place, so may as well integrate */
ze_rint(unit)
	int unit;
{
	register struct ze_softc *sc = &ze_softc[unit];
	u_char boundry, current;
	u_short len;
	struct ze_ring *packet_ptr;

	/*
	 * Set NIC to page 1 registers to get 'current' pointer
	 */
	outb(sc->nic_addr + ZE_P0_CR, ZE_CR_PAGE_1|ZE_CR_RD2|ZE_CR_STA);

	/*
	 * 'sc->next_packet' is the logical beginning of the ring-buffer - i.e.
	 *	it points to where new data has been buffered. The 'CURR'
	 *	(current) register points to the logical end of the ring-buffer
	 *	- i.e. it points to where additional new data will be added.
	 *	We loop here until the logical beginning equals the logical
	 *	end (or in other words, until the ring-buffer is empty).
	 */
	while (sc->next_packet != inb(sc->nic_addr + ZE_P1_CURR)) {

		/* get pointer to this buffer header structure */
		packet_ptr = (struct ze_ring *)(sc->smem_ring +
			 (sc->next_packet - sc->rec_page_start) * ZE_PAGE_SIZE);

		/*
		 * The byte count includes the FCS - Frame Check Sequence (a
		 *	32 bit CRC).
		 */
		len = packet_ptr->count;
		if ((len >= ETHER_MIN_LEN) && (len <= ETHER_MAX_LEN)) {
			/*
			 * Go get packet. len - 4 removes CRC from length.
			 * (packet_ptr + 1) points to data just after the packet ring
			 *	header (+4 bytes)
			 */
			ze_get_packet(sc, (caddr_t)(packet_ptr + 1), len - 4);
			++sc->arpcom.ac_if.if_ipackets;
		} else {
			/*
			 * Really BAD...probably indicates that the ring pointers
			 *	are corrupted. Also seen on early rev chips under
			 *	high load - the byte order of the length gets switched.
			 */
			log(LOG_ERR,
				"ze%d: shared memory corrupt - invalid packet length %d\n",
				unit, len);
			ze_reset(unit);
			return;
		}

		/*
		 * Update next packet pointer
		 */
		sc->next_packet = packet_ptr->next_packet;

		/*
		 * Update NIC boundry pointer - being careful to keep it
		 *	one buffer behind. (as recommended by NS databook)
		 */
		boundry = sc->next_packet - 1;
		if (boundry < sc->rec_page_start)
			boundry = sc->rec_page_stop - 1;

		/*
		 * Set NIC to page 0 registers to update boundry register
		 */
		outb(sc->nic_addr + ZE_P0_CR, ZE_CR_RD2|ZE_CR_STA);

		outb(sc->nic_addr + ZE_P0_BNRY, boundry);

		/*
		 * Set NIC to page 1 registers before looping to top (prepare to
		 *	get 'CURR' current pointer)
		 */
		outb(sc->nic_addr + ZE_P0_CR, ZE_CR_PAGE_1|ZE_CR_RD2|ZE_CR_STA);
	}
}

/*
 * Ethernet interface interrupt processor
 */
void
zeintr(unit)
	int unit;
{
	struct ze_softc *sc = &ze_softc[unit];
	u_char isr;

        if(!(sc->arpcom.ac_if.if_flags & IFF_UP))
		return;
	/*
	 * Set NIC to page 0 registers
	 */
	outb(sc->nic_addr + ZE_P0_CR, ZE_CR_RD2|ZE_CR_STA);

	/*
	 * loop until there are no more new interrupts
	 */
	while (isr = inb(sc->nic_addr + ZE_P0_ISR)) {

		/*
		 * reset all the bits that we are 'acknowleging'
		 *	by writing a '1' to each bit position that was set
		 * (writing a '1' *clears* the bit)
		 */
		outb(sc->nic_addr + ZE_P0_ISR, isr);

		/*
		 * Transmit error. If a TX completed with an error, we end up
		 *	throwing the packet away. Really the only error that is
		 *	possible is excessive collisions, and in this case it is
		 *	best to allow the automatic mechanisms of TCP to backoff
		 *	the flow. Of course, with UDP we're screwed, but this is
		 *	expected when a network is heavily loaded.
		 */
		if (isr & ZE_ISR_TXE) {
			u_char tsr = inb(sc->nic_addr + ZE_P0_TSR);
			u_char ncr = inb(sc->nic_addr + ZE_P0_NCR);

			/*
			 * Excessive collisions (16)
			 */
			if ((tsr & ZE_TSR_ABT) && (ncr == 0)) {
				/*
				 *    When collisions total 16, the P0_NCR will
				 * indicate 0, and the TSR_ABT is set.
				 */
				sc->arpcom.ac_if.if_collisions += 16;
			} else
				sc->arpcom.ac_if.if_collisions += ncr;

			/*
			 * update output errors counter
			 */
			++sc->arpcom.ac_if.if_oerrors;

			/*
			 * reset tx busy and output active flags
			 */
			sc->xmit_busy = 0;
			sc->arpcom.ac_if.if_flags &= ~IFF_OACTIVE;

			/*
			 * clear watchdog timer
			 */
			sc->arpcom.ac_if.if_timer = 0;
		}


		/*
		 * Receiver Error. One or more of: CRC error, frame alignment error
		 *	FIFO overrun, or missed packet.
		 */
		if (isr & ZE_ISR_RXE) {
			++sc->arpcom.ac_if.if_ierrors;
#ifdef ZE_DEBUG
			printf("ze%d: receive error %b\n", unit,
				inb(sc->nic_addr + ZE_P0_RSR),
			       "\20\8DEF\7REC DISAB\6PHY/MC\5MISSED\4OVR\3ALIGN\2FCS\1RCVD");
#endif
		}

		/*
		 * Overwrite warning. In order to make sure that a lockup
		 *	of the local DMA hasn't occurred, we reset and
		 *	re-init the NIC. The NSC manual suggests only a
		 *	partial reset/re-init is necessary - but some
		 *	chips seem to want more. The DMA lockup has been
		 *	seen only with early rev chips - Methinks this
		 *	bug was fixed in later revs. -DG
		 */
		if (isr & ZE_ISR_OVW) {
			++sc->arpcom.ac_if.if_ierrors;
			/*
			 * Stop/reset/re-init NIC
			 */
			ze_reset(unit);
		}

		/*
		 * Transmission completed normally.
		 */
		if (isr & ZE_ISR_PTX) {

			/*
			 * reset tx busy and output active flags
			 */
			sc->xmit_busy = 0;
			sc->arpcom.ac_if.if_flags &= ~IFF_OACTIVE;

			/*
			 * clear watchdog timer
			 */
			sc->arpcom.ac_if.if_timer = 0;

			/*
			 * Update total number of successfully transmitted
			 *	packets.
			 */
			++sc->arpcom.ac_if.if_opackets;

			/*
			 * Add in total number of collisions on last
			 *	transmission.
			 */
			sc->arpcom.ac_if.if_collisions += inb(sc->nic_addr +
				ZE_P0_TBCR0);
		}

		/*
		 * Receive Completion. Go and get the packet.
		 *	XXX - Doing this on an error is dubious because there
		 *	   shouldn't be any data to get (we've configured the
		 *	   interface to not accept packets with errors).
		 */
		if (isr & (ZE_ISR_PRX|ZE_ISR_RXE)) {
			ze_rint (unit);
		}

		/*
		 * If it looks like the transmitter can take more data,
		 *	attempt to start output on the interface. If data is
		 *	already buffered and ready to go, send it first.
		 */
		if ((sc->arpcom.ac_if.if_flags & IFF_OACTIVE) == 0) {
			if (sc->data_buffered)
				ze_xmit(&sc->arpcom.ac_if);
			ze_start(&sc->arpcom.ac_if);
		}

		/*
		 * return NIC CR to standard state: page 0, remote DMA complete,
		 * 	start (toggling the TXP bit off, even if was just set
		 *	in the transmit routine, is *okay* - it is 'edge'
		 *	triggered from low to high)
		 */
		outb(sc->nic_addr + ZE_P0_CR, ZE_CR_RD2|ZE_CR_STA);

		/*
		 * If the Network Talley Counters overflow, read them to
		 *	reset them. It appears that old 8390's won't
		 *	clear the ISR flag otherwise - resulting in an
		 *	infinite loop.
		 */
		if (isr & ZE_ISR_CNT) {
			(void) inb(sc->nic_addr + ZE_P0_CNTR0);
			(void) inb(sc->nic_addr + ZE_P0_CNTR1);
			(void) inb(sc->nic_addr + ZE_P0_CNTR2);
		}
	}
}

/*
 * Process an ioctl request. This code needs some work - it looks
 *	pretty ugly.
 */
int
ze_ioctl(ifp, command, data)
	register struct ifnet *ifp;
	int command;
	caddr_t data;
{
	register struct ifaddr *ifa = (struct ifaddr *)data;
	struct ze_softc *sc = &ze_softc[ifp->if_unit];
	struct ifreq *ifr = (struct ifreq *)data;
	int s, error = 0;

	s = splnet();

	switch (command) {

	case SIOCSIFADDR:
		ifp->if_flags |= IFF_UP;

		switch (ifa->ifa_addr->sa_family) {
#ifdef INET
		case AF_INET:
			ze_init(ifp->if_unit);	/* before arpwhohas */
			arp_ifinit((struct arpcom*) ifp, ifa);
			break;
#endif
#ifdef NS
		/*
		 * XXX - This code is probably wrong
		 */
		case AF_NS:
		    {
			register struct ns_addr *ina = &(IA_SNS(ifa)->sns_addr);

			if (ns_nullhost(*ina))
				ina->x_host =
					*(union ns_host *)(sc->arpcom.ac_enaddr);
			else {
				/*
				 *
				 */
				bcopy((caddr_t)ina->x_host.c_host,
				    (caddr_t)sc->arpcom.ac_enaddr,
					sizeof(sc->arpcom.ac_enaddr));
			}
			/*
			 * Set new address
			 */
			ze_init(ifp->if_unit);
			break;
		    }
#endif
		default:
			ze_init(ifp->if_unit);
			break;
		}
		break;

	case SIOCSIFFLAGS:
		/*
		 * When the card is offlined, `up' operation can't be permitted
		 */
		if (!sc->last_alive) {
			int tmp;
			tmp = (ifp->if_flags & IFF_UP);
			if (!sc->last_up && (ifp->if_flags & IFF_UP)) {
				ifp->if_flags &= ~(IFF_UP);
			}
			sc->last_up = tmp;
		}
		/*
		 * If interface is marked down and it is running, then stop it
		 */
		if (((ifp->if_flags & IFF_UP) == 0) &&
		    (ifp->if_flags & IFF_RUNNING)) {
			ze_stop(ifp->if_unit);
			ifp->if_flags &= ~IFF_RUNNING;
		} else {
		/*
		 * If interface is marked up and it is stopped, then start it
		 */
			if ((ifp->if_flags & IFF_UP) &&
		    	    ((ifp->if_flags & IFF_RUNNING) == 0))
				ze_init(ifp->if_unit);
		}
#if NBPFILTER > 0
		if (ifp->if_flags & IFF_PROMISC) {
			/*
			 * Set promiscuous mode on interface.
			 *	XXX - for multicasts to work, we would need to
			 *		write 1's in all bits of multicast
			 *		hashing array. For now we assume that
			 *		this was done in ze_init().
			 */
			outb(sc->nic_addr + ZE_P0_RCR,
				ZE_RCR_PRO|ZE_RCR_AM|ZE_RCR_AB);
		} else {
			/*
			 * XXX - for multicasts to work, we would need to
			 *	rewrite the multicast hashing array with the
			 *	proper hash (would have been destroyed above).
			 */
			outb(sc->nic_addr + ZE_P0_RCR, ZE_RCR_AB);
		}
#endif
		break;

	default:
		error = EINVAL;
	}
	(void) splx(s);
	return (error);
}

/*
 * Macro to calculate a new address within shared memory when given an offset
 *	from an address, taking into account ring-wrap.
 */
#define	ringoffset(sc, start, off, type) \
	((type)( ((caddr_t)(start)+(off) >= (sc)->smem_end) ? \
		(((caddr_t)(start)+(off))) - (sc)->smem_end \
		+ (sc)->smem_ring: \
		((caddr_t)(start)+(off)) ))

/*
 * Retreive packet from shared memory and send to the next level up via
 *	ether_input(). If there is a BPF listener, give a copy to BPF, too.
 */
void
ze_get_packet(sc, buf, len)
	struct ze_softc *sc;
	char *buf;
	u_short len;
{
	struct ether_header *eh;
    	struct mbuf *m, *head = NULL, *ze_ring_to_mbuf();
	u_short off;
	int resid;
	u_short etype;
	struct trailer_header {
		u_short	trail_type;
		u_short trail_residual;
	} trailer_header;

	/* Allocate a header mbuf */
	MGETHDR(m, M_DONTWAIT, MT_DATA);
	if (m == NULL)
		goto bad;
	m->m_pkthdr.rcvif = &sc->arpcom.ac_if;
	m->m_pkthdr.len = len;
	m->m_len = 0;
	head = m;

	eh = (struct ether_header *)buf;

	/* The following sillines is to make NFS happy */
#define EROUND	((sizeof(struct ether_header) + 3) & ~3)
#define EOFF	(EROUND - sizeof(struct ether_header))

	/*
	 * The following assumes there is room for
	 * the ether header in the header mbuf
	 */
	head->m_data += EOFF;
	bcopy(buf, mtod(head, caddr_t), sizeof(struct ether_header));
	buf += sizeof(struct ether_header);
	head->m_len += sizeof(struct ether_header);
	len -= sizeof(struct ether_header);

	/*
	 * Pull packet off interface. Or if this was a trailer packet,
	 * the data portion is appended.
	 */
	m = ze_ring_to_mbuf(sc, buf, m, len);
	if (m == NULL) goto bad;

#if NBPFILTER > 0
	/*
	 * Check if there's a BPF listener on this interface.
	 * If so, hand off the raw packet to bpf.
	 */
	if (sc->bpf) {
		bpf_mtap(sc->bpf, head);

		/*
		 * Note that the interface cannot be in promiscuous mode if
		 * there are no BPF listeners.  And if we are in promiscuous
		 * mode, we have to check if this packet is really ours.
		 *
		 * XXX This test does not support multicasts.
		 */
		if ((sc->arpcom.ac_if.if_flags & IFF_PROMISC) &&
			bcmp(eh->ether_dhost, sc->arpcom.ac_enaddr,
				sizeof(eh->ether_dhost)) != 0 &&
			bcmp(eh->ether_dhost, etherbroadcastaddr,
				sizeof(eh->ether_dhost)) != 0) {

			m_freem(head);
			return;
		}
	}
#endif

	/*
	 * Fix up data start offset in mbuf to point past ether header
	 */
	m_adj(head, sizeof(struct ether_header));

	ether_input(&sc->arpcom.ac_if, eh, head);
	return;

bad:	if (head)
		m_freem(head);
	return;
}

/*
 * Supporting routines
 */

/*
 * Given a source and destination address, copy 'amount' of a packet from
 *	the ring buffer into a linear destination buffer. Takes into account
 *	ring-wrap.
 */
static inline char *
ze_ring_copy(sc,src,dst,amount)
	struct ze_softc *sc;
	char	*src;
	char	*dst;
	u_short	amount;
{
	u_short	tmp_amount;

	/* does copy wrap to lower addr in ring buffer? */
	if (src + amount > sc->smem_end) {
		tmp_amount = sc->smem_end - src;
		bcopy(src,dst,tmp_amount); /* copy amount up to end of smem */
		amount -= tmp_amount;
		src = sc->smem_ring;
		dst += tmp_amount;
	}

	bcopy(src, dst, amount);

	return(src + amount);
}

/*
 * Copy data from receive buffer to end of mbuf chain
 * allocate additional mbufs as needed. return pointer
 * to last mbuf in chain.
 * sc = ze info (softc)
 * src = pointer in ze ring buffer
 * dst = pointer to last mbuf in mbuf chain to copy to
 * amount = amount of data to copy
 */
struct mbuf *
ze_ring_to_mbuf(sc,src,dst,total_len)
	struct ze_softc *sc;
	char *src;
	struct mbuf *dst;
	u_short total_len;
{
	register struct mbuf *m = dst;

	while (total_len) {
		register u_short amount = min(total_len, M_TRAILINGSPACE(m));

		if (amount == 0) { /* no more data in this mbuf, alloc another */
			/*
			 * If there is enough data for an mbuf cluster, attempt
			 * 	to allocate one of those, otherwise, a regular
			 *	mbuf will do.
			 * Note that a regular mbuf is always required, even if
			 *	we get a cluster - getting a cluster does not
			 *	allocate any mbufs, and one is needed to assign
			 *	the cluster to. The mbuf that has a cluster
			 *	extension can not be used to contain data - only
			 *	the cluster can contain data.
			 */
			dst = m;
			MGET(m, M_DONTWAIT, MT_DATA);
			if (m == NULL)
				return (0);

			if (total_len >= MINCLSIZE)
				MCLGET(m, M_DONTWAIT);

			m->m_len = 0;
			dst->m_next = m;
			amount = min(total_len, M_TRAILINGSPACE(m));
		}

		src = ze_ring_copy(sc, src, mtod(m, caddr_t) + m->m_len, amount);

		m->m_len += amount;
		total_len -= amount;

	}
	return (m);
}
#endif

