/*
 * Copyright (c) 1994 Herb Peyerl <hpeyerl@novatel.ca>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *      This product includes software developed by Herb Peyerl.
 * 4. The name of Herb Peyerl may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

/*
 * Created from if_ep.c driver by Fred Gray (fgray@rice.edu) to support
 * the 3c590 family.
 */

/*
 *	Modified from the FreeBSD 1.1.5.1 version by:
 *		 	Andres Vega Garcia
 *			INRIA - Sophia Antipolis, France
 *			avega@sophia.inria.fr
 */

/*
 *  Promiscuous mode added and interrupt logic slightly changed
 *  to reduce the number of adapter failures. Transceiver select
 *  logic changed to use value from EEPROM. Autoconfiguration
 *  features added.
 *  Done by:
 *          Serge Babkin
 *          Chelindbank (Chelyabinsk, Russia)
 *          babkin@hq.icb.chel.su
 */

#include "vx.h"
#if NVX > 0

#include "bpfilter.h"

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/devconf.h>
#include <sys/kernel.h>
#include <sys/mbuf.h>
#include <sys/ioctl.h>

#include <net/if.h>

#ifdef INET
#include <netinet/in.h>
#include <netinet/in_systm.h>
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

#include <dev/vx/if_vxreg.h>

#define ETHER_MAX_LEN	1518
#define ETHER_ADDR_LEN	6

struct vx_softc *vx_softc[NVX];

u_long vx_count;	/* both PCI and EISA */

static struct connector_entry {
  int bit;
  char *name;
} connector_table[VX_CONNECTORS] = {
#define CONNECTOR_UTP	0
  { 0x08, "utp"},
#define CONNECTOR_AUI	1
  { 0x20, "aui"},
/* dummy */
  { 0, "???"},
#define CONNECTOR_BNC	3
  { 0x10, "bnc"},
#define CONNECTOR_TX	4
  { 0x02, "tx"},
#define CONNECTOR_FX	5
  { 0x04, "fx"},
#define CONNECTOR_MII	6
  { 0x40, "mii"},
  { 0, "???"}
};

/* struct vx_softc *vxalloc __P((int)); */
/* void *vxfree __P((struct vx_softc *)); */
/* int vxattach __P((struct vx_softc *)); */
static void vxtxstat __P((struct vx_softc *));
static int vxstatus __P((struct vx_softc *));
static void vxinit __P((struct vx_softc *));
static int vxioctl __P((struct ifnet *, int, caddr_t)); 
static void vxstart __P((struct ifnet *ifp));
static void vxwatchdog __P((int));
static void vxreset __P((struct vx_softc *));
/* void vxstop __P((struct vx_softc *)); */
static void vxread __P((struct vx_softc *));
static struct mbuf *vxget __P((struct vx_softc *, int));
static void vxmbuffill __P((void *));
static void vxmbufempty __P((struct vx_softc *));
static void vxsetfilter __P((struct vx_softc *));
static void vxgetlink __P((struct vx_softc *));
static void vxsetlink __P((struct vx_softc *));
/* int vxbusyeeprom __P((struct vx_softc *)); */
/* void vxintr __P((void *)); */

#ifdef SPIN
#include <sal/salnet.h>


static void
vx_init(int unit)
{
    struct vx_softc *sc = vx_softc[unit];
    vxinit(sc);
}

void
vx_intr(int unit)
{
    struct vx_softc *sc = vx_softc[unit];
    vxintr(sc);
}
#endif

struct vx_softc *
vxalloc(unit)
    int unit;
{
    struct vx_softc *sc;

    if (unit >= NVX) {
	printf("vx%d: unit number too high.\n", unit);
	return NULL;
    }

    if (vx_softc[unit]) {
	printf("vx%d: already allocated.\n", unit);
	return NULL;
    }

    sc = malloc(sizeof(struct vx_softc), M_DEVBUF, M_NOWAIT);
    if (sc == NULL) {
	printf("vx%d: cannot malloc.\n", unit);
	return NULL;
    }
    bzero(sc, sizeof(struct vx_softc));

    vx_softc[unit] = sc;
    sc->unit = unit;
    return (sc);    
}

void
vxfree(sc)
    struct vx_softc *sc;
{
    vx_softc[sc->unit] = NULL;
    free(sc, M_DEVBUF);
    return;
}

int
vxattach(sc)
    struct vx_softc *sc;
{
    struct ifnet *ifp = &sc->arpcom.ac_if;
    int i;

    GO_WINDOW(0);
    outw(VX_COMMAND, GLOBAL_RESET);
    VX_BUSY_WAIT;

    vxgetlink(sc);

    /*
     * Read the station address from the eeprom
     */
    GO_WINDOW(0);
    for (i = 0; i < 3; i++) {
        int x;
        if (vxbusyeeprom(sc))
            return 0;
        outw(BASE + VX_W0_EEPROM_COMMAND, EEPROM_CMD_RD
	     | (EEPROM_OEM_ADDR_0 + i));
        if (vxbusyeeprom(sc))
            return 0;
        x = inw(BASE + VX_W0_EEPROM_DATA);
        sc->arpcom.ac_enaddr[(i << 1)] = x >> 8;
        sc->arpcom.ac_enaddr[(i << 1) + 1] = x;
    }

    printf(" address %s\n", ether_sprintf(sc->arpcom.ac_enaddr));

    ifp->if_unit = sc->unit;
    ifp->if_name = "vx";
    ifp->if_mtu = ETHERMTU;
    ifp->if_flags = IFF_BROADCAST | IFF_SIMPLEX | IFF_NOTRAILERS | IFF_MULTICAST;
    ifp->if_output = ether_output;
    ifp->if_start = vxstart;
    ifp->if_ioctl = vxioctl;
    ifp->if_watchdog = vxwatchdog;

    if_attach(ifp);
    ether_ifattach(ifp);

#if NBPFILTER > 0
    bpfattach(&ifp->if_bpf, ifp, DLT_EN10MB, sizeof(struct ether_header));
#endif

    sc->tx_start_thresh = 20;	/* probably a good starting point. */

    vxstop(sc);

#ifdef SPIN
    ifp->if_init = vx_init;
    ifp->if_recv = salnet_bootrecv; 
    salnet_bootdev(ifp,vx_intr,sc->arpcom.ac_enaddr);
#endif

    return 1;
}

/*
 * The order in here seems important. Otherwise we may not receive
 * interrupts. ?!
 */
static void
vxinit(sc)
    struct vx_softc *sc;
{
    register struct ifnet *ifp = &sc->arpcom.ac_if;
    int i;

    VX_BUSY_WAIT;

    GO_WINDOW(2);

    for (i = 0; i < 6; i++) /* Reload the ether_addr. */
	outb(BASE + VX_W2_ADDR_0 + i, sc->arpcom.ac_enaddr[i]);

    outw(BASE + VX_COMMAND, RX_RESET);
    VX_BUSY_WAIT;
    outw(BASE + VX_COMMAND, TX_RESET);
    VX_BUSY_WAIT;

    GO_WINDOW(1);	/* Window 1 is operating window */
    for (i = 0; i < 31; i++)
	inb(BASE + VX_W1_TX_STATUS);

    outw(BASE + VX_COMMAND,SET_RD_0_MASK | S_CARD_FAILURE |
			S_RX_COMPLETE | S_TX_COMPLETE | S_TX_AVAIL);
    outw(BASE + VX_COMMAND,SET_INTR_MASK | S_CARD_FAILURE |
			S_RX_COMPLETE | S_TX_COMPLETE | S_TX_AVAIL);

    /*
     * Attempt to get rid of any stray interrupts that occured during
     * configuration.  On the i386 this isn't possible because one may
     * already be queued.  However, a single stray interrupt is
     * unimportant.
     */
    outw(BASE + VX_COMMAND, ACK_INTR | 0xff);

    vxsetfilter(sc);
    vxsetlink(sc);

    outw(BASE + VX_COMMAND, RX_ENABLE); /* Enable the receiver. */
    outw(BASE + VX_COMMAND, TX_ENABLE); /* Enable transmitter. */

    vxmbuffill((caddr_t) sc);

    /* Interface is now `running', with no output active. */
    ifp->if_flags |= IFF_RUNNING;
    ifp->if_flags &= ~IFF_OACTIVE;

    /* Attempt to start output, if any. */
    vxstart(ifp);
}

static void
vxsetfilter(sc)
    struct vx_softc *sc;
{
    register struct ifnet *ifp = &sc->arpcom.ac_if;  
        
    GO_WINDOW(1);           /* Window 1 is operating window */
    outw(BASE + VX_COMMAND, SET_RX_FILTER | FIL_INDIVIDUAL | FIL_BRDCST |
#if 0
	((ifp->if_flags & IFF_MULTICAST) ? FIL_MULTICAST : 0 ) |
	((ifp->if_flags & IFF_PROMISC) ? FIL_PROMISC : 0 ));
#else
    	0);
#endif
if (ifp->if_flags & IFF_PROMISC) printf("vxsetfilter: promisc\n\r");
}               

static void            
vxgetlink(sc)
    struct vx_softc *sc;
{
    int n, k;

    GO_WINDOW(3);
    sc->vx_connectors = inw(BASE + VX_W3_RESET_OPT) & 0x7f;
    for (n = 0, k = 0; k < VX_CONNECTORS; k++) {
      if (sc->vx_connectors & connector_table[k].bit) {
	if (n > 0) {
	  printf("/");
	}
	printf(connector_table[k].name);
	n++;
      }
    }
    if (sc->vx_connectors == 0) {
	printf("no connectors!");
	return;
    }
    GO_WINDOW(3);
    sc->vx_connector = (inl(BASE + VX_W3_INTERNAL_CFG) 
			& INTERNAL_CONNECTOR_MASK) 
			>> INTERNAL_CONNECTOR_BITS;
    if (sc->vx_connector & 0x10) {
	sc->vx_connector &= 0x0f;
	printf("[*%s*]", connector_table[sc->vx_connector].name);
        if (! sc->vx_connector & CONNECTOR_MII)
	    printf(": disable 'auto select' with DOS util!", sc->unit);
    } else {
	printf("[*%s*]", connector_table[sc->vx_connector].name);
    }
}

static void            
vxsetlink(sc)
    struct vx_softc *sc;
{       
    register struct ifnet *ifp = &sc->arpcom.ac_if;  
    int i, j;

    /*
     * S.B.
     *
     * Now behavior was slightly changed:
     *
     * if any of flags link[0-2] is used and its connector is
     * physically present the following connectors are used:
     *
     *   link0 - AUI * highest precedence
     *   link1 - BNC
     *   link2 - UTP * lowest precedence
     *
     * If none of them is specified then
     * connector specified in the EEPROM is used
     * (if present on card or AUI if not).
     */
    /* Set the xcvr. */

    if(ifp->if_flags & IFF_LINK0 && sc->vx_connectors & CONNECTOR_AUI) {
	i = CONNECTOR_AUI;
    } else if(ifp->if_flags & IFF_LINK1 && sc->vx_connectors & CONNECTOR_BNC) {
	i = CONNECTOR_BNC;
    } else if(ifp->if_flags & IFF_LINK2 && sc->vx_connectors & CONNECTOR_UTP) {
	i = CONNECTOR_UTP;
    } else {
	i = sc->vx_connector;
    }
    GO_WINDOW(3);
    j = inl(BASE + VX_W3_INTERNAL_CFG) & ~INTERNAL_CONNECTOR_MASK;
    outl(BASE + VX_W3_INTERNAL_CFG, j | (i <<INTERNAL_CONNECTOR_BITS));
    switch(i) {
      case CONNECTOR_AUI:
	/* nothing to do */
	break;
      case CONNECTOR_UTP:
	if(sc->vx_connectors & connector_table[CONNECTOR_UTP].bit) {
	    GO_WINDOW(4);
	    outw(BASE + VX_W4_MEDIA_TYPE, ENABLE_UTP);
	}
	break;
      case CONNECTOR_BNC:
	if(sc->vx_connectors & connector_table[CONNECTOR_BNC].bit) {
	    outw(BASE + VX_COMMAND, START_TRANSCEIVER);
	    DELAY(800);
	}
	break;
      case CONNECTOR_TX:
	if(sc->vx_connectors & connector_table[CONNECTOR_TX].bit) {
	    GO_WINDOW(4);
	    outw(BASE + VX_W4_MEDIA_TYPE, LINKBEAT_ENABLE);
	}
	break;
      case CONNECTOR_FX:
	if(sc->vx_connectors & connector_table[CONNECTOR_FX].bit) {
	    GO_WINDOW(4);
	    outw(BASE + VX_W4_MEDIA_TYPE, LINKBEAT_ENABLE);
	}
	break;
      case CONNECTOR_MII:
	if(sc->vx_connectors & connector_table[CONNECTOR_MII].bit) {
	    GO_WINDOW(4);
	    outw(BASE + VX_W4_MEDIA_TYPE, ENABLE_UTP);
	}
	break;
      default:
	if(sc->vx_connectors & connector_table[CONNECTOR_UTP].bit) {
	    printf("vx%d: strange connector type in EEPROM: %d\n",
		   sc->unit, sc->vx_connector);
	    printf("vx%d: assuming UTP\n", sc->unit);
	    GO_WINDOW(4);
	    outw(BASE + VX_W4_MEDIA_TYPE, ENABLE_UTP);
	} else {
 	    if (ifp->if_flags & IFF_DEBUG) {
		    printf("vx%d: strange connector type in EEPROM: %d\n\r",
			   sc->unit, sc->vx_connector);
		    printf("vx%d: assuming AUI\n\r", sc->unit);
	    }
	}
	break;
    }
    GO_WINDOW(1); 
}

#define DB /* printf */
/*
#define VXCHECKLEN
 */

static void
vxstart(ifp)
    struct ifnet *ifp;
{
    register struct vx_softc *sc = vx_softc[ifp->if_unit];
    register struct mbuf *m, *m0;
    int sh, len, pad;

    /* Don't transmit if interface is busy or not running */
    if ((sc->arpcom.ac_if.if_flags & (IFF_RUNNING|IFF_OACTIVE)) != IFF_RUNNING) 
    {
	    if (sc->arpcom.ac_if.if_flags & IFF_DEBUG)
		    printf("vx%d: interface is busy\n\r", sc->vx_connector);
	    return;
    }

startagain:
    /* Sneak a peek at the next packet */
    m0 = ifp->if_snd.ifq_head;
    if (m0 == 0) {
	return;
    }
    /* We need to use m->m_pkthdr.len, so require the header */
     if ((m0->m_flags & M_PKTHDR) == 0)
	panic("vxstart: no header mbuf");
     len = m0->m_pkthdr.len;

     pad = (4 - len) & 3;
DB("tx len %d pad %d ",len,pad);

#ifdef VXCHECKLEN
	if (len != m_length(m0)) {
		printf("vx%d: mbuf len %d not pkthdr len %d",
			sc->vx_connector,m_length(m0),len);
		panic("vxstart cannot send packet\n");
	}
#endif

    /*
     * The 3c509 automatically pads short packets to minimum ethernet length,
     * but we drop packets that are too large. Perhaps we should truncate
     * them instead?
     */
    if (len + pad > ETHER_MAX_LEN) {
	/* packet is obviously too large: toss it */
	++ifp->if_oerrors;
	IF_DEQUEUE(&ifp->if_snd, m0);
	m_freem(m0);
	goto readcheck;
    }
    /* check if there is space on the card */
DB("tx free %x ",inw(BASE + VX_W1_FREE_TX)) ;
    if (inw(BASE + VX_W1_FREE_TX) < len + pad + 4) {
	outw(BASE + VX_COMMAND, SET_TX_AVAIL_THRESH | ((len + pad + 4) >> 2));
	/* not enough room in FIFO */
	ifp->if_flags |= IFF_OACTIVE;
	ifp->if_timer = 1;
	if (ifp->if_flags & IFF_DEBUG)
		printf("vx%d: not enough room in FIFO\n\r", sc->vx_connector);
	return;
    } 
    IF_DEQUEUE(&ifp->if_snd, m0);
    if (m0 == 0) {		/* not really needed */
	return;
    }

#if 0
    outw(BASE + VX_COMMAND, SET_TX_START_THRESH |
	((len / 4 + sc->tx_start_thresh) >> 2));
#else
outw(BASE + VX_COMMAND, SET_TX_START_THRESH | 8188);
#endif


DB("tx start thresh 0x%x\n\r", ((len / 4 + sc->tx_start_thresh) >> 2));
#if NBPFILTER > 0
    if (ifp->if_bpf) {
	bpf_mtap(ifp->if_bpf, m0);
    }
#endif

    /*
     * Do the output at splhigh() so that an interrupt from another device
     * won't cause a FIFO underrun.
     */
    sh = splhigh();

    /* put out the doubleword header... */
    outl(BASE + VX_W1_TX_PIO_WR_1, len | TX_INDICATE);


    for (m = m0; m != 0;) {
	/* ... and the mbuf data in words... */
DB("tx.mbuf %d ", m->m_len );
        if (m->m_len > 3)
	    outsl(BASE + VX_W1_TX_PIO_WR_1, mtod(m, caddr_t), m->m_len / 4);
	/* ... and the remaining bytes of the mbuf. */
        if (m->m_len & 3)
	    outsb(BASE + VX_W1_TX_PIO_WR_1,
	      mtod(m, caddr_t) + (m->m_len & ~3) , m->m_len & 3);
        MFREE(m, m0);
        m = m0;
    }
DB("tx free %x\n\r",inw(BASE + VX_W1_FREE_TX)) ;
if (pad)
DB("pad %d ",pad);
    while (pad--)
	outb(BASE + VX_W1_TX_PIO_WR_1, 0);	/* Padding */
DB("tx free %x\n\r",inw(BASE + VX_W1_FREE_TX)) ;

    if (inw(BASE + VX_W1_FREE_TX) < 8188){
	/* Interrupt us when the FIFO has room for max-sized packet. */
	outw(BASE + VX_COMMAND, SET_TX_AVAIL_THRESH | (8188 >> 2));
    }

    splx(sh);

    ++ifp->if_opackets;
    ifp->if_timer = 1;

readcheck:
    if ((inw(BASE + VX_W1_RX_STATUS) & ERR_INCOMPLETE) == 0) {
	/* We received a complete packet. */
	
	if ((inw(BASE + VX_STATUS) & S_INTR_LATCH) == 0) {
	    /*
	     * No interrupt, read the packet and continue
	     * Is  this supposed to happen? Is my motherboard
	     * completely busted?
	     */
	    vxread(sc);
	} else
	    /* Got an interrupt, return so that it gets serviced. */
	    return;
    } else {
	/* Check if we are stuck and reset [see XXX comment] */
	if (vxstatus(sc)) {
	    if (ifp->if_flags & IFF_DEBUG)
	       printf("vx%d: adapter reset\n", ifp->if_unit);
	    vxreset(sc);
	}
    }

    goto startagain;
}

/*
 * XXX: The 3c509 card can get in a mode where both the fifo status bit
 *      FIFOS_RX_OVERRUN and the status bit ERR_INCOMPLETE are set
 *      We detect this situation and we reset the adapter.
 *      It happens at times when there is a lot of broadcast traffic
 *      on the cable (once in a blue moon).
 */
static int
vxstatus(sc)
    struct vx_softc *sc;
{
    int fifost;

    /*
     * Check the FIFO status and act accordingly
     */
    GO_WINDOW(4);
    fifost = inw(BASE + VX_W4_FIFO_DIAG);
    GO_WINDOW(1);

    if (fifost & FIFOS_RX_UNDERRUN) {
	if (sc->arpcom.ac_if.if_flags & IFF_DEBUG)
	    printf("vx%d: RX underrun\n", sc->unit);
	vxreset(sc);
	return 0;
    }

    if (fifost & FIFOS_RX_STATUS_OVERRUN) {
	if (sc->arpcom.ac_if.if_flags & IFF_DEBUG)
	    printf("vx%d: RX Status overrun\n", sc->unit);
	return 1;
    }

    if (fifost & FIFOS_RX_OVERRUN) {
	if (sc->arpcom.ac_if.if_flags & IFF_DEBUG)
	    printf("vx%d: RX overrun\n", sc->unit);
	return 1;
    }

    if (fifost & FIFOS_TX_OVERRUN) {
	if (sc->arpcom.ac_if.if_flags & IFF_DEBUG)
	    printf("vx%d: TX overrun\n", sc->unit);
	vxreset(sc);
	return 0;
    }

    return 0;
}

static void     
vxtxstat(sc)
    struct vx_softc *sc;
{
    int i;

    /*
    * We need to read+write TX_STATUS until we get a 0 status
    * in order to turn off the interrupt flag.
    */
    while ((i = inb(BASE + VX_W1_TX_STATUS)) & TXS_COMPLETE) {
	/* pop the status stack */
	outb(BASE + VX_W1_TX_STATUS, 0x0);

DB("tx.%x ",i);
	if (i & TXS_JABBER) {
	    ++sc->arpcom.ac_if.if_oerrors;
	    if (sc->arpcom.ac_if.if_flags & IFF_DEBUG)
		printf("vx%d: jabber (%x)\n", sc->unit, i);
	    vxreset(sc);
	} else if (i & TXS_UNDERRUN) {
	    ++sc->arpcom.ac_if.if_oerrors;
	    if (sc->arpcom.ac_if.if_flags & IFF_DEBUG)
		printf("vx%d: fifo underrun (%x) @%d\n\r",
		       sc->unit, i, sc->tx_start_thresh);
	    if (sc->tx_succ_ok < 100)
		sc->tx_start_thresh = min(ETHER_MAX_LEN, sc->tx_start_thresh + 20);
	    sc->tx_succ_ok = 0;
	    vxreset(sc);
	} else if (i & TXS_MAX_COLLISION) {
	    ++sc->arpcom.ac_if.if_collisions;
	    outw(BASE + VX_COMMAND, TX_ENABLE);
	    sc->arpcom.ac_if.if_flags &= ~IFF_OACTIVE;
	} else {
	    sc->tx_succ_ok = (sc->tx_succ_ok+1) & 127;
	}
    }
DB("txnc.%x\n\r",i);
}


void
vxintr(sc)
    struct vx_softc *sc;
{
    register short status;
    struct ifnet *ifp = &sc->arpcom.ac_if;

    for (;;) {
	outw(BASE + VX_COMMAND, C_INTR_LATCH);

	status = inw(BASE + VX_STATUS);

	if ((status & (S_TX_COMPLETE | S_TX_AVAIL |
		S_RX_COMPLETE | S_CARD_FAILURE)) == 0)
	    break;

	/*
	 * Acknowledge any interrupts.  It's important that we do this
	 * first, since there would otherwise be a race condition.
	 * Due to the i386 interrupt queueing, we may get spurious
	 * interrupts occasionally.
	 */
	outw(BASE + VX_COMMAND, ACK_INTR | status);

	if (status & S_RX_COMPLETE)
	    vxread(sc);
	if (status & S_TX_AVAIL) {
	    ifp->if_timer = 0;
	    sc->arpcom.ac_if.if_flags &= ~IFF_OACTIVE;
	    vxstart(&sc->arpcom.ac_if);
	}
	if (status & S_CARD_FAILURE) {
	    printf("vx%d: adapter failure (%x)\n", sc->unit, status);
	    ifp->if_timer = 0;
	    vxreset(sc);
	    return;
	}
	if (status & S_TX_COMPLETE) {
	    ifp->if_timer = 0;
	    vxtxstat(sc);
	    vxstart(ifp);
	}
    }

#ifdef SPIN
    /* we call if_recv here where it is always ok to send packets out dev */
    if (ifp->if_rcv.ifq_len) (*ifp->if_recv)(ifp);
#endif
    /* no more interrupts */
    return;
}

static void
vxread(sc)
    struct vx_softc *sc;
{
    struct ifnet *ifp = &sc->arpcom.ac_if;
    struct mbuf *m;
    struct ether_header *eh;
    int len;

    len = inw(BASE + VX_W1_RX_STATUS);

again:

    if (ifp->if_flags & IFF_DEBUG) {
	int err = len & ERR_MASK;
	char *s = NULL;
	if (len & ERR_INCOMPLETE)
	    s = "incomplete packet";
	else if (err == ERR_OVERRUN)
	    s = "packet overrun";
	else if (err == ERR_RUNT)
	    s = "runt packet";
	else if (err == ERR_ALIGNMENT)
	    s = "bad alignment";
	else if (err == ERR_CRC)
	    s = "bad crc";
	else if (err == ERR_OVERSIZE)
	    s = "oversized packet";
	else if (err == ERR_DRIBBLE)
	    s = "dribble bits";

	if (s)
	printf("vx%d: %s\n", sc->unit, s);
    }

    if (len & ERR_INCOMPLETE)
	return;

    if (len & ERR_RX) {
	++ifp->if_ierrors;
	goto abort;
    }

    len &= RX_BYTES_MASK;	/* Lower 11 bits = RX bytes. */

    /* Pull packet off interface. */
    m = vxget(sc, len);
    if (m == 0) {
	ifp->if_ierrors++;
	goto abort;
    }

    ++ifp->if_ipackets;

    /* We assume the header fit entirely in one mbuf. */
    eh = mtod(m, struct ether_header *);
#if NBPFILTER > 0
    /*
     * Check if there's a BPF listener on this interface.
     * If so, hand off the raw packet to BPF.
     */
    if (ifp->if_bpf) {
	bpf_mtap(ifp->if_bpf, m);

	/*
	 * Note that the interface cannot be in promiscuous mode if
	 * there are no BPF listeners.  And if we are in promiscuous
	 * mode, we have to check if this packet is really ours.
	 */
	if ((ifp->if_flags & IFF_PROMISC) &&
	    (eh->ether_dhost[0] & 1) == 0 && /* !mcast and !bcast */
	    bcmp(eh->ether_dhost, sc->arpcom.ac_enaddr,
	    sizeof(eh->ether_dhost)) != 0) {
	    m_freem(m);
	    return;
	}
    }
#endif

    /* We assume the header fit entirely in one mbuf. */
    m_adj(m, sizeof(struct ether_header));
    ether_input(ifp, eh, m);



    /*
    * In periods of high traffic we can actually receive enough
    * packets so that the fifo overrun bit will be set at this point,
    * even though we just read a packet. In this case we
    * are not going to receive any more interrupts. We check for
    * this condition and read again until the fifo is not full.
    * We could simplify this test by not using vxstatus(), but
    * rechecking the RX_STATUS register directly. This test could
    * result in unnecessary looping in cases where there is a new
    * packet but the fifo is not full, but it will not fix the
    * stuck behavior.
    *
    * Even with this improvement, we still get packet overrun errors
    * which are hurting performance. Maybe when I get some more time
    * I'll modify vxread() so that it can handle RX_EARLY interrupts.
    */
    if (vxstatus(sc)) {
	len = inw(BASE + VX_W1_RX_STATUS);
	/* Check if we are stuck and reset [see XXX comment] */
	if (len & ERR_INCOMPLETE) {
	    if (ifp->if_flags & IFF_DEBUG)
		printf("vx%d: adapter reset\n", sc->unit);
	    vxreset(sc);
	    return;
	}
	goto again;
    }

DB("vxread: read %d %s->%s\n\r",len,
	ether_sprintf(eh->ether_shost),ether_sprintf(eh->ether_dhost));
    return;

abort:
printf("vxread: abort\n\r");
    outw(BASE + VX_COMMAND, RX_DISCARD_TOP_PACK);
    VX_BUSY_WAIT;
}

static struct mbuf *
vxget(sc, totlen)
    struct vx_softc *sc;
    int totlen;
{
    struct ifnet *ifp = &sc->arpcom.ac_if;
    struct mbuf *top, **mp, *m;
    int len;
    int sh;

    m = sc->mb[sc->next_mb];
    sc->mb[sc->next_mb] = 0;
    if (m == 0) {
        MGETHDR(m, M_DONTWAIT, MT_DATA);
        if (m == 0)
            return 0;
    } else {
        /* If the queue is no longer full, refill. */
        if (sc->last_mb == sc->next_mb){
            timeout(vxmbuffill, sc, 1);
#if 1 /* XXX MEF HACK */
	    vxmbuffill(sc);
#endif /* XXX MEF HACK */
        }

        /* Convert one of our saved mbuf's. */
        sc->next_mb = (sc->next_mb + 1) % MAX_MBS;
        m->m_data = m->m_pktdat;
        m->m_flags = M_PKTHDR;
    }
    m->m_pkthdr.rcvif = ifp;
    m->m_pkthdr.len = totlen;
    len = MHLEN;
    top = 0;
    mp = &top;

    /*
     * We read the packet at splhigh() so that an interrupt from another
     * device doesn't cause the card's buffer to overflow while we're
     * reading it.  We may still lose packets at other times.
     */
    sh = splhigh();

    while (totlen > 0) {
        if (top) {
            m = sc->mb[sc->next_mb];
            sc->mb[sc->next_mb] = 0;
            if (m == 0) {
                MGET(m, M_DONTWAIT, MT_DATA);
                if (m == 0) {
                    splx(sh);
                    m_freem(top);
                    return 0;
                }
            } else {
                sc->next_mb = (sc->next_mb + 1) % MAX_MBS;
            }
            len = MLEN;
        }
        if (totlen >= MINCLSIZE) {
        MCLGET(m, M_DONTWAIT);
        if (m->m_flags & M_EXT)
            len = MCLBYTES;
        }
        len = min(totlen, len);
        if (len > 3) {
            len &= ~3;
            insl(BASE + VX_W1_RX_PIO_RD_1, mtod(m, u_int32_t *),
                len / 4);
        } else
            insb(BASE + VX_W1_RX_PIO_RD_1, mtod(m, u_int8_t *),
                len);
        m->m_len = len;
        totlen -= len;
        *mp = m;
        mp = &m->m_next;
    }

    outw(BASE +VX_COMMAND, RX_DISCARD_TOP_PACK);
    VX_BUSY_WAIT;

    splx(sh);

    return top;
}


static int
vxioctl(ifp, cmd, data)
    register struct ifnet *ifp;
    int cmd;
    caddr_t data;
{
    struct vx_softc *sc = vx_softc[ifp->if_unit];
    struct ifaddr *ifa = (struct ifaddr *) data;
    struct ifreq *ifr = (struct ifreq *) data;
    int s, error = 0;

    s = splimp();

    switch (cmd) {

    case SIOCSIFADDR:
	ifp->if_flags |= IFF_UP;

	switch (ifa->ifa_addr->sa_family) {
#ifdef INET
	case AF_INET:
	    vxinit(sc);
	    arp_ifinit(&sc->arpcom, ifa);
	    break;
#endif

#ifdef NS
	case AF_NS:
		{
	    register struct ns_addr *ina = &(IA_SNS(ifa)->sns_addr);

	    if (ns_nullhost(*ina))
	        ina->x_host = *(union ns_host *) (sc->arpcom.ac_enaddr);
	    else {
	        ifp->if_flags &= ~IFF_RUNNING;
	        bcopy((caddr_t) ina->x_host.c_host,
		      (caddr_t) sc->arpcom.ac_enaddr,
		      sizeof(sc->arpcom.ac_enaddr));
	    }
	    vxinit(sc);
	    break;
		}
#endif
	default:
	    vxinit(sc);
	    break;
        }
	break;

    case SIOCGIFADDR:
	{
	    struct sockaddr *sa; 
 
	    sa = (struct sockaddr *) & ifr->ifr_data;
	    bcopy((caddr_t) sc->arpcom.ac_enaddr, 
		    (caddr_t) sa->sa_data, ETHER_ADDR_LEN);
	}
	break;

    case SIOCSIFFLAGS:
	if ((ifp->if_flags & IFF_UP) == 0 &&
	    (ifp->if_flags & IFF_RUNNING) != 0) {
	    /*
             * If interface is marked up and it is stopped, then
             * start it.
             */
	    vxstop(sc);
	    ifp->if_flags &= ~IFF_RUNNING;
        } else if ((ifp->if_flags & IFF_UP) != 0 &&
                   (ifp->if_flags & IFF_RUNNING) == 0) {
            /*
             * If interface is marked up and it is stopped, then
             * start it.
             */
            vxinit(sc);
        } else {
            /*
             * deal with flags changes:
             * IFF_MULTICAST, IFF_PROMISC,
             * IFF_LINK0, IFF_LINK1,
             */
            vxsetfilter(sc);
            vxsetlink(sc);
        }
        break;

    case SIOCSIFMTU:
        /*
         * Set the interface MTU.
         */
        if (ifr->ifr_mtu > ETHERMTU) {
            error = EINVAL;
        } else {
            ifp->if_mtu = ifr->ifr_mtu;
        }
        break;

    case SIOCADDMULTI:
    case SIOCDELMULTI:
        error = (cmd == SIOCADDMULTI) ?
            ether_addmulti(ifr, &sc->arpcom) :
            ether_delmulti(ifr, &sc->arpcom);

        if (error == ENETRESET) {
            /*
             * Multicast list has changed; set the hardware filter
             * accordingly.
             */
            vxreset(sc);
            error = 0;
        }
        break;


    default:
        error = EINVAL;
    }

    splx(s);

    return (error);
}

static void
vxreset(sc)
    struct vx_softc *sc;
{
    int s;
    struct ifnet *ifp = &sc->arpcom.ac_if;
    if (ifp->if_flags & IFF_DEBUG)
	printf("vxreset\n\r");
    s = splimp();

    vxstop(sc);
    vxinit(sc);
    splx(s);
}

static void
vxwatchdog(unit)
    int unit;
{
    struct vx_softc *sc = vx_softc[unit];
    struct ifnet *ifp = &sc->arpcom.ac_if;

    if (ifp->if_flags & IFF_DEBUG)
	printf("vx%d: device timeout\n", unit);
    ifp->if_flags &= ~IFF_OACTIVE;
    vxstart(ifp);
    vxintr(sc);
}

void
vxstop(sc)
    struct vx_softc *sc;
{
    struct ifnet *ifp = &sc->arpcom.ac_if;

    ifp->if_timer = 0;

    outw(BASE + VX_COMMAND, RX_DISABLE);
    outw(BASE + VX_COMMAND, RX_DISCARD_TOP_PACK);
    VX_BUSY_WAIT;
    outw(BASE + VX_COMMAND, TX_DISABLE);
    outw(BASE + VX_COMMAND, STOP_TRANSCEIVER);
    DELAY(800);
    outw(BASE + VX_COMMAND, RX_RESET);
    VX_BUSY_WAIT;
    outw(BASE + VX_COMMAND, TX_RESET);
    VX_BUSY_WAIT;
    outw(BASE + VX_COMMAND, C_INTR_LATCH);
    outw(BASE + VX_COMMAND, SET_RD_0_MASK);
    outw(BASE + VX_COMMAND, SET_INTR_MASK);
    outw(BASE + VX_COMMAND, SET_RX_FILTER);

    vxmbufempty(sc);
}

int
vxbusyeeprom(sc)
    struct vx_softc *sc;
{
    int j, i = 100;

    while (i--) {
        j = inw(BASE + VX_W0_EEPROM_COMMAND);
        if (j & EEPROM_BUSY)
            DELAY(100);
        else
            break;
    }
    if (!i) {
        printf("vx%d: eeprom failed to come ready\n", sc->unit);
        return (1);
    }
    return (0);
}

static void
vxmbuffill(sp)
    void *sp;
{
    struct vx_softc *sc = (struct vx_softc *) sp;
    struct ifnet *ifp = &sc->arpcom.ac_if;
    int s, i;

    s = splimp();
    i = sc->last_mb;
    do {
	if (sc->mb[i] == NULL)
	    MGET(sc->mb[i], M_DONTWAIT, MT_DATA);
	if (sc->mb[i] == NULL)
	    break;
	i = (i + 1) % MAX_MBS;
    } while (i != sc->next_mb);
    sc->last_mb = i;
    /* If the queue was not filled, try again. */
    if (sc->last_mb != sc->next_mb){
	timeout(vxmbuffill, sc, 1);
        if (ifp->if_flags & IFF_DEBUG)
		printf("vx%d: queue was not filled\n\r", sc->unit);
    }
    splx(s);
}

static void
vxmbufempty(sc)
    struct vx_softc *sc;
{
    int s, i;

    s = splimp();
    for (i = 0; i < MAX_MBS; i++) {
	if (sc->mb[i]) {
	    m_freem(sc->mb[i]);
	    sc->mb[i] = NULL;
	}
    }
    sc->last_mb = sc->next_mb = 0;
    untimeout(vxmbuffill, sc);
    splx(s);
}

#endif				/* NVX > 0 */
