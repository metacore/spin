/*-
 * Copyright (c) 1992, 1993, University of Vermont and State
 *  Agricultural College.
 * Copyright (c) 1992, 1993, Garrett A. Wollman.
 *
 * Portions:
 * Copyright (c) 1990, 1991, William F. Jolitz
 * Copyright (c) 1990, The Regents of the University of California
 *
 * 3Com 3C507 support:
 * Copyright (c) 1993, 1994, Charles M. Hannum
 *
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
 *	This product includes software developed by the University of
 *	Vermont and State Agricultural College and Garrett A. Wollman,
 *	by William F. Jolitz, by the University of California,
 *	Berkeley, by Larwence Berkeley Laboratory, by Charles M. Hannum,
 *	and their contributors.
 * 4. Neither the names of the Universities nor the names of the authors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE UNIVERSITY OR AUTHORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	$Id: if_ie.c,v 1.1.1.1 1996/08/15 03:22:59 fgray Exp $
 */

/*
 * Intel 82586 Ethernet chip
 * Register, bit, and structure definitions.
 *
 * Written by GAW with reference to the Clarkson Packet Driver code for this
 * chip written by Russ Nelson and others.
 *
 * BPF support code stolen directly from hpdev/if_le.c, supplied with
 * tcpdump.
 */

/*
 * The i82586 is a very versatile chip, found in many implementations.
 * Programming this chip is mostly the same, but certain details differ
 * from card to card.  This driver is written so that different cards
 * can be automatically detected at run-time.  Currently, only the
 * AT&T EN100/StarLAN 10 series are supported.
 */

/*
Mode of operation:

We run the 82586 in a standard Ethernet mode.  We keep NFRAMES received
frame descriptors around for the receiver to use, and NBUFFS associated
receive buffer descriptors, both in a circular list.  Whenever a frame is
received, we rotate both lists as necessary.  (The 586 treats both lists
as a simple queue.)  We also keep a transmit command around so that packets
can be sent off quickly.

We configure the adapter in AL-LOC = 1 mode, which means that the
Ethernet/802.3 MAC header is placed at the beginning of the receive buffer
rather than being split off into various fields in the RFD.  This also
means that we must include this header in the transmit buffer as well.

By convention, all transmit commands, and only transmit commands, shall
have the I (IE_CMD_INTR) bit set in the command.  This way, when an
interrupt arrives at ieintr(), it is immediately possible to tell
what precisely caused it.  ANY OTHER command-sending routines should
run at splimp(), and should post an acknowledgement to every interrupt
they generate.

The 82586 has a 24-bit address space internally, and the adaptor's
memory is located at the top of this region.  However, the value we are
given in configuration is normally the *bottom* of the adaptor RAM.  So,
we must go through a few gyrations to come up with a kernel virtual address
which represents the actual beginning of the 586 address space.  First,
we autosize the RAM by running through several possible sizes and trying
to initialize the adapter under the assumption that the selected size
is correct.  Then, knowing the correct RAM size, we set up our pointers
in ie_softc[unit].  `iomem' represents the computed base of the 586
address space.  `iomembot' represents the actual configured base
of adapter RAM.  Finally, `iosize' represents the calculated size
of 586 RAM.  Then, when laying out commands, we use the interval
[iomembot, iomembot + iosize); to make 24-pointers, we subtract
iomem, and to make 16-pointers, we subtract iomem and and with 0xffff.

*/

#include "ie.h"
#if NIE > 0

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/kernel.h>
#include <sys/mbuf.h>
#include <sys/protosw.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <sys/errno.h>
#include <sys/syslog.h>
#include <sys/devconf.h>

#include <net/if.h>
#include <net/if_types.h>
#include <net/if_dl.h>
#include <net/route.h>

#include "bpfilter.h"

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

#include <machine/clock.h>

#include <i386/isa/isa.h>
#include <i386/isa/isa_device.h>
#include <i386/isa/ic/i82586.h>
#include <i386/isa/if_iereg.h>
#include <i386/isa/icu.h>
#include <i386/isa/if_ie507.h>
#include <i386/isa/elink.h>

#include <vm/vm.h>

#if NBPFILTER > 0
#include <net/bpf.h>
#include <net/bpfdesc.h>
#endif

static struct mbuf *last_not_for_us;

#ifdef DEBUG
#define IED_RINT 1
#define IED_TINT 2
#define IED_RNR 4
#define IED_CNA 8
#define IED_READFRAME 16
int ie_debug = IED_RNR;
#endif

#ifndef ETHERMINLEN
#define ETHERMINLEN 60
#endif

#define IE_BUF_LEN 1512		/* length of transmit buffer */

/* Forward declaration */
struct ie_softc;

static int ieprobe(struct isa_device *dvp);
static int ieattach(struct isa_device *dvp);
static void ieinit(int unit);
static int ieioctl(struct ifnet *ifp, int command, caddr_t data);
static void iestart(struct ifnet *ifp);
static void el_reset_586(int unit);
static void el_chan_attn(int unit);
static void sl_reset_586(int unit);
static void sl_chan_attn(int unit);
static void iereset(int unit);
static void ie_readframe(int unit, struct ie_softc *ie, int bufno);
static void ie_drop_packet_buffer(int unit, struct ie_softc *ie);
static void sl_read_ether(int unit, unsigned char addr[6]);
static void find_ie_mem_size(int unit);
static int command_and_wait(int unit, int command, void volatile *pcmd, int);
static int ierint(int unit, struct ie_softc *ie);
static int ietint(int unit, struct ie_softc *ie);
static int iernr(int unit, struct ie_softc *ie);
static void start_receiver(int unit);
static int ieget(int, struct ie_softc *, struct mbuf **,
		   struct ether_header *, int *);
static caddr_t setup_rfa(caddr_t ptr, struct ie_softc *ie);
static int mc_setup(int, caddr_t, volatile struct ie_sys_ctl_block *);
static void ie_mc_reset(int unit);

#ifdef DEBUG
void print_rbd(volatile struct ie_recv_buf_desc *rbd);

int in_ierint = 0;
int in_ietint = 0;
#endif

/*
 * This tells the autoconf code how to set us up.
 */
struct isa_driver iedriver = {
  ieprobe, ieattach, "ie",
};

enum ie_hardware {
  IE_STARLAN10,
  IE_EN100,
  IE_SLFIBER,
  IE_3C507,
  IE_NI5210,
  IE_UNKNOWN
};

const char *ie_hardware_names[] = {
  "StarLAN 10",
  "EN100",
  "StarLAN Fiber",
  "3C507",
  "NI5210",
  "Unknown"
};

/*
sizeof(iscp) == 1+1+2+4 == 8
sizeof(scb) == 2+2+2+2+2+2+2+2 == 16
NFRAMES * sizeof(rfd) == NFRAMES*(2+2+2+2+6+6+2+2) == NFRAMES*24 == 384
sizeof(xmit_cmd) == 2+2+2+2+6+2 == 18
sizeof(transmit buffer) == 1512
sizeof(transmit buffer desc) == 8
-----
1946

NBUFFS * sizeof(rbd) == NBUFFS*(2+2+4+2+2) == NBUFFS*12
NBUFFS * IE_RBUF_SIZE == NBUFFS*256

NBUFFS should be (16384 - 1946) / (256 + 12) == 14438 / 268 == 53

With NBUFFS == 48, this leaves us 1574 bytes for another command or
more buffers.  Another transmit command would be 18+8+1512 == 1538
---just barely fits!

Obviously all these would have to be reduced for smaller memory sizes.
With a larger memory, it would be possible to roughly double the number of
both transmit and receive buffers.
*/

#define NFRAMES 16		/* number of frames to allow for receive */
#define NBUFFS 48		/* number of buffers to allocate */
#define IE_RBUF_SIZE 256	/* size of each buffer, MUST BE POWER OF TWO */

/*
 * Ethernet status, per interface.
 */
struct ie_softc {
  struct arpcom arpcom;
  void (*ie_reset_586)(int);
  void (*ie_chan_attn)(int);
  enum ie_hardware hard_type;
  int hard_vers;

  u_short port;
  caddr_t iomem;
  caddr_t iomembot;
  unsigned iosize;
  int bus_use;		/* 0 means 16bit, 1 means 8 bit adapter */

  int want_mcsetup;
  int promisc;
  volatile struct ie_int_sys_conf_ptr *iscp;
  volatile struct ie_sys_ctl_block *scb;
  volatile struct ie_recv_frame_desc *rframes[NFRAMES];
  volatile struct ie_recv_buf_desc *rbuffs[NBUFFS];
  volatile char *cbuffs[NBUFFS];
  int rfhead, rftail, rbhead, rbtail;

  volatile struct ie_xmit_cmd *xmit_cmds[2];
  volatile struct ie_xmit_buf *xmit_buffs[2];
  int xmit_count;
  u_char *xmit_cbuffs[2];

  struct ie_en_addr mcast_addrs[MAXMCAST + 1];
  int mcast_count;

#if NBPFILTER > 0
  caddr_t ie_bpf;
#endif

} ie_softc[NIE];

#define MK_24(base, ptr) ((caddr_t)((u_long)ptr - (u_long)base))
#define MK_16(base, ptr) ((u_short)(u_long)MK_24(base, ptr))

#define PORT ie_softc[unit].port
#define MEM ie_softc[unit].iomem

static int sl_probe(struct isa_device *);
static int el_probe(struct isa_device *);
static int ni_probe(struct isa_device *);

static struct kern_devconf kdc_ie[NIE] = { {
	0, 0, 0,		/* filled in by dev_attach */
	"ie", 0, { MDDT_ISA, 0, "net" },
	isa_generic_externalize, 0, 0, ISA_EXTERNALLEN,
	&kdc_isa0,		/* parent */
	0,			/* parentdata */
	DC_UNCONFIGURED,	/* state */
	"Ethernet adapter",	/* description */
	DC_CLS_NETIF		/* class */
} };

static inline void
ie_registerdev(struct isa_device *id)
{
	if(id->id_unit)
		kdc_ie[id->id_unit] = kdc_ie[0];
	kdc_ie[id->id_unit].kdc_unit = id->id_unit;
	kdc_ie[id->id_unit].kdc_isa = id;
	dev_attach(&kdc_ie[id->id_unit]);
}

/* This routine written by Charles Martin Hannum. */
int ieprobe(dvp)
     struct isa_device *dvp;
{
  int ret;

  ie_registerdev(dvp);

  ret = sl_probe(dvp);
  if(!ret) ret = el_probe(dvp);
  if(!ret) ret = ni_probe(dvp);
  return(ret);
}

static int sl_probe(dvp)
	struct isa_device *dvp;
{
  int unit = dvp->id_unit;
  u_char c;

  ie_softc[unit].port = dvp->id_iobase;
  ie_softc[unit].iomembot = dvp->id_maddr;
  ie_softc[unit].iomem = 0;
  ie_softc[unit].bus_use = 0;

  c = inb(PORT + IEATT_REVISION);
  switch(SL_BOARD(c)) {
  case SL10_BOARD:
    ie_softc[unit].hard_type = IE_STARLAN10;
    ie_softc[unit].ie_reset_586 = sl_reset_586;
    ie_softc[unit].ie_chan_attn = sl_chan_attn;
    break;
  case EN100_BOARD:
    ie_softc[unit].hard_type = IE_EN100;
    ie_softc[unit].ie_reset_586 = sl_reset_586;
    ie_softc[unit].ie_chan_attn = sl_chan_attn;
    break;
  case SLFIBER_BOARD:
    ie_softc[unit].hard_type = IE_SLFIBER;
    ie_softc[unit].ie_reset_586 = sl_reset_586;
    ie_softc[unit].ie_chan_attn = sl_chan_attn;
    break;

    /*
     * Anything else is not recognized or cannot be used.
     */
  default:
    return 0;
  }

  ie_softc[unit].hard_vers = SL_REV(c);

  /*
   * Divine memory size on-board the card.  Ususally 16k.
   */
  find_ie_mem_size(unit);

  if(!ie_softc[unit].iosize) {
    return 0;
  }

  dvp->id_msize = ie_softc[unit].iosize;

  switch(ie_softc[unit].hard_type) {
  case IE_EN100:
  case IE_STARLAN10:
  case IE_SLFIBER:
    sl_read_ether(unit, ie_softc[unit].arpcom.ac_enaddr);
    break;

  default:
    printf("ie%d: unknown AT&T board type code %d\n", unit,
	   ie_softc[unit].hard_type);
    return 0;
  }

  return 1;
}

/* This routine written by Charles Martin Hannum. */
static int el_probe(dvp)
	struct isa_device *dvp;
{
  struct ie_softc *sc = &ie_softc[dvp->id_unit];
  u_char c;
  int i;
  u_char signature[] = "*3COM*";
  int unit = dvp->id_unit;

  sc->port = dvp->id_iobase;
  sc->iomembot = dvp->id_maddr;
  sc->bus_use = 0;

  /* Need this for part of the probe. */
  sc->ie_reset_586 = el_reset_586;
  sc->ie_chan_attn = el_chan_attn;

  /* Reset and put card in CONFIG state without changing address. */
  elink_reset();
  outb(ELINK_ID_PORT, 0x00);
  elink_idseq(ELINK_507_POLY);
  elink_idseq(ELINK_507_POLY);
  outb(ELINK_ID_PORT, 0xff);

  c = inb(PORT + IE507_MADDR);
  if(c & 0x20) {
#ifdef DEBUG
    printf("ie%d: can't map 3C507 RAM in high memory\n", unit);
#endif
    return 0;
  }

  /* go to RUN state */
  outb(ELINK_ID_PORT, 0x00);
  elink_idseq(ELINK_507_POLY);
  outb(ELINK_ID_PORT, 0x00);

  outb(PORT + IE507_CTRL, EL_CTRL_NRST);

  for (i = 0; i < 6; i++)
    if (inb(PORT + i) != signature[i])
      return 0;

  c = inb(PORT + IE507_IRQ) & 0x0f;

  if (dvp->id_irq != (1 << c)) {
    printf("ie%d: kernel configured irq %d doesn't match board configured irq %d\n",
	   unit, ffs(dvp->id_irq) - 1, c);
    return 0;
  }

  c = (inb(PORT + IE507_MADDR) & 0x1c) + 0xc0;

  if (kvtop(dvp->id_maddr) != ((int)c << 12)) {
    printf("ie%d: kernel configured maddr %lx doesn't match board configured maddr %x\n",
	   unit, kvtop(dvp->id_maddr),(int)c << 12);
    return 0;
  }

  outb(PORT + IE507_CTRL, EL_CTRL_NORMAL);

  sc->hard_type = IE_3C507;
  sc->hard_vers = 0;	/* 3C507 has no version number. */

  /*
   * Divine memory size on-board the card.
   */
  find_ie_mem_size(unit);

  if (!sc->iosize) {
    printf("ie%d: can't find shared memory\n", unit);
    outb(PORT + IE507_CTRL, EL_CTRL_NRST);
    return 0;
  }

  if(!dvp->id_msize)
    dvp->id_msize = sc->iosize;
  else if (dvp->id_msize != sc->iosize) {
    printf("ie%d: kernel configured msize %d doesn't match board configured msize %d\n",
	   unit, dvp->id_msize, sc->iosize);
    outb(PORT + IE507_CTRL, EL_CTRL_NRST);
    return 0;
  }

  sl_read_ether(unit, ie_softc[unit].arpcom.ac_enaddr);

  /* Clear the interrupt latch just in case. */
  outb(PORT + IE507_ICTRL, 1);

  return 16;
}


static int ni_probe(dvp)
	struct isa_device *dvp;
{
  int unit = dvp->id_unit;
  int boardtype, c;

  ie_softc[unit].port = dvp->id_iobase;
  ie_softc[unit].iomembot = dvp->id_maddr;
  ie_softc[unit].iomem = 0;
  ie_softc[unit].bus_use = 1;

  boardtype = inb(PORT + IEATT_REVISION);
  c = inb(PORT + IEATT_REVISION + 1);
  boardtype = boardtype + (c << 8);
  switch(boardtype) {
  case 0x5500:		/* This is the magic cookie for the NI5210 */
    ie_softc[unit].hard_type = IE_NI5210;
    ie_softc[unit].ie_reset_586 = sl_reset_586;
    ie_softc[unit].ie_chan_attn = sl_chan_attn;
    break;

    /*
     * Anything else is not recognized or cannot be used.
     */
  default:
    return 0;
  }

  ie_softc[unit].hard_vers = 0;

  /*
   * Divine memory size on-board the card.  Either 8 or 16k.
   */
  find_ie_mem_size(unit);

  if(!ie_softc[unit].iosize) {
    return 0;
  }

  if(!dvp->id_msize)
    dvp->id_msize = ie_softc[unit].iosize;
  else if (dvp->id_msize != ie_softc[unit].iosize) {
    printf("ie%d: kernel configured msize %d doesn't match board configured msize %d\n",
	   unit, dvp->id_msize, ie_softc[unit].iosize);
    return 0;
  }

  sl_read_ether(unit, ie_softc[unit].arpcom.ac_enaddr);

  return 8;

}


/*
 * Taken almost exactly from Bill's if_is.c, then modified beyond recognition.
 */
int
ieattach(dvp)
     struct isa_device *dvp;
{
  int unit = dvp->id_unit;
  struct ie_softc *ie = &ie_softc[unit];
  struct ifnet *ifp = &ie->arpcom.ac_if;

  ifp->if_unit = unit;
  ifp->if_name = iedriver.name;
  ifp->if_mtu = ETHERMTU;
  printf(" <%s R%d> ethernet address %s\n",
	 ie_hardware_names[ie_softc[unit].hard_type],
	 ie_softc[unit].hard_vers + 1,
	 ether_sprintf(ie->arpcom.ac_enaddr));

  ifp->if_flags = IFF_BROADCAST | IFF_SIMPLEX | IFF_NOTRAILERS;
  ifp->if_flags  |= IFF_MULTICAST;

  ifp->if_init = ieinit;
  ifp->if_output = ether_output;
  ifp->if_start = iestart;
  ifp->if_ioctl = ieioctl;
  ifp->if_reset = iereset;
  ifp->if_type = IFT_ETHER;
  ifp->if_addrlen = 6;
  ifp->if_hdrlen = 14;

#if NBPFILTER > 0
  bpfattach(&ie_softc[unit].ie_bpf, ifp, DLT_EN10MB,
	    sizeof(struct ether_header));
#endif

  if_attach(ifp);
  kdc_ie[unit].kdc_description = ie_hardware_names[ie_softc[unit].hard_type];

  {
    struct ifaddr *ifa = ifp->if_addrlist;
    struct sockaddr_dl *sdl;
    while(ifa && ifa->ifa_addr && ifa->ifa_addr->sa_family != AF_LINK)
      ifa = ifa->ifa_next;

    if(!ifa || !ifa->ifa_addr) return 1;

    /* Provide our ether address to the higher layers */
    sdl = (struct sockaddr_dl *)ifa->ifa_addr;
    sdl->sdl_type = IFT_ETHER;
    sdl->sdl_alen = 6;
    sdl->sdl_slen = 0;
    bcopy(ie->arpcom.ac_enaddr, LLADDR(sdl), 6);
    return 1;
  }
}

/*
 * What to do upon receipt of an interrupt.
 */
int ieintr(unit)
     int unit;
{
  register struct ie_softc *ie = &ie_softc[unit];
  register u_short status;

  status = ie->scb->ie_status;

  /* This if statement written by Charles Martin Hannum. */
  if ((status & IE_ST_WHENCE) == 0) {
    /* Clear the interrupt latch on the 3C507. */
    if (ie->hard_type == IE_3C507 &&
	(inb(PORT + IE507_CTRL) & EL_CTRL_INTL))
      outb(PORT + IE507_ICTRL, 1);
  }

loop:
  if(status & (IE_ST_RECV | IE_ST_RNR)) {
#ifdef DEBUG
    in_ierint++;
    if(ie_debug & IED_RINT)
      printf("ie%d: rint\n", unit);
#endif
    ierint(unit, ie);
#ifdef DEBUG
    in_ierint--;
#endif
  }

  if(status & IE_ST_DONE) {
#ifdef DEBUG
    in_ietint++;
    if(ie_debug & IED_TINT)
      printf("ie%d: tint\n", unit);
#endif
    ietint(unit, ie);
#ifdef DEBUG
    in_ietint--;
#endif
  }

  if(status & IE_ST_RNR) {
#ifdef DEBUG
    if(ie_debug & IED_RNR)
      printf("ie%d: rnr\n", unit);
#endif
    iernr(unit, ie);
  }

#ifdef DEBUG
  if((status & IE_ST_ALLDONE)
     && (ie_debug & IED_CNA))
    printf("ie%d: cna\n", unit);
#endif

  /* Don't ack interrupts which we didn't receive */
  ie_ack(ie->scb, IE_ST_WHENCE & status, unit, ie->ie_chan_attn);

  if((status = ie->scb->ie_status) & IE_ST_WHENCE)
    goto loop;

  /* This comment and if statement written by Charles Martin Hannum. */
  /* Clear the interrupt latch on the 3C507. */
  if (ie->hard_type == IE_3C507)
    outb(PORT + IE507_ICTRL, 1);

  return unit;
}

/*
 * Process a received-frame interrupt.
 */
static int ierint(unit, ie)
     int unit;
     struct ie_softc *ie;
{
  int i, status;
  static int timesthru = 1024;

  i = ie->rfhead;
  while(1) {
    status = ie->rframes[i]->ie_fd_status;

    if((status & IE_FD_COMPLETE) && (status & IE_FD_OK)) {
      ie->arpcom.ac_if.if_ipackets++;
      if(!--timesthru) {
	ie->arpcom.ac_if.if_ierrors += ie->scb->ie_err_crc + ie->scb->ie_err_align +
	  ie->scb->ie_err_resource + ie->scb->ie_err_overrun;
	ie->scb->ie_err_crc = 0;
	ie->scb->ie_err_align = 0;
	ie->scb->ie_err_resource = 0;
	ie->scb->ie_err_overrun = 0;
	timesthru = 1024;
      }
      ie_readframe(unit, ie, i);
    } else {
      if(status & IE_FD_RNR) {
	if(!(ie->scb->ie_status & IE_RU_READY)) {
	  ie->rframes[0]->ie_fd_next = MK_16(MEM, ie->rbuffs[0]);
	  ie->scb->ie_recv_list = MK_16(MEM, ie->rframes[0]);
	  command_and_wait(unit, IE_RU_START, 0, 0);
	}
      }
      break;
    }
    i = (i + 1) % NFRAMES;
  }
  return 0;
}

/*
 * Process a command-complete interrupt.  These are only generated by
 * the transmission of frames.  This routine is deceptively simple, since
 * most of the real work is done by iestart().
 */
static int ietint(unit, ie)
     int unit;
     struct ie_softc *ie;
{
  int status;
  int i;

  ie->arpcom.ac_if.if_timer = 0;
  ie->arpcom.ac_if.if_flags &= ~IFF_OACTIVE;

  for(i = 0; i < ie->xmit_count; i++) {
    status = ie->xmit_cmds[i]->ie_xmit_status;

    if(status & IE_XS_LATECOLL) {
      printf("ie%d: late collision\n", unit);
      ie->arpcom.ac_if.if_collisions++;
      ie->arpcom.ac_if.if_oerrors++;
    } else if(status & IE_XS_NOCARRIER) {
      printf("ie%d: no carrier\n", unit);
      ie->arpcom.ac_if.if_oerrors++;
    } else if(status & IE_XS_LOSTCTS) {
      printf("ie%d: lost CTS\n", unit);
      ie->arpcom.ac_if.if_oerrors++;
    } else if(status & IE_XS_UNDERRUN) {
      printf("ie%d: DMA underrun\n", unit);
      ie->arpcom.ac_if.if_oerrors++;
    } else if(status & IE_XS_EXCMAX) {
      printf("ie%d: too many collisions\n", unit);
      ie->arpcom.ac_if.if_collisions += 16;
      ie->arpcom.ac_if.if_oerrors++;
    } else {
      ie->arpcom.ac_if.if_opackets++;
      ie->arpcom.ac_if.if_collisions += status & IE_XS_MAXCOLL;
    }
  }
  ie->xmit_count = 0;

  /*
   * If multicast addresses were added or deleted while we were transmitting,
   * ie_mc_reset() set the want_mcsetup flag indicating that we should do it.
   */
  if(ie->want_mcsetup) {
    mc_setup(unit, (caddr_t)ie->xmit_cbuffs[0], ie->scb);
    ie->want_mcsetup = 0;
  }

  /* Wish I knew why this seems to be necessary... */
  ie->xmit_cmds[0]->ie_xmit_status |= IE_STAT_COMPL;

  iestart(&ie->arpcom.ac_if);
  return 0;			/* shouldn't be necessary */
}

/*
 * Process a receiver-not-ready interrupt.  I believe that we get these
 * when there aren't enough buffers to go around.  For now (FIXME), we
 * just restart the receiver, and hope everything's ok.
 */
static int iernr(unit, ie)
     int unit;
     struct ie_softc *ie;
{
#ifdef doesnt_work
  setup_rfa((caddr_t)ie->rframes[0], ie);

  ie->scb->ie_recv_list = MK_16(MEM, ie_softc[unit].rframes[0]);
  command_and_wait(unit, IE_RU_START, 0, 0);
#else
  /* This doesn't work either, but it doesn't hang either. */
  command_and_wait(unit, IE_RU_DISABLE, 0, 0); /* just in case */
  setup_rfa((caddr_t)ie->rframes[0], ie); /* ignore cast-qual */

  ie->scb->ie_recv_list = MK_16(MEM, ie_softc[unit].rframes[0]);
  command_and_wait(unit, IE_RU_START, 0, 0); /* was ENABLE */

#endif
  ie_ack(ie->scb, IE_ST_WHENCE, unit, ie->ie_chan_attn);

  ie->arpcom.ac_if.if_ierrors++;
  return 0;
}

/*
 * Compare two Ether/802 addresses for equality, inlined and
 * unrolled for speed.  I'd love to have an inline assembler
 * version of this...
 */
static inline int ether_equal(u_char *one, u_char *two) {
  if(one[0] != two[0]) return 0;
  if(one[1] != two[1]) return 0;
  if(one[2] != two[2]) return 0;
  if(one[3] != two[3]) return 0;
  if(one[4] != two[4]) return 0;
  if(one[5] != two[5]) return 0;
  return 1;
}

/*
 * Check for a valid address.  to_bpf is filled in with one of the following:
 *   0 -> BPF doesn't get this packet
 *   1 -> BPF does get this packet
 *   2 -> BPF does get this packet, but we don't
 * Return value is true if the packet is for us, and false otherwise.
 *
 * This routine is a mess, but it's also critical that it be as fast
 * as possible.  It could be made cleaner if we can assume that the
 * only client which will fiddle with IFF_PROMISC is BPF.  This is
 * probably a good assumption, but we do not make it here.  (Yet.)
 */
static inline int check_eh(struct ie_softc *ie,
			   struct ether_header *eh,
			   int *to_bpf) {
  int i;

  switch(ie->promisc) {
  case IFF_ALLMULTI:
    /*
     * Receiving all multicasts, but no unicasts except those destined for us.
     */
#if NBPFILTER > 0
    *to_bpf = (ie->ie_bpf != 0); /* BPF gets this packet if anybody cares */
#endif
    if(eh->ether_dhost[0] & 1) {
      return 1;
    }
    if(ether_equal(eh->ether_dhost, ie->arpcom.ac_enaddr)) return 1;
    return 0;

  case IFF_PROMISC:
    /*
     * Receiving all packets.  These need to be passed on to BPF.
     */
#if NBPFILTER > 0
    *to_bpf = (ie->ie_bpf != 0);
#endif
    /* If for us, accept and hand up to BPF */
    if(ether_equal(eh->ether_dhost, ie->arpcom.ac_enaddr)) return 1;

#if NBPFILTER > 0
    if(*to_bpf) *to_bpf = 2; /* we don't need to see it */
#endif

    /*
     * Not a multicast, so BPF wants to see it but we don't.
     */
    if(!(eh->ether_dhost[0] & 1)) return 1;

    /*
     * If it's one of our multicast groups, accept it and pass it
     * up.
     */
    for(i = 0; i < ie->mcast_count; i++) {
      if(ether_equal(eh->ether_dhost, (u_char *)&ie->mcast_addrs[i])) {
#if NBPFILTER > 0
	if(*to_bpf) *to_bpf = 1;
#endif
	return 1;
      }
    }
    return 1;

  case IFF_ALLMULTI | IFF_PROMISC:
    /*
     * Acting as a multicast router, and BPF running at the same time.
     * Whew!  (Hope this is a fast machine...)
     */
#if NBPFILTER > 0
    *to_bpf = (ie->ie_bpf != 0);
#endif
    /* We want to see multicasts. */
    if(eh->ether_dhost[0] & 1) return 1;

    /* We want to see our own packets */
    if(ether_equal(eh->ether_dhost, ie->arpcom.ac_enaddr)) return 1;

    /* Anything else goes to BPF but nothing else. */
#if NBPFILTER > 0
    if(*to_bpf) *to_bpf = 2;
#endif
    return 1;

  default:
    /*
     * Only accept unicast packets destined for us, or multicasts
     * for groups that we belong to.  For now, we assume that the
     * '586 will only return packets that we asked it for.  This
     * isn't strictly true (it uses hashing for the multicast filter),
     * but it will do in this case, and we want to get out of here
     * as quickly as possible.
     */
#if NBPFILTER > 0
    *to_bpf = (ie->ie_bpf != 0);
#endif
    return 1;
  }
  return 0;
}

/*
 * We want to isolate the bits that have meaning...  This assumes that
 * IE_RBUF_SIZE is an even power of two.  If somehow the act_len exceeds
 * the size of the buffer, then we are screwed anyway.
 */
static inline int ie_buflen(struct ie_softc *ie, int head) {
  return (ie->rbuffs[head]->ie_rbd_actual
	  & (IE_RBUF_SIZE | (IE_RBUF_SIZE - 1)));
}

static inline int ie_packet_len(int unit, struct ie_softc *ie) {
  int i;
  int head = ie->rbhead;
  int acc = 0;

  do {
    if(!(ie->rbuffs[ie->rbhead]->ie_rbd_actual & IE_RBD_USED)) {
#ifdef DEBUG
      print_rbd(ie->rbuffs[ie->rbhead]);
#endif
      log(LOG_ERR, "ie%d: receive descriptors out of sync at %d\n",
	  unit, ie->rbhead);
      iereset(unit);
      return -1;
    }

    i = ie->rbuffs[head]->ie_rbd_actual & IE_RBD_LAST;

    acc += ie_buflen(ie, head);
    head = (head + 1) % NBUFFS;
  } while(!i);

  return acc;
}

/*
 * Read data off the interface, and turn it into an mbuf chain.
 *
 * This code is DRAMATICALLY different from the previous version; this
 * version tries to allocate the entire mbuf chain up front, given the
 * length of the data available.  This enables us to allocate mbuf
 * clusters in many situations where before we would have had a long
 * chain of partially-full mbufs.  This should help to speed up the
 * operation considerably.  (Provided that it works, of course.)
 */
static inline int ieget(unit, ie, mp, ehp, to_bpf)
     int unit;
     struct ie_softc *ie;
     struct mbuf **mp;
     struct ether_header *ehp;
     int *to_bpf;
{
  struct mbuf *m, *top, **mymp;
  int i;
  int offset;
  int totlen, resid;
  int thismboff;
  int head;

  totlen = ie_packet_len(unit, ie);
  if(totlen <= 0) return -1;

  i = ie->rbhead;

  /*
   * Snarf the Ethernet header.
   */
  bcopy((caddr_t)ie->cbuffs[i], (caddr_t)ehp, sizeof *ehp);
  /* ignore cast-qual warning here */

  /*
   * As quickly as possible, check if this packet is for us.
   * If not, don't waste a single cycle copying the rest of the
   * packet in.
   * This is only a consideration when FILTER is defined; i.e., when
   * we are either running BPF or doing multicasting.
   */
  if(!check_eh(ie, ehp, to_bpf)) {
    ie_drop_packet_buffer(unit, ie);
    ie->arpcom.ac_if.if_ierrors--; /* just this case, it's not an error */
    return -1;
  }
  totlen -= (offset = sizeof *ehp);

  MGETHDR(*mp, M_DONTWAIT, MT_DATA);
  if(!*mp) {
    ie_drop_packet_buffer(unit, ie);
    return -1;
  }

  m = *mp;
  m->m_pkthdr.rcvif = &ie->arpcom.ac_if;
  m->m_len = MHLEN;
  resid = m->m_pkthdr.len = totlen;
  top = 0;
  mymp = &top;

  /*
   * This loop goes through and allocates mbufs for all the data we will
   * be copying in.  It does not actually do the copying yet.
   */
  do {				/* while(resid > 0) */
    /*
     * Try to allocate an mbuf to hold the data that we have.  If we
     * already allocated one, just get another one and stick it on the
     * end (eventually).  If we don't already have one, try to allocate
     * an mbuf cluster big enough to hold the whole packet, if we think it's
     * reasonable, or a single mbuf which may or may not be big enough.
     * Got that?
     */
    if(top) {
      MGET(m, M_DONTWAIT, MT_DATA);
      if(!m) {
	m_freem(top);
	ie_drop_packet_buffer(unit, ie);
	return -1;
      }
      m->m_len = MLEN;
    }

    if(resid >= MINCLSIZE) {
      MCLGET(m, M_DONTWAIT);
      if(m->m_flags & M_EXT)
	m->m_len = min(resid, MCLBYTES);
    } else {
      if(resid < m->m_len) {
	if(!top && resid + max_linkhdr <= m->m_len)
	  m->m_data += max_linkhdr;
	m->m_len = resid;
      }
    }
    resid -= m->m_len;
    *mymp = m;
    mymp = &m->m_next;
  } while(resid > 0);

  resid = totlen;
  m = top;
  thismboff = 0;
  head = ie->rbhead;

  /*
   * Now we take the mbuf chain (hopefully only one mbuf most of the
   * time) and stuff the data into it.  There are no possible failures
   * at or after this point.
   */
  while(resid > 0) {		/* while there's stuff left */
    int thislen = ie_buflen(ie, head) - offset;

    /*
     * If too much data for the current mbuf, then fill the current one
     * up, go to the next one, and try again.
     */
    if(thislen > m->m_len - thismboff) {
      int newlen = m->m_len - thismboff;
      bcopy((caddr_t)(ie->cbuffs[head] + offset),
	    mtod(m, caddr_t) + thismboff, (unsigned)newlen);
      /* ignore cast-qual warning */
      m = m->m_next;
      thismboff = 0;		/* new mbuf, so no offset */
      offset += newlen;		/* we are now this far into the packet */
      resid -= newlen;		/* so there is this much left to get */
      continue;
    }

    /*
     * If there is more than enough space in the mbuf to hold the
     * contents of this buffer, copy everything in, advance pointers,
     * and so on.
     */
    if(thislen < m->m_len - thismboff) {
      bcopy((caddr_t)(ie->cbuffs[head] + offset), /* ignore warning */
	    mtod(m, caddr_t) + thismboff, (unsigned)thislen);
      thismboff += thislen;	/* we are this far into the mbuf */
      resid -= thislen;		/* and this much is left */
      goto nextbuf;
    }

    /*
     * Otherwise, there is exactly enough space to put this buffer's
     * contents into the current mbuf.  Do the combination of the above
     * actions.
     */
    bcopy((caddr_t)(ie->cbuffs[head] + offset),	/* ignore warning */
	  mtod(m, caddr_t) + thismboff, (unsigned)thislen);
    m = m->m_next;
    thismboff = 0;		/* new mbuf, start at the beginning */
    resid -= thislen;		/* and we are this far through */

    /*
     * Advance all the pointers.  We can get here from either of the
     * last two cases, but never the first.
     */
nextbuf:
      offset = 0;
      ie->rbuffs[head]->ie_rbd_actual = 0;
      ie->rbuffs[head]->ie_rbd_length |= IE_RBD_LAST;
      ie->rbhead = head = (head + 1) % NBUFFS;
      ie->rbuffs[ie->rbtail]->ie_rbd_length &= ~IE_RBD_LAST;
      ie->rbtail = (ie->rbtail + 1) % NBUFFS;
  }

  /*
   * Unless something changed strangely while we were doing the copy,
   * we have now copied everything in from the shared memory.
   * This means that we are done.
   */
  return 0;
}

/*
 * Read frame NUM from unit UNIT (pre-cached as IE).
 *
 * This routine reads the RFD at NUM, and copies in the buffers from
 * the list of RBD, then rotates the RBD and RFD lists so that the receiver
 * doesn't start complaining.  Trailers are DROPPED---there's no point
 * in wasting time on confusing code to deal with them.  Hopefully,
 * this machine will never ARP for trailers anyway.
 */
static void ie_readframe(unit, ie, num)
     int unit;
     struct ie_softc *ie;
     int num;			/* frame number to read */
{
  struct ie_recv_frame_desc rfd;
  struct mbuf *m = 0;
  struct ether_header eh;
#if NBPFILTER > 0
  int bpf_gets_it = 0;
#endif

  bcopy((caddr_t)(ie->rframes[num]), &rfd, sizeof(struct ie_recv_frame_desc));

  /* Immediately advance the RFD list, since we we have copied ours now. */
  ie->rframes[num]->ie_fd_status = 0;
  ie->rframes[num]->ie_fd_last |= IE_FD_LAST;
  ie->rframes[ie->rftail]->ie_fd_last &= ~IE_FD_LAST;
  ie->rftail = (ie->rftail + 1) % NFRAMES;
  ie->rfhead = (ie->rfhead + 1) % NFRAMES;

  if(rfd.ie_fd_status & IE_FD_OK) {
    if(
#if NBPFILTER > 0
       ieget(unit, ie, &m, &eh, &bpf_gets_it)
#else
       ieget(unit, ie, &m, &eh, (int *)0)
#endif
       ) {
      ie->arpcom.ac_if.if_ierrors++;	/* this counts as an error */
      return;
    }
  }

#ifdef DEBUG
  if(ie_debug & IED_READFRAME) {
    printf("ie%d: frame from ether %s type %x\n", unit,
	   ether_sprintf(eh.ether_shost), (unsigned)eh.ether_type);
  }
  if(ntohs(eh.ether_type) > ETHERTYPE_TRAIL
     && ntohs(eh.ether_type) < (ETHERTYPE_TRAIL + ETHERTYPE_NTRAILER))
    printf("received trailer!\n");
#endif

  if(!m) return;

  if(last_not_for_us) {
    m_freem(last_not_for_us);
    last_not_for_us = 0;
  }

#if NBPFILTER > 0
  /*
   * Check for a BPF filter; if so, hand it up.
   * Note that we have to stick an extra mbuf up front, because
   * bpf_mtap expects to have the ether header at the front.
   * It doesn't matter that this results in an ill-formatted mbuf chain,
   * since BPF just looks at the data.  (It doesn't try to free the mbuf,
   * tho' it will make a copy for tcpdump.)
   */
  if(bpf_gets_it) {
    struct mbuf m0;
    m0.m_len = sizeof eh;
    m0.m_data = (caddr_t)&eh;
    m0.m_next = m;

    /* Pass it up */
    bpf_mtap(ie->ie_bpf, &m0);
  }
  /*
   * A signal passed up from the filtering code indicating that the
   * packet is intended for BPF but not for the protocol machinery.
   * We can save a few cycles by not handing it off to them.
   */
  if(bpf_gets_it == 2) {
    last_not_for_us = m;
    return;
  }
#endif /* NBPFILTER > 0 */
  /*
   * In here there used to be code to check destination addresses upon
   * receipt of a packet.  We have deleted that code, and replaced it
   * with code to check the address much earlier in the cycle, before
   * copying the data in; this saves us valuable cycles when operating
   * as a multicast router or when using BPF.
   */

  /*
   * Finally pass this packet up to higher layers.
   */
  ether_input(&ie->arpcom.ac_if, &eh, m);
}

static void ie_drop_packet_buffer(int unit, struct ie_softc *ie) {
  int i;

  do {
    /*
     * This means we are somehow out of sync.  So, we reset the
     * adapter.
     */
    if(!(ie->rbuffs[ie->rbhead]->ie_rbd_actual & IE_RBD_USED)) {
#ifdef DEBUG
      print_rbd(ie->rbuffs[ie->rbhead]);
#endif
      log(LOG_ERR, "ie%d: receive descriptors out of sync at %d\n",
	  unit, ie->rbhead);
      iereset(unit);
      return;
    }

    i = ie->rbuffs[ie->rbhead]->ie_rbd_actual & IE_RBD_LAST;

    ie->rbuffs[ie->rbhead]->ie_rbd_length |= IE_RBD_LAST;
    ie->rbuffs[ie->rbhead]->ie_rbd_actual = 0;
    ie->rbhead = (ie->rbhead + 1) % NBUFFS;
    ie->rbuffs[ie->rbtail]->ie_rbd_length &= ~IE_RBD_LAST;
    ie->rbtail = (ie->rbtail + 1) % NBUFFS;
  } while(!i);
}


/*
 * Start transmission on an interface.
 */
static void
iestart(ifp)
	struct ifnet *ifp;
{
  struct ie_softc *ie = &ie_softc[ifp->if_unit];
  struct mbuf *m0, *m;
  unsigned char *buffer;
  u_short len;
  /* This is not really volatile, in this routine, but it makes gcc happy. */
  volatile u_short *bptr = &ie->scb->ie_command_list;

  if(!(ifp->if_flags & IFF_RUNNING))
    return;
  if(ifp->if_flags & IFF_OACTIVE)
    return;

  do {
    IF_DEQUEUE(&ie->arpcom.ac_if.if_snd, m);
    if(!m)
      break;

    buffer = ie->xmit_cbuffs[ie->xmit_count];
    len = 0;

    for(m0 = m; m && len < IE_BUF_LEN; m = m->m_next) {
      bcopy(mtod(m, caddr_t), buffer, m->m_len);
      buffer += m->m_len;
      len += m->m_len;
    }

    m_freem(m0);
    len = max(len, ETHERMINLEN);

#if NBPFILTER > 0
    /*
     * See if bpf is listening on this interface, let it see the packet
     * before we commit it to the wire.
     */
    if(ie->ie_bpf)
      bpf_tap(ie->ie_bpf, ie->xmit_cbuffs[ie->xmit_count], len);
#endif

    ie->xmit_buffs[ie->xmit_count]->ie_xmit_flags = IE_XMIT_LAST | len;
    ie->xmit_buffs[ie->xmit_count]->ie_xmit_next = 0xffff;
    ie->xmit_buffs[ie->xmit_count]->ie_xmit_buf =
      MK_24(ie->iomem, ie->xmit_cbuffs[ie->xmit_count]);

    ie->xmit_cmds[ie->xmit_count]->com.ie_cmd_cmd = IE_CMD_XMIT;
    ie->xmit_cmds[ie->xmit_count]->ie_xmit_status = 0;
    ie->xmit_cmds[ie->xmit_count]->ie_xmit_desc =
      MK_16(ie->iomem, ie->xmit_buffs[ie->xmit_count]);

    *bptr = MK_16(ie->iomem, ie->xmit_cmds[ie->xmit_count]);
    bptr = &ie->xmit_cmds[ie->xmit_count]->com.ie_cmd_link;
    ie->xmit_count++;
  } while(ie->xmit_count < 2);

  /*
   * If we queued up anything for transmission, send it.
   */
  if(ie->xmit_count) {
    ie->xmit_cmds[ie->xmit_count - 1]->com.ie_cmd_cmd |=
      IE_CMD_LAST | IE_CMD_INTR;

    /*
     * By passing the command pointer as a null, we tell
     * command_and_wait() to pretend that this isn't an action
     * command.  I wish I understood what was happening here.
     */
    command_and_wait(ifp->if_unit, IE_CU_START, 0, 0);
    ifp->if_flags |= IFF_OACTIVE;
  }

  return;
}

/*
 * Check to see if there's an 82586 out there.
 */
int check_ie_present(unit, where, size)
     int unit;
     caddr_t where;
     unsigned size;
{
  volatile struct ie_sys_conf_ptr *scp;
  volatile struct ie_int_sys_conf_ptr *iscp;
  volatile struct ie_sys_ctl_block *scb;
  u_long realbase;
  int s;

  s = splimp();

  realbase = (u_long)where + size - (1 << 24);

  scp = (volatile struct ie_sys_conf_ptr *)(realbase + IE_SCP_ADDR);
  bzero((char *)scp, sizeof *scp); /* ignore cast-qual */

  /*
   * First we put the ISCP at the bottom of memory; this tests to make
   * sure that our idea of the size of memory is the same as the controller's.
   * This is NOT where the ISCP will be in normal operation.
   */
  iscp = (volatile struct ie_int_sys_conf_ptr *)where;
  bzero((char *)iscp, sizeof *iscp); /* ignore cast-qual */

  scb = (volatile struct ie_sys_ctl_block *)where;
  bzero((char *)scb, sizeof *scb); /* ignore cast-qual */

  scp->ie_bus_use = ie_softc[unit].bus_use;	/* 8-bit or 16-bit */
  scp->ie_iscp_ptr = (caddr_t)((volatile caddr_t)iscp - /* ignore cast-qual */
			       (volatile caddr_t)realbase);

  iscp->ie_busy = 1;
  iscp->ie_scb_offset = MK_16(realbase, scb) + 256;

  (*ie_softc[unit].ie_reset_586)(unit);
  (*ie_softc[unit].ie_chan_attn)(unit);

  DELAY(100);			/* wait a while... */

  if(iscp->ie_busy) {
    splx(s);
    return 0;
  }

  /*
   * Now relocate the ISCP to its real home, and reset the controller
   * again.
   */
  iscp = (void *)Align((caddr_t)(realbase + IE_SCP_ADDR -
				 sizeof(struct ie_int_sys_conf_ptr)));
  bzero((char *)iscp, sizeof *iscp); /* ignore cast-qual */

  scp->ie_iscp_ptr = (caddr_t)((caddr_t)iscp - (caddr_t)realbase);
				/* ignore cast-qual */

  iscp->ie_busy = 1;
  iscp->ie_scb_offset = MK_16(realbase, scb);

  (*ie_softc[unit].ie_reset_586)(unit);
  (*ie_softc[unit].ie_chan_attn)(unit);

  DELAY(100);

  if(iscp->ie_busy) {
    splx(s);
    return 0;
  }

  ie_softc[unit].iosize = size;
  ie_softc[unit].iomem = (caddr_t)realbase;

  ie_softc[unit].iscp = iscp;
  ie_softc[unit].scb = scb;

  /*
   * Acknowledge any interrupts we may have caused...
   */
  ie_ack(scb, IE_ST_WHENCE, unit, ie_softc[unit].ie_chan_attn);
  splx(s);

  return 1;
}

/*
 * Divine the memory size of ie board UNIT.
 * Better hope there's nothing important hiding just below the ie card...
 */
static void find_ie_mem_size(unit)
     int unit;
{
  unsigned size;

  ie_softc[unit].iosize = 0;

  for(size = 65536; size >= 8192; size -= 8192) {
    if(check_ie_present(unit, ie_softc[unit].iomembot, size)) {
      return;
    }
  }

  return;
}

void el_reset_586(unit)
	int unit;
{
  outb(PORT + IE507_CTRL, EL_CTRL_RESET);
  DELAY(100);
  outb(PORT + IE507_CTRL, EL_CTRL_NORMAL);
  DELAY(100);
}

void sl_reset_586(unit)
     int unit;
{
  outb(PORT + IEATT_RESET, 0);
}

void el_chan_attn(unit)
     int unit;
{
  outb(PORT + IE507_ATTN, 1);
}

void sl_chan_attn(unit)
     int unit;
{
  outb(PORT + IEATT_ATTN, 0);
}

void sl_read_ether(unit, addr)
     int unit;
     unsigned char addr[6];
{
  int i;

  for(i = 0; i < 6; i++)
    addr[i] = inb(PORT + i);
}


static void
iereset(unit)
	int unit;
{
  int s = splimp();

  if(unit >= NIE) {
    splx(s);
    return;
  }

  printf("ie%d: reset\n", unit);
  ie_softc[unit].arpcom.ac_if.if_flags &= ~IFF_UP;
  ieioctl(&ie_softc[unit].arpcom.ac_if, SIOCSIFFLAGS, 0);

  /*
   * Stop i82586 dead in its tracks.
   */
  if(command_and_wait(unit, IE_RU_ABORT | IE_CU_ABORT, 0, 0))
    printf("ie%d: abort commands timed out\n", unit);

  if(command_and_wait(unit, IE_RU_DISABLE | IE_CU_STOP, 0, 0))
    printf("ie%d: disable commands timed out\n", unit);

#ifdef notdef
  if(!check_ie_present(unit, ie_softc[unit].iomembot, ie_softc[unit].iosize))
    panic("ie disappeared!");
#endif

  ie_softc[unit].arpcom.ac_if.if_flags |= IFF_UP;
  ieioctl(&ie_softc[unit].arpcom.ac_if, SIOCSIFFLAGS, 0);

  splx(s);
  return;
}

/*
 * This is called if we time out.
 */
static void
chan_attn_timeout(rock)
	caddr_t rock;
{
  *(int *)rock = 1;
}

/*
 * Send a command to the controller and wait for it to either
 * complete or be accepted, depending on the command.  If the
 * command pointer is null, then pretend that the command is
 * not an action command.  If the command pointer is not null,
 * and the command is an action command, wait for
 * ((volatile struct ie_cmd_common *)pcmd)->ie_cmd_status & MASK
 * to become true.
 */
static int command_and_wait(unit, cmd, pcmd, mask)
     int unit;
     int cmd;
     volatile void *pcmd;
     int mask;
{
  volatile struct ie_cmd_common *cc = pcmd;
  volatile int timedout = 0;

  ie_softc[unit].scb->ie_command = (u_short)cmd;

  if(IE_ACTION_COMMAND(cmd) && pcmd) {
    (*ie_softc[unit].ie_chan_attn)(unit);

    /*
     * According to the packet driver, the minimum timeout should be
     * .369 seconds, which we round up to .37.
     */
    timeout(chan_attn_timeout, (caddr_t)&timedout, 37 * hz / 100);
				/* ignore cast-qual */

    /*
     * Now spin-lock waiting for status.  This is not a very nice
     * thing to do, but I haven't figured out how, or indeed if, we
     * can put the process waiting for action to sleep.  (We may
     * be getting called through some other timeout running in the
     * kernel.)
     */
    while(1) {
      if((cc->ie_cmd_status & mask) || timedout)
	break;
    }

    untimeout(chan_attn_timeout, (caddr_t)&timedout);
				/* ignore cast-qual */

    return timedout;
  } else {

    /*
     * Otherwise, just wait for the command to be accepted.
     */
    (*ie_softc[unit].ie_chan_attn)(unit);

    while(ie_softc[unit].scb->ie_command)
      ;				/* spin lock */

    return 0;
  }
}

/*
 * Run the time-domain reflectometer...
 */
static void run_tdr(unit, cmd)
     int unit;
     struct ie_tdr_cmd *cmd;
{
  int result;

  cmd->com.ie_cmd_status = 0;
  cmd->com.ie_cmd_cmd = IE_CMD_TDR | IE_CMD_LAST;
  cmd->com.ie_cmd_link = 0xffff;
  cmd->ie_tdr_time = 0;

  ie_softc[unit].scb->ie_command_list = MK_16(MEM, cmd);
  cmd->ie_tdr_time = 0;

  if(command_and_wait(unit, IE_CU_START, cmd, IE_STAT_COMPL))
    result = 0x2000;
  else
    result = cmd->ie_tdr_time;

  ie_ack(ie_softc[unit].scb, IE_ST_WHENCE, unit,
	 ie_softc[unit].ie_chan_attn);

  if(result & IE_TDR_SUCCESS)
    return;

  if(result & IE_TDR_XCVR) {
    printf("ie%d: transceiver problem\n", unit);
  } else if(result & IE_TDR_OPEN) {
    printf("ie%d: TDR detected an open %d clocks away\n", unit,
	   result & IE_TDR_TIME);
  } else if(result & IE_TDR_SHORT) {
    printf("ie%d: TDR detected a short %d clocks away\n", unit,
	   result & IE_TDR_TIME);
  } else {
    printf("ie%d: TDR returned unknown status %x\n", unit, result);
  }
}

static void start_receiver(unit)
     int unit;
{
  int s = splimp();

  ie_softc[unit].scb->ie_recv_list = MK_16(MEM, ie_softc[unit].rframes[0]);
  command_and_wait(unit, IE_RU_START, 0, 0);

  ie_ack(ie_softc[unit].scb, IE_ST_WHENCE, unit, ie_softc[unit].ie_chan_attn);

  splx(s);
}

/*
 * Here is a helper routine for iernr() and ieinit().  This sets up
 * the RFA.
 */
static caddr_t setup_rfa(caddr_t ptr, struct ie_softc *ie) {
  volatile struct ie_recv_frame_desc *rfd = (void *)ptr;
  volatile struct ie_recv_buf_desc *rbd;
  int i;
  int unit = ie - &ie_softc[0];

  /* First lay them out */
  for(i = 0; i < NFRAMES; i++) {
    ie->rframes[i] = rfd;
    bzero((char *)rfd, sizeof *rfd); /* ignore cast-qual */
    rfd++;
  }

  ptr = (caddr_t)Align((caddr_t)rfd); /* ignore cast-qual */

  /* Now link them together */
  for(i = 0; i < NFRAMES; i++) {
    ie->rframes[i]->ie_fd_next =
      MK_16(MEM, ie->rframes[(i + 1) % NFRAMES]);
  }

  /* Finally, set the EOL bit on the last one. */
  ie->rframes[NFRAMES - 1]->ie_fd_last |= IE_FD_LAST;

  /*
   * Now lay out some buffers for the incoming frames.  Note that
   * we set aside a bit of slop in each buffer, to make sure that
   * we have enough space to hold a single frame in every buffer.
   */
  rbd = (void *)ptr;

  for(i = 0; i < NBUFFS; i++) {
    ie->rbuffs[i] = rbd;
    bzero((char *)rbd, sizeof *rbd); /* ignore cast-qual */
    ptr = (caddr_t)Align(ptr + sizeof *rbd);
    rbd->ie_rbd_length = IE_RBUF_SIZE;
    rbd->ie_rbd_buffer = MK_24(MEM, ptr);
    ie->cbuffs[i] =  (void *)ptr;
    ptr += IE_RBUF_SIZE;
    rbd = (void *)ptr;
  }

  /* Now link them together */
  for(i = 0; i < NBUFFS; i++) {
    ie->rbuffs[i]->ie_rbd_next = MK_16(MEM, ie->rbuffs[(i + 1) % NBUFFS]);
  }

  /* Tag EOF on the last one */
  ie->rbuffs[NBUFFS - 1]->ie_rbd_length |= IE_RBD_LAST;

  /* We use the head and tail pointers on receive to keep track of
   * the order in which RFDs and RBDs are used. */
  ie->rfhead = 0;
  ie->rftail = NFRAMES - 1;
  ie->rbhead = 0;
  ie->rbtail = NBUFFS - 1;

  ie->scb->ie_recv_list = MK_16(MEM, ie->rframes[0]);
  ie->rframes[0]->ie_fd_buf_desc = MK_16(MEM, ie->rbuffs[0]);

  ptr = Align(ptr);
  return ptr;
}

/*
 * Run the multicast setup command.
 * Call at splimp().
 */
static int mc_setup(int unit, caddr_t ptr,
		    volatile struct ie_sys_ctl_block *scb) {
  struct ie_softc *ie = &ie_softc[unit];
  volatile struct ie_mcast_cmd *cmd = (void *)ptr;

  cmd->com.ie_cmd_status = 0;
  cmd->com.ie_cmd_cmd = IE_CMD_MCAST | IE_CMD_LAST;
  cmd->com.ie_cmd_link = 0xffff;

				/* ignore cast-qual */
  bcopy((caddr_t)ie->mcast_addrs, (caddr_t)cmd->ie_mcast_addrs,
	ie->mcast_count * sizeof *ie->mcast_addrs);

  cmd->ie_mcast_bytes = ie->mcast_count * 6; /* grrr... */

  scb->ie_command_list = MK_16(MEM, cmd);
  if(command_and_wait(unit, IE_CU_START, cmd, IE_STAT_COMPL)
     || !(cmd->com.ie_cmd_status & IE_STAT_OK)) {
    printf("ie%d: multicast address setup command failed\n", unit);
    return 0;
  }
  return 1;
}

/*
 * This routine takes the environment generated by check_ie_present()
 * and adds to it all the other structures we need to operate the adapter.
 * This includes executing the CONFIGURE, IA-SETUP, and MC-SETUP commands,
 * starting the receiver unit, and clearing interrupts.
 *
 * THIS ROUTINE MUST BE CALLED AT splimp() OR HIGHER.
 */
static void
ieinit(unit)
     int unit;
{
  struct ie_softc *ie = &ie_softc[unit];
  volatile struct ie_sys_ctl_block *scb = ie->scb;
  caddr_t ptr;

  ptr = (caddr_t)Align((caddr_t)scb + sizeof *scb); /* ignore cast-qual */

  /*
   * Send the configure command first.
   */
  {
    volatile struct ie_config_cmd *cmd = (void *)ptr;

    ie_setup_config(cmd, ie->promisc, ie->hard_type == IE_STARLAN10);
    cmd->com.ie_cmd_status = 0;
    cmd->com.ie_cmd_cmd = IE_CMD_CONFIG | IE_CMD_LAST;
    cmd->com.ie_cmd_link = 0xffff;

    scb->ie_command_list = MK_16(MEM, cmd);

    if(command_and_wait(unit, IE_CU_START, cmd, IE_STAT_COMPL)
       || !(cmd->com.ie_cmd_status & IE_STAT_OK)) {
      printf("ie%d: configure command failed\n", unit);
      return;
    }
  }
  /*
   * Now send the Individual Address Setup command.
   */
  {
    volatile struct ie_iasetup_cmd *cmd = (void *)ptr;

    cmd->com.ie_cmd_status = 0;
    cmd->com.ie_cmd_cmd = IE_CMD_IASETUP | IE_CMD_LAST;
    cmd->com.ie_cmd_link = 0xffff;

    bcopy((char *)ie_softc[unit].arpcom.ac_enaddr, (char *)&cmd->ie_address,
	  sizeof cmd->ie_address); /* ignore cast-qual */

    scb->ie_command_list = MK_16(MEM, cmd);
    if(command_and_wait(unit, IE_CU_START, cmd, IE_STAT_COMPL)
       || !(cmd->com.ie_cmd_status & IE_STAT_OK)) {
      printf("ie%d: individual address setup command failed\n", unit);
      return;
    }
  }

  /*
   * Now run the time-domain reflectometer.
   */
  run_tdr(unit, (void *)ptr);

  /*
   * Acknowledge any interrupts we have generated thus far.
   */
  ie_ack(ie->scb, IE_ST_WHENCE, unit, ie->ie_chan_attn);

  /*
   * Set up the RFA.
   */
  ptr = setup_rfa(ptr, ie);

  /*
   * Finally, the transmit command and buffer are the last little bit of work.
   */
  ie->xmit_cmds[0] = (void *)ptr;
  ptr += sizeof *ie->xmit_cmds[0];
  ptr = Align(ptr);
  ie->xmit_buffs[0] = (void *)ptr;
  ptr += sizeof *ie->xmit_buffs[0];
  ptr = Align(ptr);

  /* Second transmit command */
  ie->xmit_cmds[1] = (void *)ptr;
  ptr += sizeof *ie->xmit_cmds[1];
  ptr = Align(ptr);
  ie->xmit_buffs[1] = (void *)ptr;
  ptr += sizeof *ie->xmit_buffs[1];
  ptr = Align(ptr);

  /* Both transmit buffers */
  ie->xmit_cbuffs[0] = (void *)ptr;
  ptr += IE_BUF_LEN;
  ptr = Align(ptr);
  ie->xmit_cbuffs[1] = (void *)ptr;

  bzero((caddr_t)ie->xmit_cmds[0], sizeof *ie->xmit_cmds[0]); /* ignore */
  bzero((caddr_t)ie->xmit_buffs[0], sizeof *ie->xmit_buffs[0]);	/* cast-qual */
  bzero((caddr_t)ie->xmit_cmds[1], sizeof *ie->xmit_cmds[0]); /* warnings */
  bzero((caddr_t)ie->xmit_buffs[1], sizeof *ie->xmit_buffs[0]);	/* here */

  /*
   * This must be coordinated with iestart() and ietint().
   */
  ie->xmit_cmds[0]->ie_xmit_status = IE_STAT_COMPL;

  ie->arpcom.ac_if.if_flags |= IFF_RUNNING; /* tell higher levels that we are here */
  start_receiver(unit);
  return;
}

static void ie_stop(unit)
    int unit;
{
  command_and_wait(unit, IE_RU_DISABLE, 0, 0);
}

static int
ieioctl(ifp, command, data)
	struct ifnet *ifp;
	int command;
	caddr_t data;
{
  struct ifaddr *ifa = (struct ifaddr *)data;
  struct ie_softc *ie = &ie_softc[ifp->if_unit];
  struct ifreq *ifr = (struct ifreq *) data;
  int s, error = 0;

  s = splimp();

  switch(command) {
  case SIOCSIFADDR:
    ifp->if_flags |= IFF_UP;

    switch(ifa->ifa_addr->sa_family) {
#ifdef INET
    case AF_INET:
      ieinit(ifp->if_unit);
      arp_ifinit((struct arpcom *)ifp, ifa);
      break;
#endif /* INET */

#ifdef NS
      /* This magic copied from if_is.c; I don't use XNS, so I have no
       * way of telling if this actually works or not.
       */
    case AF_NS:
      {
	struct ns_addr *ina = &(IA_SNS(ifa)->sns_addr);

	if(ns_nullhost(*ina)) {
	  ina->x_host = *(union ns_host *)(ie->arpcom.ac_enaddr);
	} else {
	  ifp->if_flags &= ~IFF_RUNNING;
	  bcopy((caddr_t)ina->x_host.c_host,
		(caddr_t)ie->arpcom.ac_enaddr,
		sizeof ie->arpcom.ac_enaddr);
	}

	ieinit(ifp->if_unit);
      }
      break;
#endif /* NS */

    default:
      ieinit(ifp->if_unit);
      break;
    }
    break;

  case SIOCSIFFLAGS:
    /*
     * Note that this device doesn't have an "all multicast" mode, so we
     * must turn on promiscuous mode and do the filtering manually.
     */
    if((ifp->if_flags & IFF_UP) == 0 &&
       (ifp->if_flags & IFF_RUNNING)) {
      ifp->if_flags &= ~IFF_RUNNING;
      ie_stop(ifp->if_unit);
    } else if((ifp->if_flags & IFF_UP) &&
	      (ifp->if_flags & IFF_RUNNING) == 0) {
      ie_softc[ifp->if_unit].promisc =
	ifp->if_flags & (IFF_PROMISC | IFF_ALLMULTI);
      ieinit(ifp->if_unit);
    } else if(ie_softc[ifp->if_unit].promisc ^
	      (ifp->if_flags & (IFF_PROMISC | IFF_ALLMULTI))) {
      ie_softc[ifp->if_unit].promisc =
	ifp->if_flags & (IFF_PROMISC | IFF_ALLMULTI);
      ieinit(ifp->if_unit);
    }
    break;

  case SIOCADDMULTI:
  case SIOCDELMULTI:
    /*
     * Update multicast listeners
     */
    error = ((command == SIOCADDMULTI)
	     ? ether_addmulti(ifr, &ie->arpcom)
	     : ether_delmulti(ifr, &ie->arpcom));

    if(error == ENETRESET) {
      /* reset multicast filtering */
      ie_mc_reset(ifp->if_unit);
      error = 0;
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

  default:
    error = EINVAL;
  }

  splx(s);
  return error;
}

static void ie_mc_reset(int unit) {
  struct ie_softc *ie = &ie_softc[unit];
  struct ether_multi *enm;
  struct ether_multistep step;

  /*
   * Step through the list of addresses.
   */
  ie->mcast_count = 0;
  ETHER_FIRST_MULTI(step, &ie->arpcom, enm);
  while(enm) {
    if(ie->mcast_count >= MAXMCAST
       || bcmp(enm->enm_addrlo, enm->enm_addrhi, 6) != 0) {
      ie->arpcom.ac_if.if_flags |= IFF_ALLMULTI;
      ieioctl(&ie->arpcom.ac_if, SIOCSIFFLAGS, (void *)0);
      goto setflag;
    }

    bcopy(enm->enm_addrlo, &(ie->mcast_addrs[ie->mcast_count]), 6);
    ie->mcast_count++;
    ETHER_NEXT_MULTI(step, enm);
  }

setflag:
  ie->want_mcsetup = 1;
}


#ifdef DEBUG
void print_rbd(volatile struct ie_recv_buf_desc *rbd) {
  printf("RBD at %08lx:\n"
	 "actual %04x, next %04x, buffer %08x\n"
	 "length %04x, mbz %04x\n",
	 (unsigned long)rbd,
	 rbd->ie_rbd_actual, rbd->ie_rbd_next, rbd->ie_rbd_buffer,
	 rbd->ie_rbd_length, rbd->mbz);
}
#endif /* DEBUG */
#endif /* NIE > 0 */

