/*-
 * Copyright (c) 1995
 *	Paul Richards.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer,
 *    verbatim and that no modifications are made prior to this
 *    point in the file.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by Paul Richards.
 * 4. The name Paul Richards may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY PAUL RICHARDS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL PAUL RICHARDS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 */

/*
#define DIAGNOSTIC
#define DEBUG
 *
 * TODO ----
 *
 * This driver will need bounce buffer support when dma'ing to mbufs above the
 * 16Mb mark.
 *
 * Check all the XXX comments -- some of them are just things I've left
 * unfinished rather than "difficult" problems that were hacked around.
 *
 * Check log settings.
 *
 * Check how all the arpcom flags get set and used.
 *
 * Re-inline and re-static all routines after debugging.
 *
 * Remember to assign iobase in SHMEM probe routines.
 *
 * Replace all occurences of LANCE-controller-card etc in prints by the name
 * strings of the appropriate type -- nifty window dressing
 *
 * Add DEPCA support -- mostly done.
 *
 */

#include "lnc.h"
#if NLNC > 0

#include "bpfilter.h"

/* Some defines that should really be in generic locations */
#define FCS_LEN 4
#define ETHER_ADDR_LEN 6
#define ETHER_HDR_LEN 14
#define MULTICAST_ADDR_LEN 8
#define ETHER_MIN_LEN 64

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/errno.h>
#include <sys/ioccom.h>
#include <sys/sockio.h>
#include <sys/mbuf.h>
#include <sys/socket.h>
#include <sys/syslog.h>
#include <sys/devconf.h>

#include <net/if.h>
#include <net/if_dl.h>
#include <net/if_types.h>
#ifdef INET
#include <netinet/in.h>
#include <netinet/in_systm.h>
#include <netinet/in_var.h>
#include <netinet/ip.h>
#include <netinet/if_ether.h>
#endif

#if NBPFILTER > 0
#include <net/bpf.h>
#include <net/bpfdesc.h>
#endif

#include <i386/isa/isa_device.h>
#include <i386/isa/if_lnc.h>

struct lnc_softc {
	struct arpcom arpcom;	            /* see ../../netinet/if_ether.h */
	struct nic_info nic;	            /* NIC specific info */
	int nrdre;
	struct host_ring_entry *recv_ring;  /* start of alloc'd mem */
	int recv_next;
	int ntdre;
	struct host_ring_entry *trans_ring;
	int trans_next;
	struct init_block *init_block;	    /* Initialisation block */
	int pending_transmits;        /* No. of transmit descriptors in use */
	int next_to_send;
	struct mbuf *mbufs;
	int mbuf_count;
	int initialised;
	int rap;
	int rdp;
	struct kern_devconf kdc;
#ifdef DEBUG
	int lnc_debug;
#endif
#if NBPFILTER > 0
	caddr_t bpf;	/* XXX bpf magic cookie - move to arpcom */
#endif
	LNCSTATS_STRUCT
} lnc_softc[NLNC];

/* Function prototypes */
int bicc_probe(struct isa_device *);
int depca_probe(struct isa_device *);
int lance_probe(int);
int ne2100_probe(struct isa_device *);
int pcnet_probe(int);
void lnc_init(int);
void lnc_start(struct ifnet *);
int  lnc_ioctl(struct ifnet *, int, caddr_t);
void lnc_watchdog(int);
int  lnc_probe(struct isa_device *);
int  lnc_attach(struct isa_device *);
void lnc_dump_state(int);

struct isa_driver lncdriver = {lnc_probe, lnc_attach, "lnc"};

static struct kern_devconf kdc_lnc = {
	0, 0, 0,		/* filled in by dev_attach */
	"lnc", 0, { MDDT_ISA, 0, "net" },
	isa_generic_externalize, 0, 0, ISA_EXTERNALLEN,
	&kdc_isa0,		/* parent */
	0,			/* parentdata */
	DC_UNCONFIGURED,
	"",
	DC_CLS_NETIF
};

static inline void
write_csr(int unit, u_short port, u_short val)
{
	outw(lnc_softc[unit].rap, port);
	outw(lnc_softc[unit].rdp, val);
}

static inline u_short
read_csr(int unit, u_short port)
{
	outw(lnc_softc[unit].rap, port);
	return (inw(lnc_softc[unit].rdp));
}

static inline void
lnc_registerdev(struct isa_device *isa_dev)
{
	struct lnc_softc *sc = &lnc_softc[isa_dev->id_unit];
	struct kern_devconf *kdc = &sc->kdc;
	*kdc = kdc_lnc;
	kdc->kdc_unit = isa_dev->id_unit;
	kdc->kdc_parentdata = isa_dev;

	switch(sc->nic.ic) {
		case LANCE:
			if (sc->nic.ident == BICC)
				kdc->kdc_description = "BICC (LANCE) Ethernet controller";
			else if (sc->nic.ident == NE2100)
				kdc->kdc_description = "NE2100 (LANCE) Ethernet controller";
			else if (sc->nic.ident == DEPCA)
				kdc->kdc_description = "DEPCA (LANCE) Ethernet controller";
			break;
		case C_LANCE:
			if (sc->nic.ident == BICC)
				kdc->kdc_description = "BICC (C-LANCE) Ethernet controller";
			else if (sc->nic.ident == NE2100)
				kdc->kdc_description = "NE2100 (C-LANCE) Ethernet controller";
			else if (sc->nic.ident == DEPCA)
				kdc->kdc_description = "DEPCA (C-LANCE) Ethernet controller";
			break;
		case PCnet_ISA:
			kdc->kdc_description = "PCnet-ISA Ethernet controller";
			break;
		case PCnet_ISAplus:
			kdc->kdc_description = "PCnet-ISA+ Ethernet controller";
			break;
		case PCnet_32:
			kdc->kdc_description = "PCnet-32 VL-Bus Ethernet controller";
			break;
		case PCnet_PCI:
			kdc->kdc_description = "PCnet-PCI Ethernet controller";
			break;
		default:
			break;
	}

	dev_attach(kdc);
}


void
lnc_setladrf(struct ifnet *ifp, struct lnc_softc *sc)
{

}

void
lnc_stop(int unit)
{
	write_csr(unit, CSR0, STOP);
}

void
lnc_reset(int unit)
{
	int s;
	lnc_init(unit);
}

void
lnc_free_mbufs(struct lnc_softc *sc)
{
	int i;

	/*
	 * We rely on other routines to keep the buff.mbuf field valid. If
	 * it's not NULL then we assume it points to an allocated mbuf.
	 */

	for (i = 0; i < NDESC(sc->nrdre); i++)
		if ((sc->recv_ring + i)->buff.mbuf)
			m_free((sc->recv_ring + i)->buff.mbuf);

	for (i = 0; i < NDESC(sc->ntdre); i++)
		if ((sc->trans_ring + i)->buff.mbuf)
			m_free((sc->trans_ring + i)->buff.mbuf);

	if (sc->mbuf_count)
		m_freem(sc->mbufs);
}

inline int
alloc_mbuf_cluster(struct lnc_softc *sc, struct host_ring_entry *desc)
{
	register struct mds *md = desc->md;
	struct mbuf *m=0;
	int addr;

	/* Try and get cluster off local cache */
	if (sc->mbuf_count) {
		sc->mbuf_count--;
		m = sc->mbufs;
		sc->mbufs = m->m_next;
		/* XXX m->m_data = m->m_ext.ext_buf;*/
	} else {
		MGET(m, M_DONTWAIT, MT_DATA);
   	if (!m)
			return(1);
      MCLGET(m, M_DONTWAIT);
   	if (!m->m_ext.ext_buf) {
			m_free(m);
			return(1);
		}
	}

	desc->buff.mbuf = m;
	addr = kvtop(m->m_data);
	md->md0 = addr;
	md->md1= ((addr >> 16) & 0xff) | OWN;
	md->md2 = -(short)(MCLBYTES - sizeof(struct pkthdr));
	md->md3 = 0;
	return(0);
}

inline struct mbuf *
chain_mbufs(struct lnc_softc *sc, int start_of_packet, int pkt_len)
{
	struct mbuf *head, *m;
	struct host_ring_entry *desc;

	/*
	 * Turn head into a pkthdr mbuf --
	 * assumes a pkthdr type mbuf was
	 * allocated to the descriptor
	 * originally.
	 */

	desc = sc->recv_ring + start_of_packet;

	head = desc->buff.mbuf;
	head->m_flags |= M_PKTHDR;

	m = head;
	do {
		m = desc->buff.mbuf;
		m->m_len = min((MCLBYTES - sizeof(struct pkthdr)), pkt_len);
		pkt_len -= m->m_len;
		if (alloc_mbuf_cluster(sc, desc))
			return((struct mbuf *)NULL);
		INC_MD_PTR(start_of_packet, sc->nrdre)
		desc = sc->recv_ring + start_of_packet;
		m->m_next = desc->buff.mbuf;
	} while (start_of_packet != sc->recv_next);

	m->m_next = 0;
	return(head);
}

inline struct mbuf *
mbuf_packet(struct lnc_softc *sc, int start_of_packet, int pkt_len)
{

	struct host_ring_entry *start;
	struct mbuf *head,*m,*m_prev;
	char *data,*mbuf_data;
	short blen;
	int amount;

	/* Get a pkthdr mbuf for the start of packet */
	MGETHDR(head, M_DONTWAIT, MT_DATA);
	if (!head) {
		LNCSTATS(drop_packet)
		return(0);
	}

	m = head;
	m->m_len = 0;
	start = sc->recv_ring + start_of_packet;
	/*blen = -(start->md->md2);*/
	blen = RECVBUFSIZE; /* XXX More PCnet-32 crap */
	data = start->buff.data;
	mbuf_data = m->m_data;

	while (start_of_packet != sc->recv_next) {
		/*
		 * If the data left fits in a single buffer then set
		 * blen to the size of the data left.
		 */
		if (pkt_len < blen)
			blen = pkt_len;

		/*
		 * amount is least of data in current ring buffer and
		 * amount of space left in current mbuf.
		 */
		amount = min(blen, M_TRAILINGSPACE(m));
		if (amount == 0) {
			/* mbuf must be empty */
			m_prev = m;
			MGET(m, M_DONTWAIT, MT_DATA);
			if (!m) {
				m_freem(head);
				return(0);
			}
			if (pkt_len >= MINCLSIZE)
				MCLGET(m, M_DONTWAIT);
			m->m_len = 0;
			m_prev->m_next = m;
			amount = min(blen, M_TRAILINGSPACE(m));
			mbuf_data = m->m_data;
		}
		bcopy(data, mbuf_data, amount);
		blen -= amount;
		pkt_len -= amount;
		m->m_len += amount;
		data += amount;
		mbuf_data += amount;

		if (blen == 0) {
			start->md->md1 &= HADR;
			start->md->md1 |= OWN;
			start->md->md2 = -RECVBUFSIZE; /* XXX - shouldn't be necessary */
			INC_MD_PTR(start_of_packet, sc->nrdre)
			start = sc->recv_ring + start_of_packet;
			data = start->buff.data;
			/*blen = -(start->md->md2);*/
			blen = RECVBUFSIZE; /* XXX More PCnet-32 crap */
		}
	}
	return(head);
}


inline void
lnc_rint(int unit)
{
	register struct lnc_softc *sc = &lnc_softc[unit];
	struct host_ring_entry *next, *start;
	int start_of_packet;
	struct mbuf *head;
	struct ether_header *eh;
	int lookahead;
	int flags;
	int pkt_len;

	/*
	 * The LANCE will issue a RINT interrupt when the ownership of the
	 * last buffer of a receive packet has been relinquished by the LANCE.
	 * Therefore, it can be assumed that a complete packet can be found
	 * before hitting buffers that are still owned by the LANCE, if not
	 * then there is a bug in the driver that is causing the descriptors
	 * to get out of sync.
	 */

#ifdef DIAGNOSTIC
	if ((sc->recv_ring + sc->recv_next)->md->md1 & OWN) {
		log(LOG_ERR, "lnc%d: Receive interrupt with buffer still owned by controller -- Resetting\n", unit);
		lnc_reset(unit);
		return;
	}
	if (!((sc->recv_ring + sc->recv_next)->md->md1 & STP)) {
		log(LOG_ERR, "lnc%d: Receive interrupt but not start of packet -- Resetting\n", unit);
		lnc_reset(unit);
		return;
	}
#endif

	lookahead = 0;
	next = sc->recv_ring + sc->recv_next;
	while ((flags = next->md->md1) & STP) {

		/* Make a note of the start of the packet */
		start_of_packet = sc->recv_next;

		/*
		 * Find the end of the packet. Even if not data chaining,
		 * jabber packets can overrun into a second descriptor.
	 	 * If there is no error, then the ENP flag is set in the last
		 * descriptor of the packet. If there is an error then the ERR
		 * flag will be set in the descriptor where the error occured.
		 * Therefore, to find the last buffer of a packet we search for
		 * either ERR or ENP.
		 */

		if (!(flags & (ENP | MDERR))) {
			do {
				INC_MD_PTR(sc->recv_next, sc->nrdre)
				next = sc->recv_ring + sc->recv_next;
				flags = next->md->md1;
			} while (!(flags & (STP | OWN | ENP | MDERR)));

			if (flags & STP) {
				log(LOG_ERR, "lnc%d: Start of packet found before end of previous in receive ring -- Resetting\n", unit);
				lnc_reset(unit);
				return;
			}
			if (flags & OWN) {
				if (lookahead) {
					/*
					 * Looked ahead into a packet still
					 * being received
					 */
					sc->recv_next = start_of_packet;
					break;
				} else {
					log(LOG_ERR, "lnc%d: End of received packet not found-- Resetting\n", unit);
					lnc_reset(unit);
					return;
				}
			}
		}

		pkt_len = (next->md->md3 & MCNT) - FCS_LEN;

		/* Move pointer onto start of next packet */
		INC_MD_PTR(sc->recv_next, sc->nrdre)
		next = sc->recv_ring + sc->recv_next;

		if (flags & MDERR) {
			if (flags & RBUFF) {
				LNCSTATS(rbuff)
				log(LOG_ERR, "lnc%d: Receive buffer error\n", unit);
			}
			if (flags & OFLO) {
				/* OFLO only valid if ENP is not set */
				if (!(flags & ENP)) {
					LNCSTATS(oflo)
					log(LOG_ERR, "lnc%d: Receive overflow error \n", unit);
				}
			} else if (flags & ENP) {
				/*
				 * FRAM and CRC are valid only if ENP
				 * is set and OFLO is not.
				 */
				if (flags & FRAM) {
					LNCSTATS(fram)
					log(LOG_ERR, "lnc%d: Framming error\n", unit);
					/*
					 * FRAM is only set if there's a CRC
					 * error so avoid multiple messages
					 */
				} else if (flags & CRC) {
					LNCSTATS(crc)
					log(LOG_ERR, "lnc%d: Receive CRC error\n", unit);
				}
			}

			/* Drop packet */
			LNCSTATS(rerr)
			sc->arpcom.ac_if.if_ierrors++;
			while (start_of_packet != sc->recv_next) {
				start = sc->recv_ring + start_of_packet;
				start->md->md2 = -RECVBUFSIZE; /* XXX - shouldn't be necessary */
				start->md->md1 &= HADR;
				start->md->md1 |= OWN;
				INC_MD_PTR(start_of_packet, sc->nrdre)
			}
		} else { /* Valid packet */

			sc->arpcom.ac_if.if_ipackets++;


			if (sc->nic.mem_mode == DMA_MBUF)
				head = chain_mbufs(sc, start_of_packet, pkt_len);
			else
				head = mbuf_packet(sc, start_of_packet, pkt_len);

			if (head) {
				/*
				 * First mbuf in packet holds the
				 * ethernet and packet headers
				 */
				head->m_pkthdr.rcvif = &sc->arpcom.ac_if;
				head->m_pkthdr.len = pkt_len - sizeof *eh;

				/*
				 * BPF expects the ether header to be in the first
				 * mbuf of the chain so point eh at the right place
				 * but don't increment the mbuf pointers before
				 * the bpf tap.
				 */

				eh = (struct ether_header *) head->m_data;

#if NBPFILTER > 0
				if (sc->bpf)
					bpf_mtap(sc->bpf, head);

				/* Check this packet is really for us */

				if ((sc->arpcom.ac_if.if_flags & IFF_PROMISC) &&
					!(eh->ether_dhost[0] & 1) && /* Broadcast and multicast */
					(bcmp(eh->ether_dhost, sc->arpcom.ac_enaddr,
							sizeof(eh->ether_dhost))))
						m_freem(head);
				else
#endif
				{
					/* Skip over the ether header */
					head->m_data += sizeof *eh;
					head->m_len -= sizeof *eh;

					ether_input(&sc->arpcom.ac_if, eh, head);
				}

			} else {
				log(LOG_ERR,"lnc%d: Packet dropped, no mbufs\n",unit);
				LNCSTATS(drop_packet)
			}
		}

		lookahead++;
	}

	/*
	 * At this point all completely received packets have been processed
	 * so clear RINT since any packets that have arrived while we were in
	 * here have been dealt with.
	 */

	outw(sc->rdp, RINT | INEA);
}

inline void
lnc_tint(int unit)
{
	register struct lnc_softc *sc = &lnc_softc[unit];
	struct host_ring_entry *next, *start;
	int start_of_packet;
	int lookahead;

	/*
	 * If the driver is reset in this routine then we return immediately to
	 * the interrupt driver routine. Any interrupts that have occured
	 * since the reset will be dealt with there. sc->trans_next
	 * should point to the start of the first packet that was awaiting
	 * transmission after the last transmit interrupt was dealt with. The
	 * LANCE should have relinquished ownership of that descriptor before
	 * the interrupt. Therefore, sc->trans_next should point to a
	 * descriptor with STP set and OWN cleared. If not then the driver's
	 * pointers are out of sync with the LANCE, which signifies a bug in
	 * the driver. Therefore, the following two checks are really
	 * diagnostic, since if the driver is working correctly they should
	 * never happen.
	 */

#ifdef DIAGNOSTIC
	if ((sc->trans_ring + sc->trans_next)->md->md1 & OWN) {
		log(LOG_ERR, "lnc%d: Transmit interrupt with buffer still owned by controller -- Resetting\n", unit);
		lnc_reset(unit);
		return;
	}
#endif


	/*
	 * The LANCE will write the status information for the packet it just
	 * tried to transmit in one of two places. If the packet was
	 * transmitted successfully then the status will be written into the
	 * last descriptor of the packet. If the transmit failed then the
	 * status will be written into the descriptor that was being accessed
	 * when the error occured and all subsequent descriptors in that
	 * packet will have been relinquished by the LANCE.
	 *
	 * At this point we know that sc->trans_next points to the start
	 * of a packet that the LANCE has just finished trying to transmit.
	 * We now search for a buffer with either ENP or ERR set.
	 */

	lookahead = 0;

	do {
		start_of_packet = sc->trans_next;
		next = sc->trans_ring + sc->trans_next;

#ifdef DIAGNOSTIC
	if (!(next->md->md1 & STP)) {
		log(LOG_ERR, "lnc%d: Transmit interrupt but not start of packet -- Resetting\n", unit);
		lnc_reset(unit);
		return;
	}
#endif

		/*
		 * Find end of packet.
		 */

		if (!(next->md->md1 & (ENP | MDERR))) {
			do {
				INC_MD_PTR(sc->trans_next, sc->ntdre)
				next = sc->trans_ring + sc->trans_next;
			} while (!(next->md->md1 & (STP | OWN | ENP | MDERR)));

			if (next->md->md1 & STP) {
				log(LOG_ERR, "lnc%d: Start of packet found before end of previous in transmit ring -- Resetting\n", unit);
				lnc_reset(unit);
				return;
			}
			if (next->md->md1 & OWN) {
				if (lookahead) {
					/*
					 * Looked ahead into a packet still
					 * being transmitted
					 */
					sc->trans_next = start_of_packet;
					break;
				} else {
					log(LOG_ERR, "lnc%d: End of transmitted packet not found -- Resetting\n", unit);
					lnc_reset(unit);
					return;
				}
			}
		}
		/*
		 * Check for ERR first since other flags are irrelevant if an
		 * error occurred.
		 */
		if (next->md->md1 & MDERR) {

			LNCSTATS(terr)
				sc->arpcom.ac_if.if_oerrors++;

			if (next->md->md3 & LCOL) {
				LNCSTATS(lcol)
				log(LOG_ERR, "lnc%d: Transmit late collision  -- Net error?\n", unit);
				sc->arpcom.ac_if.if_collisions++;
				/*
				 * Clear TBUFF since it's not valid when LCOL
				 * set
				 */
				next->md->md3 &= ~TBUFF;
			}
			if (next->md->md3 & LCAR) {
				LNCSTATS(lcar)
				log(LOG_ERR, "lnc%d: Loss of carrier during transmit -- Net error?\n", unit);
			}
			if (next->md->md3 & RTRY) {
				LNCSTATS(rtry)
				log(LOG_ERR, "lnc%d: Transmit of packet failed after 16 attempts -- TDR = %d\n", unit, ((sc->trans_ring + sc->trans_next)->md->md3 & TDR));
				sc->arpcom.ac_if.if_collisions += 16;
				/*
				 * Clear TBUFF since it's not valid when RTRY
				 * set
				 */
				next->md->md3 &= ~TBUFF;
			}
			/*
			 * TBUFF is only valid if neither LCOL nor RTRY are set.
			 * We need to check UFLO after LCOL and RTRY so that we
			 * know whether or not TBUFF is valid. If either are
			 * set then TBUFF will have been cleared above. A
			 * UFLO error will turn off the transmitter so we
			 * have to reset.
			 *
			 */

			if (next->md->md3 & UFLO) {
				LNCSTATS(uflo)
				/*
				 * If an UFLO has occured it's possibly due
				 * to a TBUFF error
				 */
				if (next->md->md3 & TBUFF) {
					LNCSTATS(tbuff)
					log(LOG_ERR, "lnc%d: Transmit buffer error -- Resetting\n", unit);
				} else
					log(LOG_ERR, "lnc%d: Transmit underflow error -- Resetting\n", unit);
				lnc_reset(unit);
				return;
			}
			do {
				INC_MD_PTR(sc->trans_next, sc->ntdre)
				next = sc->trans_ring + sc->trans_next;
			} while (!(next->md->md1 & STP) && (sc->trans_next != sc->next_to_send));

		} else {
			/*
			 * Since we check for ERR first then if we get here
			 * the packet was transmitted correctly. There may
			 * still have been non-fatal errors though.
			 * Don't bother checking for DEF, waste of time.
			 */

			sc->arpcom.ac_if.if_opackets++;

			if (next->md->md1 & MORE) {
				LNCSTATS(more)
				sc->arpcom.ac_if.if_collisions += 2;
			}

			/*
			 * ONE is invalid if LCOL is set. If LCOL was set then
			 * ERR would have also been set and we would have
			 * returned from lnc_tint above. Therefore we can
			 * assume if we arrive here that ONE is valid.
			 *
			 */

			if (next->md->md1 & ONE) {
				LNCSTATS(one)
				sc->arpcom.ac_if.if_collisions++;
			}
			INC_MD_PTR(sc->trans_next, sc->ntdre)
			next = sc->trans_ring + sc->trans_next;
		}

		/*
		 * Clear descriptors and free any mbufs.
		 */

		do {
			start = sc->trans_ring + start_of_packet;
			start->md->md1 &= HADR;
			if (sc->nic.mem_mode == DMA_MBUF) {
				/* Cache clusters on a local queue */
				if ((start->buff.mbuf->m_flags & M_EXT) && (sc->mbuf_count < MBUF_CACHE_LIMIT)) {
					if (sc->mbuf_count) {
						start->buff.mbuf->m_next = sc->mbufs;
						sc->mbufs = start->buff.mbuf;
					} else
						sc->mbufs = start->buff.mbuf;
					sc->mbuf_count++;
					start->buff.mbuf = 0;
				} else {
					struct mbuf *junk;
					MFREE(start->buff.mbuf, junk);
					start->buff.mbuf = 0;
				}
			}
			sc->pending_transmits--;
			INC_MD_PTR(start_of_packet, sc->ntdre)
		}while (start_of_packet != sc->trans_next);

		/*
		 * There's now at least one free descriptor
		 * in the ring so indicate that we can accept
		 * more packets again.
		 */

		sc->arpcom.ac_if.if_flags &= ~IFF_OACTIVE;

		lookahead++;

	} while (sc->pending_transmits && !(next->md->md1 & OWN));

	/*
	 * Clear TINT since we've dealt with all
	 * the completed transmissions.
	 */

	outw(sc->rdp, TINT | INEA);

	/* XXX only while doing if_is comparisons */
	if (!(sc->arpcom.ac_if.if_flags & IFF_OACTIVE))
		lnc_start(&sc->arpcom.ac_if);

}

int
lnc_probe(struct isa_device * isa_dev)
{
int nports;
int vsw;

#ifdef DIAGNOSTIC
	vsw = inw(isa_dev->id_iobase + PCNET_VSW);
	printf("Vendor Specific Word = %x\n", vsw);
#endif


	if (nports = bicc_probe(isa_dev))
		return (nports);
	if (nports = ne2100_probe(isa_dev))
		return (nports);
	if (nports = depca_probe(isa_dev))
		return (nports);

	/*
	 * Register device even though probe has
	 * failed so it can be reconfigured later.
	 */

	lnc_registerdev(isa_dev);
	return (0);
}

int
ne2100_probe(struct isa_device * isa_dev)
{
	struct lnc_softc *sc = &lnc_softc[isa_dev->id_unit];
	int i;

	sc->rap = isa_dev->id_iobase + PCNET_RAP;
	sc->rdp = isa_dev->id_iobase + PCNET_RDP;

	if (sc->nic.ic = pcnet_probe(isa_dev->id_unit)) {
		sc->nic.ident = NE2100;
		sc->nic.mem_mode = DMA_FIXED;

		/* XXX - For now just use the defines */
		sc->nrdre = NRDRE;
		sc->ntdre = NTDRE;

		/* Extract MAC address from PROM */
		for (i = 0; i < ETHER_ADDR_LEN; i++)
			sc->arpcom.ac_enaddr[i] = inb(isa_dev->id_iobase + i);
		lnc_registerdev(isa_dev);
		return (NE2100_IOSIZE);
	} else {
		return (0);
	}
}

int
bicc_probe(struct isa_device * isa_dev)
{
struct lnc_softc *sc = &lnc_softc[isa_dev->id_unit];
int i;

	/*
	 * There isn't any way to determine if a NIC is a BICC. Basically, if
	 * the lance probe succeeds using the i/o addresses of the BICC then
	 * we assume it's a BICC.
	 *
	 */

	sc->rap = isa_dev->id_iobase + BICC_RAP;
	sc->rdp = isa_dev->id_iobase + BICC_RDP;

	/* I think all these cards us the Am7990 */

	if (sc->nic.ic = lance_probe(isa_dev->id_unit)) {
		sc->nic.ident = BICC;
		sc->nic.mem_mode = DMA_FIXED;

		/* XXX - For now just use the defines */
		sc->nrdre = NRDRE;
		sc->ntdre = NTDRE;

		/* Extract MAC address from PROM */
		for (i = 0; i < ETHER_ADDR_LEN; i++)
			sc->arpcom.ac_enaddr[i] = inb(isa_dev->id_iobase + (i * 2));

		lnc_registerdev(isa_dev);
		return (BICC_IOSIZE);
	} else {
		return (0);
	}
}

/*
 * I don't have data sheets for the dec cards but it looks like the mac
 * address is contained in a 32 byte ring. Each time you read from the port
 * you get the next byte in the ring. The mac address is stored after a
 * signature so keep searching for the signature first.
 */
int
dec_macaddr_extract(u_char ring[], struct lnc_softc * sc)
{
	const unsigned char signature[] = {0xff, 0x00, 0x55, 0xaa, 0xff, 0x00, 0x55, 0xaa};

	int i, j, rindex;

	for (i = 0; i < sizeof ring; i++) {
		for (j = 0, rindex = i; j < sizeof signature; j++) {
			if (ring[rindex] != signature[j])
				break;
			if (++rindex > sizeof ring)
				rindex = 0;
		}
		if (j == sizeof signature) {
			for (j = 0, rindex = i; j < ETHER_ADDR_LEN; j++) {
				sc->arpcom.ac_enaddr[j] = ring[rindex];
				if (++rindex > sizeof ring)
					rindex = 0;
			}
			return (1);
		}
	}
	return (0);
}

int
depca_probe(struct isa_device * isa_dev)
{
	int i;
	struct lnc_softc *sc = &lnc_softc[isa_dev->id_unit];
	unsigned char maddr_ring[DEPCA_ADDR_ROM_SIZE];

	sc->rap = isa_dev->id_iobase + DEPCA_RAP;
	sc->rdp = isa_dev->id_iobase + DEPCA_RDP;

	if (sc->nic.ic = lance_probe(isa_dev->id_unit)) {
		sc->nic.ident = DEPCA;
		sc->nic.mem_mode = SHMEM;

		/* Extract MAC address from PROM */
		for (i = 0; i < DEPCA_ADDR_ROM_SIZE; i++)
			maddr_ring[i] = inb(isa_dev->id_iobase + DEPCA_ADP);
		if (dec_macaddr_extract(maddr_ring, sc)) {
			lnc_registerdev(isa_dev);
			return (DEPCA_IOSIZE);
		}
	}
	return (0);
}

int
lance_probe(int unit)
{
	write_csr(unit, CSR0, STOP);

	if ((inw(lnc_softc[unit].rdp) & STOP) && !(read_csr(unit, CSR3))) {
		/*
		 * Check to see if it's a C-LANCE. For the LANCE the INEA bit
		 * cannot be set while the STOP bit is. This restriction is
		 * removed for the C-LANCE.
		 */
		write_csr(unit, CSR0, INEA);
		if (read_csr(unit, CSR0) & INEA)
			return (C_LANCE);
		else
			return (LANCE);
	} else
		return (UNKNOWN);
}

int
pcnet_probe(int unit)
{
	u_long chip_id;
	int type;

	/*
	 * The PCnet family don't reset the RAP register on reset so we'll
	 * have to write during the probe :-) It does have an ID register
	 * though so the probe is just a matter of reading it.
	 */

	if (type = lance_probe(unit)) {

		chip_id = read_csr(unit, CSR89);
		chip_id <<= 16;
		chip_id |= read_csr(unit, CSR88);
		if (chip_id & AMD_MASK) {
			chip_id >>= 12;
			switch (chip_id & PART_MASK) {
			case Am79C960:
				return (PCnet_ISA);
			case Am79C961:
				return (PCnet_ISAplus);
			case Am79C965:
				return (PCnet_32);
			case Am79C970:
				return (PCnet_PCI);
			default:
				break;
			}
		}
	}
	return (type);
}

int
lnc_attach(struct isa_device * isa_dev)
{
	struct lnc_softc *sc = &lnc_softc[isa_dev->id_unit];
	int lnc_mem_size;

	/*
	 * Allocate memory for use by the controller.
	 *
	 * XXX -- the Am7990 and Am79C960 only have 24 address lines and so can
	 * only access the lower 16Mb of physical memory. For the moment we
	 * assume that malloc will allocate memory within the lower 16Mb
	 * range. This is not a very valid assumption but there's nothing
	 * that can be done about it yet. For shared memory NICs this isn't
	 * relevant.
	 *
	 */

	lnc_mem_size = ((NDESC(sc->nrdre) + NDESC(sc->ntdre)) *
		 sizeof(struct host_ring_entry));

	if (sc->nic.mem_mode != SHMEM)
		lnc_mem_size += sizeof(struct init_block) + (sizeof(struct mds) *
			    (NDESC(sc->nrdre) + NDESC(sc->ntdre))) +
			MEM_SLEW;

	/* If using DMA to fixed host buffers then allocate memory for them */

	if (sc->nic.mem_mode == DMA_FIXED)
		lnc_mem_size += (NDESC(sc->nrdre) * RECVBUFSIZE) + (NDESC(sc->ntdre) * TRANSBUFSIZE);

	sc->recv_ring = malloc(lnc_mem_size, M_DEVBUF, M_NOWAIT);

	if (!sc->recv_ring) {
		log(LOG_ERR, "lnc%d: Couldn't allocate memory for NIC\n", isa_dev->id_unit);
		return (0);	/* XXX -- attach failed -- not tested in
				 * calling routines */
	}
	if ((sc->nic.mem_mode != SHMEM) && (kvtop(sc->recv_ring) > 0x1000000)) {
		log(LOG_ERR, "lnc%d: Memory allocated above 16Mb limit\n", isa_dev->id_unit);
		return (0);
	}

	if ((sc->nic.mem_mode != SHMEM) &&
		 (sc->nic.ic != PCnet_32) &&
		 (sc->nic.ic != PCnet_PCI))
		isa_dmacascade(isa_dev->id_drq);

	/* Set default mode */
	sc->nic.mode = NORMAL;

	/* Fill in arpcom structure entries */

	sc->arpcom.ac_if.if_name = lncdriver.name;
	sc->arpcom.ac_if.if_unit = isa_dev->id_unit;
	sc->arpcom.ac_if.if_mtu = ETHERMTU;
	sc->arpcom.ac_if.if_flags = IFF_BROADCAST | IFF_NOTRAILERS | IFF_SIMPLEX;
	sc->arpcom.ac_if.if_timer = 0;
	sc->arpcom.ac_if.if_init = lnc_init;
	sc->arpcom.ac_if.if_output = ether_output;
	sc->arpcom.ac_if.if_start = lnc_start;
	sc->arpcom.ac_if.if_ioctl = lnc_ioctl;
	sc->arpcom.ac_if.if_reset = lnc_reset;
	sc->arpcom.ac_if.if_watchdog = lnc_watchdog;
	sc->arpcom.ac_if.if_type = IFT_ETHER;
	sc->arpcom.ac_if.if_addrlen = ETHER_ADDR_LEN;
	sc->arpcom.ac_if.if_hdrlen = ETHER_HDR_LEN;

	/*
	 * XXX -- should check return status of if_attach
	 */

	if_attach(&sc->arpcom.ac_if);
	sc->kdc.kdc_state = DC_IDLE;

	printf("lnc%d: %s, address %s\n",
	       isa_dev->id_unit,
		   sc->kdc.kdc_description,
	       ether_sprintf(sc->arpcom.ac_enaddr));

#if NBPFILTER > 0
	bpfattach(&sc->bpf, &sc->arpcom.ac_if, DLT_EN10MB, sizeof(struct ether_header));
#endif

	return (1);
}

void
lnc_init(int unit)
{
	struct lnc_softc *sc = &lnc_softc[unit];
	int s, i;
	char *lnc_mem;
	struct mbuf *m = 0;

	/* Check that interface has valid address */

	if (!sc->arpcom.ac_if.if_addrlist)
		return;

	/* Shut down interface */

	s = splimp();
	lnc_stop(unit);
	sc->arpcom.ac_if.if_flags |= IFF_BROADCAST | IFF_NOTRAILERS | IFF_SIMPLEX;

	/*
	 * This sets up the memory area for the controller. Memory is set up for
	 * the initialisation block (12 words of contiguous memory starting
	 * on a word boundary),the transmit and receive ring structures (each
	 * entry is 4 words long and must start on a quadword boundary) and
	 * the data buffers.
	 *
	 * The alignment tests are particularly paranoid.
	 */



	sc->recv_next = 0;
	sc->trans_ring = sc->recv_ring + NDESC(sc->nrdre);
	sc->trans_next = 0;

	if (sc->nic.mem_mode == SHMEM)
		lnc_mem = (char *) sc->nic.iobase;
	else
		lnc_mem = (char *) (sc->trans_ring + NDESC(sc->ntdre));

	lnc_mem = (char *)(((int)lnc_mem + 1) & ~1);
	sc->init_block = (struct init_block *) ((int) lnc_mem & ~1);
	lnc_mem = (char *) (sc->init_block + 1);
	lnc_mem = (char *)(((int)lnc_mem + 7) & ~7);

	/* Initialise pointers to descriptor entries */
	for (i = 0; i < NDESC(sc->nrdre); i++) {
		(sc->recv_ring + i)->md = (struct mds *) lnc_mem;
		lnc_mem += sizeof(struct mds);
	}
	for (i = 0; i < NDESC(sc->ntdre); i++) {
		(sc->trans_ring + i)->md = (struct mds *) lnc_mem;
		lnc_mem += sizeof(struct mds);
	}

	/* Initialise the remaining ring entries */

	if (sc->nic.mem_mode == DMA_MBUF) {

		sc->mbufs = 0;
		sc->mbuf_count = 0;

		/* Free previously allocated mbufs */
		if (sc->initialised)
			lnc_free_mbufs(sc);


		for (i = 0; i < NDESC(sc->nrdre); i++) {
			if (alloc_mbuf_cluster(sc, sc->recv_ring+i)) {
				log(LOG_ERR, "Initialisation failed -- no mbufs\n");
				splx(s);
				return;
			}
		}

		for (i = 0; i < NDESC(sc->ntdre); i++) {
			(sc->trans_ring + i)->buff.mbuf = 0;
			(sc->trans_ring + i)->md->md0 = 0;
			(sc->trans_ring + i)->md->md1 = 0;
			(sc->trans_ring + i)->md->md2 = 0;
			(sc->trans_ring + i)->md->md3 = 0;
		}
	} else {
		for (i = 0; i < NDESC(sc->nrdre); i++) {
			(sc->recv_ring + i)->md->md0 = kvtop(lnc_mem);
			(sc->recv_ring + i)->md->md1 = ((kvtop(lnc_mem) >> 16) & 0xff) | OWN;
			(sc->recv_ring + i)->md->md2 = -RECVBUFSIZE;
			(sc->recv_ring + i)->md->md3 = 0;
			(sc->recv_ring + i)->buff.data = lnc_mem;
			lnc_mem += RECVBUFSIZE;
		}
		for (i = 0; i < NDESC(sc->ntdre); i++) {
			(sc->trans_ring + i)->md->md0 = kvtop(lnc_mem);
			(sc->trans_ring + i)->md->md1 = ((kvtop(lnc_mem) >> 16) & 0xff);
			(sc->trans_ring + i)->md->md2 = 0;
			(sc->trans_ring + i)->md->md3 = 0;
			(sc->trans_ring + i)->buff.data = lnc_mem;
			lnc_mem += TRANSBUFSIZE;
		}
	}

	sc->next_to_send = 0;

	/* Set up initialisation block */

	sc->init_block->mode = sc->nic.mode;

	for (i = 0; i < ETHER_ADDR_LEN; i++)
		sc->init_block->padr[i] = sc->arpcom.ac_enaddr[i];

	for (i = 0; i < MULTICAST_ADDR_LEN; i++)
		sc->init_block->ladrf[i] = MULTI_INIT_ADDR;

	sc->init_block->rdra = kvtop(sc->recv_ring->md);
	sc->init_block->rlen = ((kvtop(sc->recv_ring->md) >> 16) & 0xff) | (sc->nrdre << 13);
	sc->init_block->tdra = kvtop(sc->trans_ring->md);
	sc->init_block->tlen = ((kvtop(sc->trans_ring->md) >> 16) & 0xff) | (sc->ntdre << 13);


	/* Set initialised to show that the memory area is valid */
	sc->initialised = 1;

	sc->pending_transmits = 0;

	/* Give the LANCE the physical address of the initialisation block */

	write_csr(unit, CSR1, kvtop(sc->init_block));
	write_csr(unit, CSR2, (kvtop(sc->init_block) >> 16) & 0xff);

	/*
	 * Depending on which controller this is, CSR3 has different meanings.
	 * For the Am7990 it controls DMA operations, for the Am79C960 it
	 * controls interrupt masks and transmitter algorithms. In either
	 * case, none of the flags are set.
	 *
	 */

	write_csr(unit, CSR3, 0);

	/* Let's see if it starts */

	write_csr(unit, CSR0, INIT);
	for (i = 0; i < 1000; i++)
		if (read_csr(unit, CSR0) & IDON)
			break;

	/*
	 * Now that the initialisation is complete there's no reason to
	 * access anything except CSR0, so we leave RAP pointing there
	 * so we can just access RDP from now on, saving an outw each
	 * time.
	 */

	if (read_csr(unit, CSR0) & IDON) {
		/*
		 * Enable interrupts, start the LANCE, mark the interface as
		 * running and transmit any pending packets.
		 */
		write_csr(unit, CSR0, STRT | INEA);
		sc->arpcom.ac_if.if_flags |= IFF_RUNNING;
		sc->arpcom.ac_if.if_flags &= ~IFF_OACTIVE;
		lnc_start(&sc->arpcom.ac_if);
	} else
		log(LOG_ERR, "lnc%d: Initialisation failed\n", unit);

	splx(s);
}

/*
 * The interrupt flag (INTR) will be set and provided that the interrupt enable
 * flag (INEA) is also set, the interrupt pin will be driven low when any of
 * the following occur:
 *
 * 1) Completion of the initialisation routine (IDON). 2) The reception of a
 * packet (RINT). 3) The transmission of a packet (TINT). 4) A transmitter
 * timeout error (BABL). 5) A missed packet (MISS). 6) A memory error (MERR).
 *
 * The interrupt flag is cleared when all of the above conditions are cleared.
 *
 * If the driver is reset from this routine then it first checks to see if any
 * interrupts have ocurred since the reset and handles them before returning.
 * This is because the NIC may signify a pending interrupt in CSR0 using the
 * INTR flag even if a hardware interrupt is currently inhibited (at least I
 * think it does from reading the data sheets). We may as well deal with
 * these pending interrupts now rather than get the overhead of another
 * hardware interrupt immediately upon returning from the interrupt handler.
 *
 */

void
lncintr(int unit)
{
	struct lnc_softc *sc = &lnc_softc[unit];
	u_short csr0;

	/*
	 * INEA is the only bit that can be cleared by writing a 0 to it so
	 * we have to include it in any writes that clear other flags.
	 */

	while ((csr0 = inw(sc->rdp)) & INTR) {

		/*
		 * Clear interrupt flags early to avoid race conditions. The
		 * controller can still set these flags even while we're in
		 * this interrupt routine. If the flag is still set from the
		 * event that caused this interrupt any new events will
		 * be missed.
		 */

		outw(sc->rdp, IDON | CERR | BABL | MISS | MERR | RINT | TINT | INEA);

		/* We don't do anything with the IDON flag */

		if (csr0 & ERR) {
			if (csr0 & CERR) {
				log(LOG_ERR, "lnc%d: Heartbeat error -- SQE test failed\n", unit);
				LNCSTATS(cerr)
			}
			if (csr0 & BABL) {
				log(LOG_ERR, "lnc%d: Babble error - more than 1519 bytes transmitted\n", unit);
				LNCSTATS(babl)
				sc->arpcom.ac_if.if_oerrors++;
			}
			if (csr0 & MISS) {
				log(LOG_ERR, "lnc%d: Missed packet -- no receive buffer\n", unit);
				LNCSTATS(miss)
				sc->arpcom.ac_if.if_ierrors++;
			}
			if (csr0 & MERR) {
				log(LOG_ERR, "lnc%d: Memory error  -- Resetting\n", unit);
				LNCSTATS(merr)
				lnc_reset(unit);
				continue;
			}
		}
		if (csr0 & RINT) {
			LNCSTATS(rint)
			lnc_rint(unit);
		}
		if (csr0 & TINT) {
			LNCSTATS(tint)
			sc->arpcom.ac_if.if_timer = 0;
			lnc_tint(unit);
		}

		/*
		 * If there's room in the transmit descriptor ring then queue
		 * some more transmit packets.
		 */

		if (!(sc->arpcom.ac_if.if_flags & IFF_OACTIVE))
			lnc_start(&sc->arpcom.ac_if);
	}
}




inline int
mbuf_to_buffer(struct mbuf *m, char *buffer)
{

	int len=0;

	for( ; m; m = m->m_next) {
		bcopy(mtod(m, caddr_t), buffer, m->m_len);
		buffer += m->m_len;
		len += m->m_len;
	}

	return(len);
}

inline struct mbuf *
chain_to_cluster(struct mbuf *m)
{
	struct mbuf *new;

	MGET(new, M_DONTWAIT, MT_DATA);
	if (new) {
		MCLGET(new, M_DONTWAIT);
		if (new->m_ext.ext_buf) {
			new->m_len = mbuf_to_buffer(m, new->m_data);
			m_freem(m);
			return(new);
		} else
			m_free(new);
	}
	return(0);
}

/*
 * IFF_OACTIVE and IFF_RUNNING are checked in ether_output so it's redundant
 * to check them again since we wouldn't have got here if they were not
 * appropriately set. This is also called from lnc_init and lncintr but the
 * flags should be ok at those points too.
 */

void
lnc_start(struct ifnet *ifp)
{

	struct lnc_softc *sc = &lnc_softc[ifp->if_unit];
	struct host_ring_entry *desc;
	int tmp;
	int end_of_packet;
	struct mbuf *head, *m;
	int len, chunk;
	char *buffer;
	int addr;
	int no_entries_needed;

	do {

		IF_DEQUEUE(&sc->arpcom.ac_if.if_snd, head);
		if (!head)
			return;

		if (sc->nic.mem_mode == DMA_MBUF) {

			no_entries_needed = 0;
			for (m=head; m; m = m->m_next)
				no_entries_needed++;

			/*
			 * We try and avoid bcopy as much as possible
			 * but there are two cases when we use it.
			 *
			 * 1) If there are not enough free entries in the ring
			 * to hold each mbuf in the chain then compact the
			 * chain into a single cluster.
			 *
			 * 2) The Am7990 and Am79C90 must not have less than
			 * 100 bytes in the first descriptor of a chained
			 * packet so it's necessary to shuffle the mbuf
			 * contents to ensure this.
			 */


			if (no_entries_needed > (NDESC(sc->ntdre) - sc->pending_transmits))
				if (!(head = chain_to_cluster(head))) {
					log(LOG_ERR, "lnc%d: Couldn't get mbuf for transmit packet -- Resetting \n ",ifp->if_unit);
					lnc_reset(ifp->if_unit);
					return;
				}
			else if ((sc->nic.ic == LANCE) || (sc->nic.ic == C_LANCE)) {
				if ((head->m_len < 100) && (head->m_next)) {
					len = 100 - head->m_len;
					if (M_TRAILINGSPACE(head) < len) {
						/*
						 * Move data to start of data
						 * area. We assume the first
						 * mbuf has a packet header
						 * and is not a cluster.
						 */
						bcopy((caddr_t)head->m_data, (caddr_t)head->m_pktdat, head->m_len);
						head->m_data = head->m_pktdat;
					}
					m = head->m_next;
					while (m && (len > 0)) {
						chunk = min(len, m->m_len);
						bcopy(mtod(m, caddr_t), mtod(head, caddr_t) + head->m_len, chunk);
						len -= chunk;
						head->m_len += chunk;
						m->m_len -= chunk;
						m->m_data += chunk;
						if (m->m_len <= 0) {
							MFREE(m, head->m_next);
							m = head->m_next;
						}
					}
				}
			}

			tmp = sc->next_to_send;

			/*
			 * On entering this loop we know that tmp points to a
			 * descriptor with a clear OWN bit.
			 */

			desc = sc->trans_ring + tmp;
			len = ETHER_MIN_LEN;
			for (m = head; m; m = m->m_next) {
				desc->buff.mbuf = m;
				addr = kvtop(m->m_data);
				desc->md->md0 = addr;
				desc->md->md1 = ((addr >> 16) & 0xff);
				desc->md->md3 = 0;
				desc->md->md2 = -m->m_len;
				sc->pending_transmits++;
				len -= m->m_len;

				INC_MD_PTR(tmp, sc->ntdre)
				desc = sc->trans_ring + tmp;
			}

			end_of_packet = tmp;
			DEC_MD_PTR(tmp, sc->ntdre)
			desc = sc->trans_ring + tmp;
			desc->md->md1 |= ENP;

			if (len > 0)
				desc->md->md2 -= len;

			/*
			 * Set OWN bits in reverse order, otherwise the Lance
			 * could start sending the packet before all the
			 * buffers have been relinquished by the host.
			 */

			while (tmp != sc->next_to_send) {
				desc->md->md1 |= OWN;
				DEC_MD_PTR(tmp, sc->ntdre)
				desc = sc->trans_ring + tmp;
			}
			sc->next_to_send = end_of_packet;
			desc->md->md1 |= STP | OWN;
		} else {
			sc->pending_transmits++;
			desc = sc->trans_ring + sc->next_to_send;
			len = mbuf_to_buffer(head, desc->buff.data);
			desc->md->md3 = 0;
			desc->md->md2 = -max(len, ETHER_MIN_LEN);
			desc->md->md1 |= OWN | STP | ENP;
			INC_MD_PTR(sc->next_to_send, sc->ntdre)
		}

		/* Force an immediate poll of the transmit ring */
		outw(sc->rdp, TDMD | INEA);

		/*
		 * Set a timer so if the buggy Am7990.h shuts
		 * down we can wake it up.
		 */

		ifp->if_timer = 2;

#if NBPFILTER > 0
		if (sc->bpf)
			bpf_mtap(sc->bpf, head);
#endif

		if (sc->nic.mem_mode != DMA_MBUF)
			m_freem(head);

	} while (sc->pending_transmits < NDESC(sc->ntdre));

	/*
	 * Transmit ring is full so set IFF_OACTIVE
	 * since we can't buffer any more packets.
	 */

	sc->arpcom.ac_if.if_flags |= IFF_OACTIVE;
	LNCSTATS(trans_ring_full)
}

int
lnc_ioctl(struct ifnet * ifp, int command, caddr_t data)
{

	struct lnc_softc *sc = &lnc_softc[ifp->if_unit];
	struct ifaddr  *ifa = (struct ifaddr *) data;
	struct ifreq *ifr = (struct ifreq *) data;
	int s, error = 0;

	s = splimp();

	switch (command) {
	case SIOCSIFADDR:
		ifp->if_flags |= IFF_UP;
		sc->kdc.kdc_state = DC_BUSY;

		switch (ifa->ifa_addr->sa_family) {
#ifdef INET
		case AF_INET:
			lnc_init(ifp->if_unit);
			arp_ifinit((struct arpcom *)ifp, ifa);
			break;
#endif
		default:
			lnc_init(ifp->if_unit);
			break;
		}
		break;

	case SIOCSIFFLAGS:
#ifdef DEBUG
		if (ifp->if_flags & IFF_DEBUG)
			sc->lnc_debug = 1;
		else
			sc->lnc_debug = 0;
#endif
		if (ifp->if_flags & IFF_PROMISC) {
			if (!(sc->nic.mode & PROM)) {
				sc->nic.mode |= PROM;
				lnc_init(ifp->if_unit);
			}
		} else if (sc->nic.mode & PROM) {
			sc->nic.mode &= ~PROM;
			lnc_init(ifp->if_unit);
		}
		if ((ifp->if_flags & IFF_UP) == 0 &&
		    (ifp->if_flags & IFF_RUNNING) != 0) {
			/*
			 * If interface is marked down and it is running,
			 * then stop it.
			 */
			lnc_stop(ifp->if_unit);
			ifp->if_flags &= ~IFF_RUNNING;
		} else if ((ifp->if_flags & IFF_UP) != 0 &&
			   (ifp->if_flags & IFF_RUNNING) == 0) {
			/*
			 * If interface is marked up and it is stopped, then
			 * start it.
			 */
			lnc_init(ifp->if_unit);
		}
		sc->kdc.kdc_state =
			((ifp->if_flags & IFF_UP) ? DC_BUSY : DC_IDLE);
		break;
#ifdef notyet
	case SIOCADDMULTI:
	case SIOCDELMULTI:
		error = (command == SIOCADDMULTI) ?
					ether_addmulti(ifr, &sc->arpcom) :
					ether_delmulti(ifr, &sc->arpcom);

		if (error == ENETRESET) {
			lnc_setladrf(ifp,sc);
			error = 0;
		}
		break;
#endif
	case SIOCSIFMTU:
		/*
		 * Set the interface MTU.
		 */

		if (ifr->ifr_mtu > ETHERMTU) {
			error = EINVAL;
		} else
			ifp->if_mtu = ifr->ifr_mtu;
		break;
	default:
		error = EINVAL;
	}
	(void) splx(s);
	return error;
}

void
lnc_watchdog(int unit)
{
	log(LOG_ERR, "lnc%d: Device timeout -- Resetting\n", unit);
	++lnc_softc[unit].arpcom.ac_if.if_oerrors;
	lnc_reset(unit);
}

#ifdef DEBUG
void
lnc_dump_state(int unit)
{
	struct lnc_softc *sc = &lnc_softc[unit];
	int             i;

	printf("\nDriver/NIC [%d] state dump\n", unit);
	printf("Memory access mode: %b\n", sc->nic.mem_mode, MEM_MODES);
	printf("Host memory\n");
	printf("-----------\n");

	printf("Receive ring: base = %x, next = %x\n",
	    sc->recv_ring, (sc->recv_ring + sc->recv_next));
	for (i = 0; i < NDESC(sc->nrdre); i++)
		printf("\t%d:%x md = %x buff = %x\n",
		  i, sc->recv_ring + i, (sc->recv_ring + i)->md,
		    (sc->recv_ring + i)->buff);

	printf("Transmit ring: base = %x, next = %x\n",
	    sc->trans_ring, (sc->trans_ring + sc->trans_next));
	for (i = 0; i < NDESC(sc->ntdre); i++)
		printf("\t%d:%x md = %x buff = %x\n",
		i, sc->trans_ring + i, (sc->trans_ring + i)->md,
		    (sc->trans_ring + i)->buff);
	printf("Lance memory (may be on host(DMA) or card(SHMEM))\n");
	printf("Init block = %x\n", sc->init_block);
	printf("\tmode = %b rlen:rdra = %x:%x tlen:tdra = %x:%x\n",
	    sc->init_block->mode, INIT_MODE, sc->init_block->rlen,
	  sc->init_block->rdra, sc->init_block->tlen, sc->init_block->tdra);
	printf("Receive descriptor ring\n");
	for (i = 0; i < NDESC(sc->nrdre); i++)
		printf("\t%d buffer = 0x%x%x, BCNT = %d,\tMCNT = %u,\tflags = %b\n",
		    i, ((sc->recv_ring + i)->md->md1 & HADR),
		    (sc->recv_ring + i)->md->md0,
		    -(short) (sc->recv_ring + i)->md->md2,
		    (sc->recv_ring + i)->md->md3,
		    (((sc->recv_ring + i)->md->md1 & ~HADR) >> 8), RECV_MD1);
	printf("Transmit descriptor ring\n");
	for (i = 0; i < NDESC(sc->ntdre); i++)
		printf("\t%d buffer = 0x%x%x, BCNT = %d,\tflags = %b %b\n",
		    i, ((sc->trans_ring + i)->md->md1 & HADR),
		    (sc->trans_ring + i)->md->md0,
		    -(short) (sc->trans_ring + i)->md->md2,
		    ((sc->trans_ring + i)->md->md1 >> 8), TRANS_MD1,
		    ((sc->trans_ring + i)->md->md3 >> 10), TRANS_MD3);
	printf("\nnext_to_send = %x\n", sc->next_to_send);
	printf("\n CSR0 = %b CSR1 = %x CSR2 = %x CSR3 = %x\n\n", read_csr(unit, CSR0), CSR0_FLAGS, read_csr(unit, CSR1), read_csr(unit, CSR2), read_csr(unit, CSR3));
	/* Set RAP back to CSR0 */
	outw(sc->rap, CSR0);
}

void
mbuf_dump_chain(struct mbuf * m)
{

#define MBUF_FLAGS \
	"\20\1M_EXT\2M_PKTHDR\3M_EOR\4UNKNOWN\5M_BCAST\6M_MCAST"

	if (!m)
		log(LOG_DEBUG, "m == NULL\n");
	do {
		log(LOG_DEBUG, "m = %x\n", m);
		log(LOG_DEBUG, "m_hdr.mh_next = %x\n", m->m_hdr.mh_next);
		log(LOG_DEBUG, "m_hdr.mh_nextpkt = %x\n", m->m_hdr.mh_nextpkt);
		log(LOG_DEBUG, "m_hdr.mh_len = %d\n", m->m_hdr.mh_len);
		log(LOG_DEBUG, "m_hdr.mh_data = %x\n", m->m_hdr.mh_data);
		log(LOG_DEBUG, "m_hdr.mh_type = %d\n", m->m_hdr.mh_type);
		log(LOG_DEBUG, "m_hdr.mh_flags = %b\n", m->m_hdr.mh_flags, MBUF_FLAGS);
		if (!(m->m_hdr.mh_flags & (M_PKTHDR | M_EXT)))
			log(LOG_DEBUG, "M_dat.M_databuf = %x\n", m->M_dat.M_databuf);
		else {
			if (m->m_hdr.mh_flags & M_PKTHDR) {
				log(LOG_DEBUG, "M_dat.MH.MH_pkthdr.len = %d\n", m->M_dat.MH.MH_pkthdr.len);
				log(LOG_DEBUG, "M_dat.MH.MH_pkthdr.rcvif = %x\n", m->M_dat.MH.MH_pkthdr.rcvif);
				if (!(m->m_hdr.mh_flags & M_EXT))
					log(LOG_DEBUG, "M_dat.MH.MH_dat.MH_databuf = %x\n", m->M_dat.MH.MH_dat.MH_databuf);
			}
			if (m->m_hdr.mh_flags & M_EXT) {
				log(LOG_DEBUG, "M_dat.MH.MH_dat.MH_ext.ext_buff %x\n", m->M_dat.MH.MH_dat.MH_ext.ext_buf);
				log(LOG_DEBUG, "M_dat.MH.MH_dat.MH_ext.ext_free %x\n", m->M_dat.MH.MH_dat.MH_ext.ext_free);
				log(LOG_DEBUG, "M_dat.MH.MH_dat.MH_ext.ext_size %d\n", m->M_dat.MH.MH_dat.MH_ext.ext_size);
			}
		}
	} while (m = m->m_next);
}
#endif

#endif
