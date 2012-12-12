/*-
 * Copyright (c) 1995 Matt Thomas (thomas@lkg.dec.com)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. The name of the author may not be used to endorse or promote products
 *    derived from this software withough specific prior written permission
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
 * $Id: pdq_os.h,v 1.1.1.1 1996/08/15 03:24:23 fgray Exp $
 *
 * $Log: pdq_os.h,v $
 * Revision 1.1.1.1  1996/08/15 03:24:23  fgray
 * x86 merge
 *
 * Revision 1.2.4.1  1995/09/12  09:19:23  davidg
 * Brought in change from rev 1.3: include systm.h.
 *
 * Revision 1.3  1995/07/16  10:07:20  bde
 * Fix compiler warnings (systm.h wasn't included).
 *
 * Revision 1.2  1995/03/25  22:40:49  bde
 * Remove wrong redeclarations of printf() and bzero().  Include the correct
 * header to declare DELAY().
 *
 * Revision 1.1  1995/03/14  09:16:07  davidg
 * Added support for generic FDDI and the DEC DEFEA and DEFPA FDDI adapters.
 *
 * Submitted by:	Matt Thomas
 *
 * Revision 1.6  1995/03/14  01:52:52  thomas
 * Update for new FreeBSD PCI Interrupt interface
 * Use inl/inb/... inline macros provided by FreeBSD and BSDI
 *
 * Revision 1.5  1995/03/10  17:42:24  thomas
 * More changes for BSDI
 *
 * Revision 1.4  1995/03/06  17:08:56  thomas
 * Add copyright/disclaimer
 * Add inx/outx macros
 *
 * Revision 1.3  1995/03/03  13:48:35  thomas
 * more fixes
 *
 *
 */

/*
 * DEC PDQ FDDI Controller; PDQ O/S dependent definitions
 *
 * Written by Matt Thomas
 *
 */

#ifndef _PDQ_OS_H
#define	_PDQ_OS_H

#define	PDQ_OS_TX_TIMEOUT		5	/* seconds */

#ifdef PDQTEST
#include <pdq_os_test.h>
#elif defined(__FreeBSD__) || defined(__bsdi__)

#include <sys/param.h>
#include <sys/systm.h>
#ifndef M_MCAST
#include <sys/mbuf.h>
#endif
#include <sys/malloc.h>
#include <vm/vm.h>
#include <vm/vm_kern.h>

#ifdef __FreeBSD__
#include <machine/clock.h>
#endif

#define	PDQ_USE_MBUFS
#define	PDQ_OS_PREFIX			"%s%d: "
#define	PDQ_OS_PREFIX_ARGS		pdq->pdq_os_name, pdq->pdq_unit

#define	PDQ_OS_PAGESIZE			NBPG
#define	PDQ_OS_USEC_DELAY(n)		DELAY(n)
#define	PDQ_OS_MEMZERO(p, n)		bzero((caddr_t)(p), (n))
#define	PDQ_OS_VA_TO_PA(p)		vtophys(p)
#define	PDQ_OS_MEMALLOC(n)		malloc(n, M_DEVBUF, M_NOWAIT)
#define	PDQ_OS_MEMFREE(p, n)		free((void *) p, M_DEVBUF)
#ifdef __FreeBSD__
#define	PDQ_OS_MEMALLOC_CONTIG(n)	vm_page_alloc_contig(n, 0, 0xffffffff, PAGE_SIZE)
#define	PDQ_OS_MEMFREE_CONTIG(p, n)	kmem_free(kernel_map, (vm_offset_t) p, n)
#else
#define	PDQ_OS_MEMALLOC_CONTIG		PDQ_OS_MEMALLOC
#define	PDQ_OS_MEMFREE_CONTIG		PDQ_OS_MEMFREE
#endif

#if defined(__FreeBSD__)
#include <machine/cpufunc.h>
#elif defined(__bsdi__)
#include <machine/inline.h>
#endif
#define PDQ_OS_IORD_32(port)		inl(port)
#define PDQ_OS_IOWR_32(port, data)	outl(port, data)
#define PDQ_OS_IORD_8(port)		inb(port)
#define PDQ_OS_IOWR_8(port, data)	outb(port, data)
#endif

#ifdef PDQ_USE_MBUFS
#define	PDQ_OS_DATABUF_SIZE			(MCLBYTES)
#define	PDQ_OS_DATABUF_FREE(b)			(m_freem(b))
#define	PDQ_OS_DATABUF_NEXT(b)			((b)->m_next)
#define	PDQ_OS_DATABUF_NEXT_SET(b, b1)		((b)->m_next = (b1))
#define	PDQ_OS_DATABUF_NEXTPKT(b)		((b)->m_nextpkt)
#define	PDQ_OS_DATABUF_NEXTPKT_SET(b, b1)	((b)->m_nextpkt = (b1))
#define	PDQ_OS_DATABUF_LEN(b)			((b)->m_len)
#define	PDQ_OS_DATABUF_LEN_SET(b, n)		((b)->m_len = (n))
#define	PDQ_OS_DATABUF_LEN_ADJ(b, n)		((b)->m_len += (n))
#define	PDQ_OS_DATABUF_PTR(b)			(mtod((b), pdq_uint8_t *))
#define	PDQ_OS_DATABUF_PTR_ADJ(b, n)		((b)->m_data += (n))
typedef struct mbuf PDQ_OS_DATABUF_T;

#define	PDQ_OS_DATABUF_ALLOC(b) do { \
    PDQ_OS_DATABUF_T *x_m0; \
    MGETHDR(x_m0, M_DONTWAIT, MT_DATA); \
    if (x_m0 != NULL) { \
	MCLGET(x_m0, M_DONTWAIT);	\
	if ((x_m0->m_flags & M_EXT) == 0) { \
	    m_free(x_m0); \
	    (b) = NULL; \
	} else { \
	    (b) = x_m0; \
	    x_m0->m_len = PDQ_OS_DATABUF_SIZE; \
	} \
    } else { \
	(b) = NULL; \
    } \
} while (0)
#define	PDQ_OS_DATABUF_RESET(b)	((b)->m_data = (b)->m_ext.ext_buf, (b)->m_len = MCLBYTES)
#endif /* PDQ_USE_MBUFS */

#ifdef PDQ_USE_STREAMS
#define	PDQ_OS_DATABUF_FREE(b)			(freemsg(b))
#define	PDQ_OS_DATABUF_NEXT(b)			((b)->b_cont)
#define	PDQ_OS_DATABUF_NEXTPKT(b)		((b)->b_next)
#define	PDQ_OS_DATABUF_NEXTPKT_SET(b, b1)	((b)->b_next = (b1))
#define	PDQ_OS_DATABUF_LEN(b)			((b)->b_wptr - (b)->b_rptr)
#define	PDQ_OS_DATABUF_PTR(b)			((pdq_uint8_t *) (b)->b_rptr)
typedef mblk_t PDQ_OS_DATABUF_T;
#endif /* PDQ_USE_STREAMS */

#define	PDQ_OS_TX_TRANSMIT		5

#define	PDQ_OS_DATABUF_ENQUEUE(q, b)	do { \
    PDQ_OS_DATABUF_NEXTPKT_SET(b, NULL); \
    if ((q)->q_tail == NULL) \
	(q)->q_head = (b); \
    else \
	PDQ_OS_DATABUF_NEXTPKT_SET(((PDQ_OS_DATABUF_T *)(q)->q_tail), b); \
    (q)->q_tail = (b); \
} while (0)

#define	PDQ_OS_DATABUF_DEQUEUE(q, b)	do { \
    if (((b) = (PDQ_OS_DATABUF_T *) (q)->q_head) != NULL) { \
	if (((q)->q_head = PDQ_OS_DATABUF_NEXTPKT(b)) == NULL) \
	    (q)->q_tail = NULL; \
	PDQ_OS_DATABUF_NEXTPKT_SET(b, NULL); \
    } \
} while (0)

extern void pdq_os_addr_fill(pdq_t *pdq, pdq_lanaddr_t *addrs, size_t numaddrs);
extern void pdq_os_receive_pdu(pdq_t *, PDQ_OS_DATABUF_T *pdu, size_t pdulen);
extern void pdq_os_restart_transmitter(pdq_t *pdq);
extern void pdq_os_transmit_done(pdq_t *pdq, PDQ_OS_DATABUF_T *pdu);

extern void pdq_print_fddi_chars(pdq_t *pdq, const pdq_response_status_chars_get_t *rsp);

extern void pdq_init_csrs(pdq_csrs_t *csrs, void *csrs_va, size_t csr_size);
extern void pdq_init_pci_csrs(pdq_pci_csrs_t *csrs, void *csrs_va, size_t csr_size);

extern void pdq_flush_databuf_queue(pdq_databuf_queue_t *q);

extern pdq_boolean_t pdq_do_port_control(const pdq_csrs_t * const csrs, pdq_uint32_t cmd);
extern void pdq_read_mla(const pdq_csrs_t * const csrs, pdq_lanaddr_t *hwaddr);
extern void pdq_read_fwrev(const pdq_csrs_t * const csrs, pdq_fwrev_t *fwrev);
extern pdq_boolean_t pdq_read_error_log(pdq_t *pdq, pdq_response_error_log_get_t *log_entry);
extern pdq_chip_rev_t pdq_read_chiprev(const pdq_csrs_t * const csrs);

extern void pdq_queue_commands(pdq_t *pdq);
extern void pdq_process_command_responses(pdq_t *pdq);
extern void pdq_process_unsolicited_events(pdq_t *pdq);

extern void pdq_process_received_data(pdq_t *pdq, pdq_rx_info_t *rx,
				      pdq_rxdesc_t *receives,
				      pdq_uint32_t completion_goal,
				      pdq_uint32_t ring_mask);

extern pdq_boolean_t pdq_queue_transmit_data(pdq_t *pdq, PDQ_OS_DATABUF_T *pdu);
extern void pdq_process_transmitted_data(pdq_t *pdq);
extern void pdq_flush_transmitter(pdq_t *pdq);


extern pdq_state_t pdq_stop(pdq_t *pdq);
extern void pdq_run(pdq_t *pdq);

extern int pdq_interrupt(pdq_t *pdq);
extern pdq_t *pdq_initialize(void *csr_va, const char *name, int unit, void *ctx, pdq_type_t type);


#endif /* _PDQ_OS_H */
