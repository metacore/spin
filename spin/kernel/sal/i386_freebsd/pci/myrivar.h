/*
 * The basis of the FreeBSD myrinet driver is the BSDi driver of the
 * MOSIX project.
 * The port was done in the Spring/summer of 1997.
 *
 * Contact: hutton@isi.edu (Anne Hutton)
 *          atomic-2@isi.edu
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
 * $Id: myrivar.h,v 1.1 1997/12/09 20:21:17 becker Exp $
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

#define DMA
#define NEWCOMPRESS

#define MIN(x, y)			((x) > (y) ? (y) : (x))

#define	PCI_VENDORID(x)			((x) & 0xFFFF)
#define	PCI_CHIPID(x)			(((x) >> 16) & 0xFFFF)

#define KERNEL_CHANNEL			0
#define MYRI_PAD			0x02AB
#define MYRI_BURST			0xf
#define MYRI_CLUSTER_NUM		((MYRI_MTU/MCLBYTES)+1)
#define MYRI_MAX_DEVICES		 8
#define MYRI_UNIT_TO_SOFTC(unit)	(myris[unit])
#define MYRI_HDR_LEN			sizeof(struct myri_softc *)
#define MAX_RBUF_SIZE			(MYRI_HDR_LEN \
						+ MYRI_PAD_LEN \
						+ sizeof(struct ether_header) \
						+ MYRI_MTU \
					)
#define MYRI_DATA_LEN			(MAX_RBUF_SIZE - MYRI_HDR_LEN)
#define MYRI_HDR_ADDR(x)		(MAX_RBUF_SIZE * (x))
#define MYRI_DATA_ADDR(x)		(MYRI_HDR_ADDR(x) + MYRI_HDR_LEN)
#ifdef	DMA
#define DMA_BUF_SIZE			(2 * MYRI_NUM_RECEIVES)
#define RECVBUFFER			(MAX_RBUF_SIZE * DMA_BUF_SIZE)
#endif

/*
 * Ethernet software status per interface.
 *
 *
 * Each interface is referenced by a network interface structure,
 * qe_if, which the routing code uses to locate the interface.
 * This structure contains the output queue for the interface, its address, ...
 */
struct myri_softc {
	struct  device myri_dev;		/* base device, must be first */
	struct  arpcom	myri_ac;		/* Ethernet common part */

#define myri_if	myri_ac.ac_if			/* Network visible part */	
#define myri_addr myri_ac.ac_enaddr		/* hardware Ethernet
						   address */
        volatile vm_offset_t myri_base;
	char	     myri_addr8[8];		/* full myrinet address */
	int	     myri_reset_count;		/* reset counter */
	struct ifqueue myri_sifq;		/* sent packets */
#ifdef	DMA
	caddr_t		recv_vaddr;			/* for DMA */
	caddr_t		recv_paddr;			/* for DMA */
	unsigned int	nfree;
	unsigned int	freelist[DMA_BUF_SIZE];
	unsigned int	refs[DMA_BUF_SIZE];
#else
	struct mbuf* myri_rbuff[MYRI_NUM_RECEIVES]; 	/* receive buffers */
#endif
	/*
	 *	/dev/mlanai support
	 */
	int	     myri_open;			/* mlanai open flag */
	u_int	    *myri_copy_block;		/* kernel allocated block */
	u_int	    *myri_memory;		/* Board memory pointer */
	struct MYRINET_EEPROM *myri_eeprom;	/* Board EEPROM pointer */
	volatile u_int	    *myri_lanai_reg;	/* LANai registers pointer */
	volatile u_short    *myri_lanai_ctl;	/* LANai control reg pointer */
};
