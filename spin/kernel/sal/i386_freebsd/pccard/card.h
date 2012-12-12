/*
 *	Include file for PCMCIA user process interface
 *
 *-------------------------------------------------------------------------
 *
 * Copyright (c) 1995 Andrew McRae.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
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
 */
#define	PIOCGSTATE	_IOR('P', 1, struct slotstate)	/* Get slot state */
#define	PIOCGMEM	_IOWR('P', 2, struct mem_desc)	/* Get memory map */
#define	PIOCSMEM	_IOW('P', 3, struct mem_desc)	/* Set memory map */
#define	PIOCGIO		_IOWR('P', 4, struct io_desc)	/* Get I/O map */
#define	PIOCSIO		_IOW('P', 5, struct io_desc)	/* Set I/O map */
#define PIOCSDRV	_IOW('P', 6, struct drv_desc)	/* Set driver */
#define PIOCRWFLAG	_IOW('P', 7, int)	/* Set flags for drv use */
#define PIOCRWMEM	_IOWR('P', 8, unsigned long) /* Set mem for drv use */
#define PIOCSPOW	_IOW('P', 9, struct power) /* Set power structure */
/*
 *	Debug codes.
 */
#define PIOCGREG	_IOWR('P',100, struct pcic_reg)	/* get reg */
#define PIOCSREG	_IOW('P', 101, struct pcic_reg)	/* Set reg */


/*
 *	Slot states for PIOCGSTATE
 */
enum cardstate { noslot, empty, filled };

/*
 *	Descriptor structure for memory map.
 */
struct mem_desc {
	int	window;		/* Memory map window number (0-4) */
	int	flags;		/* Flags - see below */
	caddr_t	start;		/* System memory start */
	int	size;		/* Size of memory area */
	unsigned long card;	/* Card memory address */
};

#define	MDF_16BITS	0x01	/* Memory is 16 bits wide */
#define	MDF_ZEROWS	0x02	/* Set no wait states for memory */
#define	MDF_WS0		0x04	/* Wait state flags */
#define	MDF_WS1		0x08
#define	MDF_ATTR	0x10	/* Memory is attribute memory */
#define	MDF_WP		0x20	/* Write protect memory */
#define	MDF_ACTIVE	0x40	/* Context active (read-only) */

/*
 *	Descriptor structure for I/O map
 */
struct io_desc {
	int	window;		/* I/O map number (0-1) */
	int	flags;		/* Flags - see below */
	int	start;		/* I/O port start */
	int	size;		/* Number of port addresses */
};

#define	IODF_WS		0x01	/* Set wait states for 16 bit I/O access */
#define	IODF_16BIT	0x02	/* I/O access are 16 bit */
#define	IODF_CS16	0x04	/* Allow card selection of 16 bit access */
#define	IODF_ZEROWS	0x08	/* No wait states for 8 bit I/O */
#define	IODF_ACTIVE	0x10	/* Context active (read-only) */

/*
 *	Device descriptor for allocation of driver.
 */
struct drv_desc {
	char	name[16];	/* Driver name */
	int	unit;		/* Driver unit number */
	unsigned long mem;	/* Memory address of driver */
	int	memsize;	/* Memory size (if used) */
	int	iobase;		/* base of I/O ports */
	int	irqmask;	/* Interrupt number(s) to allocate */
	int	flags;		/* Device flags */
	u_char	misc[128];	/* For any random info */
};

struct pcic_reg {
	unsigned char reg;
	unsigned char value;
};

/*
 *	Slot information. Used to read current status of slot.
 */
struct slotstate {
	enum cardstate state;		/* Current state of slot */
	int	maxmem;			/* Max allowed memory windows */
	int	maxio;			/* Max allowed I/O windows */
	int	irqs;			/* Bitmap of IRQs allowed */
	int	flags;			/* Capability flags */
};

/*
 *	The power values are in volts * 10, e.g. 5V is 50, 3.3V is 33.
 */
struct power {
	int	vcc;
	int	vpp;
};

/*
 *	Beep parameters
 */
#define PCCARD_BEEP_PITCH0	1600	/* inserted/removed */
#define	PCCARD_BEEP_DURATION0	20
#define PCCARD_BEEP_PITCH1	1200	/* successed */
#define	PCCARD_BEEP_DURATION1	40
#define PCCARD_BEEP_PITCH2	3200	/* failed */
#define	PCCARD_BEEP_DURATION2	40

/*
 *	Other system limits
 */
#define MAXSLOT 16
#define	NUM_MEM_WINDOWS	10
#define	NUM_IO_WINDOWS	6
#define	CARD_DEVICE	"/dev/card%d"		/* String for sprintf */
