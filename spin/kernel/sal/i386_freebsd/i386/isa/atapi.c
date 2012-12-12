/*
 * Device-independent level for ATAPI drivers.
 *
 * Copyright (C) 1995 Cronyx Ltd.
 * Author Serge Vakulenko, <vak@cronyx.ru>
 *
 * This software is distributed with NO WARRANTIES, not even the implied
 * warranties for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * Authors grant any other persons or organisations permission to use
 * or modify this software as long as this message is kept with the software,
 * all derivative works or modified versions.
 *
 * Version 1.5, Thu Sep 21 23:08:11 MSD 1995
 */

/*
 * The ATAPI level is implemented as a machine-dependent layer
 * between the device driver and the IDE controller.
 * All the machine- and controller dependency is isolated inside
 * the ATAPI level, while all the device dependency is located
 * in the device subdriver.
 *
 * It seems that an ATAPI bus will became popular for medium-speed
 * storage devices such as CD-ROMs, magneto-optical disks, tape streamers etc.
 *
 * To ease the development of new ATAPI drivers, the subdriver
 * interface was designed to be as simple as possible.
 *
 * Three routines are available for the subdriver to access the device:
 *
 *      struct atapires atapi_request_wait (ata, unit, cmd, a1, a2, a3, a4, a5,
 *              a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, addr, count);
 *      struct atapi *ata;  -- atapi controller descriptor
 *      int unit;           -- device unit number on the IDE bus
 *      u_char cmd;         -- ATAPI command code
 *      u_char a1..a15;     -- ATAPI command arguments
 *      char *addr;         -- address of the data buffer for i/o
 *      int count;          -- data length, >0 for read ops, <0 for write ops
 *
 * The atapi_request_wait() function puts the op in the queue of ATAPI
 * commands for the IDE controller, starts the controller, the waits for
 * operation to be completed (using tsleep).
 * The function should be called from the user phase only (open(), close(),
 * ioctl() etc).
 * Ata and unit args are the values which the subdriver gets from the ATAPI
 * level via attach() call.
 * Buffer pointed to by *addr should be placed in core memory, static
 * or dynamic, but not in stack.
 * The function returns the error code structure, which consists of:
 * - atapi driver code value
 * - controller status port value
 * - controller error port value
 *
 *      struct atapires atapi_request_immediate (ata, unit, cmd, a1, a2, a3,
 *              a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15,
 *              addr, count);
 *
 * The atapi_request_immediate() function is similar to atapi_request_wait(),
 * but it does not use interrupts for performing the request.
 * It should be used during an attach phase to get parameters from the device.
 *
 *      void atapi_request_callback (ata, unit, cmd, a1, a2, a3, a4, a5,
 *              a6, a7, a8, a9, a10, a11, a12, a13, a14, a15,
 *              addr, count, done, x, y);
 *      struct atapi *ata;  -- atapi controller descriptor
 *      int unit;           -- device unit number on the IDE bus
 *      u_char cmd;         -- ATAPI command code
 *      u_char a1..a15;     -- ATAPI command arguments
 *      char *addr;         -- address of the data buffer for i/o
 *      int count;          -- data length, >0 for read ops, <0 for write ops
 *      void (*done)();     -- function to call when op finished
 *      void *x, *y;        -- arguments for done() function
 *
 * The atapi_request_callback() function puts the op in the queue of ATAPI
 * commands for the IDE controller, starts the controller, then returns.
 * When the operation finishes, then the callback function done()
 * will be called on the interrupt level.
 * The function is designed to be callable from the interrupt phase.
 * The done() functions is called with the following arguments:
 *      (void) (*done) (x, y, count, errcode)
 *      void *x, *y;             -- arguments from the atapi_request_callback()
 *      int count;               -- the data residual count
 *      struct atapires errcode; -- error code structure, see above
 *
 * The new driver could be added in three steps:
 * 1. Add entries for the new driver to bdevsw and cdevsw tables in conf.c.
 *    You will need to make at least three routines: open(), close(),
 *    strategy() and possibly ioctl().
 * 2. Make attach() routine, which should allocate all the needed data
 *    structures and print the device description string (see wcdattach()).
 * 3. Add an appropriate case to the switch in atapi_attach() routine,
 *    call attach() routine of the new driver here.  Add the appropriate
 *    #include line at the top of attach.c.
 * That's all!
 *
 * Use #define DEBUG in atapi.c to enable tracing of all i/o operations
 * on the IDE bus.
 */
#undef DEBUG

#include "wdc.h"
#include "wcd.h"
/* #include "wmt.h" -- add your driver here */
/* #include "wmd.h" -- add your driver here */

#if NWDC > 0 && defined (ATAPI)

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/proc.h>
#include <sys/malloc.h>
#include <i386/include/cpufunc.h>
#include <i386/include/clock.h>
#include <i386/isa/atapi.h>

#ifdef DEBUG
#   define print(s)     printf s
#else
#   define print(s)     {/*void*/}
#endif

#define MAXCMD  (8*NWDC)

/*
 * ATAPI packet command phase.
 */
#define PHASE_CMDOUT    (ARS_DRQ | ARI_CMD)
#define PHASE_DATAIN    (ARS_DRQ | ARI_IN)
#define PHASE_DATAOUT   ARS_DRQ
#define PHASE_COMPLETED (ARI_IN | ARI_CMD)
#define PHASE_ABORTED   0                       /* nonstandard - for NEC 260 */

struct atapicmd {                       /* ATAPI command block */
	struct atapicmd *next;          /* next command in queue */
	int              busy;          /* busy flag */
	u_char           cmd[16];       /* command and args */
	int              unit;          /* drive unit number */
	int              count;         /* byte count, >0 - read, <0 - write */
	char            *addr;          /* data to transfer */
	void           (*callback) ();  /* call when done */
	void            *cbarg1;        /* callback arg 1 */
	void            *cbarg2;        /* callback arg 1 */
	struct atapires  result;        /* resulting error code */
};

struct atapi {                          /* ATAPI controller data */
	u_short          port;          /* i/o port base */
	u_char           ctrlr;         /* physical controller number */
	u_char           debug : 1;     /* trace enable flag */
	u_char           cmd16 : 1;     /* 16-byte command flag */
	u_char           intrcmd : 1;   /* interrupt before cmd flag */
	u_char           slow : 1;      /* slow reaction device */
	struct atapicmd *queue;         /* queue of commands to perform */
	struct atapicmd *tail;          /* tail of queue */
	struct atapicmd *free;          /* queue of free command blocks */
	struct atapicmd  cmdrq[MAXCMD]; /* pool of command requests */
};

struct atapi atapitab[NWDC];

static struct atapi_params *atapi_probe (int port, int unit);
static int atapi_wait (int port, u_char bits_wanted);
static void atapi_send_cmd (struct atapi *ata, struct atapicmd *ac);
static int atapi_io (struct atapi *ata, struct atapicmd *ac);
static int atapi_start_cmd (struct atapi *ata, struct atapicmd *ac);
static int atapi_wait_cmd (struct atapi *ata, struct atapicmd *ac);

extern int wdstart (int ctrlr);

void atapi_attach (int ctlr, int unit, int port, struct kern_devconf *parent)
{
	struct atapi *ata = atapitab + ctlr;
	struct atapi_params *ap;
	char buf [sizeof(ap->model) + 1];
	char revbuf [sizeof(ap->revision) + 1];
	struct atapicmd *ac;

	print (("atapi%d.%d at 0x%x: attach called\n", ctlr, unit, port));
	ap = atapi_probe (port, unit);
	if (! ap)
		return;

	bcopy (ap->model, buf, sizeof(buf)-1);
	buf[sizeof(buf)-1] = 0;

	bcopy (ap->revision, revbuf, sizeof(revbuf)-1);
	revbuf[sizeof(revbuf)-1] = 0;

	printf ("wdc%d: unit %d (atapi): <%s/%s>", ctlr, unit, buf, revbuf);

	/* device is removable */
	if (ap->removable)
		printf (", removable");

	/* packet command size */
	switch (ap->cmdsz) {
	case AT_PSIZE_12: break;
	case AT_PSIZE_16: printf (", cmd16"); ata->cmd16 = 1; break;
	default:          printf (", cmd%d", ap->cmdsz);
	}

	/* DRQ type */
	switch (ap->drqtype) {
	case AT_DRQT_MPROC: ata->slow = 1; break;
	case AT_DRQT_INTR:  printf (", intr"); ata->intrcmd = 1; break;
	case AT_DRQT_ACCEL: printf (", accel"); break;
	default:            printf (", drq%d", ap->drqtype);
	}

	/* overlap operation supported */
	if (ap->ovlapflag)
		printf (", ovlap");

	/* interleaved DMA supported */
	if (ap->idmaflag)
		printf (", idma");
	/* DMA supported */
	else if (ap->dmaflag)
		printf (", dma");

	/* IORDY can be disabled */
	if (ap->iordydis)
		printf (", iordis");
	/* IORDY supported */
	else if (ap->iordyflag)
		printf (", iordy");

	printf ("\n");

	ata->port = port;
	ata->ctrlr = ctlr;
#ifdef DEBUG
	ata->debug = 1;
#else
	ata->debug = 0;
#endif
	/* Initialize free queue. */
	ata->cmdrq[MAXCMD-1].next = 0;
	for (ac = ata->cmdrq+MAXCMD-2; ac >= ata->cmdrq; --ac)
		ac->next = ac+1;
	ata->free = ata->cmdrq;

	if (ap->proto != AT_PROTO_ATAPI) {
		printf ("wdc%d: unit %d: unknown ATAPI protocol=%d\n",
			ctlr, unit, ap->proto);
		free (ap, M_TEMP);
		return;
	}
	switch (ap->devtype) {
	default:
		/* unknown ATAPI device */
		printf ("wdc%d: unit %d: unknown ATAPI type=%d\n",
			ctlr, unit, ap->devtype);
		break;

	case AT_TYPE_DIRECT:            /* direct-access */
	case AT_TYPE_CDROM:             /* CD-ROM device */
#if NWCD > 0
		/* ATAPI CD-ROM */
		{
			int wcdattach (struct atapi*, int, struct atapi_params*,
				int, struct kern_devconf*);
			if (wcdattach (ata, unit, ap, ata->debug, parent) < 0)
				break;
		}
		/* Device attached successfully. */
		return;
#else
		printf ("wdc%d: ATAPI CD-ROMs not configured\n", ctlr);
		break;
#endif

	case AT_TYPE_TAPE:              /* streaming tape (QIC-121 model) */
#if NWMT > 0
		/* Add your driver here */
#else
		printf ("wdc%d: ATAPI streaming tapes not supported yet\n", ctlr);
		break;
#endif

	case AT_TYPE_OPTICAL:           /* optical disk */
#if NWMD > 0
		/* Add your driver here */
#else
		printf ("wdc%d: ATAPI optical disks not supported yet\n", ctlr);
		break;
#endif
	}
	/* Attach failed. */
	free (ap, M_TEMP);
}

static void bswap (char *buf, int len)
{
	u_short *p = (u_short*) (buf + len);
	while (--p >= (u_short*) buf)
		*p = ntohs (*p);
}

static void btrim (char *buf, int len)
{
	char *p;

	/* Remove the trailing spaces. */
	for (p=buf; p<buf+len; ++p)
		if (! *p)
			*p = ' ';
	for (p=buf+len-1; p>=buf && *p==' '; --p)
		*p = 0;
}

/*
 * Issue IDENTIFY command to ATAPI drive to ask it what it is.
 */
static struct atapi_params *atapi_probe (int port, int unit)
{
	struct atapi_params *ap;
	char tb [DEV_BSIZE];

	/* Wait for controller not busy. */
	if (atapi_wait (port, 0) < 0) {
		print (("atapiX.%d at 0x%x: controller busy, status=%b\n",
			unit, port, inb (port + AR_STATUS), ARS_BITS));
		return (0);
	}

	/* Issue ATAPI IDENTIFY command. */
	outb (port + AR_DRIVE, unit ? ARD_DRIVE1 : ARD_DRIVE0);
	outb (port + AR_COMMAND, ATAPIC_IDENTIFY);

	/* Check that device is present. */
	if (inb (port + AR_STATUS) == 0xff) {
		print (("atapiX.%d at 0x%x: no device\n", unit, port));
		if (unit == 1)
			/* Select unit 0. */
			outb (port + AR_DRIVE, ARD_DRIVE0);
		return (0);
	}

	/* Wait for data ready. */
	if (atapi_wait (port, ARS_DRQ) != 0) {
		print (("atapiX.%d at 0x%x: identify not ready, status=%b\n",
			unit, port, inb (port + AR_STATUS), ARS_BITS));
		if (unit == 1)
			/* Select unit 0. */
			outb (port + AR_DRIVE, ARD_DRIVE0);
		return (0);
	}

	/* Obtain parameters. */
	insw (port + AR_DATA, tb, sizeof(tb) / sizeof(short));

	ap = malloc (sizeof *ap, M_TEMP, M_NOWAIT);
	if (! ap)
		return (0);
	bcopy (tb, ap, sizeof *ap);

	/*
	 * Shuffle string byte order.
	 * Mitsumi and NEC drives don't need this.
	 */
	if (! ((ap->model[0] == 'N' && ap->model[1] == 'E') ||
	    (ap->model[0] == 'F' && ap->model[1] == 'X'))) {
		bswap (ap->model, sizeof(ap->model));
		bswap (ap->serial, sizeof(ap->serial));
		bswap (ap->revision, sizeof(ap->revision));
	}

	/* Clean up the model name, serial and revision numbers. */
	btrim (ap->model, sizeof(ap->model));
	btrim (ap->serial, sizeof(ap->serial));
	btrim (ap->revision, sizeof(ap->revision));
	return (ap);
}

/*
 * Wait uninterruptibly until controller is not busy and certain
 * status bits are set.
 * The wait is usually short unless it is for the controller to process
 * an entire critical command.
 * Return 1 for (possibly stale) controller errors, -1 for timeout errors,
 * or 0 for no errors.
 */
static int atapi_wait (int port, u_char bits_wanted)
{
	int cnt;
	u_char s;

	/* Wait 5 sec for BUSY deassert. */
	for (cnt=500000; cnt>0; --cnt) {
		s = inb (port + AR_STATUS);
		if (! (s & ARS_BSY))
			break;
		DELAY (10);
	}
	if (cnt <= 0)
		return (-1);
	if (! bits_wanted)
		return (s & ARS_CHECK);

	/* Wait 50 msec for bits wanted. */
	for (cnt=5000; cnt>0; --cnt) {
		s = inb (port + AR_STATUS);
		if ((s & bits_wanted) == bits_wanted)
			return (s & ARS_CHECK);
		DELAY (10);
	}
	return (-1);
}

void atapi_debug (struct atapi *ata, int on)
{
	ata->debug = on;
}

static struct atapicmd *atapi_alloc (struct atapi *ata)
{
	struct atapicmd *ac;

	while (! ata->free)
		tsleep ((caddr_t)ata, PRIBIO, "atacmd", 0);
	ac = ata->free;
	ata->free = ac->next;
	ac->busy = 1;
	return (ac);
}

static void atapi_free (struct atapi *ata, struct atapicmd *ac)
{
	if (! ata->free)
		wakeup ((caddr_t)&ata);
	ac->busy = 0;
	ac->next = ata->free;
	ata->free = ac;
}

/*
 * Add new command request to the end of the queue.
 */
static void atapi_enqueue (struct atapi *ata, struct atapicmd *ac)
{
	ac->next = 0;
	if (ata->tail)
		ata->tail->next = ac;
	else
		ata->queue = ac;
	ata->tail = ac;
}

static void atapi_done (struct atapi *ata)
{
	struct atapicmd *ac = ata->queue;

	if (! ac)
		return; /* cannot happen */

	ata->queue = ac->next;
	if (! ata->queue)
		ata->tail = 0;

	if (ac->callback) {
		(*ac->callback) (ac->cbarg1, ac->cbarg2, ac->count, ac->result);
		atapi_free (ata, ac);
	} else
		wakeup ((caddr_t)ac);
}

/*
 * Start new packet op.  Called from wdstart().
 * Return 1 if op started, and we are waiting for interrupt.
 * Return 0 when idle.
 */
int atapi_start (int ctrlr)
{
	struct atapi *ata = atapitab + ctrlr;
	struct atapicmd *ac;
again:
	ac = ata->queue;
	if (! ac)
		return (0);

	/* Start packet command. */
	if (atapi_start_cmd (ata, ac) < 0) {
		atapi_done (ata);
		goto again;
	}

	if (ata->intrcmd)
		/* Wait for interrupt before sending packet command */
		return (1);

	/* Wait for DRQ. */
	if (atapi_wait_cmd (ata, ac) < 0) {
		atapi_done (ata);
		goto again;
	}

	/* Send packet command. */
	atapi_send_cmd (ata, ac);
	return (1);
}

/*
 * Start new packet op. Returns -1 on errors.
 */
int atapi_start_cmd (struct atapi *ata, struct atapicmd *ac)
{
	ac->result.error = 0;
	ac->result.status = 0;

	outb (ata->port + AR_DRIVE, ac->unit ? ARD_DRIVE1 : ARD_DRIVE0);
	if (atapi_wait (ata->port, 0) < 0) {
		printf ("atapi%d.%d: controller not ready for cmd\n",
			ata->ctrlr, ac->unit);
		ac->result.code = RES_NOTRDY;
		return (-1);
	}

	/* Set up the controller registers. */
	outb (ata->port + AR_FEATURES, 0);
	outb (ata->port + AR_IREASON, 0);
	outb (ata->port + AR_TAG, 0);
	outb (ata->port + AR_CNTLO, ac->count & 0xff);
	outb (ata->port + AR_CNTHI, ac->count >> 8);
	outb (ata->port + AR_COMMAND, ATAPIC_PACKET);

	if (ata->debug)
		printf ("atapi%d.%d: start\n", ata->ctrlr, ac->unit);
	return (0);
}

/*
 * Wait for DRQ before sending packet cmd. Returns -1 on errors.
 */
int atapi_wait_cmd (struct atapi *ata, struct atapicmd *ac)
{
	/* Wait for DRQ from 50 usec to 3 msec for slow devices */
	int cnt = ata->intrcmd ? 10000 : ata->slow ? 3000 : 50;

	for (; cnt>0; cnt-=10) {
		ac->result.status = inb (ata->port + AR_STATUS);
		if (ac->result.status & ARS_DRQ)
			break;
		DELAY (10);
	}
	if (! (ac->result.status & ARS_DRQ)) {
		printf ("atapi%d.%d: no cmd drq\n", ata->ctrlr, ac->unit);
		ac->result.code = RES_NODRQ;
		ac->result.error = inb (ata->port + AR_ERROR);
		return (-1);
	}
	return (0);
}

/*
 * Send packet cmd.
 */
void atapi_send_cmd (struct atapi *ata, struct atapicmd *ac)
{
	outsw (ata->port + AR_DATA, ac->cmd, ata->cmd16 ? 8 : 6);
	if (ata->debug)
		printf ("atapi%d.%d: send cmd %x-%x-%x-%x-%x-%x-%x-%x-%x-%x-%x-%x-%x-%x-%x-%x\n",
			ata->ctrlr, ac->unit, ac->cmd[0], ac->cmd[1],
			ac->cmd[2], ac->cmd[3], ac->cmd[4], ac->cmd[5],
			ac->cmd[6], ac->cmd[7], ac->cmd[8], ac->cmd[9],
			ac->cmd[10], ac->cmd[11], ac->cmd[12],
			ac->cmd[13], ac->cmd[14], ac->cmd[15]);
}

/*
 * Interrupt routine for the controller.  Called from wdintr().
 * Finish the started op, wakeup wait-type commands,
 * run callbacks for callback-type commands, then return.
 * Do not start new op here, it will be done by wdstart,
 * which is called just after us.
 * Return 1 if op continues, and we are waiting for new interrupt.
 * Return 0 when idle.
 */
int atapi_intr (int ctrlr)
{
	struct atapi *ata = atapitab + ctrlr;
	struct atapicmd *ac = ata->queue;

	if (! ac) {
		printf ("atapi%d: stray interrupt\n", ata->ctrlr);
		return (0);
	}
	if (atapi_io (ata, ac) > 0)
		return (1);
	atapi_done (ata);
	return (0);
}

/*
 * Process the i/o phase, transferring the command/data to/from the device.
 * Return 1 if op continues, and we are waiting for new interrupt.
 * Return 0 when idle.
 */
int atapi_io (struct atapi *ata, struct atapicmd *ac)
{
	u_char ireason;
	u_short len, i;

	if (atapi_wait (ata->port, 0) < 0) {
		ac->result.status = inb (ata->port + AR_STATUS);
		ac->result.error = inb (ata->port + AR_ERROR);
		ac->result.code = RES_NOTRDY;
		printf ("atapi%d.%d: controller not ready, status=%b, error=%b\n",
			ata->ctrlr, ac->unit, ac->result.status, ARS_BITS,
			ac->result.error, AER_BITS);
		return (0);
	}

	ac->result.status = inb (ata->port + AR_STATUS);
	ac->result.error = inb (ata->port + AR_ERROR);
	len = inb (ata->port + AR_CNTLO);
	len |= inb (ata->port + AR_CNTHI) << 8;
	ireason = inb (ata->port + AR_IREASON);

	if (ata->debug) {
		printf ("atapi%d.%d: intr ireason=0x%x, len=%d, status=%b, error=%b\n",
			ata->ctrlr, ac->unit, ireason, len,
			ac->result.status, ARS_BITS,
			ac->result.error, AER_BITS);
	}
	switch ((ireason & (ARI_CMD | ARI_IN)) | (ac->result.status & ARS_DRQ)) {
	default:
		printf ("atapi%d.%d: unknown phase\n", ata->ctrlr, ac->unit);
		ac->result.code = RES_ERR;
		break;

	case PHASE_CMDOUT:
		/* Send packet command. */
		if (! (ac->result.status & ARS_DRQ)) {
			printf ("atapi%d.%d: no cmd drq\n",
				ata->ctrlr, ac->unit);
			ac->result.code = RES_NODRQ;
			break;
		}
		atapi_send_cmd (ata, ac);
		return (1);

	case PHASE_DATAOUT:
		/* Write data */
		if (ac->count > 0) {
			printf ("atapi%d.%d: invalid data direction\n",
				ata->ctrlr, ac->unit);
			ac->result.code = RES_INVDIR;
			break;
		}
		if (-ac->count < len) {
			print (("atapi%d.%d: send data underrun, %d bytes left\n",
				ata->ctrlr, ac->unit, -ac->count));
			ac->result.code = RES_UNDERRUN;
			outsw (ata->port + AR_DATA, ac->addr,
				-ac->count / sizeof(short));
			for (i= -ac->count; i<len; i+=sizeof(short))
				outw (ata->port + AR_DATA, 0);
		} else
			outsw (ata->port + AR_DATA, ac->addr,
				len / sizeof(short));
		ac->addr += len;
		ac->count += len;
		return (1);

	case PHASE_DATAIN:
		/* Read data */
		if (ac->count < 0) {
			printf ("atapi%d.%d: invalid data direction\n",
				ata->ctrlr, ac->unit);
			ac->result.code = RES_INVDIR;
			break;
		}
		if (ac->count < len) {
			print (("atapi%d.%d: recv data overrun, %d bytes left\n",
				ata->ctrlr, ac->unit, ac->count));
			ac->result.code = RES_OVERRUN;
			insw (ata->port + AR_DATA, ac->addr,
				ac->count / sizeof(short));
			for (i=ac->count; i<len; i+=sizeof(short))
				inw (ata->port + AR_DATA);
		} else
			insw (ata->port + AR_DATA, ac->addr,
				len / sizeof(short));
		ac->addr += len;
		ac->count -= len;
		return (1);

	case PHASE_ABORTED:
	case PHASE_COMPLETED:
		if (ac->result.status & (ARS_CHECK | ARS_DF))
			ac->result.code = RES_ERR;
		else if (ac->count < 0) {
			print (("atapi%d.%d: send data overrun, %d bytes left\n",
				ata->ctrlr, ac->unit, -ac->count));
			ac->result.code = RES_OVERRUN;
		} else if (ac->count > 0) {
			print (("atapi%d.%d: recv data underrun, %d bytes left\n",
				ata->ctrlr, ac->unit, ac->count));
			ac->result.code = RES_UNDERRUN;
			bzero (ac->addr, ac->count);
		} else
			ac->result.code = RES_OK;
		break;
	}
	return (0);
}

/*
 * Queue new packet request, then call wdstart().
 * Called on splbio().
 */
void atapi_request_callback (struct atapi *ata, int unit,
	u_char cmd, u_char a1, u_char a2, u_char a3, u_char a4,
	u_char a5, u_char a6, u_char a7, u_char a8, u_char a9,
	u_char a10, u_char a11, u_char a12, u_char a13, u_char a14, u_char a15,
	char *addr, int count, void (*done)(), void *x, void *y)
{
	struct atapicmd *ac;

	ac = atapi_alloc (ata);
	ac->cmd[0] = cmd;       ac->cmd[1] = a1;
	ac->cmd[2] = a2;        ac->cmd[3] = a3;
	ac->cmd[4] = a4;        ac->cmd[5] = a5;
	ac->cmd[6] = a6;        ac->cmd[7] = a7;
	ac->cmd[8] = a8;        ac->cmd[9] = a9;
	ac->cmd[10] = a10;      ac->cmd[11] = a11;
	ac->cmd[12] = a12;      ac->cmd[13] = a13;
	ac->cmd[14] = a14;      ac->cmd[15] = a15;
	ac->unit = unit;
	ac->addr = addr;
	ac->count = count;
	ac->callback = done;
	ac->cbarg1 = x;
	ac->cbarg2 = y;

	if (ata->debug)
		printf ("atapi%d.%d: req cb %x-%x-%x-%x-%x-%x-%x-%x-%x-%x-%x-%x-%x-%x-%x-%x len=%d\n",
			ata->ctrlr, ac->unit, ac->cmd[0], ac->cmd[1],
			ac->cmd[2], ac->cmd[3], ac->cmd[4], ac->cmd[5],
			ac->cmd[6], ac->cmd[7], ac->cmd[8], ac->cmd[9],
			ac->cmd[10], ac->cmd[11], ac->cmd[12],
			ac->cmd[13], ac->cmd[14], ac->cmd[15], count);
	atapi_enqueue (ata, ac);
	wdstart (ata->ctrlr);
}

/*
 * Queue new packet request, then call wdstart().
 * Wait until the request is finished.
 * Called on spl0().
 * Return atapi error.
 * Buffer pointed to by *addr should be placed in core memory, not in stack!
 */
struct atapires atapi_request_wait (struct atapi *ata, int unit,
	u_char cmd, u_char a1, u_char a2, u_char a3, u_char a4,
	u_char a5, u_char a6, u_char a7, u_char a8, u_char a9,
	u_char a10, u_char a11, u_char a12, u_char a13, u_char a14, u_char a15,
	char *addr, int count)
{
	struct atapicmd *ac;
	int x = splbio ();
	struct atapires result;

	ac = atapi_alloc (ata);
	ac->cmd[0] = cmd;       ac->cmd[1] = a1;
	ac->cmd[2] = a2;        ac->cmd[3] = a3;
	ac->cmd[4] = a4;        ac->cmd[5] = a5;
	ac->cmd[6] = a6;        ac->cmd[7] = a7;
	ac->cmd[8] = a8;        ac->cmd[9] = a9;
	ac->cmd[10] = a10;      ac->cmd[11] = a11;
	ac->cmd[12] = a12;      ac->cmd[13] = a13;
	ac->cmd[14] = a14;      ac->cmd[15] = a15;
	ac->unit = unit;
	ac->addr = addr;
	ac->count = count;
	ac->callback = 0;
	ac->cbarg1 = 0;
	ac->cbarg2 = 0;

	if (ata->debug)
		printf ("atapi%d.%d: req w %x-%x-%x-%x-%x-%x-%x-%x-%x-%x-%x-%x-%x-%x-%x-%x len=%d\n",
			ata->ctrlr, ac->unit, ac->cmd[0], ac->cmd[1],
			ac->cmd[2], ac->cmd[3], ac->cmd[4], ac->cmd[5],
			ac->cmd[6], ac->cmd[7], ac->cmd[8], ac->cmd[9],
			ac->cmd[10], ac->cmd[11], ac->cmd[12],
			ac->cmd[13], ac->cmd[14], ac->cmd[15], count);
	atapi_enqueue (ata, ac);
	wdstart (ata->ctrlr);
	tsleep ((caddr_t)ac, PRIBIO, "atareq", 0);

	result = ac->result;
	atapi_free (ata, ac);
	splx (x);
	return (result);
}

/*
 * Perform a packet command on the device.
 * Should be called on splbio().
 * Return atapi error.
 */
struct atapires atapi_request_immediate (struct atapi *ata, int unit,
	u_char cmd, u_char a1, u_char a2, u_char a3, u_char a4,
	u_char a5, u_char a6, u_char a7, u_char a8, u_char a9,
	u_char a10, u_char a11, u_char a12, u_char a13, u_char a14, u_char a15,
	char *addr, int count)
{
	struct atapicmd cmdbuf, *ac = &cmdbuf;
	int cnt;

	ac->cmd[0] = cmd;       ac->cmd[1] = a1;
	ac->cmd[2] = a2;        ac->cmd[3] = a3;
	ac->cmd[4] = a4;        ac->cmd[5] = a5;
	ac->cmd[6] = a6;        ac->cmd[7] = a7;
	ac->cmd[8] = a8;        ac->cmd[9] = a9;
	ac->cmd[10] = a10;      ac->cmd[11] = a11;
	ac->cmd[12] = a12;      ac->cmd[13] = a13;
	ac->cmd[14] = a14;      ac->cmd[15] = a15;
	ac->unit = unit;
	ac->addr = addr;
	ac->count = count;
	ac->callback = 0;
	ac->cbarg1 = 0;
	ac->cbarg2 = 0;

	if (ata->debug)
		printf ("atapi%d.%d: req im %x-%x-%x-%x-%x-%x-%x-%x-%x-%x-%x-%x-%x-%x-%x-%x len=%d\n",
			ata->ctrlr, ac->unit, ac->cmd[0], ac->cmd[1],
			ac->cmd[2], ac->cmd[3], ac->cmd[4], ac->cmd[5],
			ac->cmd[6], ac->cmd[7], ac->cmd[8], ac->cmd[9],
			ac->cmd[10], ac->cmd[11], ac->cmd[12],
			ac->cmd[13], ac->cmd[14], ac->cmd[15], count);

	/* Start packet command, wait for DRQ. */
	if (atapi_start_cmd (ata, ac) >= 0 && atapi_wait_cmd (ata, ac) >= 0) {
		/* Send packet command. */
		atapi_send_cmd (ata, ac);

		/* Wait for data i/o phase. */
		for (cnt=20000; cnt>0; --cnt)
			if (((inb (ata->port + AR_IREASON) & (ARI_CMD | ARI_IN)) |
			    (inb (ata->port + AR_STATUS) & ARS_DRQ)) != PHASE_CMDOUT)
				break;

		/* Do all needed i/o. */
		while (atapi_io (ata, ac))
			/* Wait for DRQ deassert. */
			for (cnt=2000; cnt>0; --cnt)
				if (! (inb (ata->port + AR_STATUS) & ARS_DRQ))
					break;
	}
	return (ac->result);
}
#endif /* NWDC && ATAPI */
