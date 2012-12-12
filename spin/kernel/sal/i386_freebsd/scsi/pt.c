/*
 * pt: Processor Type driver.
 *
 * Copyright (C) 1995, HD Associates, Inc.
 * PO Box 276
 * Pepperell, MA 01463
 * 508 433 5266
 * dufault@hda.com
 *
 * This code is contributed to the University of California at Berkeley:
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
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *      $Id: pt.c,v 1.2 1997/01/31 16:02:20 mef Exp $
 */

/*
 * XXX dufault@hda.com: We need the "kern devconf" stuff, but I'm not
 *     going to add it until it is done in a simple way that provides
 *     base behavior in scsi_driver.c
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/buf.h>
#include <sys/proc.h>
#include <scsi/scsi_all.h>
#include <scsi/scsiconf.h>

struct scsi_data {
	struct buf *buf_queue;		/* the queue of pending IO operations */
};

void ptstart(u_int32_t unit, u_int32_t flags);
void pt_strategy(struct buf *bp, struct scsi_link *sc_link);
int	pt_sense(struct scsi_xfer *scsi_xfer);

SCSI_DEVICE_ENTRIES(pt)

struct scsi_device pt_switch =
{
    pt_sense,
    ptstart,			/* we have a queue, and this is how we service it */
    NULL,
    NULL,
    "pt",
    0,
	{0, 0},
	SDEV_ONCE_ONLY,	/* Only one open allowed */
	0,
	"Processor",
	ptopen,
    sizeof(struct scsi_data),
	T_PROCESSOR,
	0,
	0,
	0,
	0,
	0,
	pt_strategy,
};
/*
 * ptstart looks to see if there is a buf waiting for the device
 * and that the device is not already busy. If both are true,
 * It dequeues the buf and creates a scsi command to perform the
 * transfer required. The transfer request will call scsi_done
 * on completion, which will in turn call this routine again
 * so that the next queued transfer is performed.
 * The bufs are queued by the strategy routine (ptstrategy)
 *
 * This routine is also called after other non-queued requests
 * have been made of the scsi driver, to ensure that the queue
 * continues to be drained.
 * ptstart() is called at splbio
 */
void
ptstart(unit, flags)
	u_int32_t	unit;
	u_int32_t flags;
{
	struct scsi_link *sc_link = SCSI_LINK(&pt_switch, unit);
	struct scsi_data *pt = sc_link->sd;
	register struct buf *bp = 0;
	struct
	{
#define PROCESSOR_SEND 0x0A
#define PROCESSOR_RECEIVE 0x08
		u_char	op_code;
		u_char	byte2;
		u_char	len[3];
		u_char	control;
	} cmd;


	SC_DEBUG(sc_link, SDEV_DB2, ("ptstart "));
	/*
	 * See if there is a buf to do and we are not already
	 * doing one
	 */
	while (sc_link->opennings != 0) {

		/* if a special awaits, let it proceed first */
		if (sc_link->flags & SDEV_WAITING) {
			sc_link->flags &= ~SDEV_WAITING;
			wakeup((caddr_t)sc_link);
			return;
		}
		if ((bp = pt->buf_queue) == NULL) {
			return;	/* no work to bother with */
		}
		pt->buf_queue = bp->b_actf;

		/*
		 *  Fill out the scsi command
		 */
		bzero(&cmd, sizeof(cmd));
		if ((bp->b_flags & B_READ) == B_WRITE) {
			cmd.op_code = PROCESSOR_SEND;
			flags |= SCSI_DATA_OUT;
		} else {
			cmd.op_code = PROCESSOR_RECEIVE;
			flags |= SCSI_DATA_IN;
		}

		scsi_uto3b(bp->b_bcount, cmd.len);
		/*
		 * go ask the adapter to do all this for us
		 */
		if (scsi_scsi_cmd(sc_link,
			(struct scsi_generic *) &cmd,
			sizeof(cmd),
			(u_char *) bp->b_un.b_addr,
			bp->b_bcount,
			0,
			10000,
			bp,
			flags) == SUCCESSFULLY_QUEUED) {
		} else {
			printf("pt%ld: oops not queued\n", unit);
			bp->b_flags |= B_ERROR;
			bp->b_error = EIO;
			biodone(bp);
		}
	} /* go back and see if we can cram more work in.. */
}

void
pt_strategy(struct buf *bp, struct scsi_link *sc_link)
{
	struct buf **dp;
	unsigned char unit;
	u_int32_t opri;
	struct scsi_data *pt;

	unit = minor((bp->b_dev));
	pt = sc_link->sd;

	opri = splbio();

	/*
	 * Use a bounce buffer if necessary
	 */
#ifdef BOUNCE_BUFFERS
	if (sc_link->flags & SDEV_BOUNCE)
		vm_bounce_alloc(bp);
#endif

	/*
	 * Place it in the queue of activities for this tape
	 * at the end (a bit silly because we only have one user..
	 * (but it could fork() ))
	 */
	dp = &(pt->buf_queue);
	while (*dp) {
		dp = &((*dp)->b_actf);
	}
	*dp = bp;
	bp->b_actf = NULL;

	/*
	 * Tell the device to get going on the transfer if it's
	 * not doing anything, otherwise just wait for completion
	 * (All a bit silly if we're only allowing 1 open but..)
	 */
	ptstart(unit, 0);

	splx(opri);
	return;
}

/*
 * sense handler: Called to determine what to do when the
 * device returns a CHECK CONDITION.
 *
 * For the processor type devices we try to handle the "info" field.
 */

int pt_sense(struct scsi_xfer *xs)
{
	struct scsi_sense_data *sense = &(xs->sense);
	struct buf *bp;

	long resid;

	if ((sense->error_code & SSD_ERRCODE_VALID) == 0 ||
	(sense->ext.extended.flags & SSD_ILI) == 0) {
		return SCSIRET_CONTINUE;	/* let the default handler handle it */
	}

	resid = ntohl(*((int32_t *) sense->ext.extended.info));

 	bp = xs->bp;

	if (resid < 0) {
		/* It synthesized data in order to fill our request.
		 * Move resid back to cover this.
		 */
		xs->resid = -resid;
		xs->flags |= SCSI_RESID_VALID;
		return 0;
	}
	else {
		/* It wanted to send more data.  We can't really do anything
		 * about this.
		 */
		return SCSIRET_CONTINUE;
	}
}
