/*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 21-Sep-95  Stefan Savage (savage) at the University of Washington
 *	We now configure cam from SAL configure, so I commented out the
 *	configuration code here
 *
 */
/*
 * This file contains just enough of the of several DEC
 * I/O routines to allow CAM to function in the SPIN
 * environment.
 */

 
#include <sys/param.h>
#include <sys/systm.h>
#include <sys/user.h>
#include <sys/buf.h>
#include <sys/disklabel.h>
#include <sys/conf.h>
#include <sys/proc.h>
#include <sys/uio.h>
#include <ufs/fs_proto.h>
#include <kern/task.h>
#include <mach/vm_param.h>
#include <vm/vm_map.h>
#ifndef SPIN
#include <svr4/sar.h>
#else
#ifdef current_thread
#undef current_thread
#endif
#endif

/*
 * Raw I/O. The arguments are
 *	The strategy routine for the device
 *	A buffer, which will always be a special buffer
 *	  header owned exclusively by the device for this purpose
 *
 *	The device number
 *	Read/write flags (B_READ/B_WRITE, B_WRITEV)
 * Essentially all the work is computing physical addresses and
 * validating them.
 * If the user has the proper access privilidges, the process is
 * marked 'delayed unlock' and the pages involved in the I/O are
 * faulted and locked. After the completion of the I/O, the above pages
 * are unlocked.
 */
physio(strat, bp, dev, rw, mincnt, uio)
	int (*strat)(); 
	register struct buf *bp;
	dev_t dev;
	int rw;
	u_int (*mincnt)();
	struct uio *uio;
{
	register struct iovec *iov;
	register int requested, done;
	char *a;
	int error, once;
	vm_offset_t start, end;

	/*
	 * Check for a special AIO request. If so, service and return.
	 */
	if (uio->uio_rw == UIO_AIORW) {
		register struct iovec *vec = uio->uio_iov;
		vec->iov_base = (caddr_t) strat;
		(++vec)->iov_base = (caddr_t) mincnt;
		return EAIO;
	}

	LASSERT(BUF_LOCK_HOLDER(bp));

#ifndef SPIN
        if (rw)
		sar_update(ts_phread, 1, TRUE); /* SAR */
        else
                sar_update(ts_phwrite, 1, TRUE); /* SAR */
#endif
	for (error = 0; uio->uio_iovcnt; uio->uio_iov++, uio->uio_iovcnt--) {
		iov = uio->uio_iov;
		if (uio->uio_segflg == UIO_USERSPACE &&
		    !useracc(iov->iov_base, (u_int)iov->iov_len,
		    (rw & B_READ) ? B_WRITE : B_READ)) {
			error = EFAULT;
			break;
		}
		bp->b_proc = current_thread();
		bp->b_error = 0;
		bp->b_un.b_addr = iov->iov_base;
		while (iov->iov_len > 0) {
			LASSERT(BUF_LOCK_HOLDER(bp));
			bp->b_flags = B_PHYS | B_RAW | rw;
			event_clear(&bp->b_iocomplete);
			bp->b_dev = dev;
			bp->b_blkno = btodb(uio->uio_offset);
			bp->b_bcount = iov->iov_len;
			/*
			 * Set once to zero... this will allow 
			 * One request for each IOV for tapes.
			 */
			once = 0;
			(*mincnt)(bp);

			/*
			 * For devices that require only one time
			 * around while loop (TAPES ) check the 
			 * results from call to drivers minphys()
			 */
			if(( bp->b_flags & B_ERROR) != NULL){
			    /*
			     * If driver's minphys() doesn't want
			     * anthing to go it sets resid == b_bcount
			     * and b_error to proper value. (TAPES)
			     */
			    if( bp->b_bcount == bp->b_resid ){
				error = bp->b_error;
				break;
			    }
			    else {
				/*
				 * Driver has indicated it wants the loop
				 * done only once. Clear error flag.
				 */
				bp->b_flags &= ~B_ERROR;
				once = 1;
			    }
			}

			requested = bp->b_bcount;
			a = bp->b_un.b_addr;
			start = trunc_page(a);
			end = round_page(a + requested);
			/* Assume already wired if sys space */
			if (uio->uio_segflg != UIO_SYSSPACE &&
			    vm_map_pageable(current_task()->map, start, end,
				(rw & B_READ) ? VM_PROT_WRITE : VM_PROT_READ)
			    != KERN_SUCCESS) {
				error = EFAULT;
				break;
			}
#ifdef SPIN
			do { int spl = splextreme();
#endif /* SPIN */
			bp->b_iocomplete.ev_event = 0;
			(*strat)(bp);
			/*
			 * Hmmm... is there a race between strat and
			 * biowait.  Not in OSF of course.
			 */
			error = biowait(bp);
#ifdef SPIN
		        splx(spl);} while(0);
#endif /* SPIN */
			LASSERT(BUF_LOCK_HOLDER(bp));

			if (uio->uio_segflg == UIO_USERSPACE)
				(void)vm_map_pageable(current_task()->map,
						start, end, VM_PROT_NONE);

			done = bp->b_bcount - bp->b_resid;
			bp->b_un.b_addr += done;
			iov->iov_len -= done;
			uio->uio_resid -= done;
			uio->uio_offset += done;
			/*
			 * Once indicates only once thru while loop
			 */
			if ((done < requested) || (done == 0) || error || once)
				break;
		}
		bp->b_flags &= ~(B_PHYS | B_RAW);
		if (( done < requested ) || (done == 0) || error)
			break;
	}
	/* Note: rwuio will cause partial transfers to 
	 * be reported as "success", even though we return an error.
	 */
	return (error);
}

/*
 * Wait for I/O completion on the buffer; return errors
 * to the user.
 */
biowait(bp)
    register struct buf *bp;
{
    LASSERT(BUF_LOCK_HOLDER(bp));
/*  sar_bio_wait; */
/*    (void) event_wait(&bp->b_iocomplete, FALSE, 0); */
/*  sar_bio_unwait; */

#ifdef sleep
#undef sleep
#endif
    if ( !bp->b_iocomplete.ev_event ) {
	sleep(&bp->b_iocomplete);
    }
    
    /*
     * Pick up the device's error number and pass it to the user;
     * if there is an error but the number is 0 set a generalized code.
     */
    if ((bp->b_flags & B_ERROR) == 0)
        return (0);
    if (bp->b_error)
        return (bp->b_error);
    return (EIO);
}

void biodone(bp)
    register struct buf *bp;
{
    int s, wakeup = 0;
    register int flags;
    
    if ( bp == (struct buf *) 0 ) {
	panic("biodone on bp == 0");
    }
    if ( bp->b_iocomplete.ev_event ) {
	panic("dup biodone");
    }
    flags = bp->b_flags;
    
    if (bp->b_iodone) {
	void (*f)() = bp->b_iodone;
	bp->b_iodone = NULL;
	(*f)(bp);
	return;
    }
    
    /*
     * We should *not* be doing any asynch operations at this
     * point.
     */

    if ( bp->b_flags & B_ASYNC ) {
	/*
	 * This buf must have been previously given away. We
	 * accept ownership here.
	 *
	 event_post(&bp->b_iocomplete);
	 BUF_ACCEPT(bp);
	 brelse(bp);
	 */
	panic("ASYNC operation found in biodone");
    }
    else {
	bp->b_iocomplete.ev_event = 1;
	thread_wakeup(&bp->b_iocomplete);
    }
}

/*
 * Attempt to read a disk label from a device
 * using the indicated stategy routine.
 * The label must be partly set up before this:
 * secpercyl and anything required in the strategy routine
 * (e.g., sector size) must be filled in before calling us.
 * Returns null on success and an error string on failure.
 */
char *
readdisklabel(dev, strat, lp)
    dev_t dev;
    int (*strat)();
    register struct disklabel *lp;
{
    register struct buf *bp;
    struct disklabel *dlp;
    char *msg = NULL;

    if (lp->d_secperunit == 0)
        lp->d_secperunit = 0x1fffffff;
    lp->d_npartitions = 1;
    if (lp->d_partitions[0].p_size == 0)
        lp->d_partitions[0].p_size = 0x1fffffff;
    lp->d_partitions[0].p_offset = 0;

    bp = geteblk((int)lp->d_secsize);
    bp->b_dev = dev;
    bp->b_blkno = LABELSECTOR;
    bp->b_bcount = lp->d_secsize;
    bp->b_flags = B_READ;
    bp->b_cylin = LABELSECTOR / lp->d_secpercyl;
    (*strat)(bp);
    if (biowait(bp)) {
        msg = "I/O error";
    } else 
	for (dlp = (struct disklabel *)bp->b_un.b_addr;
               dlp <= (struct disklabel *)
		 (bp->b_un.b_addr+DEV_BSIZE-sizeof(*dlp));
	       dlp = (struct disklabel *)((char *)dlp + sizeof(int))) {
	    if (dlp->d_magic != DISKMAGIC || dlp->d_magic2 != DISKMAGIC) {
		if (msg == NULL) msg = "no disk label";
	    }
	    else if (dlp->d_npartitions > MAXPARTITIONS || 
			dkcksum(dlp) != 0) msg = "disk label corrupted";
		 else {
		    *lp = *dlp;
		    msg = NULL;
		    break;
		 }
	}
    bp->b_flags = B_INVAL | B_AGE;
    brelse(bp);
    return(msg);
}

/*
 * Compute checksum for disk label.
 */
dkcksum(lp)
    register struct disklabel *lp;
{
    register u_short *start, *end;
    register u_short sum = 0;

    start = (u_short *)lp;
    end = (u_short *)&lp->d_partitions[lp->d_npartitions];
    while (start < end)
        sum ^= *start++;
    return(sum);
}

/*
 * Cut rate, filthy, dirty routine to get a bp and a buffer for it.
 * Note that these routines will probably only be called when you
 * first open/mount a disk.  Otherwise all the buffering will be
 * done by the CAM routines for ordinary reads/writes.
 */
struct buf *free_bp_list;

struct buf *geteblk(size)
    int size;
{
    register struct buf *bp;
    caddr_t buffer;
    
    if (size > MAXBSIZE)
	panic("geteblk: size too big");
    if ( free_bp_list == (struct buf *) 0 ) {
	bp = (struct buf *) kalloc(sizeof(struct buf));
	if ( !bp ) panic("geteblk: can't allocate struct buf");
	buffer = (caddr_t)0;
    }
    else {
	bp = free_bp_list;
	free_bp_list = free_bp_list->b_forw;
	buffer = bp->b_un.b_addr;
    }
    bzero(bp,sizeof(struct buf));
    bp->b_flags = B_INVAL;
    bp->b_error = 0;
    bp->b_resid = 0;
    bp->b_bcount = 0;
    if ( ! buffer ) {
	/* 
	 * Our loutish behaviour is to always allocate MAXBSIZE.
	 */
	bp->b_un.b_addr = (caddr_t)kalloc(MAXBSIZE);
	if ( !bp->b_un.b_addr ) panic("geteblk: can't alloc buffer");
    }
    else bp->b_un.b_addr = buffer;
    bp->b_bcount = size;
    return(bp);
}

brelse(bp)
    struct buf *bp;
{
    /* They all look the same to us, just put it on the free list */
    bp->b_forw = free_bp_list;
    free_bp_list = bp;
}

#ifdef	ibmrt
#include <sc.h>
#else
#define NSCC	0
#endif

#if	NSCC > 0
#define MAXPHYS	(64 * 512)	/* don't increase beyond NDMAXIO */
#else
#define MAXPHYS	(63 * 1024)
#endif

#if	EXL
#undef  MAXPHYS
#define MAXPHYS (32 * 4096)     /* EXL can take 32 pages for dma */
#endif

u_int
minphys(bp)
	struct buf *bp;
{
	LASSERT(BUF_LOCK_HOLDER(bp));
	if (bp->b_bcount > MAXPHYS)
		bp->b_bcount = MAXPHYS;
}
