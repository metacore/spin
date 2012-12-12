#include <sys/param.h>
#include <sys/systm.h>
#include <sys/kernel.h>
#include <sys/proc.h>
#include <sys/types.h>
#include <sys/buf.h>
#include <sys/conf.h>
#include <sys/devconf.h>
#include <spincore/src/sal/OpenArray.h>

#define ASSERT(x)    {if((x) == 0) { printf("Assertion failed in %s:%d\n", __FILE__, __LINE__); }}

#define MIN(a,b) ((a>b)?(b):(a))

/*
 * This file requires that the following symbols be exported from the SAL
 * layer:
 * 
 * DATA: bdevsw, curproc, dc_list
 * TEXT: geteblk, brelse, biowait, sprintf
 */

blkdev_open(dev_t id)
{
    int m = major(id);
    return (bdevsw[m].d_open)(id, 0, 0, curproc);
}

blkdev_close(dev_t id)
{
    int m = major(id);
    return (bdevsw[m].d_close)(id, 0, 0, curproc);
}

blkdev_read(dev_t id, struct openarray *data, unsigned int offset, 
	    unsigned int *bytes)
{
    struct buf *bp;
    int m = major(id);
    int error;
    int pos = 0;
#ifdef SPIN
    int savedcount;
#endif

    bp = geteblk(MAXBSIZE);
    ASSERT(bp != NULL);

    while(pos < *bytes) {
	bp->b_proc = NULL;
	bp->b_flags = B_BUSY | B_READ;
	bp->b_bcount = MIN(*bytes-pos, MAXBSIZE);
	bp->b_dev = id;
	bp->b_blkno = (offset+pos)/DEV_BSIZE;
	bp->b_iodone = NULL;

#ifdef SPIN
	/* XXX. i386_freebsd/scsi/sd.c::sd_strategy() assumes b_bcount is
	   multiple of DEV_BSIZE (512).  This code is SLOW!  Better to  
	   check only for the last partial block.  */
	savedcount = bp->b_bcount;
	if (bp->b_bcount % DEV_BSIZE != 0) {
	    /* should be <= MAXBSIZE */
	    bp->b_bcount =  ((bp->b_bcount/DEV_BSIZE)+1) * DEV_BSIZE; 
	}
#endif
    
	(bdevsw[m].d_strategy)(bp);
	
	if(error = biowait(bp)) {
	    brelse(bp);
	    *bytes = pos;
	    return error;
	}

#ifdef SPIN
	bcopy(bp->b_un.b_addr, data->start + pos, savedcount);
#else
	bcopy(bp->b_un.b_addr, data->start + pos, bp->b_bcount);
#endif
	pos += bp->b_bcount;
    }

    brelse(bp);
    return 0;
}

blkdev_write(dev_t id, struct openarray *data, unsigned int offset, 
	     unsigned int *bytes)
{
    struct buf *bp;
    int m = major(id);
    int error;
    int pos = 0;
#ifdef SPIN
    int saved_bcount;
#endif

    bp = geteblk(MAXBSIZE);
    ASSERT(bp != NULL);

    while(pos < *bytes) {

#ifndef SPIN
	bcopy(data->start + pos, bp->b_un.b_addr, bp->b_bcount);
#endif

	bp->b_proc = NULL;
	bp->b_flags = B_BUSY | B_WRITE;
	bp->b_bcount = MIN(*bytes-pos, MAXBSIZE);
	bp->b_dev = id;
	bp->b_blkno = (offset+pos)/DEV_BSIZE;
	bp->b_iodone = NULL;

#ifdef SPIN
	/* XXX. i386_freebsd/scsi/sd.c::sd_strategy() assumes b_bcount is
	 * multiple of DEV_BSIZE (512). 
	 */
	if (bp->b_bcount % DEV_BSIZE != 0) {
	    saved_bcount = bp->b_bcount;
	    /* should be <= MAXBSIZE */
	    bp->b_bcount =  ((bp->b_bcount/DEV_BSIZE)+1) * DEV_BSIZE; 

	    /* read the block into bp->b_un.b_addr then copy data. 
	     * the write it to disk.
	     */
	    bp->b_flags = B_BUSY | B_READ;
	    (bdevsw[m].d_strategy)(bp);
	
	    if(error = biowait(bp)) {
		brelse(bp);
		*bytes = pos;
		return error;
	    }
	    bcopy(data->start + pos, bp->b_un.b_addr, saved_bcount);

	    /* reset bp */
	    bp->b_proc = NULL;
	    bp->b_flags = B_BUSY | B_WRITE;
	    bp->b_dev = id;
	    bp->b_blkno = (offset+pos)/DEV_BSIZE;
	    bp->b_iodone = NULL;
	} else {
	    bcopy(data->start + pos, bp->b_un.b_addr, bp->b_bcount);
	}
#endif
    
	(bdevsw[m].d_strategy)(bp);
	
	if(error = biowait(bp)) {
	    brelse(bp);
	    *bytes = pos;
	    return error;
	}

	pos += bp->b_bcount;
    }

    brelse(bp);
    return 0;
}

typedef void (*register_dev_t)(char *str, dev_t id);

extern struct kern_devconf *dc_list;

void blkdev_register(register_dev_t register_dev)
{
    struct kern_devconf *kdc;
    int major;
    int minor;
    int unit;
    dev_t id;
    char name[20];
    int i, j;
    int capacity;
    
    kdc = dc_list;
    
    while(kdc != NULL) {

	major = find_major(kdc->kdc_name);
	unit = kdc->kdc_unit;

	/*
	 * There are three sorts of block devices that need to be
	 * registered in different ways: hard disk drives, floppy
	 * disk drives, and CD-ROM drives.
	 */
	
	/* 
	 * First, the hard disk drives.
	 */
	if(major == find_major("wd") || major == find_major("sd")
	   || major == find_major("vn") | major == find_major("od")) {

	    /* register a name for the whole disk in the form wd0 */
	    sprintf(name, "%s%d", kdc->kdc_name, unit);
	    minor = find_minor(0, unit, 0, 0);
	    id = makedev(major, minor);
	    (*register_dev)(name, id);

	    /* register names for each partition of the whole disk in the */
	    /* form wd0a, wd0b, ... */
	    for(i = 0; i < 8; i++) {
		sprintf(name, "%s%d%c", kdc->kdc_name, unit, 'a' + i);
		minor = find_minor(0, unit, -1, i);
		id = makedev(major, minor);
		(*register_dev)(name, id);
	    }
	
	    /* register names for each slice of the disk in the form wd0s1, */
	    /* wd0s2, ... */
	    for(i = 1; i <= 8; i++) {
		sprintf(name, "%s%ds%d", kdc->kdc_name, unit, i);
		minor = find_minor(0, unit, i, 2);
		id = makedev(major, minor);
		(*register_dev)(name, id);
		
		/* now register names for each partition within the slice in */
		/* the form wd0s1a, wd0s1b, ... */
		for(j = 0; j < 8; j++) {
		    sprintf(name, "%s%ds%d%c", kdc->kdc_name, unit, i,
			    'a' + j);
		    minor = find_minor(0, unit, i, j);
		    id = makedev(major, minor);
		    (*register_dev)(name, id);
		}
	    }
	}

	/*
	 * Next, the floppy disk drives.
	 */
	if(major == find_major("fd")) {

	    /* register a name for the whole disk in the form fd0 */
	    sprintf(name, "%s%d", kdc->kdc_name, unit);
	    minor = unit * 64;
	    id = makedev(major, minor);
	    (*register_dev)(name, id);

	    /* register names for each partition of the whole disk in the */
	    /* form fd0a, fd0b, ... These are simply aliases for the whole */
	    /* disk since you can't partition floppies. */
	    for(i = 0; i < 8; i++) {
		sprintf(name, "%s%d%c", kdc->kdc_name, unit, 'a' + i);
		minor = unit * 64;
		id = makedev(major, minor);
		(*register_dev)(name, id);
	    }

	    /* register a name for each disk capacity, in KB, in the form */
	    /* fd0.360, fd0.720, ... */
	    for(i = 1; i <= 8; i++) {
		switch(i) {
		  case 1: capacity = 1720; break;
		  case 2: capacity = 1480; break;
		  case 3: capacity = 1440; break;
		  case 4: capacity = 1200; break;
		  case 5: capacity =  820; break;
		  case 6: capacity =  800; break;
		  case 7: capacity =  720; break;
		  case 8: capacity =  360; break;
		}
		
		sprintf(name, "fd%d.%d", unit, capacity);
		minor = unit * 64 + i;
		id = makedev(major, minor);
		(*register_dev)(name, id);    
	    }
	}

	/*
	 * Finally, the CD-ROM drives.
	 */
	if(major == find_major("cd") || major == find_major("mcd")
	   || major == find_major("scd") | major == find_major("pcd") 
	   || major == find_major("wcd")) {

	    /* register a name for the whole disk in the form cd0 */
	    sprintf(name, "%s%d", kdc->kdc_name, unit);
	    minor = unit * 8 + 2;
	    id = makedev(major, minor);
	    (*register_dev)(name, id);
    
	    /* register names for the first and third partitions in the form */
	    /* cd0a, cd0c */
	    sprintf(name, "%s%da", kdc->kdc_name, unit);
	    minor = unit * 8;
	    id = makedev(major, minor);
	    (*register_dev)(name, id);
	    
	    sprintf(name, "%s%dac", kdc->kdc_name, unit);
	    minor = unit * 8 + 2;
	    id = makedev(major, minor);
	    (*register_dev)(name, id);
	}

	kdc = kdc->kdc_next;
    }
}

int find_major(char *name)
{
    /* the list is taken from i386/conf/devices.i386 */
    if(!strcmp(name, "wd"))
	return 0;
    if(!strcmp(name, "dk"))
	return 1;
    if(!strcmp(name, "fd"))
	return 2;
    if(!strcmp(name, "wt"))
	return 3;
    if(!strcmp(name, "sd"))
	return 4;
    if(!strcmp(name, "st"))
	return 5;
    if(!strcmp(name, "cd"))
	return 6;
    if(!strcmp(name, "mcd"))
	return 7;
    if(!strcmp(name, "vn"))
	return 15;
    if(!strcmp(name, "scd"))
	return 16;
    if(!strcmp(name, "pcd"))
	return 17;
    if(!strcmp(name, "wcd"))
	return 19;
    if(!strcmp(name, "od"))
	return 20;

    return -1;
}

int find_minor(int type, int unit, int slice, int partition)
{ 
    return 32 * 65536 * type + 8 * unit + 65536 * (slice+1) + partition;
} 
