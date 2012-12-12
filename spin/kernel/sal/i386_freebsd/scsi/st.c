/*
 * Written by Julian Elischer (julian@tfs.com)(now julian@DIALix.oz.au)
 * for TRW Financial Systems for use under the MACH(2.5) operating system.
 *
 * TRW Financial Systems, in accordance with their agreement with Carnegie
 * Mellon University, makes this software available to CMU to distribute
 * or use in any manner that they see fit as long as this message is kept with
 * the software. For this reason TFS also grants any other persons or
 * organisations permission to use or modify this software.
 *
 * TFS supplies this software to be publicly redistributed
 * on the understanding that TFS is not responsible for the correct
 * functioning of this software in any circumstances.
 *
 * $Id: st.c,v 1.2 1997/01/31 16:02:28 mef Exp $
 */

/*
 * Ported to run under 386BSD by Julian Elischer (julian@tfs.com) Sept 1992
 * major changes by Julian Elischer (julian@jules.dialix.oz.au) May 1993
 */

/*
 * To do:
 * work out some better way of guessing what a good timeout is going
 * to be depending on whether we expect to retension or not.
 *
 */

#include <sys/types.h>

#include <sys/param.h>
#include <sys/systm.h>

#include <sys/fcntl.h>
#include <sys/errno.h>
#include <sys/ioctl.h>
#include <sys/malloc.h>
#include <sys/buf.h>
#include <sys/proc.h>
#include <sys/user.h>
#include <sys/mtio.h>

#include <scsi/scsi_all.h>
#include <scsi/scsi_tape.h>
#include <scsi/scsiconf.h>
#include <sys/devconf.h>

u_int32_t ststrats, stqueues;

/* Defines for device specific stuff */
#define		PAGE_0_SENSE_DATA_SIZE	12
#define DEF_FIXED_BSIZE  512
#define	ST_RETRIES	4	/* only on non IO commands */

#define STUNIT(DEV)      ((minor(DEV)&0xF0) >> 4)    /* 4 bit unit.  */
#define STSETUNIT(DEV, U) makedev(major(DEV), ((U) << 4))

#define MODE(z)		(  (minor(z)       & 0x03) )
#define DSTY(z)         ( ((minor(z) >> 2) & 0x03) )
#define CTLMODE	3

#define IS_CTLMODE(DEV) (MODE(DEV) == CTLMODE)

#define SCSI_2_MAX_DENSITY_CODE	0x17	/* maximum density code specified
					 * in SCSI II spec. */
#ifndef NEW_SCSICONF
/*
 * Define various devices that we know mis-behave in some way,
 * and note how they are bad, so we can correct for them
 */
struct modes {
	u_int32_t blksiz;
	u_int32_t quirks;		/* same definitions as in rogues */
	char    density;
	char    spare[3];
};

struct rogues {
	char   *name;
	char   *manu;
	char   *model;
	char   *version;
	u_int32_t quirks;		/* valid for all modes */
	struct modes modes[4];
};

/* define behaviour codes (quirks) */
#define	ST_Q_NEEDS_PAGE_0	0x00001
#define	ST_Q_FORCE_FIXED_MODE	0x00002
#define	ST_Q_FORCE_VAR_MODE	0x00004
#define	ST_Q_SNS_HLP		0x00008		/* must do READ for good MODE SENSE */
#define	ST_Q_IGNORE_LOADS	0x00010
#define	ST_Q_BLKSIZ		0x00020		/* variable-block media_blksiz > 0 */

static struct rogues gallery[] =	/* ends with an all-null entry */
{
    {"Such an old device ", "pre-scsi", " unknown model  ", "????",
	0,
	{
	    {512, ST_Q_FORCE_FIXED_MODE, 0},	/* minor  0,1,2,3 */
	    {512, ST_Q_FORCE_FIXED_MODE, QIC_24},	/* minor  4,5,6,7 */
	    {0, ST_Q_FORCE_VAR_MODE, HALFINCH_1600},	/* minor  8,9,10,11 */
	    {0, ST_Q_FORCE_VAR_MODE, HALFINCH_6250}	/* minor  12,13,14,15 */
	}
    },
    {"Tandberg tdc3600", "TANDBERG", " TDC 3600", "????",
	ST_Q_NEEDS_PAGE_0,
	{
	    {0, 0, 0},		/* minor  0,1,2,3 */
	    {0, ST_Q_FORCE_VAR_MODE, QIC_525},	/* minor  4,5,6,7 */
	    {0, 0, QIC_150},	/* minor  8,9,10,11 */
	    {0, 0, QIC_120}	/* minor  12,13,14,15 */
	}
    },
    {"Rev 5 of the Archive 2525", "ARCHIVE ", "VIPER 2525 25462", "-005",
	0,
	{
	    {0, ST_Q_SNS_HLP, 0},	/* minor  0,1,2,3 */
	    {0, ST_Q_SNS_HLP, QIC_525},		/* minor  4,5,6,7 */
	    {0, 0, QIC_150},	/* minor  8,9,10,11 */
	    {0, 0, QIC_120}	/* minor  12,13,14,15 */
	}
    },
    {"Archive  Viper 150", "ARCHIVE ", "VIPER 150", "????",
	ST_Q_NEEDS_PAGE_0,
	{
	    {0, 0, 0},		/* minor  0,1,2,3 */
	    {0, 0, QIC_150},	/* minor  4,5,6,7 */
	    {0, 0, QIC_120},	/* minor  8,9,10,11 */
	    {0, 0, QIC_24}	/* minor  12,13,14,15 */
	}
    },
    {"Wangtek 5525ES", "WANGTEK ", "5525ES SCSI REV7", "????",
	0,
	{
	    {0, 0, 0},		/* minor  0,1,2,3 */
	    {0, ST_Q_BLKSIZ, QIC_525},	/* minor  4,5,6,7 */
	    {0, 0, QIC_150},	/* minor  8,9,10,11 */
	    {0, 0, QIC_120}	/* minor  12,13,14,15 */
	}
    },
    {"WangDAT model 1300", "WangDAT ", "Model 1300", "????",
	0,
	{
	    {0, 0, 0},					/* minor  0,1,2,3 */
	    {512, ST_Q_FORCE_FIXED_MODE, 0x13},		/* minor  4,5,6,7 */
	    {1024, ST_Q_FORCE_FIXED_MODE, 0x13},	/* minor  8,9,10,11 */
	    {0, ST_Q_FORCE_VAR_MODE, 0x13}		/* minor  12,13,14,15 */
	}
    },
    {(char *) 0}
};
#endif /* NEW_SCSICONF */

errval	st_space __P((u_int32_t unit, int32_t number, u_int32_t what, u_int32_t flags));
errval	st_rewind __P((u_int32_t unit, boolean immed, u_int32_t flags));
errval	st_erase __P((u_int32_t unit, boolean immed, u_int32_t flags));
static errval st_mode_sense __P((u_int32_t unit, u_int32_t flags, \
	struct tape_pages *page, u_int32_t pagelen, u_int32_t pagecode));
errval	st_decide_mode __P((u_int32_t unit, boolean first_read));
errval	st_rd_blk_lim __P((u_int32_t unit, u_int32_t flags));
errval	st_touch_tape __P((u_int32_t unit));
errval	st_write_filemarks __P((u_int32_t unit, int32_t number, u_int32_t flags));
errval	st_load __P((u_int32_t unit, u_int32_t type, u_int32_t flags));
errval	st_mode_select __P((u_int32_t unit, u_int32_t flags, \
	struct tape_pages *page, u_int32_t pagelen));
errval	st_comp __P((u_int32_t unit, u_int32_t mode));
void    ststrategy();
int32_t   st_chkeod();
void	ststart(u_int32_t	unit, u_int32_t flags);
void	st_unmount();
errval	st_mount_tape();
void	st_loadquirks();
#ifndef NEW_SCSICONF
void	st_identify_drive();
#endif
errval  st_interpret_sense();

#define ESUCCESS 0
#define NOEJECT 0
#define EJECT 1

struct scsi_data {
/*--------------------present operating parameters, flags etc.----------------*/
	u_int32_t flags;		/* see below                          */
	u_int32_t blksiz;		/* blksiz we are using                */
	u_int32_t density;	/* present density                    */
	u_int32_t comp;		/* present compression mode           */
	u_int32_t quirks;		/* quirks for the open mode           */
	u_int32_t last_dsty;	/* last density openned               */
/*--------------------parameters reported by the device ----------------------*/
	u_int32_t blkmin;		/* min blk size                       */
	u_int32_t blkmax;		/* max blk size                       */
#ifndef NEW_SCSICONF
	struct rogues *rogues;	/* if we have a rogue entry           */
#endif
/*--------------------parameters reported by the device for this media--------*/
	u_int32_t numblks;	/* nominal blocks capacity            */
	u_int32_t media_blksiz;	/* 0 if not ST_FIXEDBLOCKS            */
	u_int32_t media_density;	/* this is what it said when asked    */
/*--------------------quirks for the whole drive------------------------------*/
	u_int32_t drive_quirks;	/* quirks of this drive               */
/*--------------------How we should set up when openning each minor device----*/
#ifdef NEW_SCSICONF
	st_modes modes; /* plus more for each mode            */
#else
	struct modes modes[4];	/* plus more for each mode            */
#endif
	u_int8_t  modeflags[4];	/* flags for the modes                */
#define DENSITY_SET_BY_USER	0x01
#define DENSITY_SET_BY_QUIRK	0x02
#define BLKSIZE_SET_BY_USER	0x04
#define BLKSIZE_SET_BY_QUIRK	0x08
#define COMPRES_SET_BY_USER	0x10
#define COMPRES_SET_BY_QUIRK	0x20
/*--------------------storage for sense data returned by the drive------------*/
	unsigned char saved_page0[PAGE_0_SENSE_DATA_SIZE];	/*
					 * additional sense data needed
					 * for mode sense/select.
					 */
	struct buf *buf_queue;		/* the queue of pending IO operations */
	struct scsi_xfer scsi_xfer;	/* scsi xfer struct for this drive */
	u_int32_t xfer_block_wait;	/* is a process waiting? */
};

static int stunit(dev_t dev) { return STUNIT(dev); }
static dev_t stsetunit(dev_t dev, int unit) { return STSETUNIT(dev, unit); }

errval st_open(dev_t dev, int flags, int fmt, struct proc *p,
struct scsi_link *sc_link);
errval st_ioctl(dev_t dev, int cmd, caddr_t addr, int flag,
		struct proc *p, struct scsi_link *sc_link);
errval st_close(dev_t dev, int flag, int fmt, struct proc *p,
        struct scsi_link *sc_link);
void st_strategy(struct buf *bp, struct scsi_link *sc_link);

SCSI_DEVICE_ENTRIES(st)

struct scsi_device st_switch =
{
    st_interpret_sense,		/* check errors with us first */
    ststart,			/* we have a queue, and this is how we service it */
    NULL,
    NULL,			/* use the default 'done' routine */
    "st",
    0,
	{0, 0},
	0,				/* Link flags */
	stattach,
	"Sequential-Access",
	stopen,
    sizeof(struct scsi_data),
	T_SEQUENTIAL,
	stunit,
	stsetunit,
	st_open,
	st_ioctl,
	st_close,
	st_strategy,
};

#define ST_INITIALIZED	0x01
#define	ST_INFO_VALID	0x02
#define ST_OPEN		0x04
#define	ST_BLOCK_SET	0x08	/* block size, mode set by ioctl      */
#define	ST_WRITTEN	0x10	/* data have been written, EOD needed */
#define	ST_FIXEDBLOCKS	0x20
#define	ST_AT_FILEMARK	0x40
#define	ST_EIO_PENDING	0x80	/* we couldn't report it then (had data) */
#define	ST_NEW_MOUNT	0x100	/* still need to decide mode              */
#define	ST_READONLY	0x200	/* st_mode_sense says write protected */
#define	ST_FM_WRITTEN	0x400	/*
				 * EOF file mark written  -- used with
				 * ~ST_WRITTEN to indicate that multiple file
				 * marks have been written
				 */
#define	ST_BLANK_READ	0x800	/* BLANK CHECK encountered already */
#define	ST_2FM_AT_EOD	0x1000	/* write 2 file marks at EOD */
#define	ST_MOUNTED	0x2000	/* Device is presently mounted */
#define	ST_SENSE_READ	0x4000	/* mode sense read from drive */

#define	ST_PER_ACTION	(ST_AT_FILEMARK | ST_EIO_PENDING | ST_BLANK_READ)
#define	ST_PER_MOUNT	(ST_INFO_VALID | ST_BLOCK_SET | ST_WRITTEN | \
			ST_FIXEDBLOCKS | ST_READONLY | \
			ST_FM_WRITTEN | ST_2FM_AT_EOD | ST_PER_ACTION)

static int
st_externalize(struct proc *p, struct kern_devconf *kdc, void *userp,
	       size_t len)
{
	return scsi_externalize(SCSI_LINK(&st_switch, kdc->kdc_unit),
				userp, &len);
}

static struct kern_devconf kdc_st_template = {
	0, 0, 0,		/* filled in by dev_attach */
	"st", 0, MDDC_SCSI,
	st_externalize, 0, scsi_goaway, SCSI_EXTERNALLEN,
	&kdc_scbus0,		/* XXX parent */
	0,			/* parentdata */
	DC_UNKNOWN,		/* not supported */
};

static inline void
st_registerdev(int unit)
{
	struct kern_devconf *kdc;

	MALLOC(kdc, struct kern_devconf *, sizeof *kdc, M_TEMP, M_NOWAIT);
	if(!kdc) return;
	*kdc = kdc_st_template;
	kdc->kdc_unit = unit;
	kdc->kdc_description = st_switch.desc;
	dev_attach(kdc);
}

/*
 * The routine called by the low level scsi routine when it discovers
 * a device suitable for this driver
 */

errval
stattach(struct scsi_link *sc_link)
{
	u_int32_t unit;

	struct scsi_data *st = sc_link->sd;

	unit = sc_link->dev_unit;

	/*
	 * Check if the drive is a known criminal and take
	 * Any steps needed to bring it into line
	 */
#ifdef NEW_SCSICONF
	st_loadquirks(sc_link);
#else
	st_identify_drive(unit);
#endif
	/*
	 * Use the subdriver to request information regarding
	 * the drive. We cannot use interrupts yet, so the
	 * request must specify this.
	 */
	if (st_mode_sense(unit, SCSI_NOSLEEP | SCSI_NOMASK | SCSI_SILENT,
		NULL, 0, 0)) {
		printf("drive offline");
	} else {
		printf("density code 0x%lx, ", st->media_density);
		if (!scsi_test_unit_ready(sc_link, SCSI_NOSLEEP | SCSI_NOMASK | SCSI_SILENT)) {
			if (st->media_blksiz) {
				printf("%ld-byte", st->media_blksiz);
			} else {
				printf("variable");
			}
			printf(" blocks, write-%s",
			    (st->flags & ST_READONLY) ? "protected" : "enabled");
		} else {
			printf(" drive empty");
		}
	}
	/*
	 * Set up the buf queue for this device
	 */
	st->buf_queue = 0;
	st->flags |= ST_INITIALIZED;
	st_registerdev(unit);

	return 0;
}

#ifndef NEW_SCSICONF
/*
 * Use the inquiry routine in 'scsi_base' to get drive info so we can
 * Further tailor our behaviour.
 */
void
st_identify_drive(unit)
	u_int32_t unit;
{
	struct scsi_link *sc_link = SCSI_LINK(&st_switch, unit);
	struct scsi_data *st = sc_link->sd;
	struct rogues *finger;
	char    manu[32];
	char    model[32];
	char    model2[32];
	char    version[32];
	u_int32_t model_len;
 	struct scsi_inquiry_data *inqbuf = &sc_link->inqbuf;

	/*
	 * Get the device type information
	 */
	if (scsi_inquire(sc_link, inqbuf,
		SCSI_NOSLEEP | SCSI_NOMASK | SCSI_SILENT) != 0) {
		printf("st%ld: couldn't get device type, using default\n", unit);
		return;
	}
	if ((inqbuf->version & SID_ANSII) == 0) {
		/*
		 * If not advanced enough, use default values
		 */
		strncpy(manu, "pre-scsi", 8);
		manu[8] = 0;
		strncpy(model, " unknown model  ", 16);
		model[16] = 0;
		strncpy(version, "????", 4);
		version[4] = 0;
	} else {
		strncpy(manu, inqbuf->vendor, 8);
		manu[8] = 0;
		strncpy(model, inqbuf->product, 16);
		model[16] = 0;
		strncpy(version, inqbuf->revision, 4);
		version[4] = 0;
	}

	/*
	 * Load the parameters for this kind of device, so we
	 * treat it as appropriate for each operating mode.
	 * Only check the number of characters in the array's
	 * model entry, not the entire model string returned.
	 */
	finger = gallery;
	while (finger->name) {
		model_len = 0;
		while (finger->model[model_len] && (model_len < 32)) {
			model2[model_len] = model[model_len];
			model_len++;
		}
		model2[model_len] = 0;
		if ((strcmp(manu, finger->manu) == 0)
		    && (strcmp(model2, finger->model) == 0 ||
			strcmp("????????????????", finger->model) == 0)
		    && (strcmp(version, finger->version) == 0 ||
			strcmp("????", finger->version) == 0)) {
			printf("st%ld: %s is a known rogue\n", unit, finger->name);
			st->rogues = finger;
			st->drive_quirks = finger->quirks;
			st->quirks = finger->quirks;	/*start value */
			st_loadquirks(sc_link);
			break;
		} else {
			finger++;	/* go to next suspect */
		}
	}
}
#endif /* NEW_SCSICONF */

/*
 * initialise the subdevices to the default (QUIRK) state.
 * this will remove any setting made by the system operator or previous
 * operations.
 */
void
st_loadquirks(sc_link)
	struct scsi_link *sc_link;
{
	struct scsi_data *st = sc_link->sd;
	int     i;
#ifdef NEW_SCSICONF
	struct	st_mode *mode;
	struct	st_mode *mode2;

	mode = (struct st_mode*) sc_link->devmodes;
	if (!mode)
		return;

	st->quirks = st->drive_quirks = sc_link->quirks;

#else
	struct	modes *mode;
	struct	modes *mode2;

	if (!st->rogues)
		return;
	mode = st->rogues->modes;
#endif
	mode2 = st->modes;

	for (i = 0; i < 4; i++) {
		bzero(mode2, sizeof(*mode2));
		st->modeflags[i] &= ~(BLKSIZE_SET_BY_QUIRK
		    | DENSITY_SET_BY_QUIRK
		    | BLKSIZE_SET_BY_USER
		    | DENSITY_SET_BY_USER);
		if (mode->blksiz && ((mode->quirks | st->drive_quirks)
			& (ST_Q_FORCE_FIXED_MODE))) {
			mode2->blksiz = mode->blksiz;
			st->modeflags[i] |= BLKSIZE_SET_BY_QUIRK;
		} else {
			if ((mode->quirks | st->drive_quirks)
			    & ST_Q_FORCE_VAR_MODE) {
				mode2->blksiz = 0;
				st->modeflags[i] |= BLKSIZE_SET_BY_QUIRK;
			}
		}
		if (mode->density) {
			mode2->density = mode->density;
			st->modeflags[i] |= DENSITY_SET_BY_QUIRK;
		}
		mode++;
		mode2++;
	}
}

/*
 * open the device.
 */
errval
st_open(dev_t dev, int flags, int fmt, struct proc *p,
struct scsi_link *sc_link)
{
	u_int32_t unit, mode, dsty;
	errval  errno = 0;
	struct scsi_data *st;

	unit = STUNIT(dev);
	mode = MODE(dev);
	dsty = DSTY(dev);

	st = sc_link->sd;
	/*
	 * Make sure the device has been initialised
	 */
	if ((st == NULL) || (!(st->flags & ST_INITIALIZED)))
		return (ENXIO);

	/*
	 * Only allow one at a time
	 */
	if (st->flags & ST_OPEN) {
		return (EBUSY);
	}
	/*
	 * Throw out a dummy instruction to catch 'Unit attention
	 * errors (the error handling will invalidate all our
	 * device info if we get one, but otherwise, ignore it)
	 */
	scsi_test_unit_ready(sc_link, SCSI_SILENT);

	sc_link->flags |= SDEV_OPEN;	/* unit attn are now errors */
	/*
	 * If the mode is 3 (e.g. minor = 3,7,11,15)
	 * then the device has been openned to set defaults
	 * This mode does NOT ALLOW I/O, only ioctls.
	 * XXX: Where do we lock out I/O?
	 */
	if (IS_CTLMODE(dev))
		return 0;

	/*
	 * Check that the device is ready to use (media loaded?)
	 * This time take notice of the return result
	 */
	if ( (errno = scsi_test_unit_ready(sc_link, 0)) ) {
		uprintf("st%d: not ready\n", unit);
		st_unmount(unit, NOEJECT);
		return (errno);
	}
	/*
	 * if it's a different mode, or if the media has been
	 * invalidated, unmount the tape from the previous
	 * session but continue with open processing
	 */
	if ((st->last_dsty != dsty)
	    || (!(sc_link->flags & SDEV_MEDIA_LOADED))) {
		st_unmount(unit, NOEJECT);
	}
	/*
	 * If we are not mounted, then we should start a new
	 * mount session.
	 */
	if (!(st->flags & ST_MOUNTED)) {
		st_mount_tape(dev, flags);
		st->last_dsty = dsty;
	}
	/*
	 * Make sure that a tape opened in write-only mode will have
	 * file marks written on it when closed, even if not written to.
	 * This is for SUN compatibility
	 */
	if ((flags & O_ACCMODE) == FWRITE)
		st->flags |= ST_WRITTEN;

	SC_DEBUG(sc_link, SDEV_DB2, ("Open complete\n"));

	st->flags |= ST_OPEN;
	return 0;
}

/*
 * close the device.. only called if we are the LAST
 * occurence of an open device
 */
errval
st_close(dev_t dev, int flag, int fmt, struct proc *p,
        struct scsi_link *sc_link)
{
	unsigned char unit, mode;
	struct scsi_data *st;

	unit = STUNIT(dev);
	mode = MODE(dev);
	st = sc_link->sd;

	if ((st->flags & (ST_WRITTEN | ST_FM_WRITTEN)) == ST_WRITTEN)
		st_write_filemarks(unit, 1, 0);
	switch (mode & 0x3) {
	case 0:
	case 3:		/* for now */
		st_unmount(unit, NOEJECT);
		break;
	case 1:		/*leave mounted unless media seems to have been removed */
		if (!(sc_link->flags & SDEV_MEDIA_LOADED)) {
			st_unmount(unit, NOEJECT);
		}
		break;
	case 2:
		st_unmount(unit, EJECT);
		break;
	}
	sc_link->flags &= ~SDEV_OPEN;
	st->flags &= ~ST_OPEN;
	return (0);
}

/*
 * Start a new mount session.
 * Copy in all the default parameters from the selected device mode.
 * and try guess any that seem to be defaulted.
 */
errval
st_mount_tape(dev, flags)
	dev_t   dev;
	u_int32_t flags;
{
	u_int32_t unit, mode, dsty;
	struct scsi_data *st;
	struct scsi_link *sc_link;
	errval  errno = 0;

	unit = STUNIT(dev);
	mode = MODE(dev);
	dsty = DSTY(dev);
	sc_link = SCSI_LINK(&st_switch, unit);
	st = sc_link->sd;

	if (st->flags & ST_MOUNTED)
		return 0;

	SC_DEBUG(sc_link, SDEV_DB1, ("mounting\n "));
	st->flags |= ST_NEW_MOUNT;
	st->quirks = st->drive_quirks | st->modes[dsty].quirks;
	/*
	 * If the media is new, then make sure we give it a chance to
	 * to do a 'load' instruction. ( We assume it is new)
	 */
	if ( (errno = st_load(unit, LD_LOAD, 0)) ) {
		return (errno);
	}
	/*
	 * Throw another dummy instruction to catch
	 * 'Unit attention' errors. Some drives appear to give
	 * these after doing a Load instruction.
	 * (noteably some DAT drives)
	 */
	scsi_test_unit_ready(sc_link, SCSI_SILENT);

	/*
	 * Some devices can't tell you much until they have been
	 * asked to look at the media. This quirk does this.
	 */
	if (st->quirks & ST_Q_SNS_HLP) {
		if ( (errno = st_touch_tape(unit)) )
			return errno;
	}
	/*
	 * Load the physical device parameters
	 * loads: blkmin, blkmax
	 */
	if ( (errno = st_rd_blk_lim(unit, 0)) ) {
		return errno;
	}
	/*
	 * Load the media dependent parameters
	 * includes: media_blksiz,media_density,numblks
	 * As we have a tape in, it should be reflected here.
	 * If not you may need the "quirk" above.
	 */
	if ( (errno = st_mode_sense(unit, 0, NULL, 0, 0)) ) {
		return errno;
	}
	/*
	 * If we have gained a permanent density from somewhere,
	 * then use it in preference to the one supplied by
	 * default by the driver.
	 */
	if (st->modeflags[dsty] & (DENSITY_SET_BY_QUIRK | DENSITY_SET_BY_USER)) {
		st->density = st->modes[dsty].density;
	} else {
		st->density = st->media_density;
	}
	/*
	 * If we have gained a permanent blocksize
	 * then use it in preference to the one supplied by
	 * default by the driver.
	 */
	st->flags &= ~ST_FIXEDBLOCKS;
	if (st->modeflags[dsty] & (BLKSIZE_SET_BY_QUIRK | BLKSIZE_SET_BY_USER)) {
		st->blksiz = st->modes[dsty].blksiz;
		if (st->blksiz) {
			st->flags |= ST_FIXEDBLOCKS;
		}
	} else {
		if ( (errno = st_decide_mode(unit, FALSE)) ) {
			return errno;
		}
	}
	if ( (errno = st_mode_select(unit, 0, NULL, 0)) ) {
		printf("st%ld: Cannot set selected mode", unit);
		return errno;
	}
	scsi_prevent(sc_link, PR_PREVENT, 0);	/* who cares if it fails? */
	st->flags &= ~ST_NEW_MOUNT;
	st->flags |= ST_MOUNTED;
	sc_link->flags |= SDEV_MEDIA_LOADED;

	return 0;
}

/*
 * End the present mount session.
 * Rewind, and optionally eject the tape.
 * Reset various flags to indicate that all new
 * operations require another mount operation
 */
void
st_unmount(int unit, boolean eject)
{
	struct scsi_link *sc_link = SCSI_LINK(&st_switch, unit);
	struct scsi_data *st = sc_link->sd;
	int32_t   nmarks;

	if (!(st->flags & ST_MOUNTED))
		return;
	SC_DEBUG(sc_link, SDEV_DB1, ("unmounting\n"));
	st_chkeod(unit, FALSE, &nmarks, SCSI_SILENT);
	st_rewind(unit, FALSE, SCSI_SILENT);
	scsi_prevent(sc_link, PR_ALLOW, SCSI_SILENT);
	if (eject) {
		st_load(unit, LD_UNLOAD, SCSI_SILENT);
	}
	st->flags &= ~(ST_MOUNTED | ST_NEW_MOUNT);
	sc_link->flags &= ~SDEV_MEDIA_LOADED;
}

/*
 * Given all we know about the device, media, mode, 'quirks' and
 * initial operation, make a decision as to how we should be set
 * to run (regarding blocking and EOD marks)
 */
errval
st_decide_mode(unit, first_read)
	u_int32_t	unit;
	boolean	first_read;
{
	struct scsi_link *sc_link = SCSI_LINK(&st_switch, unit);
	struct scsi_data *st = sc_link->sd;

	SC_DEBUG(sc_link, SDEV_DB2, ("starting block mode decision\n"));

	/*
	 * If the user hasn't already specified fixed or variable-length
	 * blocks and the block size (zero if variable-length), we'll
	 * have to try to figure them out ourselves.
	 *
	 * Our first shot at a method is, "The quirks made me do it!"
	 */
	switch ((int)(st->quirks & (ST_Q_FORCE_FIXED_MODE | ST_Q_FORCE_VAR_MODE))) {
	case (ST_Q_FORCE_FIXED_MODE | ST_Q_FORCE_VAR_MODE):
		printf("st%ld: bad quirks\n", unit);
		return (EINVAL);
	case ST_Q_FORCE_FIXED_MODE:	/*specified fixed, but not what size */
		st->flags |= ST_FIXEDBLOCKS;
		if (st->blkmin && (st->blkmin == st->blkmax))
			st->blksiz = st->blkmin;
		else if (st->media_blksiz > 0)
			st->blksiz = st->media_blksiz;
		else
			st->blksiz = DEF_FIXED_BSIZE;
		SC_DEBUG(sc_link, SDEV_DB3, ("Quirks force fixed mode(%ld)\n",
			st->blksiz));
		goto done;
	case ST_Q_FORCE_VAR_MODE:
		st->flags &= ~ST_FIXEDBLOCKS;
		st->blksiz = 0;
		SC_DEBUG(sc_link, SDEV_DB3, ("Quirks force variable mode\n"));
		goto done;
	}
		/*
		 * If the drive can only handle fixed-length blocks and only at
		 * one size, perhaps we should just do that.
		 */
		if (st->blkmin && (st->blkmin == st->blkmax)) {
		st->flags |= ST_FIXEDBLOCKS;
		st->blksiz = st->blkmin;
		SC_DEBUG(sc_link, SDEV_DB3,
		    ("blkmin == blkmax of %ld\n", st->blkmin));
		goto done;
	}
	/*
	 * If the tape density mandates (or even suggests) use of fixed
	 * or variable-length blocks, comply.
	 */
	switch ((int)st->density) {
	case HALFINCH_800:
	case HALFINCH_1600:
	case HALFINCH_6250:
	case DDS:
		st->flags &= ~ST_FIXEDBLOCKS;
		st->blksiz = 0;
		SC_DEBUG(sc_link, SDEV_DB3, ("density specified variable\n"));
		goto done;
	case QIC_11:
	case QIC_24:
	case QIC_120:
	case QIC_150:
	case QIC_525:
	case QIC_1320:
	case QIC_3080:
		st->flags |= ST_FIXEDBLOCKS;
		if (st->media_blksiz > 0) {
			st->blksiz = st->media_blksiz;
		} else {
			st->blksiz = DEF_FIXED_BSIZE;
		}
		SC_DEBUG(sc_link, SDEV_DB3, ("density specified fixed\n"));
		goto done;
	}
	/*
	 * If we're about to read the tape, perhaps we should choose
	 * fixed or variable-length blocks and block size according to
	 * what the drive found on the tape.
	 */
	if (first_read
	    && (!(st->quirks & ST_Q_BLKSIZ)
		|| (st->media_blksiz == 0)
		|| (st->media_blksiz == DEF_FIXED_BSIZE)
		|| (st->media_blksiz == 1024))) {
		if (st->media_blksiz == 0) {
			st->flags &= ~ST_FIXEDBLOCKS;
		} else {
			st->flags |= ST_FIXEDBLOCKS;
		}
		st->blksiz = st->media_blksiz;
		SC_DEBUG(sc_link, SDEV_DB3,
		    ("Used media_blksiz of %ld\n", st->media_blksiz));
		goto done;
	}
	/*
	 * We're getting no hints from any direction.  Choose variable-
	 * length blocks arbitrarily.
	 */
	st->flags &= ~ST_FIXEDBLOCKS;
	st->blksiz = 0;
	SC_DEBUG(sc_link, SDEV_DB3, ("Give up and default to variable mode\n"));
done:

	/*
	 * Decide whether or not to write two file marks to signify end-
	 * of-data.  Make the decision as a function of density.  If
	 * the decision is not to use a second file mark, the SCSI BLANK
	 * CHECK condition code will be recognized as end-of-data when
	 * first read.
	 * (I think this should be a by-product of fixed/variable..julian)
	 */
	switch ((int)st->density) {
/*      case    8 mm:   What is the SCSI density code for 8 mm, anyway? */
	case QIC_11:
	case QIC_24:
	case QIC_120:
	case QIC_150:
	case QIC_525:
	case QIC_1320:
	case QIC_3080:
		st->flags &= ~ST_2FM_AT_EOD;
		break;
	default:
		st->flags |= ST_2FM_AT_EOD;
	}
	return 0;
}

/*
 * Actually translate the requested transfer into
 * one the physical driver can understand
 * The transfer is described by a buf and will include
 * only one physical transfer.
 */
void
st_strategy(struct buf *bp, struct scsi_link *sc_link)
{
	struct buf **dp;
	unsigned char unit;	/* XXX Everywhere else unit is "u_int32_t". Please int? */
	u_int32_t opri;
	struct scsi_data *st;

	ststrats++;
	unit = STUNIT((bp->b_dev));
	st = sc_link->sd;
	/*
	 * If it's a null transfer, return immediatly
	 */
	if (bp->b_bcount == 0) {
		goto done;
	}
	/*
	 * Odd sized request on fixed drives are verboten
	 */
	if (st->flags & ST_FIXEDBLOCKS) {
		if (bp->b_bcount % st->blksiz) {
			printf("st%d: bad request, must be multiple of %ld\n",
			    unit, st->blksiz);
			bp->b_error = EIO;
			goto bad;
		}
	}
	/*
	 * as are out-of-range requests on variable drives.
	 */
	else if (bp->b_bcount < st->blkmin || bp->b_bcount > st->blkmax) {
		printf("st%d: bad request, must be between %ld and %ld\n",
		    unit, st->blkmin, st->blkmax);
		bp->b_error = EIO;
		goto bad;
	}
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
	 * at the end (a bit silly because we only have on user..
	 * (but it could fork() ))
	 */
	dp = &(st->buf_queue);
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
	ststart(unit, 0);

	splx(opri);
	return;
bad:
	bp->b_flags |= B_ERROR;
done:
	/*
	 * Correctly set the buf to indicate a completed xfer
	 */
	biodone(bp);
	return;
}

/*
 * ststart looks to see if there is a buf waiting for the device
 * and that the device is not already busy. If both are true,
 * It dequeues the buf and creates a scsi command to perform the
 * transfer required. The transfer request will call scsi_done
 * on completion, which will in turn call this routine again
 * so that the next queued transfer is performed.
 * The bufs are queued by the strategy routine (ststrategy)
 *
 * This routine is also called after other non-queued requests
 * have been made of the scsi driver, to ensure that the queue
 * continues to be drained.
 * ststart() is called at splbio
 */
void
ststart(unit, flags)
	u_int32_t	unit;
	u_int32_t flags;
{
	struct scsi_link *sc_link = SCSI_LINK(&st_switch, unit);
	struct scsi_data *st = sc_link->sd;
	register struct buf *bp = 0;
	struct scsi_rw_tape cmd;

	SC_DEBUG(sc_link, SDEV_DB2, ("ststart "));
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
		if ((bp = st->buf_queue) == NULL) {
			return;	/* no work to bother with */
		}
		st->buf_queue = bp->b_actf;

		/*
		 * if the device has been unmounted by the user
		 * then throw away all requests until done
		 */
		if ((!(st->flags & ST_MOUNTED))
		    || (!(sc_link->flags & SDEV_MEDIA_LOADED))) {
			/* make sure that one implies the other.. */
			sc_link->flags &= ~SDEV_MEDIA_LOADED;
			goto badnews;
		}
		/*
		 * only FIXEDBLOCK devices have pending operations
		 */
		if (st->flags & ST_FIXEDBLOCKS) {
			/*
			 * If we are at a filemark but have not reported it yet
			 * then we should report it now
			 */
			if (st->flags & ST_AT_FILEMARK) {
				if ((bp->b_flags & B_READ) == B_WRITE) {
					/*
					 * Handling of ST_AT_FILEMARK in
					 * st_space will fill in the right file
					 * mark count.
					 * Back up over filemark
					 */
					if (st_space(unit, 0, SP_FILEMARKS, 0) !=
					    ESUCCESS)
						goto badnews;
				} else {
					bp->b_resid = bp->b_bcount;
					bp->b_error = 0;
					bp->b_flags &= ~B_ERROR;
					st->flags &= ~ST_AT_FILEMARK;
					biodone(bp);
					continue;	/* seek more work */
				}
			}
			/*
			 * If we are at EIO (e.g. EOM) but have not reported it
			 * yet then we should report it now
			 */
			if (st->flags & ST_EIO_PENDING) {
				bp->b_resid = bp->b_bcount;
				bp->b_error = EIO;
				bp->b_flags |= B_ERROR;
				st->flags &= ~ST_EIO_PENDING;
				biodone(bp);
				continue;	/* seek more work */
			}
		}
		/*
		 *  Fill out the scsi command
		 */
		bzero(&cmd, sizeof(cmd));
		if ((bp->b_flags & B_READ) == B_WRITE) {
			cmd.op_code = WRITE_COMMAND_TAPE;
			st->flags &= ~ST_FM_WRITTEN;
			st->flags |= ST_WRITTEN;
			flags |= SCSI_DATA_OUT;
		} else {
			cmd.op_code = READ_COMMAND_TAPE;
			flags |= SCSI_DATA_IN;
		}
		/*
		 * Handle "fixed-block-mode" tape drives by using the
		 * block count instead of the length.
		 */
		if (st->flags & ST_FIXEDBLOCKS) {
			cmd.byte2 |= SRWT_FIXED;
			scsi_uto3b(bp->b_bcount / st->blksiz, cmd.len);
		} else {
			scsi_uto3b(bp->b_bcount, cmd.len);
		}
		/*
		 * go ask the adapter to do all this for us
		 */
		if (scsi_scsi_cmd(sc_link,
			(struct scsi_generic *) &cmd,
			sizeof(cmd),
			(u_char *) bp->b_un.b_addr,
			bp->b_bcount,
			0,	/* can't retry a read on a tape really */
			100000,
			bp,
			flags) == SUCCESSFULLY_QUEUED) {
			stqueues++;
		} else {
badnews:
			printf("st%ld: oops not queued\n", unit);
			bp->b_flags |= B_ERROR;
			bp->b_error = EIO;
			biodone(bp);
		}
	} /* go back and see if we can cram more work in.. */
}

/*
 * Perform special action on behalf of the user;
 * knows about the internals of this device
 */
errval
st_ioctl(dev_t dev, int cmd, caddr_t arg, int	flag,
struct proc *p, struct scsi_link *sc_link)
{
	errval  errcode = 0;
	unsigned char unit;
	u_int32_t number, flags, dsty;
	struct scsi_data *st;
	u_int32_t hold_blksiz;
	u_int32_t hold_density;
	int32_t   nmarks;
	struct mtop *mt = (struct mtop *) arg;

	/*
	 * Find the device that the user is talking about
	 */
	flags = 0;		/* give error messages, act on errors etc. */
	unit = STUNIT(dev);
	dsty = DSTY(dev);
	st = sc_link->sd;
	hold_blksiz = st->blksiz;
	hold_density = st->density;

	switch (cmd) {

	case MTIOCGET:
		{
			struct mtget *g = (struct mtget *) arg;

			SC_DEBUG(sc_link, SDEV_DB1, ("[ioctl: get status]\n"));
			bzero(g, sizeof(struct mtget));
			g->mt_type = 0x7;	/* Ultrix compat *//*? */
			g->mt_density = st->density;
			g->mt_blksiz = st->blksiz;
			g->mt_comp = st->comp;
			g->mt_density0 = st->modes[0].density;
			g->mt_density1 = st->modes[1].density;
			g->mt_density2 = st->modes[2].density;
			g->mt_density3 = st->modes[3].density;
			g->mt_blksiz0 = st->modes[0].blksiz;
			g->mt_blksiz1 = st->modes[1].blksiz;
			g->mt_blksiz2 = st->modes[2].blksiz;
			g->mt_blksiz3 = st->modes[3].blksiz;
			g->mt_comp0 = 0;
			g->mt_comp1 = 0;
			g->mt_comp2 = 0;
			g->mt_comp3 = 0;
			break;
		}
	case MTIOCTOP:
		{

			SC_DEBUG(sc_link, SDEV_DB1, ("[ioctl: op=0x%x count=0x%lx]\n",
				mt->mt_op, mt->mt_count));

			/* compat: in U*x it is a short */
			number = mt->mt_count;
			switch ((short) (mt->mt_op)) {
			case MTWEOF:	/* write an end-of-file record */
				errcode = st_write_filemarks(unit, number, flags);
				break;
			case MTBSF:	/* backward space file */
				number = -number;
			case MTFSF:	/* forward space file */
				errcode = st_chkeod(unit, FALSE, &nmarks, flags);
				if (errcode == ESUCCESS)
					errcode = st_space(unit, number - nmarks,
					    SP_FILEMARKS, flags);
				break;
			case MTBSR:	/* backward space record */
				number = -number;
			case MTFSR:	/* forward space record */
				errcode = st_chkeod(unit, TRUE, &nmarks, flags);
				if (errcode == ESUCCESS)
					errcode = st_space(unit, number, SP_BLKS, flags);
				break;
			case MTEOD:	/* space to end of recorded medium */
				errcode = st_chkeod(unit, FALSE, &nmarks, flags);
				if (errcode == ESUCCESS)
					errcode = st_space(unit, 0, SP_EOM,
							   flags);
				break;
			case MTREW:	/* rewind */
				errcode = st_rewind(unit, FALSE, flags);
				break;
			case MTERASE:	/* erase */
				errcode = st_erase(unit, FALSE, flags);
				break;
			case MTRETENS:	/* re-tension tape */
				errcode = st_load(unit, LD_LOAD|LD_RETEN,
						  flags);
				break;
			case MTOFFL:	/* rewind and put the drive offline */
				st_unmount(unit, EJECT);
				break;
			case MTNOP:	/* no operation, sets status only */
			case MTCACHE:	/* enable controller cache */
			case MTNOCACHE:	/* disable controller cache */
				break;
			case MTSETBSIZ:	/* Set block size for device */
#ifdef	NOTYET
				if (!(st->flags & ST_NEW_MOUNT)) {
					uprintf("re-mount tape before changing blocksize");
					errcode = EINVAL;
					break;
				}
#endif
				if (number == 0) {
					st->flags &= ~ST_FIXEDBLOCKS;
				} else {
					if ((st->blkmin || st->blkmax)	/* they exist */
					    &&((number < st->blkmin
						    || number > st->blkmax))) {
						errcode = EINVAL;
						break;
					}
					st->flags |= ST_FIXEDBLOCKS;
				}
				st->blksiz = number;
				st->flags |= ST_BLOCK_SET;	/*XXX */
				goto try_new_value;

			case MTSETDNSTY:	/* Set density for device and mode */
				if (number > SCSI_2_MAX_DENSITY_CODE) {
					errcode = EINVAL;
				} else {
					st->density = number;
				}
				goto try_new_value;

			case MTCOMP:	/* enable default compression */
				errcode = st_comp(unit,number);
				break;
			default:
				errcode = EINVAL;
			}
			break;
		}
	case MTIOCIEOT:
	case MTIOCEEOT:
		break;
	default:
		if(IS_CTLMODE(dev))
			errcode = scsi_do_ioctl(dev, cmd, arg, flag, p, sc_link);
		else
			errcode = ENOTTY;
		break;
	}
	return errcode;
/*-----------------------------*/
try_new_value:
	/*
	 * Check that the mode being asked for is aggreeable to the
	 * drive. If not, put it back the way it was.
	 */
	if ( (errcode = st_mode_select(unit, 0, NULL, 0)) ) {	/* put back as it was */
		printf("st%d: Cannot set selected mode", unit);
		st->density = hold_density;
		st->blksiz = hold_blksiz;
		if (st->blksiz) {
			st->flags |= ST_FIXEDBLOCKS;
		} else {
			st->flags &= ~ST_FIXEDBLOCKS;
		}
		return (errcode);
	}
	/*
	 * As the drive liked it, if we are setting a new default,
	 * set it into the structures as such.
	 *
	 * The means for deciding this are not finalised yet
	 */
	if (IS_CTLMODE(dev)) {
		/* special mode */
		/* XXX */
		switch ((short) (mt->mt_op)) {
		case MTSETBSIZ:
			st->modes[dsty].blksiz = st->blksiz;
			st->modeflags[dsty] |= BLKSIZE_SET_BY_USER;
			break;
		case MTSETDNSTY:
			st->modes[dsty].density = st->density;
			st->modeflags[dsty] |= DENSITY_SET_BY_USER;
			break;
		}
	}
	return 0;
}

/*
 * Do a synchronous read.
 */
errval
st_read(unit, buf, size, flags)
	u_int32_t unit, size, flags;
	char   *buf;
{
	struct scsi_link *sc_link = SCSI_LINK(&st_switch, unit);
	struct scsi_data *st = sc_link->sd;
	struct scsi_rw_tape scsi_cmd;

	/*
	 * If it's a null transfer, return immediatly
	 */
	if (size == 0) {
		return (ESUCCESS);
	}
	bzero(&scsi_cmd, sizeof(scsi_cmd));
	scsi_cmd.op_code = READ_COMMAND_TAPE;
	if (st->flags & ST_FIXEDBLOCKS) {
		scsi_cmd.byte2 |= SRWT_FIXED;
		scsi_uto3b(size / (st->blksiz ? st->blksiz : DEF_FIXED_BSIZE),
		    scsi_cmd.len);
	} else {
		scsi_uto3b(size, scsi_cmd.len);
	}
	return (scsi_scsi_cmd(sc_link,
		(struct scsi_generic *) &scsi_cmd,
		sizeof(scsi_cmd),
		(u_char *) buf,
		size,
		0,		/* not on io commands */
		100000,
		NULL,
		flags | SCSI_DATA_IN));
}
#ifdef	__STDC__
#define b2tol(a)	(((unsigned)(a##_1) << 8) + (unsigned)a##_0 )
#else
#define b2tol(a)	(((unsigned)(a/**/_1) << 8) + (unsigned)a/**/_0 )
#endif

/*
 * Ask the drive what it's min and max blk sizes are.
 */
errval
st_rd_blk_lim(unit, flags)
	u_int32_t unit, flags;
{
	struct scsi_link *sc_link = SCSI_LINK(&st_switch, unit);
	struct scsi_data *st = sc_link->sd;
	struct scsi_blk_limits scsi_cmd;
	struct scsi_blk_limits_data scsi_blkl;
	errval  errno;

	/*
	 * First check if we have it all loaded
	 */
	if ((sc_link->flags & SDEV_MEDIA_LOADED))
		return 0;

	/*
	 * do a 'Read Block Limits'
	 */
	bzero(&scsi_cmd, sizeof(scsi_cmd));
	scsi_cmd.op_code = READ_BLK_LIMITS;

	/*
	 * do the command, update the global values
	 */
	if ( (errno = scsi_scsi_cmd(sc_link,
		(struct scsi_generic *) &scsi_cmd,
		sizeof(scsi_cmd),
		(u_char *) & scsi_blkl,
		sizeof(scsi_blkl),
		ST_RETRIES,
		5000,
		NULL,
		flags | SCSI_DATA_IN)) ) {
		return errno;
	}
	st->blkmin = b2tol(scsi_blkl.min_length);
	st->blkmax = scsi_3btou(&scsi_blkl.max_length_2);

	SC_DEBUG(sc_link, SDEV_DB3,
	    ("(%ld <= blksiz <= %ld)\n", st->blkmin, st->blkmax));
	return 0;
}

/*
 * Get the scsi driver to send a full inquiry to the
 * device and use the results to fill out the global
 * parameter structure.
 *
 * called from:
 * attach
 * open
 * ioctl (to reset original blksize)
 */
static errval
st_mode_sense(unit, flags, page, pagelen, pagecode)
	u_int32_t unit, flags;
	struct tape_pages *page;
	u_int32_t pagelen,pagecode;
{
	u_int32_t dat_len;
	errval  errno;
	struct scsi_mode_sense scsi_cmd;
	struct {
		struct scsi_mode_header header;
		struct blk_desc blk_desc;
		struct tape_pages page;
	} dat;

	/* Tandberg tape drives returns page 00
	 * with the sense data, whether or not
	 * you want it( ie the don't like you
	 * saying you want anything less!!!!!
	 * They also expect page 00
	 * back when you issue a mode select
	 */
	struct scsi_link *sc_link = SCSI_LINK(&st_switch, unit);
	struct scsi_data *st = sc_link->sd;

	st->flags &= ~ST_SENSE_READ;
	/*
	 * Check if we need to use a default page..
	 */
	if ((st->quirks & ST_Q_NEEDS_PAGE_0) && (!page)) {
		pagelen = PAGE_0_SENSE_DATA_SIZE;
		page = (struct tape_pages *) st->saved_page0;
		pagecode = 0;
	}
	/*
	 * Now work out the total dat size etc.
	 */
	dat_len = sizeof(struct scsi_mode_header)
		+ sizeof(struct blk_desc)
		+ (page ? pagelen : 0);
	/*
	 * Set up a mode sense
	 */
	bzero(&scsi_cmd, sizeof(scsi_cmd));
	scsi_cmd.op_code = MODE_SENSE;
	scsi_cmd.page = (u_char) pagecode;
	scsi_cmd.length = dat_len;

	/*
	 * do the command,
	 * use the results to set blksiz, numblks and density
	 * or if we need it as a template for the mode select
	 * store it away.
	 */
	if ( (errno = scsi_scsi_cmd(sc_link,
		(struct scsi_generic *) &scsi_cmd,
		sizeof(scsi_cmd),
		(u_char *) &dat,
		dat_len,
		ST_RETRIES,
		5000,
		NULL,
		flags | SCSI_DATA_IN)) ) {
		return errno;
	}
	st->numblks = scsi_3btou(dat.blk_desc.nblocks);
	st->media_blksiz = scsi_3btou(dat.blk_desc.blklen);
	st->media_density = dat.blk_desc.density;
	if (dat.header.dev_spec & SMH_DSP_WRITE_PROT) {
		st->flags |= ST_READONLY;
	}
	SC_DEBUG(sc_link, SDEV_DB3,
	    ("density code 0x%lx, %ld-byte blocks, write-%s, ",
		st->media_density, st->media_blksiz,
		st->flags & ST_READONLY ? "protected" : "enabled"));
	SC_DEBUG(sc_link, SDEV_DB3, ("%sbuffered\n",
		((dat.header.dev_spec & SMH_DSP_BUFF_MODE) ? "" : "un")));
	if (page) {
		bcopy(&dat.page, page, pagelen);
	}
	st->flags |= ST_SENSE_READ;
	return 0;
}

/*
 * Send a filled out parameter structure to the drive to
 * set it into the desire modes etc.
 */
errval
st_mode_select(unit, flags, page, pagelen)
	u_int32_t unit, flags;
	struct tape_pages *page;
	u_int32_t pagelen;
{
	u_int32_t dat_len;
	struct scsi_mode_select scsi_cmd;
	struct {
		struct scsi_mode_header header;
		struct blk_desc blk_desc;
		struct tape_pages page;
	} dat;
	struct scsi_link *sc_link = SCSI_LINK(&st_switch, unit);
	struct scsi_data *st = sc_link->sd;

	/*
	 * Check if we need to use a default page..
	 * Gee, hope we saved one before now........
	 */
	if ((st->quirks & ST_Q_NEEDS_PAGE_0) && (!page)) {
		pagelen = PAGE_0_SENSE_DATA_SIZE;
		page = (struct tape_pages *) st->saved_page0;
	}
	/*
	 * Now work out the total dat size etc.
	 */
	dat_len = sizeof(struct scsi_mode_header)
		+ sizeof(struct blk_desc)
		+ (page ? pagelen : 0);
	/*
	 * Set up for a mode select
	 */
	bzero(&dat, dat_len);
	bzero(&scsi_cmd, sizeof(scsi_cmd));
	scsi_cmd.op_code = MODE_SELECT;
	scsi_cmd.length = dat_len;
	dat.header.blk_desc_len = sizeof(struct blk_desc);
	dat.header.dev_spec |= SMH_DSP_BUFF_MODE_ON;
	dat.blk_desc.density = st->density;
	if (st->flags & ST_FIXEDBLOCKS) {
		scsi_uto3b(st->blksiz, dat.blk_desc.blklen);
	}
	if (page) {
		bcopy(page, &dat.page, pagelen);
		/* the Tandberg tapes need the block size to */
		/* be set on each mode sense/select. */
	}
	/*
	 * do the command
	 */
	return (scsi_scsi_cmd(sc_link,
		(struct scsi_generic *) &scsi_cmd,
		sizeof(scsi_cmd),
		(u_char *) &dat,
		dat_len,
		ST_RETRIES,
		5000,
		NULL,
		flags | SCSI_DATA_OUT));
}

int noisy_st = 0;
/***************************************************************\
* Set the compression mode of the drive to on (1) or off (0)	*
	still doesn't work! grrr!
\***************************************************************/
errval	st_comp(unit,mode)
u_int32_t unit,mode;
{
	struct tape_pages       page;
	int	pagesize;
	int retval;
	struct scsi_link *sc_link = SCSI_LINK(&st_switch, unit);
	struct scsi_data *st = sc_link->sd;

	bzero(&page, sizeof(page));
	pagesize = sizeof(page.pages.configuration) + PAGE_HEADERLEN;

	if ( (retval = st_mode_sense(unit, 0,
			&page, pagesize, ST_PAGE_CONFIGURATION)) )
	{
		printf("sense returned an error of %d\n",retval);
		return retval;
	}
	if ( noisy_st)
		printf("drive reports value of %d, setting %ld\n",
			page.pages.configuration.data_compress_alg,mode);

	page.pg_code &= ST_P_CODE;
	page.pg_length = sizeof(page.pages.configuration);

	switch(mode)
	{
	case 0:
		page.pages.configuration.data_compress_alg = 0;
		break;
	case 1:
		page.pages.configuration.data_compress_alg = 1;
		break;
	default:
		printf("st%ld: bad value for compression mode\n",unit);
		return EINVAL;
	}
	if ( (retval = st_mode_select(unit, 0, &page, pagesize)) )
	{
		printf("select returned an error of %d\n",retval);
		return retval;
	}
	st->comp = mode;
	return 0;
}
/*
 * skip N blocks/filemarks/seq filemarks/eom
 */
errval
st_space(unit, number, what, flags)
	u_int32_t unit, what, flags;
	int32_t   number;
{
	errval  error;
	struct scsi_space scsi_cmd;
	struct scsi_link *sc_link = SCSI_LINK(&st_switch, unit);
	struct scsi_data *st = sc_link->sd;

	switch ((int)what) {
	case SP_BLKS:
		if (st->flags & ST_PER_ACTION) {
			if (number > 0) {
				st->flags &= ~ST_PER_ACTION;
				return (EIO);
			} else if (number < 0) {
				if (st->flags & ST_AT_FILEMARK) {
					/*
					 * Handling of ST_AT_FILEMARK
					 * in st_space will fill in the
					 * right file mark count.
					 */
					error = st_space(unit, 0, SP_FILEMARKS,
					    flags);
					if (error)
						return (error);
				}
				if (st->flags & ST_BLANK_READ) {
					st->flags &= ~ST_BLANK_READ;
					return (EIO);
				}
				st->flags &= ~ST_EIO_PENDING;
			}
		}
		break;
	case SP_FILEMARKS:
		if (st->flags & ST_EIO_PENDING) {
			if (number > 0) {	/* pretend we just discover the error */
				st->flags &= ~ST_EIO_PENDING;
				return (EIO);
			} else if (number < 0) {	/* back away from the error */
				st->flags &= ~ST_EIO_PENDING;
			}
		}
		if (st->flags & ST_AT_FILEMARK) {
			st->flags &= ~ST_AT_FILEMARK;
			number--;
		}
		if ((st->flags & ST_BLANK_READ) && (number < 0)) {	/* back away from unwritten tape */
			st->flags &= ~ST_BLANK_READ;
			number++;	/* dubious */
		}
		break;
	case	SP_EOM:
		if (st->flags & ST_EIO_PENDING)
		{
			/* we are already at EOM */
			st->flags &= ~ST_EIO_PENDING;
			return(ESUCCESS);
		}
		number = 1;	/* we have only one end-of-medium */
		break;
	}
	if (number == 0) {
		return (ESUCCESS);
	}
	bzero(&scsi_cmd, sizeof(scsi_cmd));
	scsi_cmd.op_code = SPACE;
	scsi_cmd.byte2 = what & SS_CODE;
	scsi_uto3b(number, scsi_cmd.number);
	return (scsi_scsi_cmd(sc_link,
		(struct scsi_generic *) &scsi_cmd,
		sizeof(scsi_cmd),
		0,
		0,
		0,		/* no retries please , just fail */
		600000,		/* 10 mins enough? */
		NULL,
		flags));
}

/*
 * write N filemarks
 */
errval
st_write_filemarks(unit, number, flags)
	u_int32_t unit, flags;
	int32_t   number;
{
	struct scsi_write_filemarks scsi_cmd;
	struct scsi_link *sc_link = SCSI_LINK(&st_switch, unit);
	struct scsi_data *st = sc_link->sd;

	/*
	 * It's hard to write a negative number of file marks.
	 * Don't try.
	 */
	if (number < 0) {
		return EINVAL;
	}
	switch ((int)number) {
	case 0:		/* really a command to sync the drive's buffers */
		break;
	case 1:
		if (st->flags & ST_FM_WRITTEN) {	/* already have one down */
			st->flags &= ~ST_WRITTEN;
		} else {
			st->flags |= ST_FM_WRITTEN;
		}
		st->flags &= ~ST_PER_ACTION;
		break;
	default:
		st->flags &= ~(ST_PER_ACTION | ST_WRITTEN);
	}
	bzero(&scsi_cmd, sizeof(scsi_cmd));
	scsi_cmd.op_code = WRITE_FILEMARKS;
	scsi_uto3b(number, scsi_cmd.number);
	return scsi_scsi_cmd(sc_link,
		(struct scsi_generic *) &scsi_cmd,
		sizeof(scsi_cmd),
		0,
		0,
		0,		/* no retries, just fail */
		100000,		/* 10 secs.. (may need to repos head ) */
		NULL,
		flags);
}

/*
 * Make sure the right number of file marks is on tape if the
 * tape has been written.  If the position argument is true,
 * leave the tape positioned where it was originally.
 *
 * nmarks returns the number of marks to skip (or, if position
 * true, which were skipped) to get back original position.
 */
int32_t
st_chkeod(unit, position, nmarks, flags)
	u_int32_t unit;
	boolean position;
	int32_t  *nmarks;
	u_int32_t flags;
{
	errval  error;
	struct scsi_data *st = SCSI_DATA(&st_switch, unit);

	switch ((int)(st->flags & (ST_WRITTEN | ST_FM_WRITTEN | ST_2FM_AT_EOD))) {
	default:
		*nmarks = 0;
		return (ESUCCESS);
	case ST_WRITTEN:
	case ST_WRITTEN | ST_FM_WRITTEN | ST_2FM_AT_EOD:
		*nmarks = 1;
		break;
	case ST_WRITTEN | ST_2FM_AT_EOD:
		*nmarks = 2;
	}
	error = st_write_filemarks(unit, *nmarks, flags);
	if (position && (error == ESUCCESS))
		error = st_space(unit, -*nmarks, SP_FILEMARKS, flags);
	return (error);
}

/*
 * load/unload (with retension if true)
 */
errval
st_load(unit, type, flags)
	u_int32_t unit, type, flags;
{
	struct scsi_load scsi_cmd;
	struct scsi_link *sc_link = SCSI_LINK(&st_switch, unit);
	struct scsi_data *st = sc_link->sd;

	bzero(&scsi_cmd, sizeof(scsi_cmd));
	if (type != LD_LOAD) {
		errval  error;
		int32_t   nmarks;

		error = st_chkeod(unit, FALSE, &nmarks, flags);
		if (error != ESUCCESS)
			return (error);
		sc_link->flags &= ~SDEV_MEDIA_LOADED;
	}
	if (st->quirks & ST_Q_IGNORE_LOADS)
		return (0);
	scsi_cmd.op_code = LOAD_UNLOAD;
	scsi_cmd.how |= type;
	return (scsi_scsi_cmd(sc_link,
		(struct scsi_generic *) &scsi_cmd,
		sizeof(scsi_cmd),
		0,
		0,
		ST_RETRIES,
		300000,		/* 5 min */
		NULL,
		flags));
}

/*
 *  Rewind the device
 */
errval
st_rewind(unit, immed, flags)
	u_int32_t unit, flags;
	boolean immed;
{
	struct scsi_rewind scsi_cmd;
	struct scsi_link *sc_link = SCSI_LINK(&st_switch, unit);
	struct scsi_data *st = sc_link->sd;
	errval  error;
	int32_t   nmarks;

	error = st_chkeod(unit, FALSE, &nmarks, flags);
	if (error != ESUCCESS)
		return (error);
	st->flags &= ~ST_PER_ACTION;
	bzero(&scsi_cmd, sizeof(scsi_cmd));
	scsi_cmd.op_code = REWIND;
	scsi_cmd.byte2 = immed ? SR_IMMED : 0;
	return (scsi_scsi_cmd(sc_link,
		(struct scsi_generic *) &scsi_cmd,
		sizeof(scsi_cmd),
		0,
		0,
		ST_RETRIES,
		immed ? 5000 : (200 * 60 * 1000),	/* 5 sec or 200 min */
		NULL,
		flags));
}

/*
**  Erase the device
*/
errval
st_erase(unit, immed, flags)
	u_int32_t unit, flags;
	boolean immed;
{
	struct scsi_erase scsi_cmd;
	struct scsi_link *sc_link = SCSI_LINK(&st_switch, unit);
	struct scsi_data *st = sc_link->sd;
	errval  error;
	int32_t   nmarks;

	error = st_chkeod(unit, FALSE, &nmarks, flags);
	if (error != ESUCCESS)
		return (error);
	/*
	**      Archive Viper 2525 technical manual 5.7 (ERASE 19h):
	**	tape has to be positioned to BOT first before erase command
	**	is issued or command is rejected. So we rewind the tape first
	**	and exit with an error, if the tape can't be rewinded.
	*/
	error = st_rewind(unit, FALSE, SCSI_SILENT);
	if (error != ESUCCESS)
		return (error);
	st->flags &= ~ST_PER_ACTION;
	bzero(&scsi_cmd, sizeof(scsi_cmd));
	scsi_cmd.op_code = ERASE;
	scsi_cmd.byte2 = SE_LONG;		/* LONG_ERASE */
	scsi_cmd.byte2 += immed ? SE_IMMED : 0; /* immed bit is here the 2nd! */
	return (scsi_scsi_cmd(sc_link,
		(struct scsi_generic *) &scsi_cmd,
		sizeof(scsi_cmd),
		0,
		0,
		ST_RETRIES,
		immed ? 5000 : 300000,	/* 5 sec or 5 min */
		NULL,
		flags));
}

/*
 * Look at the returned sense and act on the error and detirmine
 * The unix error number to pass back... (0 = report no error)
 *                            (SCSIRET_CONTINUE = continue processing)
 */
errval
st_interpret_sense(xs)
	struct scsi_xfer *xs;
{
	struct scsi_link *sc_link = xs->sc_link;
	struct scsi_sense_data *sense = &(xs->sense);
	boolean silent = xs->flags & SCSI_SILENT;
	u_int32_t unit = sc_link->dev_unit;
	struct scsi_data *st = SCSI_DATA(&st_switch, unit);
	u_int32_t key;
	int32_t   info;

	/*
	 * Get the sense fields and work out what code
	 */
	if (sense->error_code & SSD_ERRCODE_VALID) {
		info = ntohl(*((int32_t *) sense->ext.extended.info));
	} else {
		if (st->flags & ST_FIXEDBLOCKS) {
			info = xs->datalen / st->blksiz;
		} else {
			info = xs->datalen;
		}
	}
	if ((sense->error_code & SSD_ERRCODE) != 0x70) {
		return SCSIRET_CONTINUE;/* let the generic code handle it */
	}
	if(sense->ext.extended.flags & (SSD_EOM|SSD_FILEMARK|SSD_ILI)) {
		if (st->flags & ST_FIXEDBLOCKS) {
			xs->resid = info * st->blksiz;
			xs->flags |= SCSI_RESID_VALID;
			if (sense->ext.extended.flags & SSD_EOM) {
				st->flags |= ST_EIO_PENDING;
			}
			if (sense->ext.extended.flags & SSD_FILEMARK) {
				st->flags |= ST_AT_FILEMARK;
			}
			if (sense->ext.extended.flags & SSD_ILI) {
				st->flags |= ST_EIO_PENDING;
				if (sense->error_code & SSD_ERRCODE_VALID &&
			    	!silent)
					printf("st%ld: block wrong size"
				    	", %ld blocks residual\n", unit
				    	,info);
				/*XXX*/ /* is this how it works ? */
				/* check def of ILI for fixed blk tapes */

				/*
			 	 * This quirk code helps the drive read
			 	 * the first tape block, regardless of
			 	 * format.  That is required for these
			 	 * drives to return proper MODE SENSE
			 	 * information.
			 	 */
				if ((st->quirks & ST_Q_SNS_HLP) &&
				    !(st->flags & ST_SENSE_READ)) {
					st->blksiz -= 512;
				}
			}
			/*
			 * If no data was tranfered, do it immediatly
			 */
			if (xs->resid >= xs->datalen) {
				xs->flags &= ~SCSI_RESID_VALID;
				if (st->flags & ST_AT_FILEMARK) {
					xs->flags |= SCSI_EOF;
					st->flags &= ~ST_AT_FILEMARK;
					return 0;
				}
				if (st->flags & ST_EIO_PENDING) {
					st->flags &= ~ST_EIO_PENDING;
					return EIO;
				}
			}
			return 0;
		} else {		/* must be variable mode */
			xs->resid = xs->datalen;	/* to be sure */
			if (sense->ext.extended.flags & SSD_EOM) {
				return (EIO);
			}
			if (sense->ext.extended.flags & SSD_FILEMARK) {
				xs->flags |= SCSI_EOF;
			}
			if (sense->ext.extended.flags & SSD_ILI) {
				if (info < 0) {
					/*
					 * the record was bigger than the read
					 */
					if (!silent)
						printf("st%ld: %ld-byte record "
					    	"too big\n", unit,
					    	xs->datalen - info);
					return (EIO);
				}
				xs->resid = info;
				xs->flags |= SCSI_RESID_VALID;
			}
		}
		return 0;
	}
	key = sense->ext.extended.flags & SSD_KEY;

	if (key == 0x8) {
		xs->flags |= SCSI_EOF; /* some drives need this */
		/*
		 * This quirk code helps the drive read the
		 * first tape block, regardless of format.  That
		 * is required for these drives to return proper
		 * MODE SENSE information.
		 */
		if ((st->quirks & ST_Q_SNS_HLP) &&
		    !(st->flags & ST_SENSE_READ)) {
			/* still starting */
			st->blksiz -= 512;
		} else if (!(st->flags & (ST_2FM_AT_EOD | ST_BLANK_READ))) {
			st->flags |= ST_BLANK_READ;
			xs->flags |= SCSI_EOF;
			return (ESUCCESS);
		}
	}
	return SCSIRET_CONTINUE;  /* Use the the generic handler */
}

/*
 * The quirk here is that the drive returns some value to st_mode_sense
 * incorrectly until the tape has actually passed by the head.
 *
 * The method is to set the drive to large fixed-block state (user-specified
 * density and 1024-byte blocks), then read and rewind to get it to sense the
 * tape.  If that doesn't work, try 512-byte fixed blocks.  If that doesn't
 * work, as a last resort, try variable- length blocks.  The result will be
 * the ability to do an accurate st_mode_sense.
 *
 * We know we can do a rewind because we just did a load, which implies rewind.
 * Rewind seems preferable to space backward if we have a virgin tape.
 *
 * The rest of the code for this quirk is in ILI processing and BLANK CHECK
 * error processing, both part of st_interpret_sense.
 */
errval
st_touch_tape(unit)
	u_int32_t	unit;
{
	struct scsi_data *st = SCSI_DATA(&st_switch, unit);
	char   *buf;
	u_int32_t readsiz;
	errval  errno;

	buf = malloc(1024, M_TEMP, M_NOWAIT);
	if (!buf)
		return (ENOMEM);

	if (( errno = st_mode_sense(unit, 0, NULL, 0, 0)) ) {
		goto bad;
	}
	st->blksiz = 1024;
	do {
		switch ((int)st->blksiz) {
		case 512:
		case 1024:
			readsiz = st->blksiz;
			st->flags |= ST_FIXEDBLOCKS;
			break;
		default:
			readsiz = 1;
			st->flags &= ~ST_FIXEDBLOCKS;
		} if ( (errno = st_mode_select(unit, 0, NULL, 0)) ) {
			goto bad;
		}
		st_read(unit, buf, readsiz, SCSI_SILENT);
		if ( (errno = st_rewind(unit, FALSE, 0)) ) {
bad:			free(buf, M_TEMP);
			return (errno);
		}
	} while (readsiz != 1 && readsiz > st->blksiz);
	free(buf, M_TEMP);
	return 0;
}
