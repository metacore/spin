/*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 */
/*
    cam.c

    C routines to create uio structs for cdisk_* calls into cam driver.
 */
/*
 * HISTORY
 * 05-May-96  Charles Garrett (garrett) at the University of Washington
 *	Include statement changed to not use explicit ALPHA_SPIN in
 *      pathname.
 *
 * 29-Oct-95  Brian Bershad (bershad) at the University of Washington
 *	Renamed cam_free to cam_free_buffer to reflect advertised name in
 *	 .i3 files.
 *
 * 06-Aug-95  Brian Bershad (bershad) at the University of Washington
 *	Imported from oystr.
 *
 */

#include <sys/types.h>
#include <sys/uio.h>
#include <spincore/src/sal/OpenArray.h>

#define	DEV_BSIZE	512
#define	FS_INVALID_FS		5004		/* bad file system */
#define	FS_INVALID_PARAMETER	5006		/* bad parameter to
						   a routine */

int dev2camdbg = 0;

#define dprintf(x)  if ( dev2camdbg ) printf x

extern int cdisk_open();
extern int cdisk_read();
extern int cdisk_close();

int
cam_read(dev_t dev, struct openarray *data, int offset, int *bytes)
{
    int res;
    struct uio uio;
    struct iovec iov;
    
    iov.iov_len = *bytes;
    iov.iov_base = (caddr_t)data->start;
    if ( !iov.iov_base ) {
	printf("Can't kalloc %d bytes for iov_base\n",*bytes);
	return(1);
    }
    uio.uio_rw = UIO_READ;
    uio.uio_segflg = UIO_SYSSPACE;
    uio.uio_iovcnt = 1;
    uio.uio_resid = iov.iov_len;
    uio.uio_offset = offset;
    uio.uio_iov = &iov;
    dprintf(("device_read for dev %lx, want %d bytes\n",dev,*bytes));
    dprintf(("  uio @ %lx, iov @ %lx\n",&uio,&iov));
    dprintf(("  io_base @ %lx, recnum %d\n",iov.iov_base,offset/DEV_BSIZE));
    res = cdisk_read(dev, &uio);
    if ( res ) {
	printf("disk_read returned %d\n",res);
	return(res);
    }
    *bytes = *bytes - uio.uio_resid;
    dprintf(("  data_count returned is %d\n",*bytes));
    return(res);
}

int
cam_write(dev_t dev, struct openarray *data, int offset, int *bytes)
{
    int res;
    struct uio uio;
    struct iovec iov;

    iov.iov_len = *bytes;
    iov.iov_base = data->start;
    uio.uio_rw = UIO_WRITE;
    uio.uio_segflg = UIO_SYSSPACE;
    uio.uio_iovcnt = 1;
    uio.uio_resid = iov.iov_len;
    uio.uio_offset = offset;
    uio.uio_iov = &iov;
    dprintf(("device_write for dev %lx, want %d bytes\n",dev,*bytes));
    dprintf(("  uio @ %lx, iov @ %lx\n",&uio,&iov));
    dprintf(("  io_base @ %lx, recnum %d\n",iov.iov_base,offset/DEV_BSIZE));
    res = cdisk_write(dev, &uio);
    if ( res ) {
        printf("disk_write returned %d\n",res);
        return(res);
    }
    *bytes = *bytes - uio.uio_resid;
    dprintf(("  data_count returned is %d\n",*bytes));
    return(res);
}

/*
 * Auto configuration
 *
 */

#include "CAMPrivate.h"
#define RZmajor 8
#define TZmajor 9

void
cam_conn_ctlr(char *name, int unit)
{
	/* tcds and scsi controllers will pass through here */
}

void
cam_conn_device(char *name, int unit)
{
	if (strncmp(name, "rz", 2) == 0)
		CAMPrivate__RegisterRZ( name, unit, (RZmajor<<20)|(unit<<10));
	if (strncmp(name, "tz", 2) == 0)
		CAMPrivate__RegisterTZ( name, unit, (TZmajor<<20)|(unit<<10));
}


#include <io/common/devdriver.h>
#include <io/common/iotypes.h>
#include <io/cam/cam_debug.h>
cam_attach()
{

	extern struct bus *system_bus;
	extern struct bus *bus_list, cam_bus_list[];
	extern struct controller *controller_list, cam_controller_list[];
	extern struct device *device_list, cam_device_list[];
	extern int cam_subsystem_ready;
	extern int cold;
	extern void (*conn_device_hook)();
	extern void (*conn_ctlr_hook)();

	/*
	camdbg_flag = CAMD_INOUT|CAMD_INTERRUPT|CAMD_CONFIG;
	*/
	camdbg_flag = 0;


	/* cam has some unique stuff to run in additional to the normal
		autoconf routinees
	*/
	printf("  Initializing CAM...\n");
	ccfg_initialize();
	if ( cam_subsystem_ready ) {
		printf("  Initializing XPT threads...\n");
		scsiisr_init();
		}
	else {
		printf(" Couldn't initialize CAM software\n");
		return(0);
		}



	/* setup the autoconf sturctures.  Our ioconf.c has the bus and
	    device lists needed by autoconf.
	*/
	bus_list = cam_bus_list;
	controller_list = cam_controller_list;
	device_list = cam_device_list;

	/* The devices and controllers will use these callbacks so we
		can initialize our own structures.
	*/
	conn_device_hook = cam_conn_device;
	conn_ctlr_hook = cam_conn_ctlr;

	/* Start the drivers */
	cold=1; /* the cam bus reset routine looks at cold */
	(*system_bus->confl1)(-1,0,system_bus);
	cold=0;
	(*system_bus->confl2)(-1,0,system_bus);


	conn_device_hook = 0;
	conn_ctlr_hook = 0;

	return 1;
}
