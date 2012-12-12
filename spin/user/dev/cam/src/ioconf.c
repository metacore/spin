#include <sys/param.h>
#include <sys/buf.h>
#include <sys/map.h>
#include <sys/vm.h>
#include <sys/config.h>
#include <io/common/devdriver.h>

#include <io/dec/uba/ubavar.h>


int nulldev();

int (*tcint0[])() = { nulldev, 0 };	/* no interrupt routine for tc */
extern tcdsintr();
int	 (*tcdsint0[])()	= { tcdsintr, 0 } ;

extern struct driver scsidriver;
extern scsiintr();
int	 (*scsiint0[])()	= { scsiintr, 0 } ;

int (*pciint0[])() = { nulldev, 0 };	/* no interrupt routine for pci */
extern psiopintr();
int	 (*psiopint0[])()	= { psiopintr, 0 } ;

int (*isaint0[])() = { nulldev, 0 };	/* no interrupt routine for isa */


/* struct	port {	*/
/*	int	(*conf)();		/* config routine for this port */
/*};	*/





extern int tcconfl1();
extern int tcconfl2();
extern int tcdsconfl1();
extern int tcdsconfl2();
extern int pciconfl1();
extern int pciconfl2();
extern int psiopconfl1();
extern int psiopconfl2();
extern int isaconfl1();
extern int isaconfl2();


/* struct	bus {	*/
/*	u_long	        *bus_mbox	/* bus mail box */
/*	struct bus	*nxt_bus	/* next bus				*/
/*	struct controller *ctlr_list;	/* controllers connected to this bus */
/*	struct bus	*bus_hd;	/* pointer to bus this bus connected to */
/*	struct bus	*bus_list;	/* buses connected to this bus */
/*	int		bus_type;	/* bus type 			*/
/*	char		*bus_name;	/* bus name 			*/
/*	int		bus_num;	/* bus number			*/
/*	int		slot;		/* node or slot number		*/
/*	char		*connect_bus;	/* conected to bus name 	*/
/*	int		connect_num;	/* connected to bus number	*/
/*	int		(*confl1)();	/* Level 1 configuration routine */
/*	int		(*confl2)();	/* Level 2 configuration routine */
/*	char		*pname;		/* port name, if needed		*/
/*	struct port	*port;		/* pointer to port structure	*/
/*	int		(**intr)();	/* interrupt routine(s) for this bus  */
/*	int		alive;		/* alive indicator		*/
/*	struct bus_framework	*framework; /* Subsystem expansion routines */
/*	char		*driver_name;	/* name of controlling driver */
/*	void		*private[8];	/* Reserved for bus use		*/
/*	void		*conn_priv[8];	/* Reserved for connected bus use*/
/*	void 		*rsvd[8];	/* Reserved for future expansion */
/* };	*/


struct bus cam_bus_list[] = {
	{ 0,0,0,0,0,0,"tc",0,-1,"nexus",-1,tcconfl1,tcconfl2,"",0,tcint0,0,0,"",0,0,0},
	{ 0,0,0,0,0,0,"tcds",0,-1,"tc",0,tcdsconfl1,tcdsconfl2,"",0,tcdsint0,0,0,"",0,0,0},
	{ 0,0,0,0,0,0,"pci",0,-1,"nexus",-1,pciconfl1,pciconfl2,"",0,pciint0,0,0,"",0,0,0},
	{ 0,0,0,0,0,0,"psiop",0,-1,"pci",0,psiopconfl1,psiopconfl2,"",0,psiopint0,0,0,"",0,0,0},
	{ 0,0,0,0,0,0,"isa",0,-1,"pci",0,isaconfl1,isaconfl2,"",0,isaint0,0,0,"",0,0,0},
	{ 0,0,0,0,0,0,(char *)0,0,0,"",0,0,0,"",0,0,0,0,(char *)0,0,0,0}
};

/* struct	controller {	*/
/*	u_long         *ctlr_mbox;	/* ctlr mailbox */
/*	struct controller *nxt_ctlr;	/* pointer to next ctlr on this bus */
/*	struct device	*dev_list;	/* devices connected to this ctlr */
/*	struct bus	*bus_hd;	/* pointer to bus for this ctlr   */
/*	struct driver	*driver;	/* pointer to driver structure for */
/*					/* this controller 		   */
/*	int		ctlr_type;	/* controller type		*/
/*	char		*ctlr_name;	/* controller name		*/
/*	int		ctlr_num;	/* controller number		*/
/*	char		*bus_name;	/* bus name			*/
/*	int		bus_num;	/* bus number connected to 	*/
/*	int		rctlr;		/* remote controller number	*/
/*					/* e.g. ci node or scsi id	*/
/*	int		slot;		/* node or slot number		*/
/*	int		alive;		/* alive indicator		*/
/*	char		*pname;		/* port name			*/
/*	struct port	*port;		/* port structure		*/
/*	int		(**intr)();	/* interrupt routine(s) for this ctlr */
/*	caddr_t		addr;		/* virtual address of controller */
/*	caddr_t		addr2;		/* virtual address of second ctlr */
/*					/* register space		  */
/*	int		flags;		/* flags from from config 	*/
/*	int		bus_priority;	/* bus priority from from config */
/*	int		ivnum;		/* interrupt vector number	*/
/*	int		priority;	/* system ipl level		*/
/*	int		cmd;		/* cmd for go routine		*/
/*	caddr_t		physaddr;	/* physical address of addr	*/
/*	caddr_t		physaddr2;	/* physical address of addr2	*/
/*	void 		*private[8];	/* Reserved for ctlr use	*/
/*	void 		*conn_priv[8];	/* Reserved for connected bus use*/
/*	void 		*rsvd[8];	/* reserved for future expansion */
/* };	*/




struct controller cam_controller_list[] = {
	{ 0,0,0,0,&scsidriver,0,"scsi",0,"*",-99,0,-1,0,"",0,scsiint0,0,0,0x0,0,0x0,0,0,0,0,0,0,0},
	{ 0,0,0,0,0,0,(char *)0,0,"",0,0,0,0,"",0,0,0,0,0,0,0,0,0,0,0,0,0}
};

/* struct	device_list {	*/
/*	struct device	*nxt_dev;	/* pointer to next dev on this ctlr */
/*	struct controller *ctlr_hd;	/* pointer to ctlr for this device */
/*	char		*dev_type;	/* device type			*/
/*	char		*dev_name;	/* device name			*/
/*	int		logunit;	/* logical unit	number		*/
/*	int		unit;		/* physical unit number		*/
/*	int		ctlr_num;	/* controller number for this device */
/*	char		*ctlr_name;	/* controller name connected to */
/*	int		alive;		/* alive indicator ( -1 if at   */
/*	                                /* nexus, -2 if dead, -4 if     */
/*	                                /* read only, -8 if write only )*/
/*	void		private[8];	/* reserved for device use	*/
/*	void 		conn_priv[8];	/* Reserved for connected ctlr use*/
/*	void 		rsvd[8];	/* reserved for future expansion */
/* };	*/


struct device cam_device_list[] = {
	{ 0,0,"disk","rz",0,0,"scsi",0,0,0,0,0},
	{ 0,0,"disk","rz",1,8,"scsi",0,0,0,0,0},
	{ 0,0,"disk","rz",2,16,"scsi",0,0,0,0,0},
	{ 0,0,"disk","rz",3,24,"scsi",0,0,0,0,0},
	{ 0,0,"disk","rz",4,32,"scsi",0,0,0,0,0},
	{ 0,0,"disk","rz",5,40,"scsi",0,0,0,0,0},
	{ 0,0,"disk","rz",6,48,"scsi",0,0,0,0,0},
	{ 0,0,"disk","rz",7,56,"scsi",0,0,0,0,0},
	{ 0,0,"",(char *)0,0,0,"",0,0,0,0,0}
};
