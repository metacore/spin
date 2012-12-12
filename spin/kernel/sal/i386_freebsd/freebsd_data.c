#include <sys/param.h>
#include <sys/kernel.h>
#include <vm/vm.h>

/*****
 from i386/i386/swtch.s
 */

struct pcb *curpcb = NULL;

/*****
 from kern/init_main.c
 */

struct	proc proc0;
struct	proc *curproc = &proc0;
char	copyright[] =
"Copyright (c) 1982, 1986, 1989, 1991, 1993\n\tThe Regents of the University of California.  All rights reserved.\n\n";


/*****
 from kern/kern_time.c
 */

extern	int tickadj;			/* "standard" clock skew, us./tick */
int	tickdelta;			/* current clock skew, us. per tick */
long	timedelta;			/* unapplied time correction, us. */
long	bigadj = 1000000;		/* use 10x skew above bigadj us. */

/*****
 from kern/kern_sysctl.c
 */

char kernelname[MAXPATHLEN] = "/kernel";	/* XXX bloat */

/*****
 from kern/kern_synch.c
 */

/*
 * lbolt should be updated on each hardclock interrupt
 */
int	lbolt;			/* once a second sleep address */

/*****
 from kern/kern_clock.c
 */

#include <sys/dkstat.h>
#include <sys/timex.h>

long cp_time[CPUSTATES];
long dk_seek[DK_NDRIVE];
long dk_time[DK_NDRIVE];
long dk_wds[DK_NDRIVE];
long dk_wpms[DK_NDRIVE];
long dk_xfer[DK_NDRIVE];

int dk_busy;
int dk_ndrive = 0;
char dk_names[DK_NDRIVE][DK_NAMELEN];


long tk_cancc;
long tk_nin;
long tk_nout;
long tk_rawcc;

int	stathz;
int	profhz;
int	profprocs;
int	ticks;
static int psdiv, pscnt;	/* prof => stat divider */
int	psratio;		/* ratio: prof / stat */

volatile struct	timeval time;
volatile struct	timeval mono_time;

/*****
 from kern/subr_log.c
 */

int	log_open;			/* also used in log() */


/*****
 from vm/vm_kern.c
 */

#include <sys/malloc.h>
vm_map_t kernel_map;
vm_map_t kmem_map;
vm_map_t mb_map;



/*****
 from net/if.c
 */

#include <net/if.h>

struct	ifqueue ipintrq;
