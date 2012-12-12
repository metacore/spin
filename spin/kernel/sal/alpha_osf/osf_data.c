
#include <net/if.h>
#include <kern/task.h>
#include <vm/vm_map.h>
#include <machine/pcb.h>

/* controls how many zones DEC OSF/1 allocates. */
int NumZonePages = 50;     /* # of pages for zalloc.  leave alone */
vm_offset_t	zdata;
vm_size_t	zdata_size;


pmap_t current_pmap;
struct task current_task;

vm_map_t	kernel_map;
vm_map_t	kernel_pageable_map;


struct proc *current_proc;
#undef u
struct pcb u;

int tu_eisa_options; /* we do not have eisa tu; only pci tu */

struct       ifqueue ipintrq;

/*
 * These variables satisfy references in arch/alpha/kernargs.c.
 * They refer to boot time modifiable kernel parameters
 *  (which we generally don't use) [savage]
 */
long	syscalltrace = 0;  /* unused */
long	msgbuf_size = 0;   /* unused */
long	lockdebug = 0;     /* unused */
long	locktimeout = 0;   /* unused */
long	rt_preempt_opt = 0;/* unused */
long    bufpages = 0;	   /* unused */
long    nbuf = 0;	   /* unused */

/* 
 * Used by kernargs, pmap_init, and some net code.  lockmode indicates
 * the requested level of lock support, 0 = uniproc.
 */
long	lockmode = 0;


#include <mach/machine.h>
/*
 * these store information about the cpu's in the system
 *
 * Location in DEC UNIX: kern/machine.c
 */
struct machine_info	machine_info;
struct machine_slot	machine_slot[NCPUS];

/*
 * these define the IPL name, args for the system, and the name of the
 * booted image (not really used for SPIN)
 *
 * Location in DEC UNIX: bsd/kern_exec.c
 */
#define MAX_INIT_NAME 128
#define BOOTEDFILELEN   80
char init_args[MAX_INIT_NAME] = "-sa\0";
char init_program_name[MAX_INIT_NAME] = "SPUNK\0";
char bootedfile[BOOTEDFILELEN];


/*
 * indicates if /dev/klog and /dev/kcon is open for bsd/subr_prf.c
 *    (doesn't matter in SPIN)
 *
 * Location in DEC UNIX: bsd/subr_log.c
 */
int log_open = 1;
int con_open = 1;

/*
 * cfree, cfreelist, and cfreecount are used to manage the character
 * lists (clists) that we use in the tty code.  nclist indicates how
 * many independent such lists there can be. 
 *
 * Location in DEC UNIX: conf/param.c (used by bsd/tty.c)
 */
struct cblock *cfree = 0;
struct cblock *cfreelist = 0;
int cfreecount = 0;
int nclist = 20;  /* arbitrary */


/*
 * tk_nin, and tk_nout count the number of charcaters passing through
 * the tty calls.
 *
 * Location in DEC UNIX: bsd/init_main.c (used by bsd/tty.c)
 */
long	tk_nin;
long	tk_nout;


/*
 * time structure holds the current system time.  
 *
 * Location in DEC UNIX: bsd/init_main.c
 */
/* struct timeval	time = {0,0};  */
struct timeval	time;
/*
 * hz is clock interrupts per second
 * tick is usecs between clock interrupts
 * tickadj is clock skew in usecs per tick
 * fixtick is spew to deal with clock freqs which don't evenly divide 1 sec
 *
 * Location in DEC UNIX: bsd/init_main.c or conf/param.c
 *   (set in arch/alpha/alpha_init.c)
 */
int hz;
int tick;
int tickadj;
int fixtick;

/*
 * Location in DEC UNIX: bsd/kern_time.c
 */
int     tickdelta;                      /* current clock skew, us. per tick */
long    timedelta;                      /* unapplied time correction, us. */


/*
 * lbolt should be updated on each hardclock interrupt
 *
 * Location in DEC UNIX: bsd/kern_clock.c
 */
long	lbolt;

int select_max_elements = 512;
int select_chunk_elements = 128;
