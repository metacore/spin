/*
  	freebsd_subr.c

	Contains all the subroutines borrowed from freebsd where we
	chose not to take the entire file.  Most are just stubs for
	symbols used in the entire files we did borrow.

	Some, like main and the vm procs, are our a reimplementation.

	All signatures and comments are copied directory from the
	original file.  The original file name leads each section of
	copied code.

	created by David Becker Wed Jun 25 09:34:50 PDT 1997
 */
/*
 * HISTORY
 * 28-Aug-97  becker at the University of Washington
 *	Added biodone and biowait for bdev
 *
 * 25-Jun-97  becker at the University of Washington
 *	Moved network subroutines to freebsd_netsubr since thats all
 *	salboot needed.
 */

#include <sal/salhook.h>
#include <sys/param.h>
#include <sys/kernel.h>
#include <sys/systm.h>

#include <vm/vm.h>
#include <vm/vm_kern.h>

#include <sal/i386_freebsd/sal.h>

/*****
 from kern/kern_clock.c
 */

#include <sys/timex.h>
#include <machine/clock.h>

/*
 * Phase-lock loop (PLL) definitions
 *
 * The following variables are read and set by the ntp_adjtime() system
 * call.
 *
 * time_state shows the state of the system clock, with values defined
 * in the timex.h header file.
 *
 * time_status shows the status of the system clock, with bits defined
 * in the timex.h header file.
 *
 * time_offset is used by the PLL to adjust the system time in small
 * increments.
 *
 * time_constant determines the bandwidth or "stiffness" of the PLL.
 *
 * time_tolerance determines maximum frequency error or tolerance of the
 * CPU clock oscillator and is a property of the architecture; however,
 * in principle it could change as result of the presence of external
 * discipline signals, for instance.
 *
 * time_precision is usually equal to the kernel tick variable; however,
 * in cases where a precision clock counter or external clock is
 * available, the resolution can be much less than this and depend on
 * whether the external clock is working or not.
 *
 * time_maxerror is initialized by a ntp_adjtime() call and increased by
 * the kernel once each second to reflect the maximum error
 * bound growth.
 *
 * time_esterror is set and read by the ntp_adjtime() call, but
 * otherwise not used by the kernel.
 */
int time_status = STA_UNSYNC;	/* clock status bits */
int time_state = TIME_OK;	/* clock state */
long time_offset = 0;		/* time offset (us) */
long time_constant = 0;		/* pll time constant */
long time_tolerance = MAXFREQ;	/* frequency tolerance (scaled ppm) */
long time_precision = 1;	/* clock precision (us) */
long time_maxerror = MAXPHASE;	/* maximum error (us) */
long time_esterror = MAXPHASE;	/* estimated error (us) */

/*
 * The following variables establish the state of the PLL and the
 * residual time and frequency offset of the local clock. The scale
 * factors are defined in the timex.h header file.
 *
 * time_phase and time_freq are the phase increment and the frequency
 * increment, respectively, of the kernel time variable at each tick of
 * the clock.
 *
 * time_freq is set via ntp_adjtime() from a value stored in a file when
 * the synchronization daemon is first started. Its value is retrieved
 * via ntp_adjtime() and written to the file about once per hour by the
 * daemon.
 *
 * time_adj is the adjustment added to the value of tick at each timer
 * interrupt and is recomputed at each timer interrupt.
 *
 * time_reftime is the second's portion of the system time on the last
 * call to ntp_adjtime(). It is used to adjust the time_freq variable
 * and to increase the time_maxerror as the time since last update
 * increases.
 */
long time_phase = 0;		/* phase offset (scaled us) */
long time_freq = 0;		/* frequency offset (scaled ppm) */
long time_adj = 0;		/* tick adjust (scaled 1 / hz) */
long time_reftime = 0;		/* time at last adjustment (s) */


/*
 * Statistics clock.  Grab profile sample, and if divider reaches 0,
 * do process and kernel statistics.
 */
void
statclock(frame)
	register struct clockframe *frame;
{
/* ignore */
}

/*
 * Bump a timeval by a small number of usec's.
 */
#define BUMPTIME(t, usec) { \
	register volatile struct timeval *tp = (t); \
	register long us; \
 \
	tp->tv_usec = us = tp->tv_usec + (usec); \
	if (us >= 1000000) { \
		tp->tv_usec = us - 1000000; \
		tp->tv_sec++; \
	} \
}

/*
 * The real-time timer, interrupting hz times per second.
 */
void
hardclock(frame)
	register struct clockframe *frame;
{
	/*
	 * If no separate statistics clock is available, run it from here.
	 */
	if (stathz == 0)
		statclock(frame);

	/*
	 * Increment the time-of-day.
	 */
	ticks++;
	{
		int time_update;
		struct timeval newtime = time;
		long ltemp;

		if (timedelta == 0) {
			time_update = tick;
		} else {
			time_update = tick + tickdelta;
			timedelta -= tickdelta;
		}

		/*
		 * Compute the phase adjustment. If the low-order bits
		 * (time_phase) of the update overflow, bump the high-order bits
		 * (time_update).
		 */
		time_phase += time_adj;
		if (time_phase <= -FINEUSEC) {
		  ltemp = -time_phase >> SHIFT_SCALE;
		  time_phase += ltemp << SHIFT_SCALE;
		  time_update -= ltemp;
		}
		else if (time_phase >= FINEUSEC) {
		  ltemp = time_phase >> SHIFT_SCALE;
		  time_phase -= ltemp << SHIFT_SCALE;
		  time_update += ltemp;
		}

		BUMPTIME(&mono_time, time_update);
		newtime.tv_usec += time_update;

		/*
		 * On rollover of the second the phase adjustment to be used for
		 * the next second is calculated. Also, the maximum error is
		 * increased by the tolerance. If the PPS frequency discipline
		 * code is present, the phase is increased to compensate for the
		 * CPU clock oscillator frequency error.
		 *
		 * With SHIFT_SCALE = 23, the maximum frequency adjustment is
		 * +-256 us per tick, or 25.6 ms/s at a clock frequency of 100
		 * Hz. The time contribution is shifted right a minimum of two
		 * bits, while the frequency contribution is a right shift.
		 * Thus, overflow is prevented if the frequency contribution is
		 * limited to half the maximum or 15.625 ms/s.
		 */
		if (newtime.tv_usec >= 1000000) {
		  newtime.tv_usec -= 1000000;
		  newtime.tv_sec++;
		  time_maxerror += time_tolerance >> SHIFT_USEC;
		  if (time_offset < 0) {
		    ltemp = -time_offset >>
		      (SHIFT_KG + time_constant);
		    time_offset += ltemp;
		    time_adj = -ltemp <<
		      (SHIFT_SCALE - SHIFT_HZ - SHIFT_UPDATE);
		  } else {
		    ltemp = time_offset >>
		      (SHIFT_KG + time_constant);
		    time_offset -= ltemp;
		    time_adj = ltemp <<
		      (SHIFT_SCALE - SHIFT_HZ - SHIFT_UPDATE);
		  }

		  ltemp = time_freq;

		  if (ltemp < 0)
		    time_adj -= -ltemp >>
		      (SHIFT_USEC + SHIFT_HZ - SHIFT_SCALE);
		  else
		    time_adj += ltemp >>
		      (SHIFT_USEC + SHIFT_HZ - SHIFT_SCALE);

		  /*
		   * When the CPU clock oscillator frequency is not a
		   * power of two in Hz, the SHIFT_HZ is only an
		   * approximate scale factor. In the SunOS kernel, this
		   * results in a PLL gain factor of 1/1.28 = 0.78 what it
		   * should be. In the following code the overall gain is
		   * increased by a factor of 1.25, which results in a
		   * residual error less than 3 percent.
		   */
		  /* Same thing applies for FreeBSD --GAW */
		  if (hz == 100) {
		    if (time_adj < 0)
		      time_adj -= -time_adj >> 2;
		    else
		      time_adj += time_adj >> 2;
		  }

		  /* XXX - this is really bogus, but can't be fixed until
		     xntpd's idea of the system clock is fixed to know how
		     the user wants leap seconds handled; in the mean time,
		     we assume that users of NTP are running without proper
		     leap second support (this is now the default anyway) */
		  /*
		   * Leap second processing. If in leap-insert state at
		   * the end of the day, the system clock is set back one
		   * second; if in leap-delete state, the system clock is
		   * set ahead one second. The microtime() routine or
		   * external clock driver will insure that reported time
		   * is always monotonic. The ugly divides should be
		   * replaced.
		   */
		  switch (time_state) {

		  case TIME_OK:
		    if (time_status & STA_INS)
		      time_state = TIME_INS;
		    else if (time_status & STA_DEL)
		      time_state = TIME_DEL;
		    break;

		  case TIME_INS:
		    if (newtime.tv_sec % 86400 == 0) {
		      newtime.tv_sec--;
		      time_state = TIME_OOP;
		    }
		    break;

		  case TIME_DEL:
		    if ((newtime.tv_sec + 1) % 86400 == 0) {
		      newtime.tv_sec++;
		      time_state = TIME_WAIT;
		    }
		    break;

		  case TIME_OOP:
		    time_state = TIME_WAIT;
		    break;

		  case TIME_WAIT:
		    if (!(time_status & (STA_INS | STA_DEL)))
		      time_state = TIME_OK;
		  }
		}
		CPU_CLOCKUPDATE(&time, &newtime);
	}

      /* upcall to spincore */
      salhook_clock_intr((void*)frame);
}


/*****
 from i386/i386/trap.c
 */

#include <machine/cpu.h>
#include <machine/psl.h>
#include <machine/../isa/isa_device.h>

extern inthand_t IDTVEC(syscall);
extern void syscall __P((struct trapframe frame));

#define MAX_TRAP_MSG		27
char *trap_msg[] = {
	"",					/*  0 unused */
	"privileged instruction fault",		/*  1 T_PRIVINFLT */
	"",					/*  2 unused */
	"breakpoint instruction fault",		/*  3 T_BPTFLT */
	"",					/*  4 unused */
	"",					/*  5 unused */
	"arithmetic trap",			/*  6 T_ARITHTRAP */
	"system forced exception",		/*  7 T_ASTFLT */
	"",					/*  8 unused */
	"general protection fault",		/*  9 T_PROTFLT */
	"trace trap",				/* 10 T_TRCTRAP */
	"",					/* 11 unused */
	"page fault",				/* 12 T_PAGEFLT */
	"",					/* 13 unused */
	"alignment fault",			/* 14 T_ALIGNFLT */
	"",					/* 15 unused */
	"",					/* 16 unused */
	"",					/* 17 unused */
	"integer divide fault",			/* 18 T_DIVIDE */
	"non-maskable interrupt trap",		/* 19 T_NMI */
	"overflow trap",			/* 20 T_OFLOW */
	"FPU bounds check fault",		/* 21 T_BOUND */
	"FPU device not available",		/* 22 T_DNA */
	"double fault",				/* 23 T_DOUBLEFLT */
	"FPU operand fetch fault",		/* 24 T_FPOPFLT */
	"invalid TSS fault",			/* 25 T_TSSFLT */
	"segment not present fault",		/* 26 T_SEGNPFLT */
	"stack fault",				/* 27 T_STKFLT */
};


/*****
 from kern/subr_log.c
 */

void
logwakeup()
{
	printf("logwakeup: Not supported\n");
}

/*****
 from i386/i386/vm_machdep.c
 */

#include <sys/buf.h>

/*
 * Map an IO request into kernel virtual address space.
 *
 * All requests are (re)mapped into kernel VA space.
 * Notice that we use b_bufsize for the size of the buffer
 * to be mapped.  b_bcount might be modified by the driver.
 */
void
vmapbuf(bp)
	register struct buf *bp;
{
}

/*
 * Free the io map PTEs associated with this IO operation.
 * We also invalidate the TLB entries and restore the original b_addr.
 */
void
vunmapbuf(bp)
	register struct buf *bp;
{
}

/*****
 from vm/vm_pager.c
 */

void
relpbuf(bp)
        struct buf *bp;
{
  brelse(bp);
}

struct buf *
getpbuf()
{
  return geteblk(0);
}


/*****
 from kern/vfs_bio.c
 */

#include <sys/malloc.h>

/* 
 * simulated buffer pool
 */

/*
 * Wait for buffer I/O completion, returning error status.
 */
int
biowait(register struct buf * bp)
{
	int s;

	s = splbio();
	while ((bp->b_flags & B_DONE) == 0)
		tsleep((caddr_t) bp, PRIBIO, "biowait", 0);
	splx(s);
	if (bp->b_flags & B_EINTR) {
		bp->b_flags &= ~B_EINTR;
		return (EINTR);
	}
	if (bp->b_flags & B_ERROR) {
		return (bp->b_error ? bp->b_error : EIO);
	} else {
		return (0);
	}
}

void biodone(bp)
    register struct buf *bp;
{
	int s;

	s = splbio();
	if (!(bp->b_flags & B_BUSY))
		panic("biodone: buffer not busy");

	if (bp->b_flags & B_DONE) {
		splx(s);
		printf("biodone: buffer already done\n");
		return;
	}
	bp->b_flags |= B_DONE;

#ifdef BOUNCE_BUFFERS
	if (bp->b_flags & B_BOUNCE)
		vm_bounce_free(bp);
#endif

	/* call optional completion function if requested */
	if (bp->b_flags & B_CALL) {
		bp->b_flags &= ~B_CALL;
		(*bp->b_iodone) (bp);
		splx(s);
		return;
	}

	/*
	 * No asynchronous operations allowed yet.
	 */
	if (bp->b_flags & B_ASYNC) {
	  panic("No asynchronous operations yet!");
	} else {
		bp->b_flags &= ~B_WANTED;
		wakeup((caddr_t) bp);
	}
	splx(s);
}

/*
 * Get an empty, disassociated buffer of given size.
 */
struct buf *
geteblk(int size)
{
    static int inited = 0;
    register struct buf *bp;
    caddr_t buffer;
   
    if (size > MAXBSIZE)
        panic("geteblk: size too big");

    if (!inited) {
	TAILQ_INIT(&bp_queue);
	inited = 1;
    }

    if(!bp_queue.tqh_first) {
	bp = (struct buf *) malloc(sizeof(struct buf),M_VMOBJ,0);
        if ( !bp ) panic("geteblk: can't allocate struct buf");
        buffer = (caddr_t)0;
    }
    else {
      bp = bp_queue.tqh_first;
      TAILQ_REMOVE(&bp_queue, bp, b_freelist);
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
      bp->b_un.b_addr = (caddr_t)malloc(MAXBSIZE,M_VMOBJ,0);
      if ( !bp->b_un.b_addr ) panic("geteblk: can't alloc buffer");
    }
    else bp->b_un.b_addr = buffer;
    bp->b_bcount = size;
    return(bp);
}

/*
 * Release a buffer.
 */
void
brelse(struct buf * bp)
{
  /* They all look the same to us, just put it on the free list */
  TAILQ_INSERT_TAIL(&bp_queue, bp, b_freelist);
}

/*****
 from vm/vm_glue.c
 */

int
kernacc(addr, len, rw)
	caddr_t addr;
	int len, rw;
{
  return 1;
}

int
useracc(addr, len, rw)
	caddr_t addr;
	int len, rw;
{
  return 1;
}

/*****
 from vm/vm_kern.c
 */

/*
 *	kmem_suballoc:
 *
 *	Allocates a map to manage a subrange
 *	of the kernel virtual address space.
 *
 *	Arguments are as follows:
 *
 *	parent		Map to take range from
 *	size		Size of range to find
 *	min, max	Returned endpoints of map
 *	pageable	Can the region be paged
 */
vm_map_t
kmem_suballoc(register vm_map_t parent,
        vm_offset_t *min,
        vm_offset_t *max,
        vm_size_t size,
        boolean_t pageable)
{
	vm_offset_t kernel_min;
        vm_map_t result;
	vm_map_entry_t new_entry;
	int s;

	s = splhigh();
	
        size = round_page(size);
	kernel_min = pmem_alloc(size);

	if (vm_map_list_index > 15) {
		printf("kmem_suballoc(upcalls.c): Nobody loves me, everybody hates me, now I gotta eat a worm\n");
		standalone_halt();
	}
	result=&(vm_map_list[vm_map_list_index++]);
	
        *min = kernel_min;
        *max = *min + size;
	
#ifdef WATCHMEM
	printf("sub map %lx, min %lx, max %lx, size %lx\n",
	       result,*min,*max,*max-*min);
#endif
	result->min_offset = *min;
	result->max_offset = *max;

	new_entry = standalone_get_map_entry(*min,*max,
				       vm_map_to_entry(result),
				       vm_map_to_entry(result));


        vm_map_first_entry(result) = new_entry; 
	vm_map_last_entry(result) =  new_entry;

	splx(s);
        return (vm_map_t)result;
}

/*
 * Allocate wired-down memory in the kernel's address map for the higher
 * level kernel memory allocator (kern/kern_malloc.c).  We cannot use
 * kmem_alloc() because we may need to allocate memory at interrupt
 * level where we cannot block (canwait == FALSE).
 *
 * This routine has its own private kernel submap (kmem_map) and object
 * (kmem_object).  This, combined with the fact that only malloc uses
 * this routine, ensures that we will never block in map or object waits.
 *
 * Note that this still only works in a uni-processor environment and
 * when called at splhigh().
 *
 * We don't worry about expanding the map (adding entries) since entries
 * for wired maps are statically allocated.
 */
vm_offset_t
kmem_malloc(map, size, waitflag)
	register vm_map_t map;
	register vm_size_t size;
	boolean_t waitflag;
{
  return kmem_alloc(map, size);
}

/*
 *	kmem_alloc_pageable:
 *
 *	Allocate pageable memory to the kernel's address map.
 *	map must be "kernel_map" below.
 */

vm_offset_t
kmem_alloc_pageable(map, size)
	vm_map_t map;
	register vm_size_t size;
{
	vm_offset_t	addrp;
	if (kernel_memory_allocate(map, &addrp, size, 0, 0) != KERN_SUCCESS)
		return 0;
	else
		return addrp;
}

/*
 *	Allocate wired-down memory in the kernel's address map
 *	or a submap.
 */
vm_offset_t
kmem_alloc(map, size)
	register vm_map_t map;
	register vm_size_t size;
{
	vm_offset_t	addrp;
	vm_map_entry_t	next_entry;
	vm_offset_t	start;
	vm_offset_t	end;

	vm_size_t       orig_size = size;

	int s;
	s = splhigh();
	if (standalone_map_debug)  {
		printf("in kmem_alloc size=%d (0x%lx 0x%lx 0x%lx)\n",
			size,
			&vm_map_entries[0],
			&vm_map_entries[STANDALONE_NUM_MAP_ENTRIES-1],
			map);
		printf("list entries before:\n");

		for (next_entry = vm_map_first_entry(map);
		     next_entry != vm_map_to_entry(map);
	     	     next_entry = next_entry->next)  {
			printf("start = %lx, end = %lx\n",
			      next_entry->start,
			      next_entry->end);
		}
	}
	size = round_page(size);

	for (next_entry = vm_map_first_entry(map);
	     next_entry != vm_map_to_entry(map);
     	     next_entry = next_entry->next)  {
		if ((next_entry->end - next_entry->start) >= size)
		{
		   start = next_entry->start;
		   next_entry->start+=size;
		   if (next_entry->start==next_entry->end)
		   {
			   vm_map_entry_t prev = next_entry->prev;
                           prev->next = next_entry->next;
                           next_entry->next->prev = prev;
			   standalone_free_map_entry(next_entry);
                           next_entry = prev;
                   }
		   else
			   kmem_coalesce(next_entry);
		
                   if (standalone_map_debug)  {
       			   printf("list entries after:\n");
			   next_entry = vm_map_first_entry(map);
			   while (next_entry != vm_map_to_entry(map)) {
				   printf("start = %lx, end = %lx\n",
					  next_entry->start,
					  next_entry->end);
				   next_entry = next_entry->next;
			   }
			   printf("returning memory chunk  %lx\n",start);
		   }
		   splx(s);
		   return(start);
	        }
	}

	printf("kmem_alloc(%lx,%d,%d) is looking for an axe and found nothing\n",
	       map,size,orig_size);

	for (next_entry = vm_map_first_entry(map);
	     next_entry != vm_map_to_entry(map);
	     next_entry = next_entry->next)  {
		printf("start = %lx, end = %lx\n",
		      next_entry->start,
		      next_entry->end);
		}

	splx(s);
	panic("kmem_alloc");
	/* NOT REACHED */

	printf("kmem_alloc(%lx,%d,%d) no memory\n",map,size,orig_size);
	splx(s);
	return (0);
}

/*
 *	kmem_free:
 *
 *	Release a region of kernel virtual memory allocated
 *	with kmem_alloc, kmem_alloc_wired, or kmem_alloc_pageable,
 *	and return the physical pages associated with that region.
 */

void
kmem_free(
	vm_map_t	map,
	vm_offset_t	addr,
	vm_size_t	size)
{
	int kr;
	vm_offset_t start;
	vm_offset_t end;
	vm_map_entry_t new_entry;
	vm_map_entry_t next_entry;
	vm_map_entry_t prev_entry;
	int s;

	s = splhigh();

	start = addr;
	end = addr+size;

	if  (standalone_map_debug)  {
		printf("in kmem_free (0x%lx 0x%lx 0x%lx)\n",
			&vm_map_entries[0],
			&vm_map_entries[STANDALONE_NUM_MAP_ENTRIES-1],
			map);
		printf("list entries before:\n");


		for (next_entry = vm_map_first_entry(map);
		     next_entry != vm_map_to_entry(map);
	     	     next_entry = next_entry->next)  {        
			printf("start = %lx, end = %lx\n",
			      next_entry->start,
			       next_entry->end);
		}
	}


	for (next_entry = vm_map_first_entry(map);
	     next_entry != vm_map_to_entry(map);
     	     next_entry = next_entry->next)  {
		if (start < next_entry->start)
			break;
	}
	new_entry = standalone_get_map_entry(start, end, 
			next_entry->prev, next_entry);


	/* we should be right before this entry */
	next_entry->prev->next = new_entry;
	next_entry->prev = new_entry;

	/* Now coalesce entries next to each other */
	kmem_coalesce(new_entry);

	if (standalone_map_debug)  {
		printf("list entries after:\n");
		for (next_entry = vm_map_first_entry(map);
		     next_entry != vm_map_to_entry(map);
	     	     next_entry = next_entry->next)  {
			printf("start = %lx, end = %lx\n",
			      next_entry->start,
			      next_entry->end);
		}
	}

	splx(s);
	kr = KERN_SUCCESS;
	if (kr != KERN_SUCCESS)
		panic("kmem_free");
}

/*
 *      kmem_free_wakeup
 *
 *      Returns memory to a submap of the kernel, and wakes up any threads
 *      waiting for memory in that map.
 */
void
kmem_free_wakeup(map, addr, size)
        vm_map_t map;
        vm_offset_t addr;
        vm_size_t size;
{
        (void) kmem_free(map, trunc_page(addr), round_page(addr + size));
        thread_wakeup((int) map);
}


/*****
 from kern/sys_generic.c
 */

/*ARGSUSED*/
int
seltrue(dev, flag, p)
	dev_t dev;
	int flag;
	struct proc *p;
{
        return (1);
}

/*****
 from kern/kern_prot.c
 */

/*
 * Test whether the specified credentials imply "super-user"
 * privilege; if so, and we have accounting info, set the flag
 * indicating use of super-powers.
 * Returns 0 or error.
 */
int
suser(cred, acflag)
	struct ucred *cred;
	u_short *acflag;
{
  return 0;
}

/*****
 from kern/kern_proc.c
 */

/*
 * Locate a process by number
 */
struct proc *
pfind(pid)
	register pid_t pid;
{
  printf("pfind %d: not supported\n", pid);
  return NULL;
}

/*****
 from kern/kern_sig.c
 */


/*
 * Send a signal caused by a trap to the current process.
 * If it will be caught immediately, deliver it with correct code.
 * Otherwise, post it normally.
 */
void
trapsignal(p, signum, code)
	struct proc *p;
	register int signum;
	u_int code;
{
}

/*
 * Send the signal to the process.  If the signal has an action, the action
 * is usually performed by the target process rather than the caller; we add
 * the signal to the set of pending signals for the process.
 *
 * Exceptions:
 *   o When a stop signal is sent to a sleeping process that takes the
 *     default action, the process is stopped without awakening it.
 *   o SIGCONT restarts stopped processes (or puts them back to sleep)
 *     regardless of the signal action (eg, blocked or ignored).
 *
 * Other ignored signals are discarded immediately.
 */
void
psignal(p, signum)
	register struct proc *p;
	register int signum;
{
}

/*****
 from kern/kern_xxx.c
 */

void
shutdown_nice(void)
{
  boot(0);
}


/*****
 from kern/init_main.c
 */

void bootp_init();
void ttd_init();

void
main(framep)
	void *framep;
{
	register struct proc *p;
	register struct filedesc0 *fdp;
	register int i;
	int s, rval[2];

	/*
	 * Initialize the current process pointer (curproc) before
	 * any possible traps/probes to simplify trap processing.
	 */
	p = &proc0;
	curproc = p;
	printf(copyright);

	vm_mem_init();
	kmeminit();
	cpu_startup();

	/* initialize clock interrupts */
	cpu_initclocks();

       	/* Initialize mbuf's. */
	mbinit();

	/* Initialize clists. */
	clist_init();

#ifdef GPROF
	/* Initialize kernel profiling. */
	kmstartup();
#endif

	bootp_init();
	ttd_init();
	
	mono_time = time;

        printf("--- calling SpinProgram.c:salhook_main_program() ---\n");
        salhook_main_program();
}
