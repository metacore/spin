#include <sal/salhook.h>
#include <sal/salnet.h>

#include <sys/param.h>
#include <sys/mbuf.h>
#include <net/if.h>

#include <sal/ttd/ttd_types.h>

int salhook_mallocpages=2000;

void
salhook_main_program()
{
	printf("salhook_main_program: looping\n");
	while(1) ;
}

#ifdef __FreeBSD__
/* using function pointer so that higher level OS can override this
   functionality directly. */

static void 
salhook_clock_intr_stub(machine_state frame){/*do nothing*/};
void (*salhook_clock_intr)(machine_state frame) = salhook_clock_intr_stub;
#endif /* __FreeBSD__ */

/* DebuggerInterface
  
   Defines the interface that SAL expects the operating system
   define for TTD support.
 
   NOTE: Some of the interface arguments purposely use "void*" types,
   as they are both architecture and operating specific.  For example,
   "machine_state" is specific to the architecture and "m3array"
   currently is specific to the OS.

 */

unsigned int
salhook_current_thread()
{
	return 1;
}

void
salhook_thread_name(unsigned int thread,char* bp,unsigned long len)
{
	strncpy(bp, "BOOT", len);
}

int
salhook_is_user_thread(unsigned int thread)
{
	return 0; /* FALSE */
}

void
salhook_get_state(unsigned int thread, int get_boundary_regs, machine_state machine_state)
{
	extern void ttd_recover_esp(void *ttd_state);
	if (get_boundary_regs == 0) {
		ttd_recover_esp(machine_state);
	}
}

void
salhook_set_state(unsigned int thread,machine_state machine_state)
{
	extern void ttd_overwrite_esp(void *ttd_state);
	ttd_overwrite_esp(machine_state);
}

int
salhook_next_domain(long domain_cnt, void *buf, int bytes, void* textaddr)
{
	return 0; /* FALSE */
}


int
salhook_next_thread(unsigned int * thread)
{
	/* simple case where we have only one thread */
	if (*thread == TTD_NO_THREAD)
		*thread = 0;
	else if (*thread == 0)
		*thread = TTD_NO_THREAD;
	return 0; /* FALSE */
}

int
salhook_valid_thread(unsigned int t)
{
	return t == 1;
}


#include <sys/systm.h>

void 
timeout(void (*fun)(), void *arg, int t)
{
	printf("timeout called.\n");
}

void
untimeout(void (*fun)(), void *arg)
{
	printf("untimeout called.\n");
}


void
wakeup(void *chan)
{
}

#ifdef __FreeBSD__
/* TEMPORARILY NEEDED FOR TRAP */
#include <machine/spl.h>
#include <sys/systm.h>
#include <sys/proc.h>
#include <sys/user.h>
#include <sys/acct.h>
#include <sys/kernel.h>
#include <sys/syscall.h>
#include <sys/sysent.h>
#ifdef KTRACE
#include <sys/ktrace.h>
#endif

#include <vm/vm_param.h>
#include <vm/pmap.h>
#include <vm/vm_kern.h>
#include <vm/vm_map.h>
#include <vm/vm_page.h>

#include <machine/cpu.h>
#include <machine/md_var.h>
#include <machine/psl.h>
#include <machine/reg.h>
#include <machine/trap.h>
#include <machine/../isa/isa_device.h>

#ifdef POWERFAIL_NMI
# include <syslog.h>
# include <machine/clock.h>
#endif


void
thread_wakeup(int chan)
{
}

int
tsleep(void *chan, int pri, char *wmesg, int timo)
{
  return 0;
}

/* TEMPORARY UNTIL WE WRITE A LOCORE TRAP HANDLING FOR SPIN */

/*
 * Exception, fault, and trap interface to the FreeBSD kernel.
 * This common code is called from assembly language IDT gate entry
 * routines that prepare a suitable stack frame, and restore this
 * frame after the exception has been processed.
 */

/* following two in freebsd_subr.c */
#define MAX_TRAP_MSG		27
extern char *trap_msg[];
extern inthand_t IDTVEC(syscall);

void
trap(frame)
	struct trapframe frame;
{
    int type, code;

    type = frame.tf_trapno;
    code = frame.tf_err;

    if (ISPL(frame.tf_cs) == SEL_UPL) {
	printf ("user trap:  ");
	if (type <= MAX_TRAP_MSG)
	    printf("%s\n",trap_msg[type]);
	else
	    printf("unknown/reserved trap\n");
	printf("going to ttd\n");
	if (ttd_trap (type, 0, &frame))
		return;
    } else {
	/* kernel trap */
	
	switch (type) {
	  case T_PAGEFLT:			/* page fault */
	    printf("kernel trap: %s 0x%x\n",trap_msg[type],rcr2());
	    printf("going to ttd\n");
	    if (ttd_trap (type, 0, &frame))
		return;
	    return;

	  case T_PROTFLT:		/* general protection fault */
	  case T_SEGNPFLT:	/* segment not present fault */
#ifndef SPIN
	    /*
	     * Invalid segment selectors and out of bounds
	     * %eip's and %esp's can be set up in user mode.
	     * This causes a fault in kernel mode when the
	     * kernel tries to return to user mode.  We want
	     * to get this fault so that we can fix the
	     * problem here and not have to check all the
	     * selectors and pointers when the user changes
	     * them.
	     */
#define	MAYBE_DORETI_FAULT(where, whereto)				\
	do {								\
		if (frame.tf_eip == (int)where) {			\
			frame.tf_eip = (int)whereto;			\
			return;						\
		}							\
	} while (0)

	    if (intr_nesting_level == 0) {
		MAYBE_DORETI_FAULT(doreti_iret,
				   doreti_iret_fault);
		MAYBE_DORETI_FAULT(doreti_popl_ds,
				   doreti_popl_ds_fault);
		MAYBE_DORETI_FAULT(doreti_popl_es,
				   doreti_popl_es_fault);
	    }
	    if (curpcb && curpcb->pcb_onfault) {
		frame.tf_eip = (int)curpcb->pcb_onfault;
		return;
	    }
#endif
	    break;

	  case T_TSSFLT:
	    /*
	     * PSL_NT can be set in user mode and isn't cleared
	     * automatically when the kernel is entered.  This
	     * causes a TSS fault when the kernel attempts to
	     * `iret' because the TSS link is uninitialized.  We
	     * want to get this fault so that we can fix the
	     * problem here and not every time the kernel is
	     * entered.
	     */
	    if (frame.tf_eflags & PSL_NT) {
		frame.tf_eflags &= ~PSL_NT;
		return;
	    }
	    break;

	  case T_TRCTRAP:	 /* trace trap */
	    if (frame.tf_eip == (int)IDTVEC(syscall)) {
	       /*
		* We've just entered system mode via the
		* syscall lcall.  Continue single stepping
		* silently until the syscall handler has
		* saved the flags.
		*/
		return;
	    }
	    if (frame.tf_eip == (int)IDTVEC(syscall) + 1) {
	        /*
		 * The syscall handler has now saved the
		 * flags.  Stop single stepping it.
		 */
		frame.tf_eflags &= ~PSL_T;
		return;
	    }
	    /*
	     * Fall through.
	     */
	  case T_BPTFLT:
	    /*
	     * If DDB is enabled, let it handle the debugger trap.
	     * Otherwise, debugger traps "can't happen".
	     */
#ifdef TTD
	    if (ttd_trap (type, 0, &frame))
		return;
#endif
#ifdef DDB
	    if (kdb_trap (type, 0, &frame))
		return;
#endif	
	    break;
	    
#if NISA > 0
	  case T_NMI:
#ifdef POWERFAIL_NMI
#ifndef TIMER_FREQ
#  define TIMER_FREQ 1193182
#endif
	  handle_powerfail:
	    {
		static unsigned lastalert = 0;
		
		if(time.tv_sec - lastalert > 10)
		    {
			log(LOG_WARNING, "NMI: power fail\n");
			sysbeep(TIMER_FREQ/880, hz);
			lastalert = time.tv_sec;
		    }
		return;
	    }
#else /* !POWERFAIL_NMI */
#ifdef DDB	
	    /* NMI can be hooked up to a pushbutton for debugging */
	    printf ("NMI ... going to debugger\n");
	    if (ttd_trap (type, 0, &frame))
		return;
#endif /* DDB */
#ifdef DDB
	    /* NMI can be hooked up to a pushbutton for debugging */
	    printf ("NMI ... going to debugger\n");
	    if (kdb_trap (type, 0, &frame))
		return;
#endif /* DDB */
	    /* machine/parity/power fail/"kitchen sink" faults */
	    if (isa_nmi(code) == 0) return;
	    /* FALL THROUGH */
#endif /* POWERFAIL_NMI */
#endif /* NISA > 0 */
	}

#ifdef TTD
	if(ttd_trap(type, 0, &frame))
	    return;
#endif
#ifdef KDB
	if (kdb_trap(&psl))
	    return;
#endif
#ifdef DDB
	if (kdb_trap (type, 0, &frame))
	    return;
#endif
	if (type <= MAX_TRAP_MSG)
	    panic(trap_msg[type]);
	else
	    panic("unknown/reserved trap");
    }
    
  out:
    return;
}

/*
 * System call request from POSIX system call gate interface to kernel.
 * Like trap(), argument is call by reference.
 */
void
syscall(frame)
	struct trapframe frame;
{
	    printf("syscall trap\n");
}

#endif




#ifdef OSF
void
pgsignal(pg, sig, checkctty)
     long pg, sig, checkctty;
{
    printf("pgsignal called\n");
}
#endif 

#ifdef __FreeBSD__
void
pgsignal (struct pgrp *pgrp, int sig, int checkctty)
{
    printf("pgsignal called\n");
}
#endif /* __FreeBSD__ */


#ifdef OSF

int
copyin(caddr_t user_src, caddr_t kernel_dest, int count)
{
    printf("copyin called\n");
    return 0;
}

int
copyout(caddr_t kernel_src, caddr_t user_dest, int count)
{
    printf("copyout called\n");
    return 0;
}

void
vm_pg_alloc(long *pp)
{
	panic("vm_pg_alloc called.\n");
}
	
void
vm_pg_free(long p)
{
	printf("vm_pg_free called.\n");
}

thread_t
kernel_thread(task_t task, long (*func)(), char *arg)
{
	printf("kernel_thread called.\n");
}

thread_t
current_thread()
{
		return 0;
}

void
thread_wakeup_prim(vm_offset_t event, boolean_t one_thread, int result)
{
	printf("thread_wakeup called.\n");
}

void
assert_wait_mesg(vm_offset_t event, boolean_t interruptible, 
		      const char *mesg)
{
	printf("assert_wait_mesg called.\n");
}

int
mpsleep(void *chan, long pri, char *wmesg, long timo,
        void *lockp, long flags)
{
	printf("mpsleep called.\n");
    return 0;
}

void
thread_block()
{
	printf("thread_block called.\n");
}

void
clear_wait(thread_t thread, int result, boolean_t interrupt_only)
{
	printf("clear_wait called.\n");
}

void
thread_set_timeout(int t)    /* timeout interval in ticks */
{
	printf("thread_set_timeout called.\n");
}
#endif
