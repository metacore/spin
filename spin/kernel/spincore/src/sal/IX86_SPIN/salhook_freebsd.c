#include <sal/salhook.h>
#include <sal/salnet.h>
#include <spincore/src/sal/OpenArray.h>
#include <sys/types.h>
#include <sys/select.h>


#define TTD_NO_THREAD -1


#include <spincore/IX86_SPIN/Debugger.h>
#include <spincore/IX86_SPIN/Clock.h>
#include <spincore/IX86_SPIN/SalSync.h>
#include <spincore/IX86_SPIN/Sal.h>
#include <spincore/IX86_SPIN/Select.h>


#include <sys/param.h>
#include <machine/spl.h>
typedef void *thread_t;

/* NEEDED FOR TRAP */

#include <spincore/IX86_SPIN/MachineTrapPrivate.h>
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

/* XXX should get these from sal compile area */

#define NISA 1 /* #include "isa.h" */
#define NNPX 1 /* #include "npx.h" */
#define TTD  1 /* #include "ttd.h" */
#define DDB  1 /* #include "ddb.h" */



/*
 * From OSF sys/param.h.
 */
#define MP_CAN_DO               0x9             /* Lock simple, no re-lock */
/*
 * We assume that we are in a single processor environment
 * where all of the simple_lock routines are null macros.
 * This is still a cheap hack, the timo part should be fixed.
 */
int
tsleep(void *chan, int pri, char *wmesg, int timo)
{
    int catch = 0;  /* pri & PCATCH; for real */
    int s; 

    s = splhigh();
    if (chan)
	assert_wait_mesg((vm_offset_t)chan, (catch ? TRUE : FALSE), wmesg);

    if (timo) 
        thread_set_timeout(timo);

    if (*SalSync__Sleep)
	SalSync__Sleep();
    else
	printf("mpsleep called too early.\n");
    splx(s);

    /*
     * mpsleep is supposed to return 0 if awakened
     * and EWOULDBLOCK if the timeout expires.
     * As near as I can see there's is no clock to
     * read so for now we always return 0.
     */
    return 0;
}

/* TEMPORARY UNTIL WE WRITE A LOCORE TRAP HANDLING FOR SPIN */

/*
 * 386 Trap and System call handling
 */

extern void trap __P((struct trapframe frame));
extern int trapwrite __P((unsigned addr));
extern void syscall __P((struct trapframe frame));

extern inthand_t IDTVEC(syscall);

/* following two in freebsd_subr.c */
#define MAX_TRAP_MSG		27
extern char *trap_msg[];

/*
 * Exception, fault, and trap interface to the FreeBSD kernel.
 * This common code is called from assembly language IDT gate entry
 * routines that prepare a suitable stack frame, and restore this
 * frame after the exception has been processed.
 */


void
trap(frame)
	struct trapframe frame;
{
    int type, code;

    type = frame.tf_trapno;
    code = frame.tf_err;

#ifdef DIAGNOSTIC
    u_long eva;
#endif
    if (ISPL(frame.tf_cs) == SEL_UPL) {
	/* user trap */
	switch (type) {
	  case T_PRIVINFLT:	/* privileged instruction fault */
	    if(MachineTrapPrivate__PrivilegedInstructionFault)
		(*MachineTrapPrivate__PrivilegedInstructionFault)(&frame);
	    break;
	    
	  case T_BPTFLT:		/* bpt instruction fault */
	    if(MachineTrapPrivate__Breakpoint)
		(*MachineTrapPrivate__Breakpoint)(&frame);
	    break;

	  case T_TRCTRAP:		/* trace trap */
	    if(MachineTrapPrivate__TraceTrap)
		(*MachineTrapPrivate__TraceTrap)(&frame);
	    break;

	  case T_ARITHTRAP:	/* arithmetic trap */
	    if(MachineTrapPrivate__ArithmeticTrap)
		(*MachineTrapPrivate__ArithmeticTrap)(&frame);
	    break;

	  case T_ASTFLT:		/* Allow process switch */
	    astoff();
	    cnt.v_soft++;
	    goto out;

	  case T_PROTFLT:		/* general protection fault */
	  case T_SEGNPFLT:	/* segment not present fault */
	  case T_STKFLT:		/* stack fault */
	  case T_TSSFLT:		/* invalid TSS fault */
	  case T_DOUBLEFLT:	/* double fault */
	  default:
	    if(MachineTrapPrivate__ProtectionFault)
		(*MachineTrapPrivate__ProtectionFault)(&frame);
	    break;

	  case T_PAGEFLT:		/* page fault */
	    if(MachineTrapPrivate__PageFault)
		(*MachineTrapPrivate__PageFault)(&frame); 
	    break;

	  case T_DIVIDE:		/* integer divide fault */
	    if(MachineTrapPrivate__DivideFault)
		(*MachineTrapPrivate__DivideFault)(&frame);
	    break;

#if NISA > 0
	  case T_NMI:
#ifdef POWERFAIL_NMI
	    goto handle_powerfail;
#else /* !POWERFAIL_NMI */
#ifdef TTD
	    printf ("NMI ... going to debugger\n");
	    if (ttd_trap (type, 0, &frame))
		return;		
#endif
#ifdef DDB
	    /* NMI can be hooked up to a pushbutton for debugging */
	    printf ("NMI ... going to debugger\n");
	    if (kdb_trap (type, 0, &frame))
		return;
#endif /* DDB */
	    /* machine/parity/power fail/"kitchen sink" faults */
	    if (isa_nmi(code) == 0) return;
	    panic("NMI indicates hardware failure");
#endif /* POWERFAIL_NMI */
#endif /* NISA > 0 */
	    
	  case T_OFLOW:		/* integer overflow fault */
	    if(MachineTrapPrivate__OverflowFault)
		(*MachineTrapPrivate__OverflowFault)(&frame);
	    break;

	  case T_BOUND:		/* bounds check fault */
	    if(MachineTrapPrivate__BoundsCheckFault)
		(*MachineTrapPrivate__BoundsCheckFault)(&frame);
	    break;

	  case T_DNA:
#if NNPX > 0
	    /* if a transparent fault (due to context switch "late") */
	    if (npxdna())
		return;
#endif	/* NNPX > 0 */

#if defined(MATH_EMULATE) || defined(GPL_MATH_EMULATE)
	    i = math_emulate(&frame);
	    if (i == 0) {
		if (!(frame.tf_eflags & PSL_T))
		    return;
		frame.tf_eflags &= ~PSL_T;
		if(MachineTrapPrivate__FPFault)
		    (*MachineTrapPrivate__FPFault)(&frame);
		break;
	    }
			/* else ucode = emulator_only_knows() XXX */
#else	/* MATH_EMULATE || GPL_MATH_EMULATE */
	    if(MachineTrapPrivate__FPFault)
		(*MachineTrapPrivate__FPFault)(&frame);
#endif	/* MATH_EMULATE || GPL_MATH_EMULATE */
	    break;

	  case T_FPOPFLT:		/* FPU operand fetch fault */
	    if(MachineTrapPrivate__FPOperandFault)
		(*MachineTrapPrivate__FPOperandFault)(&frame);
	    break;
		}
    } else {
	/* kernel trap */
	
	switch (type) {
	  case T_PAGEFLT:			/* page fault */
	    if(MachineTrapPrivate__PageFault)
		(*MachineTrapPrivate__PageFault)(&frame); 
	    return;

	  case T_PROTFLT:		/* general protection fault */
	  case T_SEGNPFLT:	/* segment not present fault */
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
 * Compensate for 386 brain damage (missing URKR).
 * This is a little simpler than the pagefault handler in trap() because
 * it the page tables have already been faulted in and high addresses
 * are thrown out early for other reasons.
 */
int trapwrite(addr)
	unsigned addr;
{
	struct proc *p;
	vm_offset_t va, v;
	struct vmspace *vm;
	int rv;

	va = trunc_page((vm_offset_t)addr);
	/*
	 * XXX - MAX is END.  Changed > to >= for temp. fix.
	 */
	if (va >= VM_MAXUSER_ADDRESS)
		return (1);

	p = curproc;
	vm = p->p_vmspace;

	++p->p_lock;

	if ((caddr_t)va >= vm->vm_maxsaddr
	    && (caddr_t)va < (caddr_t)USRSTACK) {
		if (!grow(p, va)) {
			--p->p_lock;
			return (1);
		}
	}

	v = trunc_page(vtopte(va));

	--p->p_lock;

	if (rv != KERN_SUCCESS)
		return 1;

	return (0);
}

/*
 * System call request from POSIX system call gate interface to kernel.
 * Like trap(), argument is call by reference.
 */
void
syscall(frame)
	struct trapframe frame;
{
    if (*MachineTrapPrivate__SyscallWrap)
	(*MachineTrapPrivate__SyscallWrap)(&frame);
}

void 
selrecord (struct proc *selector, struct selinfo *sip) 
{
    if (*Select__Record) Select__Record(selector, sip);
}

void 
selwakeup (struct selinfo *sip) 
{
    if (*Select__Wakeup) Select__Wakeup(sip);
}

thread_t
current_thread()
{
    if (*SalSync__CurrentThread)
      return (thread_t) SalSync__CurrentThread();
    
    return 0;
}

extern int cold;
void
thread_wakeup(int chan)
{
  if (*SalSync__ThreadWakeup) {
        SalSync__ThreadWakeup(chan);
        return;
  }
    if (!cold) printf("thread_wakeup called too early.\n");
}

void
wakeup(void *chan)
{
    thread_wakeup((unsigned long)chan);
}

void
pgsignal (struct pgrp *p, int sig, int checkctty)
{
    printf("pgsignal: ignored signal %d\n",sig);
}

