/* 
 * HISTORY
 * 28-Aug-97  Marc Fiuczynski (mef) at the University of Washington
 *	Added trap() and syscall(), and cleaned up the definition of
 *	other procedures.  Both trap() and syscall() should disappear
 *	when we write a Core.S for SPIN that vectors directly into
 *	Modula-3 code as it does for the ALPHA_SPIN platform.
 *
 * 16-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Created from FreeBSD trap.c
 *
 */

#include <sal/salhook.h>
#include <sal/salnet.h>
#include <spincore/src/sal/OpenArray.h>


#define TTD_NO_THREAD -1


#ifdef OSF
/* There is a bootstrap requirement here.  Whatever is included from OSF
must be created in sal before spincore is built.  In sal/alpha_osf/Makefile,
'make configure' and the spincorefiles var covers any files that need to
be created before building spincore.
 */
#include <mach/alpha/vm_types.h>
#include <mach/alpha/boolean.h>
#include <kern/thread.h>

#undef thread_wakeup_one
#undef thread_wakeup
#undef sleep

#include <spincore/ALPHA_SPIN/Debugger.h>
#include <spincore/ALPHA_SPIN/Clock.h>
#include <spincore/ALPHA_SPIN/SalSync.h>
#include <spincore/ALPHA_SPIN/Sal.h>
#include <spincore/ALPHA_SPIN/MemoryForSAL.h>
#endif


#ifdef __FreeBSD__
#include <spincore/IX86_SPIN/Debugger.h>
#include <spincore/IX86_SPIN/Clock.h>
#include <spincore/IX86_SPIN/SalSync.h>
#include <spincore/IX86_SPIN/Sal.h>
#include <spincore/IX86_SPIN/Select.h>


#include <sys/param.h>
#include <machine/spl.h>
typedef void *thread_t;

/* NEEDED FOR TRAP */

#include <spincore/IX86_SPIN/TrapPrivate.h>
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

#endif /* __FreeBSD__ */



#ifdef __notused__
int salhook_mallocpages=2000;
void
salhook_main_program()
{
	salhook_main_program is defined in start/src/BASETARGET/SpinProgram.c
}
#endif /* __notused__ */

/* salhook_clock_intr is a function pointer so that higher level OS
   can override this functionality directly, rather than testing at
   runtime whether the vtable exported entry is NIL or not . */
static void
salhook_clock_intr_stub(void*frame){/*do nothing*/};
void (*salhook_clock_intr)(void*frame) = salhook_clock_intr_stub;


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
        if (*Debugger__CurrentTID) return (*Debugger__CurrentTID)();

	return 1;
}

void
salhook_thread_name(unsigned int thread,char* bp,unsigned long len)
{
	if (*Debugger__GetCName) {
		(*Debugger__GetCName)(thread,bp,len);
		return;
		}

	strncpy(bp, "BOOT", len);
}

int
salhook_is_user_thread(unsigned int thread)
{
	if (*Debugger__InUserMode) return (*Debugger__InUserMode)();

	return 0; /* FALSE */
}

void
salhook_get_state(unsigned int thread, int get_boundary_regs, machine_state machine_state)
{
	if (*Debugger__GetRegs) {
		(*Debugger__GetRegs)(thread,get_boundary_regs,machine_state);
		return;
		}

	if (get_boundary_regs == 0) {
		extern void ttd_recover_esp(void *ttd_state);
		ttd_recover_esp(machine_state);
	}
}

void
salhook_set_state(unsigned int thread,machine_state machine_state)
{
	extern void ttd_overwrite_esp(void *ttd_state);
	if (*Debugger__SetRegs) {
		(*Debugger__SetRegs)(thread,machine_state);
		return;
		}
	ttd_overwrite_esp(machine_state);
}

int
salhook_next_domain(long domain_cnt, void *buf, int bytes, void* textaddr)
{
	if (*Debugger__GetDomain) {
		struct openarray m3chars;
		m3chars.start = buf;
		m3chars.size  = bytes;

		return (*Debugger__GetDomain)(domain_cnt,&m3chars, textaddr);
		}

	return 0; /* FALSE */
}


int
salhook_next_thread(unsigned int * thread)
{
	if (*Debugger__NextTID) return (*Debugger__NextTID)(thread);

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
	if (*Debugger__ValidTID) return (*Debugger__ValidTID)(t);
	return t == 1;
}

void 
timeout(void (*fun)(), void *arg, int t)
{
    if (*Clock__SetCAlarm) {
    	Clock__SetCAlarm(t, fun, arg, 0, 0);
	return;
	}

    printf("timeout called too early.\n");
    return ; /* XXX should return slot number of processor */
}

thread_t
current_thread()
{
	if (*SalSync__CurrentThread)
		return (thread_t) SalSync__CurrentThread();

	return 0;
}

void
thread_set_timeout(int t)    /* timeout interval in ticks */
{
    if (*SalSync__SetTimeout) {
    	SalSync__SetTimeout(current_thread(),t);
	return;
	}

    printf("thread_set_timeout called too early.\n");
}

extern int cold;

void
untimeout(void (*fun)(), void *arg)
{
    if (*Clock__CancelCAlarm) {
    	Clock__CancelCAlarm(fun, arg);
	return;
	}
	
    if (!cold) printf("untimeout called too early.\n");
    return ;
}

#ifdef OSF
void
thread_wakeup(unsigned long chan)
#endif 
#ifdef __FreeBSD__
void
thread_wakeup(int chan)
#endif 

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
assert_wait_mesg(vm_offset_t event, boolean_t interruptible, 
		 const char *mesg)
{
    if (*SalSync__AssertWait) {
    	SalSync__AssertWait(event, interruptible);
	return;
	}

    printf("assert_wait_mesg called too early.\n");
}

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
mpsleep(void *chan, long pri, char *wmesg, long timo,
        void *lockp, long flags)
{
    int catch = 0;  /* pri & PCATCH; for real */
    int s; 

    s = splhigh();
    if (chan)
	assert_wait_mesg((vm_offset_t)chan, (catch ? TRUE : FALSE), wmesg);

    if ( lockp ) {
        if ( (flags&MP_CAN_DO) != MP_CAN_DO ) {
            panic("mpsleep with a lock");
        }
    }

    if ( timo ) {
        thread_set_timeout(timo);
    }

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


void 
pgsignal (struct pgrp *pgrp, int sig, int checkctty)
{
#ifdef OSF
	/* called from tty.c:tty_input

	   SPIN overloads tp->t_pgrp with a TIOCSPGRP ioctl in
	   dev/console/src/console.c to be a function pointer to a
	   signal handler
	 */
        void (*pg)(int) = (void(*)(int))pgrp;
	
	if (pg) (*pg)(sig);
	else
#endif
	  printf("pgsignal: ignored signal %d\n",sig);
}

#ifdef __FreeBSD__
int
tsleep(void *chan, int pri, char *wmesg, int timo)
{
    mpsleep(chan, pri, wmesg, timo, 0, 0);
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

int printtrap=0;

void
trap(frame)
	struct trapframe frame;
{
    int type, code;

    type = frame.tf_trapno;
    code = frame.tf_err;

	if (printtrap)
		printf("\n%s trap 0x%x\n",(ISPL(frame.tf_cs) == SEL_UPL)?"user":"kernel", type);
#ifdef DIAGNOSTIC
    u_long eva;
#endif
    if (ISPL(frame.tf_cs) == SEL_UPL) {
	/* user trap */
	switch (type) {
	  case T_PRIVINFLT:	/* privileged instruction fault */
	    if(TrapPrivate__PrivilegedInstructionFaultWrap)
		(*TrapPrivate__PrivilegedInstructionFaultWrap)(&frame);
	    break;
	    
	  case T_BPTFLT:		/* bpt instruction fault */
	    if(TrapPrivate__BreakpointWrap)
		(*TrapPrivate__BreakpointWrap)(&frame);
	    break;

	  case T_TRCTRAP:		/* trace trap */
	    if(TrapPrivate__TraceTrapWrap)
		(*TrapPrivate__TraceTrapWrap)(&frame);
	    break;

	  case T_ARITHTRAP:	/* arithmetic trap */
	    if(TrapPrivate__ArithmeticTrapWrap)
		(*TrapPrivate__ArithmeticTrapWrap)(&frame);
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
	    if(TrapPrivate__ProtectionFaultWrap)
		(*TrapPrivate__ProtectionFaultWrap)(&frame);
	    break;

	  case T_PAGEFLT:		/* page fault */
	    if(TrapPrivate__PageFaultWrap)
		(*TrapPrivate__PageFaultWrap)(&frame); 
	    break;

	  case T_DIVIDE:		/* integer divide fault */
	    if(TrapPrivate__DivideFaultWrap)
		(*TrapPrivate__DivideFaultWrap)(&frame);
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
	    if(TrapPrivate__OverflowFaultWrap)
		(*TrapPrivate__OverflowFaultWrap)(&frame);
	    break;

	  case T_BOUND:		/* bounds check fault */
	    if(TrapPrivate__BoundsCheckFaultWrap)
		(*TrapPrivate__BoundsCheckFaultWrap)(&frame);
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
		if(TrapPrivate__FPFaultWrap)
		    (*TrapPrivate__FPFaultWrap)(&frame);
		break;
	    }
			/* else ucode = emulator_only_knows() XXX */
#else	/* MATH_EMULATE || GPL_MATH_EMULATE */
	    if(TrapPrivate__FPFaultWrap)
		(*TrapPrivate__FPFaultWrap)(&frame);
#endif	/* MATH_EMULATE || GPL_MATH_EMULATE */
	    break;

	  case T_FPOPFLT:		/* FPU operand fetch fault */
	    if(TrapPrivate__FPOperandFaultWrap)
		(*TrapPrivate__FPOperandFaultWrap)(&frame);
	    break;
		}
    } else {
	/* kernel trap */
	
	switch (type) {
	  case T_PAGEFLT:			/* page fault */
	    if(TrapPrivate__PageFaultWrap)
		(*TrapPrivate__PageFaultWrap)(&frame); 
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
    if(TrapPrivate__SyscallWrap)
	(*TrapPrivate__SyscallWrap)(&frame);
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

#endif /* __FreeBSD__ */

#ifdef OSF


int
copyin(void *udaddr, void *kaddr, u_int len)
{
    if (*Sal__CopyIn)
	    return (*Sal__CopyIn)(udaddr, kaddr, len);

    printf("copyin called\n");
    return 0;
}

int
copyout(void *kaddr, void *udaddr, u_int len)
{
    if (*Sal__CopyOut)
	    return (*Sal__CopyOut)(kaddr, udaddr, len);

    printf("copyout called\n");
    return 0;
}

void
vm_pg_alloc(long *pp)
{
	(*MemoryForSAL__AllocatePhysPage)(pp);
}
	
int
vm_pg_free(long p)
{
	return (*MemoryForSAL__FreePhysPage)(p);
}


thread_t
kernel_thread(task_t task, long (*func)(), char *arg)
{
	if (*SalSync__KernelThread)
		return (thread_t)SalSync__KernelThread(func, arg);

	printf("kernel_thread called.\n");
}

void
thread_wakeup_one(vm_offset_t event)
{
    if (*SalSync__ThreadWakeupOne)
	SalSync__ThreadWakeupOne(event);
    else
	printf("thread_wakeup_one called too early.\n");
}

void
thread_wakeup_prim(vm_offset_t event, boolean_t one_thread, int result)
{
    if(one_thread)
	thread_wakeup_one(event);
    else
	thread_wakeup(event);
}

void
thread_block()
{
    if (*SalSync__BlockWithContinuation)
	SalSync__BlockWithContinuation(0, 0);
    else
	printf("thread_block called too early.\n");
}

void
clear_wait(thread_t thread, int result, boolean_t interrupt_only)
{
    if (*SalSync__ClearWait)
	SalSync__ClearWait(thread);
    else
	printf("clear_wait called too early.\n");
}

void 
wakeup_one(void *event) {
    thread_wakeup_one((vm_offset_t)event);
}

void
sleep(void *chan, int pri) {
    mpsleep(chan,pri, 0, 0, 0, 0);
}
#endif
