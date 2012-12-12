
/* There is a bootstrap requirement here.  Whatever is included from OSF
must be created in sal before spincore is built.  In sal/alpha_osf/Makefile,
'make configure' and the spincorefiles var covers any files that need to
be created before building spincore.
 */
#include <sys/types.h>
#include <kern/thread.h>

#undef thread_wakeup_one
#undef thread_wakeup
#undef sleep

#include <spincore/ALPHA_SPIN/Debugger.h>
#include <spincore/ALPHA_SPIN/Clock.h>
#include <spincore/ALPHA_SPIN/SalSync.h>
#include <spincore/ALPHA_SPIN/Sal.h>
#include <spincore/ALPHA_SPIN/MemoryForSAL.h>

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

    if (lockp) {
        if ( (flags&MP_CAN_DO) != MP_CAN_DO ) {
            panic("mpsleep with a lock");
        }
    }

    if (timo) {
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
pgsignal(void (*pg)(int), int sig, int checkctty)
{
	/* called from tty.c:tty_input

	   SPIN overloads tp->t_pgrp with a TIOCSPGRP ioctl in
	   dev/console/src/console.c to be a function pointer to a
	   signal handler
	 */
	
	if (pg) (*pg)(sig);
	else printf("pgsignal: ignored signal %d\n",sig);
}



int
copyin(void *udaddr, void *kaddr, u_int len)
{
    bcopy(udaddr, kaddr, len);
    return 0;
}

int
copyout(void *kaddr, void *udaddr, u_int len)
{
    bcopy(kaddr, udaddr, len);
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
extern int cold;

void
thread_wakeup(unsigned long chan)
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
sleep(void *chan, int pri) 
{
    mpsleep(chan,pri, 0, 0, 0, 0);
}


thread_t
current_thread()
{
    if (*SalSync__CurrentThread)
      return (thread_t) SalSync__CurrentThread();
    
    return 0;
}
