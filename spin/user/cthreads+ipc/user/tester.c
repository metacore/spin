/* 
 * HISTORY
 * 16-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Various thread fork, join and syscall tests and timings.
 *
 */
#include "cthreads.h"
#include "io.h"
#define STACKSIZE 8192 * 2

long func0(void *a1) {
    return readtimer();
}

long func1(void *a1) {
    /* sysprintd((long) a1); */
    return 0xdead;
}

main() {
    int i, res1, res2;
    void *th1, *th2;
    long start, end, sp;
    char buf[256];

    /* syscalls no args */
    start = readtimer();
    for(i = 10000; i; --i) {
	nopsyscall();
    }
    end = readtimer();
    sysprintd(end - start);
    sysprintd(0xffffffffffffffff);
    /* syscalls 128 bytes resultno args */
    start = readtimer();
    for(i = 10000; i; --i) {
	xferbytes(buf);
    }
    end = readtimer();
    sysprintd(end - start);
    sysprintd(0xffffffffffffffff);
    /* overhead */
    for(i = 10; i; --i) {
	sp = spin_sbrk(STACKSIZE); sp += STACKSIZE;
	start = readtimer();
	th1 = cthread_layered_fork(func1, (void *)12, 0, 0, sp);
	end = readtimer();
	res1 = cthread_layered_join(th1);
	sysprintd(res1);
	sysprintd(end - start);
    }
    sysprintd(0xffffffffffffffff);
    for(i = 10; i; --i) {
	sp = spin_sbrk(STACKSIZE); sp += STACKSIZE;
	start = readtimer();
	th1 = cthread_strand_fork(func1, (void *)12, 0, 0, sp);
	end = readtimer();
	res1 = cthread_strand_join(th1);
	sysprintd(end - start);
    }
    sysprintd(0xffffffffffffffff);
    /* latency */
    for(i = 10; i; --i) {
	sp = spin_sbrk(STACKSIZE); sp += STACKSIZE;
	start = readtimer();
	th1 = cthread_layered_fork(func0, (void *)12, 0, 0, sp);
	end = cthread_layered_join(th1);
	sysprintd(end - start);
    }
    sysprintd(0xffffffffffffffff);
    for(i = 10; i; --i) {
	sp = spin_sbrk(STACKSIZE); sp += STACKSIZE;
	start = readtimer();
	th1 = cthread_strand_fork(func0, (void *)12, 0, 0, sp);
	end = cthread_strand_join(th1);
	sysprintd(end - start);
    }
    /* fork & join */
    sysprintd(0xffffffffffffffff);
    for(i = 10; i; --i) {
	sp = spin_sbrk(STACKSIZE); sp += STACKSIZE;
	start = readtimer();
	res1 = cthread_layered_join(cthread_layered_fork(func1,
							 (void *)12,0,0,sp));
	end = readtimer();
	sysprintd(res1);
	sysprintd(end - start);
    }
    sysprintd(0xffffffffffffffff);
    for(i = 10; i; --i) {
	sp = spin_sbrk(STACKSIZE); sp += STACKSIZE;
	start = readtimer();
	res1 = cthread_strand_join(cthread_strand_fork(func1,
						       (void *)12,0,0,sp));
	end = readtimer();
	sysprintd(res1);
	sysprintd(end - start);
    }
    sysprintd(0xffffffffffffffff);
}

