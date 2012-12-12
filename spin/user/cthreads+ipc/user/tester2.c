/* 
 * HISTORY
 * 16-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Fork latency timer for layered cthreads.
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
    long start, end, sp, sp1, sp2;
    char buf[256];

    sysprintd(0xffffffffffffffff);
    for(i = 1000; i; --i) {
	sp = spin_sbrk(STACKSIZE);
	sp += STACKSIZE;
	start = readtimer();
	th1 = cthread_layered_fork(func0, (void *)12, 0, 0, sp);
	end = readtimer();
	res1 = cthread_layered_join(th1);
	sysprintd(end - start);
    }
    sysprintd(0xffffffffffffffff);
}

