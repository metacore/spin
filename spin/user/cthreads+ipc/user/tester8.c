/* 
 * HISTORY
 * 16-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Fork/join test.
 *
 */
#include "cthreads.h"
#include "io.h"
#define STACKSIZE 8192 * 2

long func1(void *a1) {
    return 0xdead;
}

main() {
    int i, res1, res2;
    void *th1, *th2;
    long start, end, sp, sp1, sp2;
    char buf[256];

    sp = spin_sbrk(STACKSIZE);
    sp += STACKSIZE - 64;
    sysprintd(sp);
    for(i = 1; i <= 96; ++i) {
	th1 = cthread_strand_fork(func1, (void *)12, 0, 0, sp);
	res1 = cthread_strand_join(th1);
	sysprintd(i);
	if(res1 != 0xdead)
	    sysprintd(-1);
    }
}

