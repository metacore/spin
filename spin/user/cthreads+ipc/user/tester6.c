/* 
 * HISTORY
 * 16-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Fork/join test.
 *
 */
#include "cthreads.h"
#include "io.h"

long func1(void *a1) {
    return 0xdead;
}

main() {
    int i, res1, res2;
    void *th1, *th2;
    long start, end, sp, sp1, sp2;
    char buf[256];

    sp = spin_sbrk(8192 * 2);
    sysprintd(sp);
}

