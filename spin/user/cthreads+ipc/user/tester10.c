/* 
 * HISTORY
 * 16-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Syscall with no args, delay.
 *
 */
#include "cthreads.h"
#include "io.h"
g(int i) { sysprintd(i); }
a(int i) { g(i); }

main() {
    int i;
    char buf[] = "hello world";

    /* syscalls no args */
    for(i = 1; 1; ++i) {
	a(i);
    }
}

