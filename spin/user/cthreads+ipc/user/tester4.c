/* 
 * HISTORY
 * 16-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Syscall with no args, delay.
 *
 */
#include "cthreads.h"
#include "io.h"

main() {
    int i;
    char buf[128];

    for(i = 1000000; i; --i)
	;
    /* syscalls no args */
    for(i = 1; 1; ++i) {
	nopsyscall();
	if((i & 8191) == 0)
	    sysprintd(i >> 13);
    }
}

