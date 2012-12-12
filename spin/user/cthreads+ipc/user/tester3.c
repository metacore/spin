/* 
 * HISTORY
 * 16-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Syscall with no arguments.
 *
 */
#include "cthreads.h"
#include "io.h"

main() {
    int i;

    /* syscalls no args */
    for(i = 1; 1; ++i) {
	nopsyscall();
	if((i & 8191) == 0)
	    sysprintd(i >> 13);
    }
}

