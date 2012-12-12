/* 
 * HISTORY
 * 16-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. IPC client with no data transfer.
 *
 */
#include "cthreads.h"
#include "io.h"

long client() {
    int i, x;
    long start, end;
    char buf[130];

    for(i = 100; i; --i) {
	start = readtimerv();
	x = rpcrequest();
	end = readtimerv();
	sysprintd(x);
	sysprintd(end - start);
    }
}

main() {
    client();
}

