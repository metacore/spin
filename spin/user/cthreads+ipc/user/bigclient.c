/* 
 * HISTORY
 * 16-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. IPC client with data transfer.
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
	rpcrequest2(buf);
	end = readtimerv();
	if(i == 100) {
	    sysprintd(buf[12]);
	    sysprintd(buf[13]);
	    sysprintd(buf[14]);
	}
	sysprintd(end - start);
    }
}

main() {
    client();
}

