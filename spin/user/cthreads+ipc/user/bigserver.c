/* 
 * HISTORY
 * 16-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. IPC client with data transfer.
 *      Server downloads IPC code into the kernel.
 */
#include "cthreads.h"
#include "io.h"

void server() {
    int i, x = 0;
    char resbuf[130];

    resbuf[12] = 'g';
    resbuf[13] = 'u';
    resbuf[14] = 'n';

    rpcregister2();
    for(i = 100; i; --i) {
	rpcreturn2(resbuf);
    }
}

main() {
    InitCode();
    server();
}

