/* 
 * HISTORY
 * 16-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. IPC client with no data transfer.
 *      Server downloads IPC code into the kernel.
 */
#include "cthreads.h"
#include "io.h"

void server() {
    int i, x = 0;
    char resbuf[130];

    rpcregister();
    for(i = 100; i; --i) {
	rpcreturn(x++);
    }
}

main() {
    InitCode();
    server();
}

