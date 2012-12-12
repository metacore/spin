/* 
 * HISTORY
 * 16-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Various thread fork, join and syscall tests and timings.
 *
 */
#include "cthreads.h"
#include "io.h"
#define STACKSIZE 8192 * 2
#define PRINT(a, b)  timingprint(a, strlen(a), b)

long func0(void *a1) {
    return readtimerv();
}

long func1(void *a1) {
    return 0xdead;
}

main() {
    int i, res1, res2;
    void *th1, *th2;
    long start, end, sp, sp1, sp2;
    char buf[256], strbuf[256];

    /* syscalls no args */
    start = readtimerv();
    for(i = 1024; i; --i) {
	nopsyscall();
    }
    end = readtimerv();
    PRINT("Null system call: ", (end - start) >> 10);
    /* syscalls 128 bytes resultno args */
    start = readtimerv();
    for(i = 1024; i; --i) {
	xferbytes(buf);
    }
    end = readtimerv();
    PRINT("128 byte system call: ", (end - start) >> 10);
    /* overhead */
    for(i = 5; i; --i) {
	sp = spin_sbrk(STACKSIZE); sp += STACKSIZE;
	start = readtimerv();
	th1 = cthread_layered_fork(func1, (void *)12, 0, 0, sp);
	end = readtimerv();
	res1 = cthread_layered_join(th1);
 	PRINT("Layered cthread fork: ", end - start);
    }
    for(i = 5; i; --i) {
	sp = spin_sbrk(STACKSIZE); sp += STACKSIZE;
	start = readtimerv();
	th1 = cthread_strand_fork(func1, (void *)12, 0, 0, sp);
	end = readtimerv();
	res1 = cthread_strand_join(th1);
	PRINT("Native cthread fork: ", end - start);
    }
    /* latency */
    for(i = 5; i; --i) {
	sp = spin_sbrk(STACKSIZE); sp += STACKSIZE;
	start = readtimerv();
	th1 = cthread_layered_fork(func0, (void *)12, 0, 0, sp);
	end = cthread_layered_join(th1);
	PRINT("Layered cthread fork/run: ", end - start);
    }
    for(i = 5; i; --i) {
	sp = spin_sbrk(STACKSIZE); sp += STACKSIZE;
	start = readtimerv();
	th1 = cthread_strand_fork(func0, (void *)12, 0, 0, sp);
	end = cthread_strand_join(th1);
	PRINT("Native cthread fork/run: ", end - start);
    }
    /* fork & join */
    for(i = 5; i; --i) {
	sp = spin_sbrk(STACKSIZE); sp += STACKSIZE;
	start = readtimerv();
	res1 = cthread_layered_join(cthread_layered_fork(func1,
							 (void *)12,0,0,sp));
	end = readtimerv();
	PRINT("Layered cthread fork/join: ", end - start);
    }
    for(i = 5; i; --i) {
	sp = spin_sbrk(STACKSIZE); sp += STACKSIZE;
	start = readtimerv();
	res1 = cthread_strand_join(cthread_strand_fork(func1,
						       (void *)12,0,0,sp));
	end = readtimerv();
	PRINT("Native cthread fork/join: ", end - start);
    }
    _exit(0);
}

