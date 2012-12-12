#include "cthreads.h"
#include "io.h"

mutex_t m;
cond_t startup, c1, c2;

int counter;

long inctest_nonatomic() {
    int i;

    syncinit();
    for(i = 20000000; i; --i) {
	counter++;
    }
    return counter;
}

long inctest_atomic() {
    int i;

    syncinit();
    for(i = 20000000; i; --i) {
	atomicinc(&counter);
    }
    return counter;
}

long curly(void *a2) {
    int i;
    long start, end;
    
    syncinit();
    mutex_lock(m);
    cond_wait(startup, m);
    start = readtimer();
    for(i = 1000; i; --i) {
	cond_signal(c1);
	cond_wait(c2, m);
    }
    end = readtimer();
    mutex_unlock(m);
    return (end - start);
}

long moe(void *a2) {
    int i;
    long start, end;

    syncinit();
    mutex_lock(m);
    cond_signal(startup);
    start = readtimer();
    for(i = 1000; i; --i) {
	cond_wait(c1, m);
	cond_signal(c2);
    }
    end = readtimer();
    mutex_unlock(m);
    return (end - start);
}

main() {
    int i, res1, res2;
    void *th1, *th2;
    long start, end, sp, sp1, sp2;

    startup = cond_alloc();
    c1 = cond_alloc();
    c2 = cond_alloc();
    m = mutex_alloc();

    sp1 = spin_sbrk(8192 * 2);
    sp2 = spin_sbrk(8192 * 2);
    th1 = cthread_strand_fork(curly, (void *)10, 0, 0, sp1);
    th2 = cthread_strand_fork(moe, (void *)10, 0, 0, sp2);
    res2 = cthread_strand_join(th2);
    res1 = cthread_strand_join(th1);
    sysprintd(res1);
    sysprintd(res2);
}
