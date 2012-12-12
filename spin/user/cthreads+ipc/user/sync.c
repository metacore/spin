/*
 * HISTORY
 * 07-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created.
 *
 */
#include "cthreads.h"

long mutexes[10000];
int nmutex = 0;

void syncinit() {
    /*
     * register beginpc and length
     */
    rasregister(mutex_trylock, 32);
}

mutex_t mutex_alloc() {
    int v = atomicinc(&nmutex);
    return &mutexes[v];
}

void mutex_lock(mutex_t m) {
    while(mutex_trylock(m) == 0)
	cthread_yield();
}

void mutex_unlock(mutex_t m) {
    *m = 0;
}

void cond_wait(cond_t c, mutex_t m) {
    condition_wait(c, m);
    while(mutex_trylock(m) == 0)
	cthread_yield();
}
