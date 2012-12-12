/*
 * HISTORY
 * 06-Mar-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created.
 *
 */

typedef void *cthread_t;

/*
 * layered in-kernel implementation
 */
cthread_t cthread_layered_fork(long (*func)(void *),
			       void *arg,
			       long dummy1,
			       long dummy2,
			       long sp);
void cthread_layered_exit();
long cthread_layered_join(cthread_t thread);

/*
 * in-kernel, directly on strands
 */
cthread_t cthread_strand_fork(long (*func)(void *),
			      void *arg,
			      long dummy1,
			      long dummy2,
			      long sp);
void cthread_strand_exit();
long cthread_strand_join(cthread_t thread);

void cthread_yield();

typedef long *mutex_t;
typedef long cond_t;

void syncinit();

/* mutexes */
mutex_t mutex_alloc();

int mutex_trylock(mutex_t m);

void mutex_lock(mutex_t m);

void mutex_unlock(mutex_t m);

/* condition variables */
cond_t cond_alloc();

void cond_wait(cond_t c, mutex_t m);

void cond_signal(cond_t c);

void cond_broadcast(cond_t c);

int atomicinc(int *);
