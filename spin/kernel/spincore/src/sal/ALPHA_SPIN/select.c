

#include <sys/types.h>
#include <sys/time.h>
#include <sys/param.h>
#include <sys/kernel.h>

/*****
 from sys/poll.h
 */

struct pollfd {
	int (*selectop)(long descriptor, short *events, short *revents, int scanning);
	long descriptor;
        short   events;
        short   revents;
};

/* Poll masks */
#define	POLLNORM	01	/* message available on read queue */
#define	POLLOUT		04	/* stream is writable */
#define	POLLERR		010	/* error message has arrived */
#define	POLLHUP		020	/* hangup has occurred */
#define	POLLNVAL	040	/* invalid fd */
#define	POLLRDNORM	0100	/* A non-priority message is available */
#define	POLLRDBAND	0200	/* A band>0 message is available */
#define	POLLWRNORM	0400	/* Same as POLLOUT */
#define	POLLWRBAND	01000	/* A priority band exists and is writable */
#define	POLLMSG		02000	/* A SIGPOLL message has reached the
				 * front of the queue */

#define LOCAL_FDS       32      /* This many fd's locally polled/selected */



#ifdef __FreeBSD__
#define TIME_READ(t)
/* XXX this is surely wrong. TIME_READ reads the current time
   into "t". someone fix this please - yas */
#endif

/*****
 from bsd/kern_time.c
 */

/*
 * Compute number of hz until specified time.
 * Used to compute third argument to timeout() from an
 * absolute time.
 */
int
hzto(struct timeval *tv)
{
	register int ticks, sec, usec;
	struct timeval t;

	TIME_READ(t);
	sec = tv->tv_sec - t.tv_sec;
	usec = tv->tv_usec - t.tv_usec;

	/*
	 * If number of seconds will fit in 32 bit arithmetic,
	 * then compute number of seconds to time and scale to
	 * ticks.  Otherwise round times greater than representible
	 * to maximum value.
	 *
	 * hz may range from 1 to 2147 without loss of (32-bit) precision.
	 * Maximum value for any timeout depends on hz.
	 * Must potentially correct for roundoff error in tick (1000000/hz)
	 * when passed as tv = { 0, tick }.
	 */
	if (usec < 0 && sec > 0){
		sec--;
		usec = 1000000 + usec;
	}
	if (sec + 1 <= INT_MAX / hz) {
		ticks = (sec * hz) + (((usec + tick - 1) * hz) / (1000*1000));
		if (ticks <= 0) ticks = 1;
	} else
		ticks = INT_MAX;
	return (ticks);
}


/*****
 from bsd/kern_time.c
 */

/*
 * Add and subtract routines for timevals.
 * N.B.: subtract routine doesn't deal with
 * results which are before the beginning,
 * it just gets very confused in this case.
 * Caveat emptor.
 */
void
timevalfix(t1)
	struct timeval *t1;
{

	if (t1->tv_usec < 0) {
		t1->tv_sec--;
		t1->tv_usec += 1000000;
	}
#ifdef HW_DIVIDE
	/* If the Alpha had a hardwarde integer divide, we'd
	 * do it this way.
	 */
	if (t1->tv_usec >= 1000000) {
		t1->tv_sec  += t1->tv_usec / 1000000;
		t1->tv_usec %= 1000000;
	}
#else HW_DIVIDE
	/* Unfortunately, we have to hack in this loop because
	 * they tell me INTEGER DIVIDE is simulated in software.
	 */
	while (t1->tv_usec >= 1000000) {
		t1->tv_sec++;
		t1->tv_usec -= 1000000;
	}
#endif HW_DIVIDE
}

void
timevaladd(t1, t2)
	struct timeval *t1, *t2;
{

	t1->tv_sec += t2->tv_sec;
	t1->tv_usec += t2->tv_usec;
	timevalfix(t1);
}

/*****
 from bsd/sys_generic.c
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/errno.h>

/*
 * Select/Poll.
 */

int
selscan(
	register struct pollfd *fd_ptr,
	unsigned long *pnfds,
	long *retval)
{
	long number = 0;
	int error;
	register unsigned long nfds = *pnfds;

	while (nfds > 0) {
		fd_ptr->revents = 0;
		error = (*fd_ptr->selectop)(fd_ptr->descriptor,&fd_ptr->events, &fd_ptr->revents, 1);
		if (error || (fd_ptr->revents & POLLNVAL)) {
			*pnfds -= nfds;
			if (error)
				return (error);
			return (EBADF);
		}
		/*
		 * For select, POLLHUP means readable, POLLERR means both.
		 * And always, be sure to ignore any extra bits.
		 */
		if (fd_ptr->revents & POLLERR)
			fd_ptr->revents |= (POLLNORM|POLLOUT);
		if (fd_ptr->revents & POLLHUP)
			fd_ptr->revents |= POLLNORM;
		if (fd_ptr->revents &= fd_ptr->events)
			number++;
		fd_ptr++;
		nfds--;
	}
	*retval = number;
	return (0);
}


static void
undo_scan(
	register struct pollfd *fd_ptr,
	register unsigned long nfds)
{
	int error;
	register struct file *fp;
	struct ufile_state *ufp;

	while (nfds-- > 0) {
		error = (*fd_ptr->selectop)(fd_ptr->descriptor,&fd_ptr->events, &fd_ptr->revents, 0);
		++fd_ptr;
	}
}

int do_scan(
	struct pollfd *fd_ptr,
	unsigned long nfds,
	long *retval,
	struct timeval *atv,
	int has_timeout)
{
	int timo = 0, error;
	struct timeval t;
	void *eventp = (void*)current_thread();

	if (has_timeout) {
		TIME_READ(t);
		timevaladd(atv, &t);
	}
	for (;;) {
		error = selscan(fd_ptr, &nfds, retval);
		if (error || *retval)
			break;
		if (has_timeout) {
			TIME_READ(t);
			/* timercmp(&t, atv, >=) cannot be used */
			if (t.tv_sec > atv->tv_sec ||
			    (t.tv_sec == atv->tv_sec &&
			     t.tv_usec >= atv->tv_usec))
				break;
			timo = hzto(atv);
		}
		error = mpsleep((caddr_t)eventp,
				(long)((PZERO + 1) | PCATCH), "event",
				(long)timo, 0, 0);
		if (error)
			break;
		undo_scan(fd_ptr, nfds);
	}
	undo_scan(fd_ptr, nfds);
	if (error == ERESTART)
		error = EINTR;	/* not restartable */
	else if (error == EWOULDBLOCK)
		error = 0;
	return (error);
}
