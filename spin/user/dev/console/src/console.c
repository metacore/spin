/*
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *      Added console_spgrp for new pgsignal based tty sig handling
 *
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Used more generic names for the ioctls TCGETS (TIOCGETA) and 
 *      TCSETS (TIOCSETA) that are available under FreeBSD as well as 
 *      Digital Unix.
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/types.h>
#include <sys/errno.h>
#include <sys/ioctl.h>
#include <sys/termios.h>
#include <sys/proc.h>
#include <sys/uio.h>
#include <spincore/src/sal/OpenArray.h>

/* refer to bsd/sys_generic.c for rwuio */

#define CONSOLEDEV 0	/* major+minor dev_t for console */
#define FLAGS 0	/* could be O_NONBLOCK or O_NDELAY*/

/*
 * Common handling for read, readv, write, and writev system calls
 *
 * NOTE: write()/read() are directly before and soo_read()/soo_write() are
 * directly after in kernel text space for best possible cache locality.
 */
static int /* added when arg order changed to defend against outside callers */
cn_rwuio(retval, uio, rw)
	long *retval;
	register struct uio *uio;
	enum uio_rw rw;
{
	struct iovec *iov;
	int i, count, error;

	int flag = 0;  /* other options are: IO_NDELAY|IO_NONBLOCK */

	count = 0;
	iov = uio->uio_iov;
	i = uio->uio_iovcnt; /* verified by caller to be greater than 0 */
	do {
		if (iov->iov_len < 0 || (count += iov->iov_len) < 0)
			goto einval;
		iov++;
	} while (--i > 0);
	uio->uio_resid = count;
	uio->uio_segflg = UIO_SYSSPACE; /* SPIN console is in SYSSPACE */
	uio->uio_rw = rw;

	if (rw == UIO_READ)
		error = cnread(CONSOLEDEV,uio,flag);
	else
		error = cnwrite(CONSOLEDEV,uio,flag);

	if (error) {
		/*
		 * If some data has been moved, then we should
		 * report the movement count rather than the error.
		 */
		if (uio->uio_resid != count)
			error = 0;
	}
	count -= uio->uio_resid;
	*retval = (long)count;
error:
	return (error);
einval:
	return (EINVAL);
}

int
console_nread(int *bytes)
{
	return cnioctl(CONSOLEDEV,FIONREAD,&bytes,FLAGS);
}

int
console_read(struct openarray *data, long *bytes)
{
        struct uio auio;
        struct iovec aiov;

        aiov.iov_base = (caddr_t)data->start;
        aiov.iov_len = *bytes;
        auio.uio_iov = &aiov;
        auio.uio_iovcnt = 1;

	return cn_rwuio(bytes, &auio, UIO_READ);
}


int
console_write(struct openarray *data, long *bytes)
{
        struct uio auio;
        struct iovec aiov;

        aiov.iov_base = (caddr_t)data->start;;
        aiov.iov_len = *bytes;
        auio.uio_iov = &aiov;
        auio.uio_iovcnt = 1;

	return cn_rwuio(bytes, &auio, UIO_WRITE);
}

int
console_ioctl(int cmd, struct openarray *data, int flags)
{
    return cnioctl(CONSOLEDEV, cmd, data->start, flags);
}
int
console_spgrp(void *data)
{
    /* on a tty.c:tty_input calls pgsignal which will call (*data)(signal) */
    return cnioctl(CONSOLEDEV, TIOCSPGRP, data, 0);
}


/* termios.h sets defaults we don't like when _KERNEL is defined.
   Here are the user defaults flags people are used to.
 */
#undef TTYDEF_LFLAG
#undef TTYDEF_IFLAG
#define TTYDEF_LFLAG    (ECHO | ICANON | ISIG | IEXTEN | ECHOE|ECHOKE|ECHOCTL)
#define TTYDEF_IFLAG    (BRKINT | ICRNL | IMAXBEL | IXON | IXANY)

console_open()
{
	int rc;
	struct termios t;
	if (rc=cnopen(CONSOLEDEV,0)) return rc;
	if (rc=cnioctl(CONSOLEDEV,TIOCGETA,&t,FLAGS)) return rc;
	t.c_iflag = TTYDEF_IFLAG;
	t.c_lflag = TTYDEF_LFLAG;
	t.c_cc[VERASE] = 0177;

	/* we started using ^A and now for historical compatibility...  */
	t.c_cc[VQUIT] = 1;

	if (rc=cnioctl(CONSOLEDEV,TIOCSETA,&t,FLAGS)) return rc;

	cnioctl(CONSOLEDEV, TIOCSPGRP, 0, 0); /* clear signal handler */

	return rc;
}

console_close()
{
	return cnclose(CONSOLEDEV);
}
