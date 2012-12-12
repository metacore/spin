/* 
 * HISTORY 
 * 12-Jun-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created using Jan's soo_select functionality.
 */

#include "net/net_globals.h"
#include <sys/secdefines.h>

#include "sys/param.h"
#include "sys/systm.h"
#include "sys/ioctl.h"
#include "sys/poll.h"
#include "sys/user.h"
#include "sys/file.h"
#include "sys/uio.h"
#include "sys/stat.h"

#include "sys/mbuf.h"
#include "sys/socket.h"
#include "sys/socketvar.h"
#include "sys/domain.h"
#include "sys/protosw.h"

soo_select(fp, events, revents, scanning)
	struct file *fp;
	short *events, *revents;
	int scanning;
{
        register struct socket *so = (struct socket *)fp;
	DOMAIN_FUNNEL_DECL(f)

	DOMAIN_FUNNEL(sodomain(so), f);
	SOCKET_LOCK(so);

	/*
	 * Note - only the so_rcv selqueue is used.
	 * It's protected by the socket lock.
	 */
	if (scanning) {
		if (so->so_error)
			*revents |= POLLERR;
		else if (so->so_state & SS_CANTRCVMORE)
			*revents |= POLLHUP;
		if (*events & (POLLNORM|POLLPRI)) {
			SOCKBUF_LOCK(&so->so_rcv);
			if (*events & POLLNORM) {
				if (soreadable(so))
					*revents |= POLLNORM;
				else if (*revents == 0)
					so->so_rcv.sb_flags |= SB_SEL;
			}
			if (*events & POLLPRI) {
				if (so->so_oobmark ||
				    (so->so_state & SS_RCVATMARK))
					*revents |= POLLPRI;
				else if (*revents == 0)
					so->so_rcv.sb_flags |= SB_SEL;
			}
			SOCKBUF_UNLOCK(&so->so_rcv);
		}
		if (*events & POLLOUT) {
			SOCKBUF_LOCK(&so->so_snd);
			if (sowriteable(so))
				*revents |= POLLOUT;
			else if (*revents == 0)
				so->so_snd.sb_flags |= SB_SEL;
			SOCKBUF_UNLOCK(&so->so_snd);
		}
		if (*revents == 0)
			select_enqueue(&so->so_rcv.sb_selq);
	} else
		select_dequeue(&so->so_rcv.sb_selq);

	SOCKET_UNLOCK(so);
	DOMAIN_UNFUNNEL(f);
	return (0);
}
