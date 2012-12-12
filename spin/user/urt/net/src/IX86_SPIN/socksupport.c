#include <sys/param.h>
#include <sys/systm.h>
#include <sys/proc.h>
#include <sys/file.h>
#include <sys/mbuf.h>
#include <sys/protosw.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/socketvar.h>
#include <sys/ioctl.h>
#include <sys/stat.h>

#include <net/if.h>
#include <net/route.h>

int
soo_select(fp, which, p)
        struct file *fp;
        int which;
        struct proc *p;
{
        struct socket *so = (struct socket*)fp; /* Socket.m3 actually passes
						   socket. */
        register int s = splnet();

        switch (which) {

        case FREAD:
                if (soreadable(so)) {
                        splx(s);
                        return (1);
                }
                selrecord(p, &so->so_rcv.sb_sel);
                so->so_rcv.sb_flags |= SB_SEL;
                break;

        case FWRITE:
                if (sowriteable(so)) {
                        splx(s);
                        return (1);
                }
                selrecord(p, &so->so_snd.sb_sel);
                so->so_snd.sb_flags |= SB_SEL;
                break;

        case 0:
                if (so->so_oobmark || (so->so_state & SS_RCVATMARK)) {
                        splx(s);
                        return (1);
                }
                selrecord(p, &so->so_rcv.sb_sel);
                so->so_rcv.sb_flags |= SB_SEL;
                break;
        }
        splx(s);
        return (0);
}

/* these symbols are here so that this stuff will compile.  No other reason. */
/* In particular, don't try to use this stuff. */

sosbwait(sb, so)
	struct sockbuf *sb;
	struct socket *so;
{
  return sbwait(sb);
}

sosblock(sb, so)
	register struct sockbuf *sb;
	struct socket *so;
{
  int error;

  if ((sb)->sb_flags & SB_LOCK) {
    error = sb_lock(sb);
  } else {
    (sb)->sb_flags |= SB_LOCK;    
    error = 0;
  }
  return error;
}

sosbunlock(sb)
	struct sockbuf *sb;
{
  sbunlock(sb);
}


sosleep(so, addr, pri, tmo)
	struct socket *so;
	caddr_t addr;
	int pri, tmo;
{
  extern char netcon[]; /* from uipc_socket2.c */
  return tsleep(addr,pri,netcon,tmo);
}
