


#include <sys/types.h>
#include <sys/param.h>
#include <sys/kernel.h>
#ifdef OSF
#include <varargs.h>
#endif
#ifdef __FreeBSD__
#include <machine/varargs.h>
#endif
#define SALNET
#include <sys/mbuf.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <net/if.h>
#include <netinet/if_ether.h>
#include <machine/spl.h>
#include <rpc/types.h>
#include <rpc/xdr.h>
#include <rpc/pmap_prot.h>
#include <nfs/nfsv2.h>
#include <nfs/rpcv2.h>
#include <sal/salnet.h>

#define MSG_CALL      0


struct rpc_reply {
	int id;
	int type;
	int rstatus;
	int verifier;
	int v2;
	int astatus;
	int data[1];
	};


static int rpc_id;
static int portseed=0;
static char hostname[]="loom"; /* XXX wants to be 4 chars for mount */

/**************************************************************************
PRINTF and friends

	Formats:
		%X	- 4 byte ASCII (8 hex digits)
		%x	- 2 byte ASCII (4 hex digits)
		%b	- 1 byte ASCII (2 hex digits)
		%d	- decimal
		%c	- ASCII char
		%s	- ASCII string
		%I	- Internet address in x.x.x.x notation
		%L	- Binary long
		%S	- String (multiple of 4 bytes) preceded with 4 byte
			  binary length
		%M	- Copy memory.  Takes two args, len and ptr
**************************************************************************/
static char hex[]="0123456789ABCDEF";
char *xdr_do_printf(buf, fmt, va)
	char *buf, *fmt;
	va_list va;
{
	register char *p;
	char tmp[16];
	while (*fmt) {
		if (*fmt == '%') {	/* switch() uses more space */
			fmt++;
			if (*fmt == 'L') {
				register int h = va_arg(va, int);
				*(buf++) = h>>24;
				*(buf++) = h>>16;
				*(buf++) = h>>8;
				*(buf++) = h;
			}
			if (*fmt == 'S') {
				register int len = 0;
				char *lenptr = buf;
				p = (char *)va_arg(va, char*);
				buf += 4;
				while (*p) {
					*(buf++) = *p++;
					len ++;
				}
				*(lenptr++) = len>>24;
				*(lenptr++) = len>>16;
				*(lenptr++) = len>>8;
				*lenptr = len;
				while (len & 3) {
					*(buf++) = 0;
					len ++;
				}
			}
			if (*fmt == 'M') {
				register int len = va_arg(va, int);
				bcopy((char *)va_arg(va, char*), buf, len);
				buf += len;
			}
			if (*fmt == 'X') {
				register int h = va_arg(va, int);
				*(buf++) = hex[(h>>28)& 0x0F];
				*(buf++) = hex[(h>>24)& 0x0F];
				*(buf++) = hex[(h>>20)& 0x0F];
				*(buf++) = hex[(h>>16)& 0x0F];
				*(buf++) = hex[(h>>12)& 0x0F];
				*(buf++) = hex[(h>>8)& 0x0F];
				*(buf++) = hex[(h>>4)& 0x0F];
				*(buf++) = hex[h& 0x0F];
			}
			if (*fmt == 'x') {
				register int h = va_arg(va, int);
				*(buf++) = hex[(h>>12)& 0x0F];
				*(buf++) = hex[(h>>8)& 0x0F];
				*(buf++) = hex[(h>>4)& 0x0F];
				*(buf++) = hex[h& 0x0F];
			}
			if (*fmt == 'b') {
				register int h = va_arg(va, int);
				*(buf++) = hex[(h>>4)& 0x0F];
				*(buf++) = hex[h& 0x0F];
			}
			if (*fmt == 'd') {
				register int dec = va_arg(va, int);
				p = tmp;
				if (dec < 0) {
					*(buf++) = '-';
					dec = -dec;
				}
				do {
					*(p++) = '0' + (dec%10);
					dec = dec/10;
				} while(dec);
				while ((--p) >= tmp) *(buf++) = *p;
			}
			if (*fmt == 'I') {
				register int ip = va_arg(va, int);
				buf = (char*)sprintf(buf,"%d.%d.%d.%d",
					(ip>>24) & 0x00FF,
					(ip>>16) & 0x00FF,
					(ip>>8) & 0x00FF,
					ip & 0x00FF);
			}
			if (*fmt == 'c')
				*(buf++) = va_arg(va, char);
			if (*fmt == 's') {
				p = (char *) va_arg(va, char*);
				while (*p) *(buf++) = *p++;
			}
		} else *(buf++) = *fmt;
		fmt++;
	}
	*buf = 0;
	return(buf);
}

int xdrprintf(va_alist)        /* buf, fmt, va_alist */
        va_dcl
{
        va_list valist;
	char *end, *buf, *fmt;

	va_start(valist);
	buf = (char*)va_arg(valist, u_char *);
	fmt = (char*)va_arg(valist, u_char *);
	end = xdr_do_printf(buf, fmt, valist);
	va_end(valist);
				 
	return(end-buf);
}

salnet_err
rpclookup(servip, prog, ver, port)
	int servip, prog, ver, *port;
{
	salnet_route portmap;
	int bytes;
	struct mbuf *m;
	struct rpc_reply *rpc;
	char rpcether[6];
	char request[256];
	int reqbytes, err;

	if (!port) return SALNET_BADARGS;
	*port=0;

	err = salnet_arp(servip,rpcether);
	if (err) return err;
	salnet_getlocalroute(&portmap);

	portmap.localudpport=700+(portseed++&0xff);
	portmap.remotudpport=PMAPPORT;
	portmap.remotip=servip;
	portmap.remotether=rpcether;

	reqbytes = xdrprintf(request,"%L%L%L%L%L%L%L%L%L%L%L%L%L%L",
		++rpc_id, MSG_CALL, 2, PMAPPROG, 2, PMAPPROC_GETPORT,
		0, 0, 0, 0, prog, ver, IPPROTO_UDP, 0);
	err = salnet_udploop(&portmap, request, reqbytes,
			3, 2000, sizeof(struct rpc_reply), &m);
	if (err) return err;

	rpc = mtod(m, struct rpc_reply *);
	if (rpc->rstatus || rpc->verifier || rpc->astatus) {
		*port=ntohl(rpc->data[0]);
		m_freem(m);
		return SALNET_NFSERROR;
		}
	*port = ntohl(rpc->data[0]);
	m_freem(m);
	return SALNET_SUCCESS;
}

/***************************************************************************

NFS_MOUNT:  Mount an NFS Filesystem

***************************************************************************/
salnet_err
nfs_mount(servip, port, path, fh)
	int servip;
	int port;
	char *path;
	char *fh;
{
	salnet_route mountd;
	struct mbuf *m;
	struct rpc_reply *rpc;
	char rpcether[6];
	char request[256];
	int reqbytes, err;

	if (!path || !fh) return SALNET_BADARGS;

	err = salnet_arp(servip,rpcether);
	if (err) return err;
	salnet_getlocalroute(&mountd);
	mountd.localudpport=700+(portseed++&0xff);
	mountd.remotudpport=port;
	mountd.remotip=servip;
	mountd.remotether=rpcether;

	reqbytes = xdrprintf(request,"%L%L%L%L%L%L%L%L%L%S%L%L%L%L%L%L%L%S",
		++rpc_id, MSG_CALL, 2, RPCPROG_MNT, 1, RPCMNT_MOUNT,
		1, strlen(hostname) + 28,0, hostname,0,0,2,0,0,0,0, path);
	err = salnet_udploop(&mountd, request, reqbytes,
			3, 2000, sizeof(struct rpc_reply), &m);
	if (err) return err;

	rpc = mtod(m, struct rpc_reply *);
	if (rpc->rstatus || rpc->verifier || rpc->astatus || rpc->data[0]) {
		*(int*)fh=ntohl(rpc->data[0]);
		m_freem(m);
		return SALNET_NFSERROR;
		}

	m_copydata(m,sizeof(struct rpc_reply), 32, fh);
	m_freem(m);
	return SALNET_SUCCESS;
}

salnet_err
nfs_readlink(servip, port, fh, path)
	int servip;
	int port;
	char *fh;
	char *path;
{
	salnet_route nfsd;
	struct mbuf *m;
	struct rpc_reply *rpc;
	char rpcether[6];
	int pathlen;
	char request[256];
	int reqbytes, err;

	if (!path || !fh) return SALNET_BADARGS;

	err = salnet_arp(servip,rpcether);
	if (err) return err;
	salnet_getlocalroute(&nfsd);
	nfsd.localudpport=700+(portseed++&0xff);
	nfsd.remotudpport=port;
	nfsd.remotip=servip;
	nfsd.remotether=rpcether;

	reqbytes = xdrprintf(request,
		"%L%L%L%L%L%L%L%L%L%S%L%L%L%L%L%L%L%M",
		++rpc_id, MSG_CALL, 2, RPCPROG_NFS, 2, NFSPROC_READLINK,
		1, strlen(hostname) + 28,0,hostname,0,0,2,0,0,0,0,
		32, fh);
	err = salnet_udploop(&nfsd, request, reqbytes,
			3, 2000, sizeof(struct rpc_reply), &m);
	if (err) return err;

	rpc = mtod(m, struct rpc_reply *);
	if (rpc->rstatus || rpc->verifier || rpc->astatus || rpc->data[0]) {
		*(int*)path=ntohl(rpc->data[0]);
		m_freem(m);
		return SALNET_NFSERROR;
		}

	pathlen = ntohl(rpc->data[1]);
	m_copydata(m,sizeof(struct rpc_reply)+4, pathlen, path);
	path[pathlen]=0;
	m_freem(m);
	return SALNET_SUCCESS;
}


/* The real nfs_fattr also cover the diskless protocol NQ so we do our
	own to simplify our file attribute code*/
struct nfs_fattr {
	u_int	fa_type;
	u_int	fa_mode;
	u_int	fa_nlink;
	u_int	fa_uid;
	u_int	fa_gid;
	u_int	nfsfa_size;
	u_int	nfsfa_blocksize;
	u_int	nfsfa_rdev;
	u_int	nfsfa_blocks;
	u_int	nfsfa_fsid;
	u_int	nfsfa_fileid;
	struct nfsv2_time nfsfa_atime;
	struct nfsv2_time nfsfa_mtime;
	struct nfsv2_time nfsfa_ctime;
	} fa_nfsv2;

char *index(char*,char);
int
pathmatch(char *p, char *q)
{
	if (!index(p,'/')) return 0;
	if (!index(q,'/')) return 0;
	while(*p++==*q++)
		;
	if (index(p,'/')) return 0;
	if (index(q,'/')) return 0;
	return 1;
}

/***************************************************************************

NFS_LOOKUP:  Lookup Pathname

***************************************************************************/
salnet_err
nfs_lookuppath(servip, port, fh, reqpath, file_fh)
	int servip;
	int port;
	char *fh;
	char *reqpath;
	char *file_fh;
{
	salnet_route nfsd;
	struct mbuf *m;
	struct rpc_reply *rpc;
	char rpcether[6];
	char *slash, *path=reqpath;
	struct nfs_fattr fattr;
	char dirfh[32];
	char request[256];
	int reqbytes, err;

	static char cache_path[256]="/0";
	static char cache_fh[32];

	if (!path || !file_fh || !fh) return SALNET_BADARGS;


	err = salnet_arp(servip,rpcether);
	if (err) return err;

	if (pathmatch(path, cache_path)) {
		bcopy(cache_fh, dirfh, 32);
		while (slash=index(path,'/')) path=slash+1;
		}
	else	bcopy(fh, dirfh, 32);

	do {
		slash=index(path,'/');
		if (slash) *slash=0;

		salnet_getlocalroute(&nfsd);
		nfsd.localudpport=700+(portseed++&0xff);
		nfsd.remotudpport=port;
		nfsd.remotip=servip;
		nfsd.remotether=rpcether;

		reqbytes = xdrprintf(request,
			"%L%L%L%L%L%L%L%L%L%S%L%L%L%L%L%L%L%M%S",
			++rpc_id, MSG_CALL, 2, RPCPROG_NFS, 2, NFSPROC_LOOKUP,
			1, strlen(hostname) + 28,0,hostname,0,0,2,0,0,0,0,
			32, dirfh, path);
		err = salnet_udploop(&nfsd, request, reqbytes,
				3, 2000, sizeof(struct rpc_reply), &m);
		if (err) {
			if (slash) *slash='/';
			return err;
			}

		rpc = mtod(m, struct rpc_reply *);
		if(rpc->rstatus||rpc->verifier||rpc->astatus||rpc->data[0]){
			*(int*)file_fh=ntohl(rpc->data[0]);
			m_freem(m);
			if (slash) *slash='/';
			return SALNET_NFSERROR;
			}
		m_copydata(m,sizeof(struct rpc_reply), 32, file_fh);
		m_copydata(m,sizeof(struct rpc_reply)+32, sizeof(fattr),
			(void*)&fattr);
		m_freem(m);

		if ( ntohl(fattr.fa_type) == NFLNK) {
			char linkpath[NFS_MAXPATHLEN];
			nfs_readlink(servip, port, file_fh, linkpath);
			err=nfs_lookuppath(servip,port,dirfh,linkpath,file_fh);
			if (err) {
				if (slash) *slash='/';
				return err;
				}
			}
		if (slash) {
			bcopy(file_fh, dirfh, 32);
			*slash='/';
			++slash;
			}
		path=slash;
	} while (path && *path);

	strcpy(cache_path,reqpath);
	bcopy(dirfh, cache_fh, 32);

	return SALNET_SUCCESS; /* final lookup is in file_fh */
}

#define NFSBLOCK 1024*8

/***************************************************************************

NFS_READ:  Read File

***************************************************************************/
salnet_err
nfs_readfile(servip, port, fh, maxsize, buffer, bytes)
	int servip;
	int port;
	char *fh;
	int maxsize;
	char *buffer;
	int *bytes;
{
	salnet_route nfsd;
	struct mbuf *m;
	struct rpc_reply *rpc;
	char rpcether[6];
	int rlen, retry=3;
	int offset = 0;
	char request[256];
	int reqbytes, err;

	if (!bytes) return SALNET_BADARGS;
	*bytes=0;
	if (!fh || !buffer) return SALNET_BADARGS;

	err = salnet_arp(servip,rpcether);
	if (err) return err;

	do {
		salnet_getlocalroute(&nfsd);
		nfsd.remotudpport=port;
		nfsd.remotip=servip;
		nfsd.remotether=rpcether;
		nfsd.localudpport=700+(portseed++&0xff);
		reqbytes = xdrprintf(request,
			"%L%L%L%L%L%L%L%L%L%S%L%L%L%L%L%L%L%M%L%L%L",
			++rpc_id, MSG_CALL, 2, RPCPROG_NFS, 2, NFSPROC_READ,
			1, strlen(hostname) + 28,0,hostname,0,0,2,0,0,0,0,
			32, fh, offset, NFSBLOCK, 0);
		err = salnet_udploop(&nfsd, request, reqbytes,
				3, 6000, sizeof(struct rpc_reply), &m);
		if (err) return err;

		rpc = mtod(m, struct rpc_reply *);
		if (rpc->id!=ntohl(rpc_id)) {
			err = SALNET_NOTMINE;
			m_freem(m);
			if (--retry<=0) {
				salnet_perror("nfs read",err);
				break;
				}
			continue;
			}
		retry=3;

		if(rpc->rstatus||rpc->verifier||rpc->astatus||rpc->data[0]){
			*(int*)buffer=ntohl(rpc->data[0]);
			m_freem(m);
			return SALNET_NFSERROR;
			}

		m_copydata(m,sizeof(struct rpc_reply)+17*4, sizeof(int), 
			(void*) &rlen);
		rlen = ntohl(rlen);
		if (offset+rlen>maxsize) rlen=maxsize-offset;
		m_copydata(m,sizeof(struct rpc_reply)+18*4,rlen,buffer+offset);
		m_freem(m);
		offset+=rlen;
		/*
		printf("nfs: %d@offset\n",rlen,offset);
		*/
	} while(rlen==NFSBLOCK && offset<maxsize);

	*bytes = offset;
	return SALNET_SUCCESS;
}

/***************************************************************************

RPC_ERR - Print RPC Errors

***************************************************************************/
rpc_err(rpc)
	struct rpc_reply	*rpc;
{
	int err = ntohl(rpc->data[0]);
	printf("***RPC Error: (%d,%d,%d):\r\n ",
		ntohl(rpc->rstatus),
		ntohl(rpc->verifier),
		ntohl(rpc->astatus));
}

salnfs_err(err)
	int err;
{
	if (err == NFSERR_PERM)		printf("Not owner");
	else if (err == NFSERR_NOENT) 	printf("No such file or directory");
	else if (err == NFSERR_ACCES)	printf("Permission denied");
	else if (err == ESTALE)		printf("Stale NFS file handle");
	else printf("Error %d",err);
	printf("\r\n");
}



salnet_err
salnfs_mount(ipaddr_t servip, char *dir,
		/*OUT*/ nfshandle_t mount)
{
	int err;
	int mount_port;

	rpc_id = cyclecounter();
	err = rpclookup(servip, RPCPROG_MNT, 1, &mount_port);
	if (err) return err;

	err = nfs_mount(servip, mount_port, dir, mount);
	return err;
}

salnet_err
salnfs_fetch(ipaddr_t servip, nfshandle_t mount, char *path, long maxbytes,
		/*OUT*/ void *buf, long *bytes)
{
	int rlen, err;
	char filehandle[32];
	int nfs_port;

	if (!bytes) return SALNET_BADARGS;
	*bytes = 0;
	if (!buf || !path) return SALNET_BADARGS;

#if USERPC
	err = rpclookup(servip, RPCPROG_NFS, 2, &nfs_port);
	if (err) return err;
#else
	nfs_port = NFS_PORT;
#endif

	err = nfs_lookuppath(servip, nfs_port, mount, path, filehandle);
	if (err) {
		*(int*)buf = *(int*)filehandle;
		return err;
		}

	err=nfs_readfile(servip, nfs_port, filehandle, maxbytes, buf, bytes);

	return err;
}
