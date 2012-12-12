
#include <spincore/src/sal/OpenArray.h>
#include <sal/salnet.h>

void m3dnsquery(struct openarray *hostname, /*OUT*/ ipaddr_t *ip)
{
#ifdef __FreeBSD__
	saldns_query(hostname->start, ip);
#endif
}

int
m3tftpfetch(ipaddr_t server, struct openarray *path,
		/*OUT*/ struct openarray *buf, long *bytes)
{
	return saltftp_fetch(server,path->start, buf->size, buf->start, bytes);
}


int
m3nfsmount(ipaddr_t server, struct openarray *dir,
		/*OUT*/ nfshandle_t mount)
{
	return salnfs_mount(server, dir->start, mount);
}

int
m3nfsfetch(ipaddr_t server, nfshandle_t mount, struct openarray *path,
		/*OUT*/ struct openarray *buf, long *bytes)
{
	return salnfs_fetch(server, mount, path->start, buf->size, buf->start, bytes);
}
