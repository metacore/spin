#include <stdio.h>
#include <sys/param.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/select.h>
#include <sys/time.h>
#include <errno.h>
#include <sys/fcntl.h>
#include <sys/ioctl.h>
#include <net/if.h>

char ifreqbuf[2048];

main(argc, argv)
    int  argc;
    char *argv[];
{
    int s, n;
    struct ifreq ifr;
    struct sockaddr_in *sinp;
    struct ifreq *ifrp;
    struct ifconf ifconf;
    
    if ( argc != 2 ) {
	printf("usage: sockioc <interface-name>\n");
	exit(1);
    }
    
    strcpy(ifr.ifr_name,argv[1]);
    s = socket(AF_INET, SOCK_DGRAM, 0);
    if ( s < 0 ) {
	perror("socket");
	exit(1);
    }
    if ( ioctl(s,SIOCGIFFLAGS,&ifr) < 0 ) {
	perror("SIOCGIFFLAGS");
	close(s);
	exit(1);
    }
    printf("flags are %x\n",ifr.ifr_flags);
    if ( ioctl(s,SIOCGIFBRDADDR,&ifr) < 0 ) { 
	perror("SIOCGIFBRDADDR");
	close(s);
	exit(1);
    }
    sinp = &ifr.ifr_broadaddr;
    printf("broad cast addr is %x\n",sinp->sin_addr.s_addr);

    ifconf.ifc_len = sizeof(ifreqbuf);
    ifconf.ifc_buf = ifreqbuf;
    
    if ( ioctl(s,SIOCGIFCONF,&ifconf) < 0 ) {
	perror("SIOCGIFCONF");
	close(s);
	exit(1);
    }
    printf("got %d bytes of interface info...\n",ifconf.ifc_len);
    for ( ifrp = ifconf.ifc_req,
	  n = ifconf.ifc_len / sizeof(struct ifreq);
	  --n >= 0; ifrp++ ) {
	sinp = (struct sockaddr_in *)&ifrp->ifr_addr;
	printf(" name %s, family 0x%x, addr 0x%x\n",ifrp->ifr_name,
		 sinp->sin_family, sinp->sin_addr.s_addr);
    }
    close(s);
    exit(0);
}

