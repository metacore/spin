/*
 * TCP echo server.
 * Used to measure round trip latency.
 *
 * Chris Maeda (cmaeda@cs.cmu.edu)
 * This code is in the public domain.
 */

#include <stdio.h>

#include <sys/param.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#ifndef mips
#include <sys/select.h>
#endif
#include <sys/time.h>
#include <errno.h>
#ifdef mips
#include <fcntl.h>
#else
#include <sys/fcntl.h>
#endif

char *buffer = 0;
int bufsiz = 0;
fd_set netfds;
fd_set tmpfds;

struct linger ling = { 0, 0 };

struct timeval timo = {
    30, 0
};

extern int errno;

CloseExit( int i )
{
    int j, k;

    while ( i ) {
	j = ffs(i) - 1;
	printf("closing %d\n",j);
	close(j);
	i &= ~(1 << j);
    }
    exit(0);
}

#define USAGE "usage: %s bufsiz [port]\n"
struct sockaddr_in peeraddr;
int peerlen;

main(int argc, char **argv)
{
	int sock, new_sock, sinlen;
	struct sockaddr_in sin;
	long dlen, resid, cc;
	char *bp;
	unsigned short port;
	int nready;
	int total_recvd, nbunch;
	
	/*
	 * Argument processing.
	 */
	if (argc < 2) {
		fprintf(stderr, USAGE, argv[0]);
		exit(1);
	}
	bufsiz = atoi(argv[1]);
        if (bufsiz < 2048) {
	    printf("This is a BIG server, min. bufsiz is 2K\n");
	    exit(0);
	}
	
	buffer = (char *) malloc(bufsiz);
	if (buffer == (char *) NULL) {
		perror("malloc");
		exit(1);
	}
	if (argc > 2)
		port = atoi(argv[2]);
	else
		port = 3000;

	/*
	 * set up listener socket
	 */
	sock = socket(AF_INET, SOCK_STREAM, 0);
	if (sock < 0) {
		perror("socket");
		exit(1);
	}

	sin.sin_family = AF_INET;
	sin.sin_addr.s_addr = INADDR_ANY;
	sin.sin_port = ntohs(port);

	resid = bind(sock, (struct sockaddr *)&sin, sizeof(sin));
	if (resid < 0) {
		perror("bind");
		exit(1);
	}

	if (listen(sock, 5) < 0) {
		perror("listen");
		exit(1);
	}
	
	sinlen = sizeof(sin);
	if (getsockname(sock, (struct sockaddr *)&sin, &sinlen) < 0) {
		perror("getsockname");
		exit(1);
	}

	printf("server host %s, listening on tcp port %d\n",
	       inet_ntoa(sin.sin_addr), ntohs(sin.sin_port));

	/*
	 * Just to test setsockopt path.
	 */
	nready = 1;
	resid = setsockopt(sock, IPPROTO_TCP, TCP_NODELAY,
			   (char *)&nready, sizeof(int));
	if ( resid < 0 ) {
	    perror("setsockopt TCP_NODELAY");
	}
	resid = setsockopt(sock, SOL_SOCKET, SO_REUSEADDR,
			   (char *)&nready, sizeof(int));
	if ( resid < 0 ) {
	     perror("setsockopt SO_REUSEADDR");
	}
	resid = setsockopt(sock, SOL_SOCKET, SO_LINGER,
			   (char *)&ling, sizeof(ling));
	if ( resid < 0 ) {
	    perror("setsockopt SO_LINGER");
	}
	
	FD_ZERO(&netfds);
	FD_SET(sock,&netfds);
	
	while(1) {
	    nextconn:
		bcopy(&netfds,&tmpfds,8);
		nready = select(32, &tmpfds, (char *)0, (char *)0, &timo);
		if ( nready == 0 ) {
		    printf("select timed out\n");
		    continue;
		}
		if ( FD_ISSET(sock, &tmpfds) ) {
		    /* New connection */
		    sinlen = sizeof(sin);
		    new_sock = accept(sock, (struct sockaddr *)&sin, &sinlen);
		    if (new_sock < 0) {
			perror("accept");
			CloseExit(netfds.fds_bits[0]);
		    }
		    printf("New connection received, fd %d\n",new_sock);
		    peerlen = sizeof(struct sockaddr_in);
		    resid = getpeername(new_sock, &peeraddr, &peerlen);
		    if ( resid < 0 ) {
			perror("getpeername");
		    }
		    else {
			printf("from ip addr %lx, port %d\n",
				peeraddr.sin_addr.s_addr,
				peeraddr.sin_port);
		    }
		    FD_CLR(sock, &tmpfds);
/****
		    if ( fcntl(new_sock, F_SETFL, O_NONBLOCK) < 0 ) {
			printf("Can't set nonblocking on new sock: %d\n",
				errno);
			close(new_sock);
		    }
		    else
 ****/
		        FD_SET(new_sock,&netfds);
		}
		while( (new_sock = ffs(tmpfds.fds_bits[0]) ) ) {
		    --new_sock;
		    printf("reading from %d..\n",new_sock);
		    total_recvd = nbunch = 0;
		    while ( 1 ) {
			resid = read(new_sock,buffer,bufsiz);
			if ( resid < 0 ) {
			    if ( errno == EWOULDBLOCK ) {
				continue;
			    }
			    else {
				printf("..read error %d\n",errno);
				FD_CLR(new_sock,&netfds);
			    }
			    break;
			}
			if ( resid == 0 ) {
			    printf("..read 0 bytes, socket closed\n");
			    close(new_sock);
			    FD_CLR(new_sock,&netfds);
			    break;
			}
			if ( (buffer[0] == 'q') && (buffer[1] = 'u') )  {
			    printf("recvd quit\n");
			    close(new_sock);
			    FD_CLR(new_sock,&netfds);
			    exit(0);
			    break;
			}
			total_recvd += resid;
			nbunch += resid;
			if ( nbunch > 1000000 ) {
			    printf("rcvd %d...\n",total_recvd);
			    nbunch = 0;
			}
		    }
		    FD_CLR(new_sock,&tmpfds);
		}
	}
}

SendItBack( int fd, int bufsiz, char *buffer )
{
    int i,j;
    
    j = atoi(buffer);
    if ( j > bufsiz ) {
	printf("Can't send %d bytes of a %d-byte buffer\n",j,bufsiz);
	return;
    }
    printf("Sending %d bytes back...\n",j);
    i = write(fd, buffer, j);
    if ( i < 0 ) {
	perror("buffer write");
	return;
    }
    
    printf("Wrote %d bytes of %d-byte buffer\n",i,j);
}
