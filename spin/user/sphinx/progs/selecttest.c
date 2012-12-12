/*
 * TCP echo server.
 * Used to measure round trip latency.
 *
 * Chris Maeda (cmaeda@cs.cmu.edu)
 * This code is in the public domain.
 */

#include <stdio.h>

#include <sys/types.h>
#include <sys/param.h>
#include <sys/socket.h>
#include <netinet/in.h>

char *buffer = 0;
int bufsiz = 0;

extern int errno;

#define USAGE "usage: %s [port]\n"

static void 
do_select(int fd)
{
    fd_set readfds;
    fd_set writefds;
    fd_set exceptfds;
    char ch;

    printf("start on %d\n", fd);
    for (;;) {
	FD_ZERO(&readfds);
	FD_ZERO(&writefds);
	FD_ZERO(&exceptfds);

	FD_SET(fd, &readfds);
	FD_SET(fd, &exceptfds);
	if (select(10, &readfds, &writefds, &exceptfds, NULL) < 0) {
	    perror("select");
	    exit(1);
	}
	printf("@");
	if (read(fd, &ch, 1) <= 0) {
	    close(fd);
	    return;
	}

	printf("%c\n", ch);
    }
}

main(int argc, char **argv)
{
    int sock, new_sock, sinlen;
    struct sockaddr_in sin;
    long dlen, resid, cc;
    char *bp;
    unsigned short port;

    /*
     * set up listener socket
     */
    sock = socket(AF_INET, SOCK_STREAM, 0);
    if (sock < 0) {
	perror("socket");
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
    
    while(1) {
      nextconn:
	sinlen = sizeof(sin);
	new_sock = accept(sock, (struct sockaddr *)&sin, &sinlen);
	if (new_sock < 0) {
	    perror("accept");
	    exit(1);
	}
	do_select(new_sock);
	close(new_sock);
    }
}




	
