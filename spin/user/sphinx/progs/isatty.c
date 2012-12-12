#include <stdio.h>

#include <sys/param.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/select.h>
#include <sys/time.h>
#include <errno.h>
#include <sys/fcntl.h>

/*
 * Just to see what this will do.
 */
main(argc, argv)
    int  argc;
    char *argv[];
{
    int fd, i;
    
    printf("Checking stdin..\n");
    i = isatty(0);
    printf("..is %s a tty\n",i ? "" : "not");
    printf("Trying a file...\n");
    fd = open("/spin/oystr/camflags.c",0,0);
    if ( fd < 0 ) {
	perror("open file");
    }
    else {
	i = isatty(fd);
	printf("..is %s a tty\n",i ? "" : "not");
	close(fd);
    }
    printf("Trying a socket...\n");
    fd = socket(AF_INET, SOCK_DGRAM, 0);
    if ( fd < 0 ) {
	perror("open socket");
    }
    else {
	i = isatty(fd);
	printf("..is %s a tty\n",i ? "" : "not");
	close(fd);
    }
}
