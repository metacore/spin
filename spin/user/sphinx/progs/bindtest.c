/*
 * UDP Echo Server
 * Used to test round trip latency.
 *
 * Chris Maeda (cmaeda@cs.cmu.edu)
 * This code is in the public domain.
 */

#include <stdio.h>

#include <sys/param.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>

extern int errno;

#define USAGE "usage: %s port\n"

char *buffer = 0;
int bufsiz = 0;

main(int argc, char **argv)
{
	int s, res, len, cc, trace, pcnt;
	struct sockaddr_in sin, insin;
	unsigned short port;
	char mefbuf[1000];

	/*
	 * Argument processing.
	 */
	if (argc < 2) {
		fprintf(stderr, USAGE, argv[0]);
		exit(1);
	}
	if (argc > 1)
		port = atoi(argv[1]);
	else
		port = 0;


	/*
	 * Set up server socket.
	 */
	s = socket(AF_INET, SOCK_DGRAM, 0);
	if (s < 0) {
		perror("socket");
		exit(1);
	}
	printf("socket: %d\n", s);

	sin.sin_family = AF_INET;
	sin.sin_addr.s_addr = INADDR_ANY;
	sin.sin_port = ntohs(0);

	res = bind(s, (struct sockaddr *)&sin, sizeof(sin));
	if (res < 0) {
		perror("bind");
		exit(1);
	}
	
	sin.sin_addr.s_addr = inet_addr("128.95.2.20");
	sin.sin_port = ntohs(port);
	res = sendto(s, &mefbuf, 64, 0,
		     (struct sockaddr *)&sin, sizeof(sin));
	printf("res: %d, errno: %d\n", res, errno);
	if (res < 0) {
		perror("sendto");
		exit(1);
	}
}


