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

#define USAGE "usage: %s bufsize [port [printfreq]]\n"

char *buffer = 0;
int bufsiz = 0;

main(int argc, char **argv)
{
	int s, res, len, cc, trace, pcnt;
	struct sockaddr_in sin, insin;
	unsigned short port;

	/*
	 * Argument processing.
	 */
	if (argc < 2) {
		fprintf(stderr, USAGE, argv[0]);
		exit(1);
	}
	bufsiz = atoi(argv[1]);
	buffer = (char *) malloc(bufsiz);
	if (buffer == (char *) NULL) {
		perror("malloc");
		exit(1);
	}
	if (argc > 2)
		port = atoi(argv[2]);
	else
		port = 0;
	if (argc > 3)
		trace = atoi(argv[3]);
	else
		trace = 0;


	/*
	 * Set up server socket.
	 */
	s = socket(AF_INET, SOCK_DGRAM, 0);
	if (s < 0) {
		perror("socket");
		exit(1);
	}

	sin.sin_family = AF_INET;
	sin.sin_addr.s_addr = INADDR_ANY;
	sin.sin_port = ntohs(port);

	res = bind(s, (struct sockaddr *)&sin, sizeof(sin));
	if (res < 0) {
		perror("bind");
		exit(1);
	}

	len = sizeof(sin);
	res = getsockname(s, (struct sockaddr *)&sin, &len);
	if (res < 0) {
		perror("getsockname");
		exit(1);
	}

	printf("server host %s, listening on udp port %d\n",
	       inet_ntoa(sin.sin_addr.s_addr), ntohs(sin.sin_port));

	if (trace)
		pcnt = trace;

	while (1) {
		len = sizeof(insin);
		cc = recvfrom(s, buffer, bufsiz, 0,
			      (struct sockaddr *)&insin, &len);
		if (cc < 0) {
			perror("recvfrom");
			exit(1);
		}

		res = sendto(s, buffer, cc, 0,
			     (struct sockaddr *)&insin, len);
		if (res < 0) {
			perror("sendto");
			exit(1);
		}
		
		/*
		 * Print a notification off client's critical path.
		 */
		if (trace) {
			pcnt--;
			if (pcnt == 0) {
				fputc('.', stderr);
				fflush(stderr);
				pcnt = trace;
			}
		}
	}
	/*NOTREACHED*/
}


