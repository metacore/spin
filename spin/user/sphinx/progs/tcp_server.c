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

char *buffer = 0;
int bufsiz = 0;

extern int errno;

#define USAGE "usage: %s bufsiz [port]\n"

main(int argc, char **argv)
{
	int sock, new_sock, sinlen;
	struct sockaddr_in sin;
	long dlen, resid, cc;
	char *bp;
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

		/*
		 * How many bytes do we read before echoing?
		 *
		 * Client will send a 32 bit fixnum.
		 */
		{
			char numbuf[sizeof(int)];
			int *num;

			resid = sizeof(int);
			bp = numbuf;
			while (resid) {
				cc = read(new_sock,bp,resid);
				if (cc < 0) {
					perror("read");
					close(new_sock);
					goto nextconn;
				}
				if (cc == 0) {
					close(new_sock);
					goto nextconn;
				}
				resid -= cc;
				bp += cc;
			}
			num = (int *)numbuf;
			dlen = ntohl(*num);
			if (dlen > bufsiz) {
				close(new_sock);
				goto nextconn;
			}
		}

		printf("connection from %s port %d with %d bytes msgs\n",
		       inet_ntoa(sin.sin_addr), ntohs(sin.sin_port), dlen);

		while(1) {
			int len, i, cc;

			resid = dlen;
			bp = buffer;
			while (resid) {
				cc = read(new_sock, bp, resid);
				printf("Errno: %d\n", errno);
				if (cc < 0) {
					perror("read");
					exit(1);
				}
				if (cc == 0) {
					close(new_sock);
					goto nextconn;
				}
				resid -= cc;
				bp += cc;

			}

			/*
			 * Echo to sender.
			 */
			resid = dlen;
			bp = buffer;
			while (resid) {
				cc = write(new_sock, bp, resid);
				if (cc < 0) {
					perror("write");
					exit(1);
				}
				resid -= cc;
				bp += cc;
			}
		}
	}
}




	
