
#include <stdio.h>

#include <sys/param.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>

#include <ctype.h>
#include <strings.h>
#include <netdb.h>


/*
 * Buffer used to send and recv data.
 */
char *send_buffer = 0;
char *recv_buffer = 0;

/*
 * Socket address of client and server.
 */
struct sockaddr_in sin_client, sin_server;

/*
 * Put results here.
 */
FILE *log = NULL;

int pcount;			/* number of messages per trial */
int dcount;			/* number of bytes per message */
int rcount;			/* number of trials */

int protocol = 0;		/* 1 == udp, 0 == tcp */

int last_global;
int my_port = 0;


/*
 * Parse args.  Set global variables.
 */
#define USAGE \
"usage: %s [-t|u] host port <local-port>\n"
#define ARGS_REQUIRED 2

void
init(
     int argc,
     char **argv
     )
{
	unsigned short port;
	char **nargv;
	int nargc;

	/*
	 * Get a pointer to the arguments.
	 */
	nargv = argv + 1;
	nargc = argc - 1;

	/*
	 * Look for dash ('-') args.  These must come first.
	 */
	while (nargc > 0 && nargv[0][0] == '-') {
		if (strcmp(nargv[0], "-u") == 0) {
			protocol = 1;
			nargv++;
			nargc--;
		}
		else if (strcmp(nargv[0], "-t") == 0) {
			protocol = 0;
			nargv++;
			nargc--;
		}
		else {
			fprintf(stderr, USAGE, argv[0]);
			exit(1);
		}
	}

	/*
	 * Make sure we have all required args.
	 */
	if (nargc < ARGS_REQUIRED) {
		fprintf(stderr, USAGE, argv[0]);
		exit(1);
	}
	
	/*
	 * Figure out who the server is.
	 * Consume args 1 and 2.  Fill in sin_server.
	 */
	if (isdigit(nargv[0][0])) {
		sin_server.sin_addr.s_addr = inet_addr(nargv[0]);
	}
	else {
		struct hostent *host;

		host = gethostbyname(nargv[0]);
		if (host == (struct hostent *) NULL) {
			extern int errno;

			fprintf(stderr, "error: gethostbyname %d\n", errno);
			exit(1);
		}

		bcopy(host->h_addr, &sin_server.sin_addr,
		      sizeof(sin_server.sin_addr));
	}
	nargv++; nargc--;

	/*
	 * Destination server port.
	 */
	port = atoi(nargv[0]);
	nargv++; nargc--;

	if ( nargc > 0 ) {
	    my_port = atoi(nargv[0]);
	    printf("using local port %s\n",nargv[0]);
	}
	nargv++;
	nargc++;
	
	/*
	 * Fill in rest of server's address.
	 */
	sin_server.sin_port = htons(port);
	sin_server.sin_family = AF_INET;

	log = stdout;

	/*
	 * Allocate send and recv buffers.
	 */
	send_buffer = (char *) malloc(128);
	if (send_buffer == (char *) NULL) {
		perror("malloc: send_buffer");
		exit(1);
	}
	recv_buffer = (char *) malloc(128);
	if (recv_buffer == (char *) NULL) {
		perror("malloc: recv_buffer");
		exit(1);
	}
}


main (int argc, char **argv)
{
	int sock, i;

	init(argc, argv);		/* parse args */

	fprintf(log, "# server: %s port: %d %s packets\n",
		inet_ntoa(sin_server.sin_addr),
		ntohs(sin_server.sin_port),
		protocol ? "udp" : "tcp");
	fflush(log);
	
	/*
	 * Set up local socket.
	 */
	sock = socket (AF_INET, protocol ? SOCK_DGRAM : SOCK_STREAM, 0);
	if (sock < 0) {
		perror("socket");
		exit(1);
	}
	sin_client.sin_family = AF_INET;
	sin_client.sin_addr.s_addr = INADDR_ANY;
	sin_client.sin_port = htons((short)my_port);
	if (bind (sock, (struct sockaddr *)&sin_client,
		                   sizeof (sin_client))) {
		perror("bind");
		exit(1);
	}

	/*
	 * If we're doing tcp, we have to tell the server
	 * how many bytes are in each message.
	 */
	if (protocol == 0) {
		char numbuf[sizeof(int)], *bp;
		int *num;
		int cc,resid;

		cc = connect(sock,
			     (struct sockaddr *)&sin_server,
			     sizeof(sin_server));
		if (cc < 0) {
			perror("connecting to tcp server");
			exit(1);
		}

	}

        while ( 1 ) {
	    printf("> ");
	    if ( fgets(send_buffer, 128, stdin) == NULL) {
		printf("\nexiting...\n");
		close(sock);
		exit(0);
	    }
	    write(sock,send_buffer,strlen(send_buffer)+1);
        }
}

