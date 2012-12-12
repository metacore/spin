
#include <stdio.h>

#include <sys/param.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>

#include <ctype.h>
#include <strings.h>
#include <netdb.h>

#ifdef SPIN
#define random rand
#define srandom srand
#endif
extern int errno;

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


/*
 * Parse args.  Set global variables.
 */
#define USAGE \
"usage: %s [-t|u] host port\n"
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
	if ( (nargv[0][0] >= '0') && (nargv[0][0] <= '9') ) {
		sin_server.sin_addr.s_addr = inet_addr(nargv[0]);
	}
	else {
#ifdef SPIN
	     printf("bucket address must be a.b.c.d\n");
	     exit(0);
#else
		struct hostent *host;

		host = gethostbyname(nargv[0]);
		if (host == (struct hostent *) NULL) {
			fprintf(stderr, "error: gethostbyname %d\n", errno);
			exit(1);
		}

		bcopy(host->h_addr, &sin_server.sin_addr,
		      sizeof(sin_server.sin_addr));
#endif
	}
	nargv++; nargc--;

	/*
	 * Destination server port.
	 */
	port = atoi(nargv[0]);
	nargv++; nargc--;

	/*
	 * Fill in rest of server's address.
	 */
	sin_server.sin_port = htons(port);
	sin_server.sin_family = AF_INET;

	log = stdout;

	/*
	 * Allocate send and recv buffers.
	 */
	send_buffer = (char *) malloc(5120);
	if (send_buffer == (char *) NULL) {
		perror("malloc: send_buffer");
		exit(1);
	}
	recv_buffer = (char *) malloc(10240);
	if (recv_buffer == (char *) NULL) {
		perror("malloc: recv_buffer");
		exit(1);
	}
}


main (int argc, char **argv)
{
	int sock, i, nsend, ngot, nbunch;
	int total_sent;
	
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
	sin_client.sin_port = htons(0);
	if (bind (sock, (struct sockaddr *)&sin_client,
		                   sizeof (sin_client))) {
		perror("bind");
		exit(1);
	}
	ngot = 1;
	nsend = setsockopt(sock, SOL_SOCKET, SO_REUSEADDR,
			   (char *)&ngot, sizeof(int));
	if ( nsend < 0 ) {
	     perror("setsockopt SO_REUSEADDR");
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

	srandom(1);
	printf("starting send..\n");
	total_sent = nbunch = 0;
	
        while ( 1 ) {
	    nsend = random() % 3072;
	    if ( nsend < 512 ) nsend += 512;
	    
	    i = write(sock,send_buffer,nsend);
	    if ( i < nsend ) {
		printf("sent %d/%d, errno %d\n",i,nsend,errno);
	        if ( i < 0 ) {
		    close(sock);
		    exit(1);
		}
	    }
	    total_sent += i;
	    nbunch += i;
	    if ( nbunch > 1000000 ) {
		printf("total %d\n",total_sent);
		nbunch = 0;
	    }
        }
}

