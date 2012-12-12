/*
 * UDP/TCP Echo Client
 * Measures round trip latency.
 *
 * Written by Chris Maeda (cmaeda@cs.cmu.edu)
 * This code is in the public domain.
 */

#include <stdio.h>

#include <sys/param.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>

#include <ctype.h>
#include <strings.h>
#include <netdb.h>

struct trial {
	unsigned long elapsed_time;
	unsigned long cache_overhead;
};

/* free running system counter */
#define TIMERLO(the_timer) (int)((the_timer)  & ((1 << 32) - 1))

/* free running counter for user thread */
#define TIMERHI(the_timer) (int) ((the_timer) >> 32)

#define TIMERDIFF(finish,start) (unsigned long)(TIMERLO(finish)-TIMERLO(start))

#define VTIME(the_timer) (int)(TIMERLO(the_timer) + TIMERHI(the_timer))
#define VTIMERDIFF(finish,start) (unsigned long)(VTIME(finish)-VTIME(start))
extern unsigned long rpcc();
unsigned long rpcc() {return 0;}
long delta(long start,long stop)
{
	stop &= 0xffffffff;
	start &= 0xffffffff;

	if (stop<start) /* correct for wraparound */
		return 0x100000000-start+stop;

	return stop-start;

}	

#ifdef CACHE_FLUSH
extern void cache_flush(caddr_t from, caddr_t to);
#else
#define cache_flush(start,end)
#endif

int first_global;

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

int sendonly = 0;		/* just send? */
int interactive = 0;		/* ask for input? */
int flush_caches = 1;		/* flush caches during each loop? */
int protocol = 1;		/* 1 == udp, 0 == tcp */

int last_global;


/*
 * Parse args.  Set global variables.
 */
#define USAGE \
"usage: %s [-sendonly] [-noflush] [-t|u] host port pcount dcount [rcount] [logfile]\n"
#define ARGS_REQUIRED 4

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
		if (strcmp(nargv[0], "-sendonly") == 0) {
			sendonly = 1;
			nargv++;
			nargc--;
		}
		else if (strcmp(nargv[0], "-noflush") == 0) {
			flush_caches = 0;
			nargv++;
			nargc--;
		}
		else if (strcmp(nargv[0], "-u") == 0) {
			protocol = 1;
			nargv++;
			nargc--;
		}
		else if (strcmp(nargv[0], "-i") == 0) {
			interactive = 1;
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

	/*
	 * Fill in rest of server's address.
	 */
	sin_server.sin_port = htons(port);
	sin_server.sin_family = AF_INET;

	pcount = atoi(nargv[0]);
	nargv++; nargc--;

	dcount = atoi(nargv[0]);
	nargv++; nargc--;

	/*
	 * Process optional args.
	 */
	if (nargc) {
		rcount = atoi(nargv[0]);
		nargv++; nargc--;
	}
	else
		rcount = 1;

	if (nargc) {
		/*
		 * Use specified logfile.
		 */
		log = fopen(nargv[0], "a");
		if (log == (FILE *) NULL) {
			perror("fopen");
			exit(1);
		}		
	}
	else
		log = stdout;

	/*
	 * Allocate send and recv buffers.
	 */
	send_buffer = (char *) malloc(dcount);
	if (send_buffer == (char *) NULL) {
		perror("malloc: send_buffer");
		exit(1);
	}
	recv_buffer = (char *) malloc(dcount);
	if (recv_buffer == (char *) NULL) {
		perror("malloc: recv_buffer");
		exit(1);
	}
}

/*
 * Run a single udp trial.  
 */
void
udptrial(
	 int			socket,
	 char			*sndbuf,
	 char			*rcvbuf,
	 int			cacheflush,
	 struct trial		*datapt
	 )
{
	long start,stop;

	struct timeval now, then;
	long timediff;
	int i, cc;
	struct sockaddr_in sin;

	/*
	 * Compute cache flush overhead.
	 */
	if (cacheflush) {
		gettimeofday(&then, 0);

		for (i = 0; i < pcount; i++) {
			cache_flush((caddr_t)&first_global,
				    (caddr_t)&last_global);
			cache_flush((caddr_t)udptrial,
				    (caddr_t)tcptrial);
		}

		gettimeofday(&now, 0);
		{
			long v1, v2;

			v1 = (now.tv_sec * 1000) + (now.tv_usec / 1000);
			v2 = (then.tv_sec * 1000) + (then.tv_usec / 1000);
			
			timediff = v1 - v2;
		}
	}
	else
		timediff = 0;

	datapt->cache_overhead = timediff;

	start = rpcc();
	/* gettimeofday(&then, 0); */

	for (i = 0; i < pcount; i++) {

		if (cacheflush) {
			cache_flush((caddr_t)&first_global,
				    (caddr_t)&last_global);
			cache_flush((caddr_t)udptrial,
				    (caddr_t)tcptrial);
		}
	
		/*
		 * Send the packet to server.
		 */
		cc = sendto (socket, sndbuf, dcount, 0,
			     (struct sockaddr *)&sin_server,
			     sizeof(sin_server));
		if (cc < 0) {
			perror("sendto");
			exit(1);
		}

		if (sendonly) continue;

		/*
		 * Receive reply.
		 */
		cc = recv (socket, rcvbuf, dcount, 0);
		if (cc < 0) {
			perror("recv");
			exit(1);
		}
	}

	stop = rpcc();
	/* gettimeofday(&now, 0); */

#if 0
	{
		long v1, v2;
		
		v1 = (now.tv_sec * 1000) + (now.tv_usec / 1000);
		v2 = (then.tv_sec * 1000) + (then.tv_usec / 1000);
		
		datapt->elapsed_time = v1 - v2 - timediff;
	}
#endif
	datapt->elapsed_time = delta(start,stop);
}

/*
 * Like udptrial except we deal with the streamedness of tcp.
 */
void
tcptrial(
	 int			socket,
	 char 			*sndbuf,
	 char			*rcvbuf,
	 int			cacheflush,
	 struct trial 		*datapt
	 )
{
	struct timeval now, then;
	long timediff;
	int i, cc;
	struct sockaddr_in sin;
	int resid;
	char *bp;
	long start, stop;

	/*
	 * Compute cache flush overhead, then do the trial.
	 */
	if (cacheflush) {
		gettimeofday(&then, 0);

		for (i = 0; i < pcount; i++) {
			cache_flush((caddr_t)&first_global,
				    (caddr_t)&last_global)
			cache_flush((caddr_t)tcptrial,
				    (caddr_t)main);
		}

		gettimeofday(&now, 0);
		{
			long v1, v2;
			
			v1 = (now.tv_sec * 1000) + (now.tv_usec / 1000);
			v2 = (then.tv_sec * 1000) + (then.tv_usec / 1000);
			
			timediff = v1 - v2;
		}
	}
	else
		timediff = 0;

	datapt->cache_overhead = timediff;

	gettimeofday(&then, 0);
	
	start = rpcc();

	for (i = 0; i < pcount; i++) {

		if (cacheflush) {
			cache_flush((caddr_t)&first_global,
				    (caddr_t)&last_global)
			cache_flush((caddr_t)tcptrial,
				    (caddr_t)main);
		}

		/*
		 * Send the packet to server.
		 */
		bp = sndbuf;
		resid = dcount;
		while (resid) {
			cc = write (socket, bp, resid);
			if (cc < 0) {
				perror("write");
				exit(1);
			}
			resid -= cc;
			bp += cc;
		}

		if (interactive) {
		  char buf[4];
		  printf(">> sent %d bytes, hit return to continue\n");
		  fgets(buf, sizeof(buf), stdin);
		}

		if (sendonly) continue;

		/*
		 * Receive reply.
		 */
		bp = rcvbuf;
		resid = dcount;
		while (resid) {
			cc = read (socket, bp, resid);
			if (cc < 0) {
				perror("read");
				exit(1);
			}
			resid -= cc;
			bp += cc;
		}

		if (interactive) {
		  char buf[4];
		  printf(">> received %d bytes, hit return to continue\n");
		  fgets(buf, sizeof(buf), stdin);
		}
	}
	stop = rpcc();

	gettimeofday(&now, 0);

	{
		long v1, v2;
	
		v1 = (now.tv_sec * 1000) + (now.tv_usec / 1000);
		v2 = (then.tv_sec * 1000) + (then.tv_usec / 1000);
		
		datapt->elapsed_time = v1 - v2 - timediff;
	}
	datapt->elapsed_time = delta(start,stop);
}

long tickstousecs(long cycles)
{
	return cycles/133;
}

main (int argc, char **argv)
{
	int sock, i;
	struct trial datapt;

	init(argc, argv);		/* parse args */

	fprintf(log, "# server: %s port: %d - %d %d byte %s packets\n",
		inet_ntoa(sin_server.sin_addr),
		ntohs(sin_server.sin_port),
		pcount, dcount,
		protocol ? "udp" : "tcp");
	fprintf(log, "# trial\tpkts\tdata\ttime(us)\tcache(ms)\n");
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
	sin_client.sin_port = 0;
	if (bind (sock, (struct sockaddr *)&sin_client, sizeof (sin_client))) {
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

		/*
		 * Tell the server what we want by
		 * writing a number in netbyte order.
		 */

		num = (int *)numbuf;
		*num = htonl(dcount);
		resid = sizeof(int);
		bp = numbuf;
		while (resid) {
			cc = send(sock,bp,resid,0);
			if (cc < 0) {
				perror("send");
				exit(1);
			}
			resid -= cc;
			bp += cc;
		}
	}

	for (i = 0; i < rcount; i++) {
		if (protocol)
			udptrial(sock, send_buffer, recv_buffer, 
				 flush_caches, &datapt);
		else
			tcptrial(sock, send_buffer, recv_buffer,
				 flush_caches, &datapt);

		fprintf(log, "%d\t%d\t%d\t%u\t%u\n",
			i, pcount, dcount, 
			tickstousecs(datapt.elapsed_time)/pcount,
			datapt.cache_overhead);
		fflush(log);
	}
	exit(0);
}

