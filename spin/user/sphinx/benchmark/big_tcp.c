
#include <stdio.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>

#include <ctype.h>
#include <strings.h>
#include <netdb.h>
#include <spy.h>

void usage ()
{
    fprintf(stderr, "big_tcp [-s] [-n REPETITION] host port\n");
    exit(1);
}

static void read_msg(int sock, int kilos)
{
    char buf[1024];
    int i = 0;
    char x = ' ';
    printf("reading %d\n", kilos);
    for (i = 0; i < kilos; i++) {
	int n;
	int j;
	int k = sizeof buf;
	while (k > 0) {
	    n = read(sock, buf, k);
	    if (n <= 0) {
		perror("read");
		exit(1);
	    }
	    k -= n;
	}
	for (j = 0; j < sizeof buf; j++) {
	    if (buf[j] != x) {
		printf("char mismatch: %d, %d, %s<=>%c\n", 
		       i, j, buf, x);
		exit(1);
	    }
	    x++;
	    if (x >= 0x7f) x = ' ';
	}
	printf("ok\n");
    }
}

static void write_msg (int sock, int kilos)
{
    char buf[10][1024];
    int idx = 0;
    int i = 0;
    char x = ' ';
    for (i = 0; i < kilos; i++) {
	int j, n;
	for (j = 0; j < sizeof buf[idx]; j++) {
	    buf[idx][j] = x++;
	    if (x >= 0x7f) x = ' ';
	}
	n = send(sock, buf[idx], sizeof buf[idx], 0);
	if (n < sizeof buf[idx]) {
	    perror("write");
	    exit(1);
	}
	idx++;
	if (idx >= 10) {
	    idx = 0;
	}
    }
}

main (int argc, char **argv)
{
    int sock;
    int i;
    spy_t spy;
    int kilos = 1000;
    int ch;
    int become_server = 0;
    
    char x = ' ';

    while ((ch = getopt(argc, argv, "sn:")) != EOF) {
	switch (ch) {
	  case 's':
	    become_server = 1;
	    break;
	  case 'n':
	    kilos = atoi(optarg);
	    break;
	  default:
	    usage();
	}
    }

    if (become_server) {
	struct sockaddr_in sin_client;
	char buf[1024];
	int main_sock, sock;
	int n;
	main_sock = server_init();
	for (;;) {
	    int sinlen = sizeof(sin_client);
	    sock = accept(main_sock, (struct sockaddr *)&sin_client, &sinlen);
	    if (sock < 0) {
		perror("accept");
		exit(1);
	    }
	    n = read(sock, &kilos, sizeof(kilos));
	    if (n < sizeof(kilos)) {
		perror("read-1");
		exit(1);
	    }
	    if (kilos <= 0 || kilos >= 1024 * 1024) {
		printf("bogus kilo val(%d).\n", kilos);
		exit(1);
	    }
	    read_msg(sock, kilos);
	    write_msg(sock, kilos);
	    close(sock);
	}
    } else {
	char *host = argv[optind];
	char *port = argv[optind+1];
	if (host == 0 || port == 0) 
	  usage();
	client_init(host, port);
	printf("connected to %s:%s\n", host, port);
	spy = spy_create("client", 1);
	sock = client_connect();
	write(sock, &kilos, sizeof(kilos));
	spy_start(spy);
	write_msg(sock, kilos);
	read_msg(sock, kilos);
	close(sock);
	spy_stop(spy);
    }
    spy_set_display_mode(SPY_MICROSEC_ONLY);
    spy_dump_all();
}

