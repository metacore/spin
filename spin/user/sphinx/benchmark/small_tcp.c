
#include <stdio.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <fcntl.h>

#include <ctype.h>
#include <strings.h>
#include <netdb.h>
#include <spy.h>

void usage ()
{
    fprintf(stderr, "small_tcp [-s] [-n REPETITION] host port\n");
    exit(1);
}
main (int argc, char **argv)
{
    int sock;
    int i;
    spy_t spy;
    int repetition = 100;
    int resp_size = 4971;
    int ch;
    int become_server = 0;

    while ((ch = getopt(argc, argv, "sn:b:")) != EOF) {
	switch (ch) {
	  case 's':
	    become_server = 1;
	    break;
	  case 'b':
	    resp_size = atoi(optarg);
	    break;
	  case 'n':
	    repetition = atoi(optarg);
	    break;
	  default:
	    usage();
	}
    }

    if (become_server) {
	int sockbufsize = 4096;
	struct sockaddr_in sin_client;
	char buf[1024];
	char *send_buf = malloc(resp_size);
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
	    if (setsockopt(sock, SOL_SOCKET, SO_SNDBUF, (void *) &sockbufsize, 
			  sizeof(sockbufsize)) < 0) {
		perror("setsockopt");
	    }
	    if (fcntl(sock, F_SETFL, O_NDELAY) < 0) {
		perror("fcntl");
	    }
	    n = read(sock, buf, sizeof buf);
	    if (n > 0) {
		if (write(sock, send_buf, 4096) < 4096) {
		    perror("write");
		    exit(1);
		}
		if (write(sock, send_buf, 4971-4096) < 4971-4096) {
		    perror("write2");
		    exit(1);
		}
	    }
	    close(sock);
	}
    } else {
	char x = ' ';
	char *host = argv[optind];
	char *port = argv[optind+1];
	if (host == 0 || port == 0) 
	  usage();
	client_init(host, port);
	printf("connected to %s:%s\n", host, port);
	spy = spy_create("client", repetition);
	
	for (i = 0; i < repetition; i++) {
	    int j, n;
	    char send_msg[80];
	    char *recv_msg = malloc(resp_size);
	    for (j = 0; j < 78; j++) {
		send_msg[j] = x++;
		if (x >= 0x7f) x = ' ';
	    }
	    send_msg[j++] = '\n';
	    send_msg[j++] = '\0';
	    
	    spy_start(spy);
	    sock = client_connect();
	    n = write(sock, send_msg, j);
	    if (n < j) {
		perror("write");
		exit(1);
	    }
	    
	    {
		int k = resp_size;
		while (k > 0) {
		    n = read(sock, recv_msg + resp_size-k, k);
		    if (n == 0) break;
		    if (n < 0) {
			perror("read");
			exit(1);
		    }
		    k -= n;
		}
	    }
	    close(sock);
	    spy_stop(spy);
/*	    if (strcmp(send_msg, recv_msg)) {
		printf("msg mismatch : \n%s\n%s\n", send_msg, recv_msg);
		exit(1);
	    }*/
	}
    }
    spy_set_display_mode(SPY_MICROSEC_ONLY);
    spy_dump_all();
}

