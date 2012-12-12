#include <stdio.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <ctype.h>
#include <strings.h>
#include <netdb.h>

struct sockaddr_in sin_server;
client_init(char *host, char *port)
{
    if (isdigit(host[0])) {
	sin_server.sin_addr.s_addr = inet_addr(host);
    } else {
	struct hostent *hent;

	hent = gethostbyname(host);
	if (hent == (struct hostent *) NULL) {
	    perror(host);
	    exit(1);
	}
	bcopy(hent->h_addr, 
	      (char*)&sin_server.sin_addr,
	      sizeof(sin_server.sin_addr));
    }
    
    /*
     * Fill in rest of server's address.
     */
    sin_server.sin_port = htons(atoi(port));
    sin_server.sin_family = AF_INET;
}

int client_connect ()
{
    struct sockaddr_in sin_client;
    int sock;
    int cc;
    sock = socket(AF_INET, SOCK_STREAM, 0);
    if (sock < 0) {
	perror("socket");
	exit(1);
    }
    sin_client.sin_family = AF_INET;
    sin_client.sin_addr.s_addr = INADDR_ANY;
    sin_client.sin_port = 0;
    if (bind (sock, (struct sockaddr *)&sin_client,
	      sizeof (sin_client))) {
	perror("bind");
	exit(1);
    }
    cc = connect(sock,
		 (struct sockaddr *)&sin_server,
		 sizeof(sin_server));
    if (cc < 0) {
	perror("connecting to tcp server");
	exit(1);
    }
    return sock;
}

int server_sock;
int server_init(char *port)
{
    int sinlen;
    int true = 1;
    server_sock = socket(AF_INET, SOCK_STREAM, 0);
    if (server_sock < 0) {
	perror("socket");
	exit(1);
    }
    if((setsockopt(server_sock, SOL_SOCKET, SO_REUSEADDR, (void *)&true, 
		   sizeof(true))) == -1) {
	perror("setsockopt");
    }

    sin_server.sin_family = AF_INET;
    sin_server.sin_port = 0;
    sin_server.sin_addr.s_addr = htonl(INADDR_ANY);
    
    if (bind(server_sock, (struct sockaddr *)&sin_server, sizeof(sin_server)) < 0) {
	perror("bind");
	exit(1);
    }
    
    if (listen(server_sock, 5) < 0) {
	perror("listen");
	exit(1);
    }
    sinlen = sizeof(sin_server);
    if (getsockname(server_sock, (struct sockaddr *)&sin_server,
		    &sinlen) < 0) {
	perror("getsockname");
	exit(1);
    }
    printf("server host %s, listening on tcp port %d\n",
	   inet_ntoa(sin_server.sin_addr), ntohs(sin_server.sin_port));
    return server_sock;
}
