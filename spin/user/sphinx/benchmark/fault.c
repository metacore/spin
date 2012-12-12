#include <stdio.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <signal.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <spy.h>

#define alpha_trunc_page(x)     (((unsigned long)(x)) & ~(ALPHA_PGBYTES-1))
#define ALPHA_PGBYTES   8192    /* bytes per alpha page */

#define NUM_PAGEFAULT_ITERATIONS (10000)
#define NUM_DIRTY_ITERATIONS (10000)
#define NUM_PROT1_ITERATIONS (10000)
#define NUM_PROT100_ITERATIONS (10000)
#define NUM_UNPROT100_ITERATIONS (10000)
#define NUM_APPEL1_ITERATIONS (10000)
#define NUM_APPEL2_ITERATIONS (1000)
#define NUM_TRAP_ITERATIONS (10000)
#define NUM_FAULTFETCH_ITERATIONS (100)
#define SERVER_HOST "128.95.2.241"
#define SERVER_PORT 1234
#define MHZ (133)

spy_t globaltimer;
char *buffer;
char *badaddr;
int 			sockfd;
struct sockaddr_in	serveraddr;
int			serveraddrlen = sizeof(serveraddr);;

fetchpage(char *addr)
{
	int n;
	char buf[1];
	
	buf[0] = 6;
	/* request a page */
	if (sendto(sockfd, buf, 1, 0, 
		   &serveraddr, sizeof(serveraddr)) < 0) {
		perror("sendto");
		exit(1);
	}
	if ((n = recvfrom(sockfd, addr, 64, 0,
			  &serveraddr, &serveraddrlen)) < 0) {
		perror("recvfrom");
		exit(1);
	}
}

int protection_fault_handler()
{
	mprotect(alpha_trunc_page(badaddr),ALPHA_PGBYTES,
		 PROT_READ | PROT_WRITE);
	return 0;
}

int protection_fault_handler2()
{
	spy_stop(globaltimer);
	mprotect(alpha_trunc_page(badaddr),ALPHA_PGBYTES,
		 PROT_READ | PROT_WRITE);
	return 0;
}

int protection_fault_handler3()
{
	mprotect(alpha_trunc_page(badaddr),ALPHA_PGBYTES,
		 PROT_READ | PROT_WRITE);
	badaddr += ALPHA_PGBYTES;
	mprotect(alpha_trunc_page(badaddr),ALPHA_PGBYTES,
		 PROT_READ);
	spy_stop(globaltimer);
	return 0;
}

int faultandfetch_handler()
{
	exit();
	mprotect(alpha_trunc_page(badaddr),ALPHA_PGBYTES,
		 PROT_READ | PROT_WRITE);
	fetchpage(badaddr);
	return 0;
}

main(int argc, char *argv[])
{
	unsigned long i,j;
	int ret;
	long value;

	if (argc != 2) {
		printf("usage: %s [1-8]\n",
		       argv[0]);
		exit();
	}

	if ((sockfd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
		perror("socket_create");
		exit(1);
	}
	bzero((char *) &serveraddr, sizeof(struct sockaddr_in));
	serveraddr.sin_family      = AF_INET;

	serveraddr.sin_addr.s_addr = INADDR_ANY;
	serveraddr.sin_port        = 0;
	
	if ( bind(sockfd, 
		  (struct sockaddr *)&serveraddr, 
		  sizeof(struct sockaddr_in)) < 0) {
		perror("socket_bind");
		exit(2);
	}

	bzero((char *) &serveraddr, sizeof(struct sockaddr_in));
	serveraddr.sin_family      = AF_INET;

	serveraddr.sin_addr.s_addr = inet_addr(SERVER_HOST);
	serveraddr.sin_port        = htons(SERVER_PORT);


	buffer = (char *)malloc(10*ALPHA_PGBYTES);
	buffer = (char *)malloc(105*ALPHA_PGBYTES);

	if (atoi(argv[1]) == 1) {
	  spy_t timer = spy_create("OSF/1 PageFault test",
				   NUM_PAGEFAULT_ITERATIONS);
	  
	  signal(SIGSEGV, (void (*)(int))protection_fault_handler); 
	  
	  for (i=0;i<NUM_PAGEFAULT_ITERATIONS;i++) {
	    if ((ret = mprotect(alpha_trunc_page(buffer),
				ALPHA_PGBYTES,
				PROT_READ)) != 0)
	      {
		perror("mprotect failure");
		exit();
	      } 
	    spy_start(timer);  
	    badaddr = buffer+1;
	    *badaddr = 2;
	    spy_stop(timer); 
	  }
	  spy_dump(timer);
	}
	if (atoi(argv[1]) == 2) {
	  spy_t timer = spy_create("OSF/1 Dirty test",
				   NUM_DIRTY_ITERATIONS);

	  signal(SIGSEGV, (void (*)(int))protection_fault_handler); 
	  
	  for (i=0;i<NUM_DIRTY_ITERATIONS;i++) {
	    if ((ret = mprotect(alpha_trunc_page(buffer),
				ALPHA_PGBYTES,
				PROT_EXEC)) != 0)
	      {
		perror("mprotect failure");
		exit();
	      } 
	    spy_start(timer); 
	    badaddr = buffer+1; 
	    *badaddr = 2;
	    spy_stop(timer); 
	  }
	  spy_dump(timer);
	}
	if (atoi(argv[1]) == 3) {
	  spy_t timer = spy_create("OSF/1 Prot1 test", NUM_PROT1_ITERATIONS);
	  for (i=0;i<NUM_PROT1_ITERATIONS;i++) {
	    if ((ret = mprotect(alpha_trunc_page(buffer),
				100* ALPHA_PGBYTES,
				PROT_READ | PROT_WRITE)) != 0)
	      {
		perror("mprotect failure");
		exit();
	      } 
	    for (j=0;j<100;j++)
	      buffer[j*ALPHA_PGBYTES] = value;
	    spy_start(timer);  
	    if ((ret = mprotect(alpha_trunc_page(buffer),
				ALPHA_PGBYTES,
				PROT_READ)) != 0)
	      {
		perror("mprotect failure");
		exit();
	      } 
	    spy_stop(timer); 
	  }
	  spy_dump(timer);
	}
	if (atoi(argv[1]) == 4) {
	  spy_t timer = spy_create("OSF/1 Prot100 test", NUM_PROT100_ITERATIONS);
		for (i=0;i<NUM_PROT100_ITERATIONS;i++) {
			if ((ret = mprotect(alpha_trunc_page(buffer),
					    100* ALPHA_PGBYTES,
					    PROT_READ | PROT_WRITE)) != 0)
			{
				perror("mprotect failure");
				exit();
			} 
			for (j=0;j<100;j++)
				buffer[j*ALPHA_PGBYTES] = value;
			spy_start(timer);  
			if ((ret = mprotect(alpha_trunc_page(buffer),
					    100* ALPHA_PGBYTES,
					    PROT_READ)) != 0)
			{
				perror("mprotect failure");
				exit();
			} 
			spy_stop(timer); 
		}
		spy_dump(timer);
	}
	if (atoi(argv[1]) == 5) {
	  spy_t timer = spy_create("OSF/1 Unprot100 test", NUM_PROT100_ITERATIONS);
	  for (i=0;i<NUM_UNPROT100_ITERATIONS;i++) {
	    if ((ret = mprotect(alpha_trunc_page(buffer),
				100* ALPHA_PGBYTES,
				PROT_READ)) != 0)
	      {
		perror("mprotect failure");
		exit();
	      } 
	    
	    spy_start(timer);  
	    if ((ret = mprotect(alpha_trunc_page(buffer),
				100* ALPHA_PGBYTES,
				PROT_READ | PROT_WRITE)) != 0)
	      {
		perror("mprotect failure");
		exit();
	      } 
	    spy_stop(timer); 
	    for (j=0;j<100;j++)
	      buffer[j*ALPHA_PGBYTES] = value;
	    
	  }
	  spy_dump(timer);
	}
	
	if (atoi(argv[1]) == 6) {
	  globaltimer = spy_create("OSF/1 Appel1 test", NUM_APPEL1_ITERATIONS);
	  signal(SIGSEGV, (void (*)(int))protection_fault_handler3); 
	  
	  if ((ret = mprotect(alpha_trunc_page(buffer),
			      ALPHA_PGBYTES,
			      PROT_READ)) != 0)
	    {
	      perror("mprotect failure");
	      exit();
	    } 
	  badaddr = buffer+1;
	  j =0;
	  for (i=0;i<NUM_APPEL1_ITERATIONS;i++) {
	    j++;
	    if (j>100) badaddr = buffer+1;
	    spy_start(globaltimer);  
	    *badaddr = 10;
	    
	  }
	  spy_dump(globaltimer);
	}
	if (atoi(argv[1]) == 7) {
	  spy_t timer = spy_create("OSF/1 Appel2 test", NUM_APPEL2_ITERATIONS);
	  signal(SIGSEGV, (void (*)(int))protection_fault_handler); 
	  
	  if ((ret = mprotect(alpha_trunc_page(buffer),
			      100*ALPHA_PGBYTES,
			      PROT_READ)) != 0)
	    {
	      perror("mprotect failure");
	      exit();
	    } 
	  for (i=0;i<NUM_APPEL2_ITERATIONS;i++) {
	    badaddr = buffer;
	    
	    value = *badaddr;
	    spy_start(timer);  
	    if ((ret = mprotect(alpha_trunc_page(buffer),
				100*ALPHA_PGBYTES,
				PROT_READ)) != 0)
	      {
		perror("mprotect failure");
		exit();
	      } 
	    
	    for (j=0;j<100;j++) {
	      *badaddr = 10;
	      badaddr += (ALPHA_PGBYTES);
	    }
	    spy_stop(timer); 
	  }
	  spy_dump(timer);
	}
	
	if (atoi(argv[1]) == 8) {
	  globaltimer = spy_create("OSF/1 Trap", NUM_TRAP_ITERATIONS);
	  signal(SIGSEGV, (void (*)(int))protection_fault_handler2); 
	  
	  buffer[1]=1;
	  
	  for (i=0;i<NUM_TRAP_ITERATIONS;i++) {
	    badaddr = buffer;
	    if ((ret = mprotect(alpha_trunc_page(buffer),
				ALPHA_PGBYTES,
				PROT_READ)) != 0)
	      {
		perror("mprotect failure");
		exit();
	      } 
	    spy_start(globaltimer);  
	    
	    *badaddr = value;
	  }
	  spy_dump(globaltimer);
	}
	if (atoi(argv[1]) == 9) {
	  spy_t timer = spy_create("Fault and Fetch test",
				   NUM_FAULTFETCH_ITERATIONS);
	    
	    signal(SIGSEGV, (void (*)(int))faultandfetch_handler);
	    
	    for (i=0;i<NUM_FAULTFETCH_ITERATIONS;i++) {
	      if ((ret = mprotect(alpha_trunc_page(buffer),
				  ALPHA_PGBYTES,
				  PROT_READ)) != 0)
		{
		  perror("mprotect failure");
		  exit();
		} 
	      spy_start(timer);  
	      badaddr = buffer+1;
	      *badaddr = 2;
	      spy_stop(timer); 
	    }
	    spy_dump(timer);
	}



}
