#include <sys/types.h>
#include <sys/stat.h>

struct stat statbuf;

main(argc, argv)
    int  argc;
    char *argv[];
{
    int fd;
    
    if (argc >= 2) {
	fd = open(argv[1],0,0);
	if (fd < 0) {
	    perror(argv[1]);
	    exit(1);
	}
    }
    else
      fd = 0;

    
    if (fstat(fd,&statbuf) < 0) {
	perror("fstat");
	exit(1);
    }
    printf("stat OK, size: %d\n",statbuf.st_size);
    close(fd);
}
