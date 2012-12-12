#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mount.h>
#include <sys/resource.h>
#include <unistd.h>

gimmeabreak()
{
    abort();
}
cancel_memory_limits()
{
    struct rlimit limit;
    getrlimit(RLIMIT_DATA, &limit);
    printf("current data limit=%lx, max=%lx.\n",
	   limit.rlim_cur, limit.rlim_max);
    printf("setting data limit to the maximum.\n");
    limit.rlim_cur = limit.rlim_max;
    if (setrlimit(RLIMIT_DATA, &limit)) {
	perror("setrlimit");
	exit(1);
    }
}
#define MAX_SIZE (1000 * 1024 * 1024)

long get_file_size(int fd)
{
    char buf[512];
    long org_pos;
    struct stat s;
    struct statfs fsbuf;
    long cur_max, cur_min;

    if (fstat(fd, &s) < 0) {
	perror("stat");
	exit(1);
    }
    if (!(s.st_mode & S_IFCHR)) {
	/* reg file. */
	return s.st_size;
    } 
    org_pos = lseek(fd, 0, SEEK_CUR);

    /* Do a binary search to locate the EOF. Sounds stupid, but this
       is the only way I know of. */
    cur_max = MAX_SIZE;
    cur_min = 0;

    while (cur_max - cur_min >= 10) {
	long n;
	lseek(fd, (cur_max+cur_min)/2, SEEK_SET);
	n = read(fd, buf, 512);
	if (n < 0) {
	    perror("get_file_size:lseek");
	    exit(1);
	}
	if (n <= 0) {
	    /* past eof */
	    cur_max = (cur_max+cur_min)/2;
	} else {
	    cur_min = (cur_max+cur_min)/2;
	}
    }
    lseek(fd, org_pos, SEEK_SET);
    printf("\nfile size = %ld\n", cur_min);
    return cur_min;
}

