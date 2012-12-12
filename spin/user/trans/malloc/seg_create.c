/*
 * HISTORY
 * 23-Dec-96  Yasushi Saito (yasushi) at the University of Washington
 *	Added size guessing.
 * 10-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 */

/* Stripped down version of RVM segment module for the SPIN transaction
   system. */
#include <stdio.h>
#include <errno.h>
#include <fcntl.h>
#include <stddef.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mount.h>
#include <ctype.h>
#include <unistd.h>
#include <assert.h>
#include "trans_malloc.h"


long 
scan_number (char *s)
{
    long n;
    if (*s == '0' && s[1] == 'x') {
	sscanf(s+2, "%lx", &n);
    } else {
	sscanf(s, "%ld", &n);
	while (isdigit(*s)) s++;
	if (toupper(*s) == 'M') {
	    n *= 1024*1024;
	} else if (toupper(*s) == 'K') {
	    n *= 1024;
	}
    }
    return n;
}

#define MAX_SIZE (1000 * 1024 * 1024)
/* Get the size of the file if it is raw disk device. Otherwise, return
   0. */
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
    return cur_min;
}

void 
help ()
{
    fprintf(stderr, "seg_create : set up persistent malloc library.\n");
    fprintf(stderr, "seg_create [-ch] FILE VADDR HEAPSIZE STATICSIZE.\n");
    fprintf(stderr, "-c : clear the file contents.\n");
    fprintf(stderr, "-h : this message.\n");
    fprintf(stderr, "VADDR : the virtual address of the start of the heap.\n");
    fprintf(stderr, "HEAPSIZE : the size of mallocable heap.\n");
    fprintf(stderr, "STATICSIZE : the size of region for static objects.\n");
    fprintf(stderr, "The number formats:\n");
    fprintf(stderr, " XXX : decimal.\n");
    fprintf(stderr, " 0xXXX : hex.\n");
    fprintf(stderr, " XXXk : kilobytes(XXX is interpreted as decimal).\n");
    fprintf(stderr, " XXXm : megabytes(XXX is interpreted as decimal).\n");
    fprintf(stderr, "current brk = 0x%lx\n", sbrk(0));
    exit(1);
}

void
main (int argc, char **argv)
{
    char buf[8192];
    char *file_name;
    long addr;
    long heap_size, static_size;
    long file_size;
    heap_header_t *hdr;
    free_header_t *free_hdr;
    int fd;
    long pagesize = getpagesize();
    int no_clear_file = 0;
    int c;

    while (((c = getopt(argc, argv, "nh")) != EOF)) {
	switch (c) {
	  case 'n':
	    no_clear_file = 1;
	    break;
	  default:
	    help();
	}
    }
    if (argc-optind < 4) {
	help();
    }


    file_name = argv[optind++];
    addr = scan_number(argv[optind++]);
    heap_size = scan_number(argv[optind++]);
    static_size = scan_number(argv[optind++]);
    unlink(file_name);
    fd = open(file_name, O_RDWR|O_CREAT, 0644);
    if (fd < 0) {
	perror(file_name);
	exit(1);
    }
    file_size = get_file_size(fd);
    if (file_size == 0) {
	ftruncate(fd, heap_size + static_size + getpagesize());
    } else if (!no_clear_file) {
	int size = file_size;
	char *malloc();
	char *x = malloc(1024 * 1024 + 8192 * 2);
	char *buf = (char*)((long)x/8192*8192 + 8192);
	long i;
	memset(buf, 0, 1024*1024);

	if (size > heap_size + static_size + getpagesize()) {
	    size = heap_size + static_size + getpagesize();
	}
	printf("clearing %s(%d mb). this will take a while.", file_name,
	       size/1024/1024);
	fflush(stdout);
	for (i = 0; i < size; i += 1024*1024) {
	    int n, x;
	    lseek(fd, i, 0);
	    x = 1024 * 1024;
	    if (x > size-i) x = size-i;

	    n = write(fd, buf, x);
	    if (n < x) {
		printf("write error at %ld", i);
		file_size = i;
		break;
	    }
	    putchar('.'); fflush(stdout);
	}
	printf("done.\n");
	free(x);
    }
    if (heap_size <= 0 || static_size <= 0) {
	if (file_size == 0) {
	    printf("You can't omit heap/static size in regular file.\n");
	    exit(1);
	}
	file_size = file_size/(1024*1024)*(1024*1024);
	if (heap_size > 0) {
	    static_size = file_size - heap_size - pagesize;
	} else if (static_size > 0) {
	    heap_size = file_size - static_size - pagesize; 
	} else {
	    printf("heap size and static size can't both be 0.\n");
	    exit(1);
	}
    }


    memset(buf, 0, sizeof(buf));
    hdr = (heap_header_t*)buf;
    strcpy(hdr->version, MALLOC_VERSION_STRING);
    hdr->n_regions = 2; /* heap and static. */
    hdr->r[0].offset = getpagesize();
    hdr->r[0].length = heap_size;
    hdr->r[0].vmaddr = addr;
    hdr->r[1].offset = hdr->r[0].offset + heap_size;
    hdr->r[1].length = static_size;
    hdr->r[1].vmaddr = hdr->r[0].vmaddr + heap_size;
    hdr->heap_length = heap_size-sizeof(long);
    hdr->free_bytes = heap_size-sizeof(long);
    hdr->free.prev = (void*)hdr->r[0].vmaddr;
    hdr->free.next = (void*)hdr->r[0].vmaddr;

    lseek(fd, 0, SEEK_SET);
    write(fd, buf, 4096);
    assert((hdr->r[0].offset % getpagesize()) == 0);
    printf("         addr           fileoff        length\n");
    printf("heap:    0x%-12lx 0x%-12lx 0x%-12lx\n", 
	   hdr->r[0].vmaddr, hdr->r[0].offset, hdr->r[0].length);
    printf("static:  0x%-12lx 0x%-12lx 0x%-12lx\n", 
	   hdr->r[1].vmaddr, hdr->r[1].offset, hdr->r[1].length);

    /* seek before hdr gets corrupted.. */
    lseek(fd, hdr->r[0].offset, SEEK_SET);
    memset(buf, 0, sizeof(buf));
    free_hdr = (free_header_t*)buf;
    free_hdr->size = heap_size - sizeof(long);
    free_hdr->fingerprint = DEADBEEF;
    /* let free point to &heap_hdr->free. */
    free_hdr->prev = (void*)(addr-getpagesize() + offsetof(heap_header_t, free));
    free_hdr->next = free_hdr->prev;
    write(fd, buf, 4096);
    close(fd);
}


