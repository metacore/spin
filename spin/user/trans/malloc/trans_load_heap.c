#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/types.h>
#include <spin_trans.h>
#include "rvm.h"
#include "trans_malloc.h"

static void *
get_tmp_mmap_addr ()
{
    void *sbrk(long);
    long m = (long)sbrk(0);
    m = (m/8192*8192) + 500*1024*1024; /* leave 500M for heap. */
    return (void*)m;
}

void 
trans_load_heap (char *file_name, long size, char **static_addr, int *err)
{
    extern void *malloc(int);

    int n_regions;
    heap_region_t *regions;
    tid_t tid;
    int fd;
    int i;
    struct trans_stat stat;
    
    /* First, map only the first page of the device to get the
       heap header info. Note that we can't use fopen stuff because
       FILE_NAME might be remote.*/
    fd = trans_open(file_name);
    if (fd < 0) {
	perror(file_name);
	*err = 930;
	return;
    }
    trans_getstat(fd, &stat);
    _heap_header = (heap_header_t*)get_tmp_mmap_addr();
    trans_mmap((char*)_heap_header, 8192*2, 0,0, fd, 0);
    tid = trans_begin(0); /* we need to be in a transaction context to
			     access any contents in the file.*/
    if (strcmp(_heap_header->version, MALLOC_VERSION_STRING)) {
	fprintf(stderr, "load_seg: version stamp mismatch %s<->%s.\n",
		_heap_header->version, MALLOC_VERSION_STRING);
	*err = 998;
	return;
    }

    /* Copy the region info into memory. */
    regions = malloc(sizeof(heap_region_t)*_heap_header->n_regions);
    for (i = 0; i < _heap_header->n_regions; i++) {
	heap_region_t *r = &_heap_header->r[i];
	fprintf(stderr, "%s from 0x%0lx, length 0x%lx\n",
		file_name,  r->vmaddr, r->length);
	regions[i] = *r;
    }
    n_regions = _heap_header->n_regions;
    trans_munmap(_heap_header, 8192*2);
    trans_commit(tid);

    if (n_regions != 2) {
	fprintf(stderr, "load_heap: %s has more than 2 segments(%d).\n", 
		file_name, n_regions);
	free(regions);
	*err = 999;
	return;
    }

    /* Map the heap onto the real location */
    _heap_header = (heap_header_t*)(regions[0].vmaddr - getpagesize());
    trans_mmap((void*)_heap_header, stat.size * getpagesize(), 0,0, fd, 0);

    (*static_addr) = (char *)regions[1].vmaddr;
    _heap_end = (void*)(regions[1].vmaddr + regions[1].length);
    printf("heap: %lx to %lx.\n", _heap_header, _heap_end);
    *err = 0;
}


