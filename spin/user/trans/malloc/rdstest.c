#include <stdio.h>
#include "trans_malloc.h"

#define NPTRS 1024
#define HEAPSIZE (8*1024*1024)
static void *ptrs[NPTRS];
void *malloc();

main()
{
    int i, j, err;
    char *x = malloc(HEAPSIZE);
    free_header_t *free = (free_header_t*)(x+getpagesize());

    _heap_header = (heap_header_t*)x;
    _heap_end = x + HEAPSIZE;
    trans_initqueue(&_heap_header->free);
    free->size = HEAPSIZE-getpagesize();
    free->fingerprint = DEADBEEF;
    trans_enqueue(&_heap_header->free, free);
    srandom(1);
    for (i = 0; i < NPTRS; i++) {
	int size = random() % 128 + 10;
	ptrs[i] = trans_malloc(size, &err);
	trans_checkheapsanity();
    }

    for (;;) {
	j++;
	if ((j % 1) == 0) {
	    putchar('.'); fflush(stdout);
	}
	  
	for (i = 0; i < NPTRS; i++) {
	    if (ptrs[i] && random() % 128 >= 64) {
		trans_free(ptrs[i], &err);
		ptrs[i] = 0;
		trans_checkheapsanity();
	    } else if (!ptrs[i] && random() % 128 >= 64) {
		int size = random() % 128 + 10;
		ptrs[i] = trans_malloc(size, &err);
		trans_checkheapsanity();
	    }
	}
    }
}
