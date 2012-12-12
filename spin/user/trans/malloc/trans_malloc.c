#include "trans_malloc.h"
#include <assert.h>
#include <limits.h>

char *_min_rds_addr = LONG_MAX;
char *_max_rds_addr = 0;

void *
trans_malloc (int size, int *err)
{
    char *m;
    free_header_t *cur_free_hdr = _heap_header->free.next;
    used_header_t *p;

    assert(size > 0);

    size = ((size-1)/8 + 1) * 8; /* round up to 8 */
    size += sizeof(used_header_t);
    if (size < sizeof(free_header_t))
      size = sizeof(free_header_t);

    while (cur_free_hdr != &_heap_header->free) {
	assert(cur_free_hdr->fingerprint == DEADBEEF);
	assert((char*)cur_free_hdr >= (char*)_heap_header);
	assert((char*)cur_free_hdr < (char*)_heap_end);

	if (cur_free_hdr->size >= size) {
	    if (cur_free_hdr->size - size <= sizeof(free_header_t)*2) {
		/* To avoid excessive fragmentation, use up this block.
		   "*2" is arbitrary. */
		size = cur_free_hdr->size;
		trans_dequeue(cur_free_hdr);
	    } else {
		free_header_t *new_free_hdr = (free_header_t*)((char*)cur_free_hdr + size);
		new_free_hdr->size = cur_free_hdr->size - size;
		new_free_hdr->fingerprint = DEADBEEF;
		trans_dequeue(cur_free_hdr);
		trans_enqueue(&_heap_header->free, new_free_hdr);
	    }
	    break;
	} 
	cur_free_hdr = cur_free_hdr->next;
    }
    p = (used_header_t*)cur_free_hdr;
    p->size = size;
    p->fingerprint = LIVEBEEF;

    *err = 0;
    m = (char*)p + sizeof(used_header_t);
    if (m < _min_rds_addr) _min_rds_addr = m;
    if (m > _max_rds_addr) _max_rds_addr = m;
    return m;
}


void 
trans_print_malloc_stat ()
{
    long n;
    printf("min rds addr = 0x%lx, max rds addr=0x%lx.\n", 
	   _min_rds_addr, _max_rds_addr);
    n = _max_rds_addr - _min_rds_addr;
    printf("0x%lx bytes (%ld megabytes)\n", n, n/1024/1024);
}
