#include <assert.h>
#include "trans_malloc.h"

int
trans_checkheapsanity()
{
    int n_free_bytes = 0;
    int n_live_bytes = 0;
    int n_free_blocks = 0;

    int n_free_bytes2 = 0;
    int n_free_blocks2 = 0;
    free_header_t *cur_hdr;
    
    /* Scan the whole heap and count the # of bytes free & used */
    cur_hdr = (free_header_t*)((char*)_heap_header + getpagesize());
    while (cur_hdr < _heap_end) {
	switch (cur_hdr->fingerprint) {
	  case DEADBEEF:
	    n_free_bytes += cur_hdr->size;
	    n_free_blocks++;
	    break;
	  case LIVEBEEF:
	    n_live_bytes += cur_hdr->size;
	    break;
	  default:
	    abort();
	}
	cur_hdr = (free_header_t*)((char*)cur_hdr + cur_hdr->size);
    }

    assert(cur_hdr == _heap_end);
    
    /* scan the free list */
    cur_hdr = _heap_header->free.next;
    while (cur_hdr != &_heap_header->free) {
	assert(cur_hdr->fingerprint == DEADBEEF);
	n_free_blocks2++;
	n_free_bytes2 += cur_hdr->size;
	cur_hdr = cur_hdr->next; 
    }

    assert(n_free_blocks == n_free_blocks2);
    assert(n_free_bytes == n_free_bytes2);
    return 1;
}
