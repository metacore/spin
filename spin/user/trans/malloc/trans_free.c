#include <assert.h>
#include "trans_malloc.h"
void
trans_free (void *p, int *err)
{
    used_header_t *h = (used_header_t*)((char*)p - sizeof(used_header_t));
    free_header_t *free = (free_header_t*)h;
    free_header_t *neighbor;

    assert(h->fingerprint == LIVEBEEF);
    neighbor = (free_header_t*)((char*)h + h->size);

    free->fingerprint = DEADBEEF;
    /* Note: free->size remains same */

    /* look at the next neighbor */
    if (neighbor->fingerprint == DEADBEEF) {
	/* coalesce the current region into the next one. */
	free->size += neighbor->size;
	trans_dequeue(neighbor);
	trans_enqueue(&_heap_header->free, free);
    } else {
	assert(neighbor->fingerprint == LIVEBEEF);
	trans_enqueue(&_heap_header->free, free);
    }

    /* XXX we don't look at the previous neighbor */
    *err = 0;
}
