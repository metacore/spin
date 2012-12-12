#include "trans_malloc.h"
heap_header_t *_heap_header;
void *_heap_end;

void 
trans_enqueue (free_header_t *head, free_header_t *item)
{
    item->next = head->next;
    item->prev = head;
    head->next->prev = item;
    head->next = item;
}

void 
trans_dequeue (free_header_t *item)
{
    item->next->prev = item->prev;
    item->prev->next = item->next;
    item->prev = 0;
    item->next = 0;
}

void 
trans_initqueue (free_header_t *head)
{
    head->next = head;
    head->prev = head;
}
