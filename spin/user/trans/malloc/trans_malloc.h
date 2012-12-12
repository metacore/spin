#ifndef _TRANS_MALLOC_H
#define _TRANS_MALLOC_H

/*
   malloc/free on persistent storage.
   geared toward ulitmate space efficiency, but very slow.

   */

/*
 * HISTORY
 * 10-May-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 */


#define MALLOC_VERSION_STRING "SPIN transaction malloc 1.0"

typedef struct {
    unsigned long offset, length, vmaddr;
} heap_region_t;

typedef struct free_header_t {
    unsigned int fingerprint; /* DEADBEEF */
    unsigned int size; /* includes the size of the free_header  itself */
    struct free_header_t *prev, *next;
} free_header_t;

typedef struct {
    unsigned int fingerprint; /* LIVEBEEF */
    unsigned int size; /* includes the size of the used_header */
} used_header_t;

typedef struct {
    char version[128]; /* Version String */
    int n_regions;
    heap_region_t r[2]; /* region 0 : heap area
			   region 1 : static area, left untouched. */
    unsigned long heap_length, free_bytes;
    free_header_t free; /* only next and prev are used. */
} heap_header_t;
#define DEADBEEF 0xdeadbeef
#define LIVEBEEF 0x12344321

/* Global data extern declarations. */
extern heap_header_t *_heap_header;
extern void *_heap_end;

void trans_initqueue(free_header_t *head);
void trans_enqueue(free_header_t *head, free_header_t *item);
void trans_dequeue(free_header_t *item);

void *trans_malloc(int size, int *err);
void trans_free(void*, int *err);
void trans_load_heap(char *file_name, long size, char **static_addr, int *err);
int trans_checkheapsanity();
void trans_print_malloc_stat();


#endif






