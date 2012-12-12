#include <malloc.h>

#define NBLOCKS 100


void stress (int size)
{
    int i,j;
    char *ptr;
    void *blocks[NBLOCKS];

    printf("start mallocing blocks of size %d\n", size);
    ptr = 0;

    for (i = 0; i < NBLOCKS; i++) {
	blocks[i] = malloc(size);
    }

    printf("now writing on all blocks.\n");
    for (i = 0; i < NBLOCKS; i++) {
	    ptr = (char *)blocks[i];
	    for (j =0; j < size; j++) {
		    *(ptr + j) = (char)0;
	    }
    }
    
    printf("now, freeing all the blocks.\n");

    for (i = 0; i < NBLOCKS; i++) {
	free(blocks[i]);
    }

}
main()
{
    stress(getpagesize());
    stress(123);
}
