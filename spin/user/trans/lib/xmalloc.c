#include <stdio.h>
extern void *malloc(int);

void *xmalloc(int size) {
    void *m = malloc(size);
    if (m == 0) {
	fprintf(stderr, "xmalloc: memory exhausted.\n");
	exit(1);
    }
    return m;
}

