#ifndef __LIBTRANS_H
#define __LIBTRANS_H
#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    long sid;
    char *from;
    char *to;
} mmap_region;

mmap_region *mmap_region_find(char *addr);
void mmap_region_add(long sid, char *from, long len);

#ifdef __cplusplus
}
#endif

#endif
