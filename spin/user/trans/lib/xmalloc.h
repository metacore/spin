#ifndef __XMALLOC_H
#define __XMALLOC_H

#ifdef __cplusplus
extern "C"
#endif

void *xmalloc(int size);

#define xfree free

#ifdef __cplusplus
}
#endif
#endif
