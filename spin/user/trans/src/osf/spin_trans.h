typedef long tid_t;
typedef long sid_t;
typedef long trans_status_t;

#define TRANS_TIMEOUT_INFINITY 0x7fffffffffffffff
extern char *MI_CSupport;

typedef unsigned long (*_PROC)();

/* Alert: the order of the below functions has to be same as
   CSupport. */

#define PAGE_SIZE 8192
#define ROUNDDOWN(x) ((x) & ~(PAGE_SIZE-1))
#define ROUNDUP(x) ((((x)-1) & ~(PAGE_SIZE-1))+PAGE_SIZE)

#define trans_begin (*(_PROC*)((char*)(&MI_CSupport)+88))
#define trans_commit (*(_PROC*)((char*)(&MI_CSupport)+96))
#define trans_abort (*(_PROC*)((char*)(&MI_CSupport)+104))
#define trans_open (*(_PROC*)((char*)(&MI_CSupport)+112))
#define trans_storage_base (*(_PROC*)(((char*)&MI_CSupport)+120))
#define trans_pin(sid, tid, from, len, excl, timeout)			      \
    (*(_PROC*)(((char*)&MI_CSupport)+128))(sid,tid,			      \
					   ROUNDDOWN(from), 		      \
					   ROUNDUP(from+len)-ROUNDDOWN(from), \
					   excl, timeout)

#define trans_modifyrange (*(_PROC*)(((char*)&MI_CSupport)+136))

