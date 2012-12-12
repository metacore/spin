#include <assert.h>
#include "spin_trans.h"
#include "../ALPHA_OSF/CSupport.h"

typedef long tid_t;
typedef long sid_t;
typedef long trans_status_t;

#define TRANS_TIMEOUT_INFINITY 0x7fffffffffffffff

/* Alert: the order of the below functions has to be same as
   CSupport. */

#define PAGE_SIZE 8192
#define ROUNDDOWN(x) ((x) & ~(PAGE_SIZE-1))
#define ROUNDUP(x) ((((x)-1) & ~(PAGE_SIZE-1))+PAGE_SIZE)

static tid_t cur_tid;

void trans_null()
{
}
long _trans_begin(long flags)
{
    /*return (*(_PROC*)((char*)(&MI_CSupport)+88))(flags);*/
    cur_tid = CSupport__trans_begin(flags);
    return cur_tid;
}
long 
_trans_commit (long tid)
{
    /*return (*(_PROC*)((char*)(&MI_CSupport)+96))(tid);*/
    assert(tid == cur_tid);
    return CSupport__trans_commit(tid);
}

long
trans_abort (long tid)
{
    /*return (*(_PROC*)((char*)(&MI_CSupport)+104))(tid);*/
    assert(tid == cur_tid);
    return CSupport__trans_abort(tid);
}
long
_trans_open (char *file)
{
    /*return (*(_PROC*)((char*)(&MI_CSupport)+112))(file);*/
    return CSupport__trans_open(file);
}
long
trans_storage_base (long sid)
{
    /*return (*(_PROC*)(((char*)&MI_CSupport)+120))(sid);*/
    return CSupport__trans_storage_base(sid);
}

long
_trans_mmap (long addr, long len, long prot, long flags,
	   long fd, long off)
{
    /*return (*(_PROC*)(((char*)&MI_CSupport)+144))(addr, len, prot, flags, fd, off);*/
    return CSupport__trans_mmap(addr, len, prot, flags, fd, off);

}
void 
_trans_munmap (long addr, long len)
{
    CSupport__trans_munmap(addr, len);
}

int
trans_getstat(sid_t fd, trans_stat_t stat)
{
    return CSupport__trans_getstat(fd, stat);
}
void trans_flush_logs()
{
    CSupport__trans_flush_logs();
}
void trans_close(long sid)
{
    CSupport__trans_close(sid);
}


#include <signal.h>
#include "libtrans.h"

static int n_pagefault;
static int n_invalid_pagefault;
static void 
segv_handler (int x, int code, struct sigcontext *ctx)
{
    char *addr = (char*)ctx->sc_traparg_a0;
    int type = ctx->sc_traparg_a2;
    mmap_region *m = mmap_region_find(addr);
    if (m == 0) {
	printf("segv addr = %lx\n", addr);
	n_invalid_pagefault++;
	if (n_invalid_pagefault > 2) 
	  /* let one page fault go through to give GDB chance to inspect. */
	  abort();
    } else {
	n_pagefault++;
	CSupport__trans_pagefault(m->sid, cur_tid, ROUNDDOWN(addr - m->from),
				  type);
    }
}

void 
print_stat ()
{
    printf("%d page faults.\n", n_pagefault);
}

void _trans_initialize()
{
    signal(SIGSEGV, segv_handler);
    atexit(print_stat);
}
void trans_barrier()
{
}
void USyscall_Profile()
{
}

void USyscall_System(char *x)
{
    CSupport__usyscall_system(x);
}
