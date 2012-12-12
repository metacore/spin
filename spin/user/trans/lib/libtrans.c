#include <stdio.h>
#include <assert.h>
#include "splay.h"
#include "spin_trans.h"
#include "libtrans.h"
#include "xmalloc.h"
#include "spy.h"

#ifdef __alpha
#define PAGE_SIZE 8192 /* this has to be a const to achieve maximal
			  performance. */
#elif defined(__i386)
#define PAGE_SIZE 4196
#endif

static char loaded;
#define MAX_LOCK_CACHE 256
#define MAX_REGION 128

typedef struct mod_node {
    Tree _node;
    long end;
    tid_t tid;
    long sid;
} mod_node;
mod_node *mod_regions;

enum lock_type_t {
    LOCK_READ = 0, LOCK_WRITE = 1
};
typedef long tid_t;

static long page_lock[MAX_LOCK_CACHE];
static char page_lock_type[MAX_LOCK_CACHE];
static int page_locks_idx;
static boundary;
static coalesce_limit;
static mmap_region mapped_region[MAX_REGION];

#pragma inline
mmap_region *
mmap_region_find (char *addr)
{
    int i;
    for (i = 0; i < MAX_REGION; i++) {
	if (mapped_region[i].from == 0) return 0;
	if (addr >= mapped_region[i].from && addr < mapped_region[i].to)
	  return &mapped_region[i];
    }
    return 0;
}

static void
mmap_region_add (long sid, char *from, long len)
{
    int i;
    assert(!mmap_region_find(from));
    assert(!mmap_region_find(from+len));
    for (i = 0; i < MAX_REGION; i++) {
	if (mapped_region[i].from == 0) {
	    mapped_region[i].sid = sid;
	    mapped_region[i].from = from;
	    mapped_region[i].to = from+len;
	    return;
	}
    }
    abort();
}

static spy_t begin_spy, commit_spy, setrange_spy;

#ifndef SPY
#undef spy_start(x)
#undef spy_stop(x)
#endif

void 
bootstrap_trans ()
{
    if (!loaded) {
#ifdef spin
	/* SPIN extension */
	__usyscall_bootstrap("Trans", "trans_open");
#else
	/* Emulation on OSF. */
	_trans_initialize();
#endif
	begin_spy = spy_create("begin", 400);
	commit_spy = spy_create("commit", 400);
	setrange_spy = spy_create("setrange", 400);
	loaded = 1;
    }
}

static int trans_stack_idx;
static tid_t cur_tid;

long 
trans_begin (int flags)
{
    Tree *mb, *existed;
    bootstrap_trans();

    /* Free up the setrange data structured built during the previous 
       transaction. */
    boundary = 0;
    coalesce_limit = 500;
    while (mod_regions) {
	mod_regions = (mod_node*)splay_delete_root((Tree*)mod_regions, &mb);
	xfree(mb);
    }

    /* Emulate nested transaction in a bogus way: 
       just ignore nested trans_begin. */
    if (trans_stack_idx == 0) {
	spy_start(begin_spy);
	cur_tid = _trans_begin(flags);
	spy_stop(begin_spy);
    }
    trans_stack_idx++;
    return cur_tid;
}

long 
trans_open (char *path)
{
    bootstrap_trans();
    return _trans_open(path);
}

int
trans_mmap (char *addr, long len, long prot, long flags, long sid, long off) 
{
    long rc = _trans_mmap((long)addr, len, prot, flags, sid, off);
    if (rc == 0) {
	mmap_region_add(sid, addr, len);
    }
    return rc;
}
long 
trans_munmap (char *addr, long len)
{
    int src, dst;
    src = dst = 0;

    /* Delete the entry from the mmaped_region, and shift all the trailing
       entries forward. */
    while (src < MAX_REGION) {
	if (mapped_region[src].from == 0) break;
	if (addr >= mapped_region[src].from && addr < mapped_region[src].to) {
	    /* delete this guy */
	    src++;
	} else {
	    if (dst != src) mapped_region[dst] = mapped_region[src];
	    src++;
	    dst++;
	}
    }
    while (dst < MAX_REGION) {
	mapped_region[dst].from = 0;
	dst++;
    }
    _trans_munmap((long)addr, len);
    return 0;
}

/* notused */
void 
trans_lock (tid_t t, int rw, void *addr, long len)
{
    int i;
    long page = ROUND_DOWN((long)addr, PAGE_SIZE);
    long end_page = ROUND_UP((long)addr+len, PAGE_SIZE);
    /* If the lock for the same page is already held by this, then do
       nothing. */
    for (i = 0; i < page_locks_idx; i++) {
	if (page_lock[i] == page && page_lock_type[i] == rw) {
	    /* Lock is already held. Do nothing. */
	    return;
	}
    }
    page_lock[page_locks_idx] = page;
    page_lock_type[page_locks_idx] = rw;
    page_locks_idx++;
    return;
}

void trans_setrange (tid_t t, char *addr, long len)
{
    return;
}

int
trans_commit (tid_t t) 
{

    page_locks_idx = 0;
    trans_stack_idx--;
    assert(trans_stack_idx >= 0);
    if (trans_stack_idx == 0) {
	assert(cur_tid == t);
	spy_start(commit_spy);
	_trans_commit(t);
	spy_stop(commit_spy);
	assert((cur_tid = -1, 1));
    }

}

/* XXX do stack manipulation in trans_abort also. */

void
trans_terminate ()
{
    int i;
    for (i = 0; i < MAX_REGION; i++) {
	if (mapped_region[i].from != 0) {
	    trans_close(mapped_region[i].sid);
	}
    }
}
