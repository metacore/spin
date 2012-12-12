/*
 * HISTORY
 * 07-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 */

#ifndef __SPIN_TRANS_H
#define __SPIN_TRANS_H

#ifdef __cplusplus
extern "C" {
#endif

typedef long tid_t; /* XXX this has to be 8 byte long. This definition
		       works only on Alpha. */
typedef long sid_t; /* Storage ID */

/* trans_stat_t is a trans version of "struct stat". We had to create a 
   separate stat structure because the transaction file destriptor is not
   realla file. We need to unify them in the future. */
typedef struct trans_stat {
    long size;
} *trans_stat_t;

#define TRANS_TIMEOUT_INFINITY 0x7fffffffffffffff

#define TRANSMODE_UNDOLOG 1 /* Generate undo log in addition to redo log. */
#define TRANSMODE_NOATOMICITY 2 /* No log is generated. UNDOLOG is not
				   meaningful. */
#define TRANSMODE_NOLOCKS 4 /* NOLOCK */
#define TRANSMODE_PAGEGRAINLOGGING 8

tid_t trans_begin(int flags); /* flags is bitor of TRANSMODE_XXXs */

void trans_set_range(tid_t t, void *addr, long len);

void trans_set_multiple_ranges(tid_t t, sid_t sid,
			       void *record, long record_len);

void trans_lock(tid_t t, int rw, void *addr, long len);
sid_t trans_open(char *path);
int trans_mmap(char *addr, long len, long prot,
	       long flags, long sid, long off);
void trans_lock(tid_t t, int rw, void *addr, long len);
int trans_commit(tid_t t);
int trans_getstat(sid_t sid, trans_stat_t stat);

void trans_terminate();
void bootstrap_trans();

#ifndef ROUND_DOWN
#define ROUND_DOWN(v, boundary) ((v) & ~((boundary)-1))
#endif

#ifndef ROUND_UP
#define ROUND_UP(v, boundary) ((v-1) & ~((boundary)-1)) + (boundary)
#endif

#ifdef __cplusplus
}
#endif

#endif
