/* 
   Stripped down version of rvm.h for use by rds.

 */
/*
 * HISTORY
 * 07-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 */

#ifndef __RVM_H
#define __RVM_H

#ifdef __cplusplus
extern "C" {
#endif

typedef long rvm_tid_t;
typedef enum {
    RVM_SUCCESS = 0,                    /* success return code */

    rvm_first_code = 199,               /* internal use only */

    RVM_EINIT,                          /* RVM not initialized */
    RVM_EINTERNAL,                      /* internal error, see rvm_errmsg */
    RVM_EIO,                            /* I/O error, see errno */
    RVM_ELOG,                           /* invalid log device */
    RVM_ELOG_VERSION_SKEW,              /* RVM log format version skew */
    RVM_EMODE,                          /* invalid transaction begin/end mode */
    RVM_ENAME_TOO_LONG,                 /* device name longer than 1023 chars */
    RVM_ENO_MEMORY,                     /* heap exhausted */
    RVM_ENOT_MAPPED,                    /* designated region not mapped */
    RVM_EOFFSET,                        /* invalid segment offset */
    RVM_EOPTIONS,                       /* invalid options record or pointer */
    RVM_EOVERLAP,                       /* region overlaps existing seg mapping */
    RVM_EPAGER,                         /* invalid external pager */
    RVM_ERANGE,                         /* invalid virtual memory address */
    RVM_EREGION,                        /* invalid region descriptor or pointer */
    RVM_EREGION_DEF,                    /* invalid region definition descriptor */
    RVM_ESRC,                           /* invalid address range for new
                                           values */
    RVM_ESTATISTICS,                    /* invalid statistics record */
    RVM_ESTAT_VERSION_SKEW,             /* RVM statistics format version skew */
    RVM_ETERMINATED,                    /* terminated by error already reported */
    RVM_ETHREADS,                       /* illegal C Thread library */
    RVM_ETID,                           /* invalid transaction identifier or ptr */
    RVM_ETOO_BIG,                       /* internal resouces exceeded */
    RVM_EUNCOMMIT,                      /* uncommitted transaction(s) pending */
    RVM_EVERSION_SKEW,                  /* RVM library version skew */
    RVM_EVM_OVERLAP,                    /* region overlaps existing vm mapping */

    rvm_last_code                       /* internal use only */
    }
rvm_return_t;
/*  Transaction mode codes: rvm_mode_t */
typedef enum
    {
    rvm_first_mode = 139,               /* internal use only */

    rm_restore,                            /* restore memory on abort */
    rm_no_restore,                         /* do not restore memory on abort */
    rm_flush,                              /* flush records to logdev on commit */
    rm_no_flush,                           /* do not flush records on commit */

    rvm_last_mode                       /* internal use only */
    }
rvm_mode_t;

#define bool int
#define false 0
#define true 1

typedef long rvm_length_t;
typedef long rvm_offset_t;

rvm_tid_t *rvm_malloc_tid();
void rvm_free_tid(rvm_tid_t *t);
long rvm_page_size();

rvm_return_t rvm_begin_transaction(rvm_tid_t *t, rvm_mode_t mode);

#define rvm_set_range(a,b,c) RVM_SUCCESS

#define RVM_MK_OFFSET(x,y) ((x)+(y))
#define RVM_ROUND_LENGTH_UP_TO_PAGE_SIZE(x)  ((rvm_length_t)( \
    ((rvm_length_t)(x)+rvm_page_size()-1) & ~(rvm_page_size()-1)))

#ifdef __cplusplus
}
#endif

#endif
