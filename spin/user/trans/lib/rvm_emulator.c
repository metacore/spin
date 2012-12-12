#include "rvm.h"
#include "spin_trans.h"
#include "xmalloc.h"

rvm_tid_t *
rvm_malloc_tid ()
{
    rvm_tid_t *r = xmalloc(sizeof(*r));
    *r = 0;
    return r;
}

void rvm_free_tid (rvm_tid_t *t)
{
    free(t);
}

rvm_return_t 
rvm_begin_transaction (rvm_tid_t *t, rvm_mode_t mode)
{
    *t = trans_begin(0);
    return RVM_SUCCESS;
}

#undef rvm_set_range
/* for those of you who don't include rvm.h. */
rvm_return_t
rvm_set_range (rvm_tid_t *t, void *addr, int len)
{
    return RVM_SUCCESS;
}

rvm_return_t 
rvm_abort_transaction (rvm_tid_t *t)
{
    trans_abort(*t);
    return RVM_SUCCESS;
}

rvm_return_t
rvm_end_transaction (rvm_tid_t *t, rvm_mode_t mode)
{
    trans_commit(*t);
    return RVM_SUCCESS;
}

rvm_return_t
rvm_truncate ()
{
    return RVM_SUCCESS;
}

rvm_return_t
rvm_terminate ()
{
    trans_terminate();
    return RVM_SUCCESS;
}

rvm_return_t
rvm_flush ()
{
    trans_flush_logs();
    return RVM_SUCCESS;
}

long 
rvm_page_size()
{
    static long x;
    if (x == 0) {
	x = getpagesize();
    }
    return x;
}
static char *return_codes[(long)rvm_last_code-(long)rvm_first_code-1] =
    {
    "RVM_EINIT","RVM_EINTERNAL","RVM_EIO","RVM_ELOG",
    "RVM_ELOG_VERSION_SKEW","RVM_EMODE","RVM_ENAME_TOO_LONG",
    "RVM_ENO_MEMORY","RVM_ENOT_MAPPED","RVM_EOFFSET",
    "RVM_EOPTIONS","RVM_EOVERLAP","RVM_EPAGER","RVM_ERANGE",
    "RVM_EREGION","RVM_EREGION_DEF","RVM_ESRC","RVM_ESTATISTICS",
    "RVM_ESTAT_VERSION_SKEW","RVM_ETERMINATED","RVM_ETHREADS",
    "RVM_ETID","RVM_ETOO_BIG","RVM_EUNCOMMIT",
    "RVM_EVERSION_SKEW","RVM_EVM_OVERLAP"
     };


char *
rvm_return (rvm_return_t code)
{
    if (code == RVM_SUCCESS) return "RVM_SUCCESS";
    
    if (((long)code > (long)rvm_first_code) &&
        ((long)code < (long)rvm_last_code))
      return return_codes[(long)code-(long)rvm_first_code-1];
    else
      return "Invalid RVM return code";
}
