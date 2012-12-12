#ifndef __RDS_H
#define __RDS_H

#include "trans_malloc.h"
#define rds_malloc(size,tid,err) trans_malloc(size,err)
#define rds_free(size,tid,err) trans_free(size,err)
#define rds_free(size,tid,err) trans_free(size,err)
#define rds_load_heap trans_load_heap
#define rds_prealloc(size,n,tid,err) trans_prealloc(size,n,tid)

#endif

