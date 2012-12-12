#ifndef __RVM_DEF_H
#define __RVM_DEF_H
#include <rvm.h>
#include <setjmp.h>
#include <rvm.h>
#include <rds.h>
#include <assert.h>


#define CODA_STACK_LENGTH 0x20000      /* 128 K */

struct root_struct *_root;

typedef struct {
    rvm_tid_t *tid;
    jmp_buf abort;
} rvm_perthread_t;

extern rvm_perthread_t rvm_data;
extern int _status;
#define RVM_THREAD_DATA (&rvm_data)

#define	BEGIN_TRANSACTION() begin_transaction(rm_restore)
#define	END_TRANSACTION() end_transaction(rm_flush, &_status)

#define REC(name) _root->name


#define MODIFY(object, newValue)					    \
do {									    \
        rvm_return_t ret = rvm_set_range(RVM_THREAD_DATA->tid, (char *)&object, sizeof(object)); \
	if (ret != RVM_SUCCESS)						    \
	    printf("Modify Bytes error %s\n",rvm_return(ret));		    \
        assert(ret == RVM_SUCCESS);					    \
        (object) = (newValue);						    \
} while(0)

#define MODIFY_BYTES(objectPtr, newValuePtr, length)		    	    \
do {									    \
        rvm_return_t ret =						    \
	    rvm_modify_bytes(RVM_THREAD_DATA->tid, (char *)objectPtr,	    \
			     (char *)newValuePtr, (int)length);             \
	if (ret != RVM_SUCCESS)						    \
	    printf("Modify Bytes error for %x, %s\n",objectPtr, rvm_return(ret));\
        assert(ret == RVM_SUCCESS);					    \
}while(0)

#define DEBUG(level, message) \
     if (debuglevel >= (level)) printf message

#endif

