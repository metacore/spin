#ifndef _BLURB_
#define _BLURB_
/*

    RVM: an Experimental Recoverable Virtual Memory Package
			Release 1.3

      Copyright (c) 1990-1994 Carnegie Mellon University
                     All Rights Reserved.

Permission  to use, copy, modify and distribute this software and
its documentation is hereby granted (including for commercial  or
for-profit use), provided that both the copyright notice and this
permission  notice  appear  in  all  copies  of   the   software,
derivative  works or modified versions, and any portions thereof,
and that both notices appear  in  supporting  documentation,  and
that  credit  is  given  to  Carnegie  Mellon  University  in all
publications reporting on direct or indirect use of this code  or
its derivatives.

RVM  IS  AN  EXPERIMENTAL  SOFTWARE  PACKAGE AND IS KNOWN TO HAVE
BUGS, SOME OF WHICH MAY  HAVE  SERIOUS  CONSEQUENCES.    CARNEGIE
MELLON ALLOWS FREE USE OF THIS SOFTWARE IN ITS "AS IS" CONDITION.
CARNEGIE MELLON DISCLAIMS ANY  LIABILITY  OF  ANY  KIND  FOR  ANY
DAMAGES  WHATSOEVER RESULTING DIRECTLY OR INDIRECTLY FROM THE USE
OF THIS SOFTWARE OR OF ANY DERIVATIVE WORK.

Carnegie Mellon encourages (but does not require) users  of  this
software to return any improvements or extensions that they make,
and to grant Carnegie Mellon the  rights  to  redistribute  these
changes  without  encumbrance.   Such improvements and extensions
should be returned to Software.Distribution@cs.cmu.edu.

*/
#endif _BLURB_

/* bench.h 
 * This file contains the definitions of the structures used by TPC A benchmark
 * This is used both by the rvm and the camelot version of the benchmark
 */
#define MAXLENGTH	128
#define NBRANCHES	1
#define NTELLERS	10
/* #define NACCOUNTS	1 << 18		NOT USED  approx 128 K  */
/* #define NHISTORY	1 << 21		 approx 2 meg entries */
#define NHISTORY	1 << 20		/* approx 1 meg entries */
#define MAXDELTA	1 << 8
#define MAXEXPTS	32

extern int alarmrang;
extern int duration;
extern int naccounts[];

typedef enum {Sequential, Random, Localized} Locality;
typedef struct branch{
    int branch_id;
    int branch_balance;
    char filler[MAXLENGTH - sizeof(int) - sizeof(int)];
} branch;

typedef struct teller {
    int teller_id;
    int branch_id;
    int teller_balance;
    char filler[MAXLENGTH - (3 * sizeof(int))];
} teller;

typedef struct account{
    int acct_id;
    int branch_id;
    int acct_balance;
    char filler[MAXLENGTH - (3 * sizeof(int))];
} account;

typedef struct history {
    int trans_id;
    int branch_id;
    int acct_id;
    int delta;
    int tstamp1;
    int tstamp2;
    char filler[(MAXLENGTH/2) - (6 * sizeof(int))];
} history;


#ifdef RVM

#define CODA_STACK_LENGTH 0x20000      /* 128 K */
#define BEGIN_RECOVERABLE_DECLARATIONS struct camlib_recoverable_segment {
#define END_RECOVERABLE_DECLARATIONS };\
struct camlib_recoverable_segment *camlibRecoverableSegment;

typedef struct {
    rvm_tid_t *tid;
    jmp_buf abort;
    intentionList_t list;
} rvm_perthread_t;

extern rvm_perthread_t rvm_data;

#define RVM_THREAD_DATA (&rvm_data)

extern void rvmlib_internal_abort(char *);

#define	BEGIN_TRANSACTION(restore_mode)\
{\
    rvm_perthread_t *_rvm_data;\
    rvm_tid_t tid;\
    rvm_return_t _status;\
\
	/* Initialize the rvm_perthread_t object. */\
	_rvm_data = RVM_THREAD_DATA;\
	if (_rvm_data == 0) rvmlib_internal_abort("BeginTransaction: _rvm_data = 0");\
	if (_rvm_data->tid != 0) { \
	   rvmlib_internal_abort("_rvm_data->tid is non zero during begin transaction");\
	   }\
	rvm_init_tid(&tid);\
	_rvm_data->tid = &tid;\
	_rvm_data->list.table = NULL;\
	_rvm_data->list.count = 0;\
	_rvm_data->list.size = 0;\
\
	/* Begin the transaction. */\
	_status = rvm_begin_transaction(_rvm_data->tid, (restore_mode));\
	if (_status == RVM_SUCCESS)\
	    _status = (rvm_return_t)_setjmp(_rvm_data->abort);\
\
        if (_status == RVM_SUCCESS) {\
	/* User code goes in this block. */

#define	END_TRANSACTION(flush_mode, statusp)\
	/* User code goes in this block. */\
    }\
\
	/* End the transaction. */\
	if (_status == RVM_SUCCESS) {\
	   if (flush_mode == no_flush) {\
		_status = rvm_end_transaction(_rvm_data->tid, flush_mode);\
                if ((_status == RVM_SUCCESS) && (_rvm_data->list.table != NULL))\
                _status = (rvm_return_t)rds_do_free(&_rvm_data->list, flush_mode);\
		}\
           else {\
              /* flush mode */\
              if (_rvm_data->list.table != NULL) {\
		_status = rvm_end_transaction(_rvm_data->tid, no_flush);\
                if (_status == RVM_SUCCESS) \
                _status = (rvm_return_t)rds_do_free(&_rvm_data->list, flush);\
	      }\
              else \
                 _status = rvm_end_transaction(_rvm_data->tid, flush);\
           }\
	}\
	if (statusp)\
	*(statusp) = _status;\
\
	/* De-initialize the rvm_perthread_t object. */\
	_rvm_data->tid = 0;\
        if (_rvm_data->list.table) free(_rvm_data->list.table);\
}

#define REC(name) \
((/* (struct camlib_recoverable_segment *) */camlibRecoverableSegment)->name)


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

#endif RVM

#ifdef SPIN
/* SPIN transaction manager */

#include "spin_trans.h"
#define RVM_SUCCESS 0
#define CODA_STACK_LENGTH 0x20000      /* 128 K */
#define BEGIN_RECOVERABLE_DECLARATIONS struct camlib_recoverable_segment {
#define END_RECOVERABLE_DECLARATIONS };\
struct camlib_recoverable_segment *camlibRecoverableSegment;

#define	BEGIN_TRANSACTION(restore_mode)		\
    _tid = trans_begin();

#define	END_TRANSACTION(flush_mode, statusp)	\
    trans_commit(_tid);

#define REC(name) \
   (camlibRecoverableSegment->name)

#define MODIFY(object, newValue)					   \
do {									   \
    typeof (object) old = (object);					   \
    trans_pin(_sid, _tid,						   \
	     (char*)src-(char*)camlibRecoverableSegment,		   \
	     len,							   \
	     1, TRANS_TIMEOUT_INFINITY)					   \
    (object) = (newValue);						   \
    trans_modifyrange(_sid, _tid,					   \
		      (char*)&REC(object)-(char*)camlibRecoverableSegment, \
		      (char*)&old, sizeof(old));			   \
} while(0)

#define MODIFY_BYTES(objectPtr, newValuePtr, length)			\
do {									\
    char old[1000];							\
    memcpy(old, objectPtr, length);					\
    memcpy(objectPtr, newValuePtr, length);				\
    trans_pin(_sid, _tid,						   \
	     (char*)objectPtr-(char*)camlibRecoverableSegment,		   \
	     length,							   \
	     1, TRANS_TIMEOUT_INFINITY);				   \
    trans_modifyrange(_sid, _tid, 					\
		      (char*)objectPtr-(char*)camlibRecoverableSegment,	\
		      old, length);					\
}while(0)

#define DEBUG(level, message)  printf message


    
#endif RVM

