#ifndef __SPIN_DEF_H
#define __SPIN_DEF_H

/* SPIN transaction manager */
#include <spin_trans.h>
#include <rvm.h> /* this is a spoofed version provided by spin trans. */

#define RVM_SUCCESS 0

extern struct root_struct *_root;
extern long _sid;
extern long _tid;

#define	BEGIN_TRANSACTION() _tid = trans_begin(trans_mode)
#define	END_TRANSACTION() trans_commit(_tid)

#define REC(name) _root->name

#define MODIFY(object, newValue)					   \
    (object) = (newValue);

#define MODIFY_BYTES(objectPtr, newValuePtr, length)			\
    memcpy(objectPtr, newValuePtr, length)


#define DEBUG(level, message)  printf message


#endif
