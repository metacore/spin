#ifndef __RVMDEF_H_
#define __RVMDEF_H_

//#define COUNT_DEREFS

#include "builtin.h"
#include "stdlib.h"
#include <fstream.h>

extern "C" {
#include "rvm.h"
}

#define BEGIN_NEW(x,y)  ( persNew = 1, new y
#define END_NEW   ); 

#define BEGIN_TEMP_NEW

#define END_TEMP_NEW 

#define MAININIT 

#define GENDB_LOCAL_VARS  

#define DB_INITIALIZE_GENDB(arg)  GenDBInitialize(arg);

#define DB_BEGIN_TRANSACTION_GENDB
			       
#define SETROOT GenDBSetRoot();

#define DB_END_TRANSACTION_GENDB  

#define DB_FINALIZE_GENDB  GenDBFinalize();

#define BENCH_LOCAL_VARS

#define DB_INITIALIZE_BENCH  BenchInitialize();

#define DB_BEGIN_TRANSACTION_BENCH BEGIN_TRANSACTION

#define DB_OPEN_DATABASE_BENCH Open_Database(argv[3]);

#define DB_SETUP_REFERENCES_BENCH 
			       
#define DB_END_TRANSACTION_BENCH END_TRANSACTION

#define DB_FINALIZE_BENCH  BenchFinalize();

#define DESIGNLIB DesignLib

#define DEFINE_TRANSACTION
#define BEGIN_TRANSACTION BeginTransaction(cur_tid);
#define END_TRANSACTION Commit(cur_tid);

extern long persNew;
extern void BeginTransaction(rvm_tid_t *);
extern void SetRange(rvm_tid_t *, char *, rvm_length_t);
extern void Commit(rvm_tid_t *);

extern rvm_tid_t *cur_tid;

#define DO_SET_RANGE 1
extern rvm_tid_t *cur_tid;
#define SETRANGE(b, c) if (DO_SET_RANGE) SetRange(cur_tid, b, c);

#endif __RVMDEF_H_
