#ifndef __OPALDEF_H_
#define __OPALDEF_H_

//#define COUNT_DEREFS

#include "builtin.h"
#include "stdlib.h"
#include <fstream.h>
#include "threads/ThreadPackage.h"
#include "sets/List.h"
#include "opal/Opal.h"
#include "misc/MemoryPool.h"

extern MemoryPool *__savedMP;
extern MemoryPool *__newMP;
extern void SwitchToTemp();
extern void SwitchBackToOriginal();

extern int MyPushMemoryPool(int , void *);
extern void MyPopMemoryPool();


#define BEGIN_NEW(x,y)  ( MyPushMemoryPool(sizeof(y), x) , new y

#define END_NEW   ); MyPopMemoryPool();

#define BEGIN_TEMP_NEW  SwitchToTemp();

#define END_TEMP_NEW  SwitchBackToOriginal();

#define MAININIT PrimeUnixThreads(); LocalRuntimeInit(0);

#define GENDB_LOCAL_VARS  

#define DB_INITIALIZE_GENDB  GenDBInitialize();

#define DB_BEGIN_TRANSACTION_GENDB
			       
#define SETROOT GenDBSetRoot();

#define DB_END_TRANSACTION_GENDB  

#define DB_FINALIZE_GENDB  GenDBFinalize();

#define BENCH_LOCAL_VARS

#define DB_INITIALIZE_BENCH  BenchInitialize();

#define DB_BEGIN_TRANSACTION_BENCH BEGIN_TRANSACTION

#define DB_OPEN_DATABASE_BENCH(fileName) Open_Database(fileName);

#define DB_SETUP_REFERENCES_BENCH Setup_References();
			       
#define DB_END_TRANSACTION_BENCH END_TRANSACTION

#define DB_FINALIZE_BENCH  BenchFinalize();

#define DESIGNLIB DesignLib

#define DEFINE_TRANSACTION
#define BEGIN_TRANSACTION
#define END_TRANSACTION


#endif __OPALDEF_H_
