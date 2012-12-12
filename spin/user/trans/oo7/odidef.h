#ifndef __ODIDEF_H_
#define __ODIDEF_H_

// ostore specific externs

#include <ostore/ostore.hh>
#include <ostore/coll.hh>
#include <ostore/relat.hh>
extern os_database* oo7db;
extern os_segment* single_seg;

#define BEGIN_NEW(x,y) new (os_segment::of(x)) y
// #define BEGIN_NEW(x,y) new (single_seg) y

#define END_NEW ;

#define BEGIN_TEMP_NEW

#define END_TEMP_NEW

#define MAININIT

#define GENDB_LOCAL_VARS 

#define DB_INITIALIZE_GENDB GenDBInitialize();

#define DB_BEGIN_TRANSACTION_GENDB OS_BEGIN_TXN(xact, 0, os_transaction::update); 

#define SETROOT GenDBSetRoot();

#define DB_END_TRANSACTION_GENDB OS_END_TXN(xact);

#define DB_FINALIZE_GENDB GenDBFinalize();

#define GENDB_MISC_FUNC 

#define BENCH_LOCAL_VARS

#define DB_INITIALIZE_BENCH  BenchInitialize();

#define DB_BEGIN_TRANSACTION_BENCH BEGIN_TRANSACTION

#define DB_OPEN_DATABASE_BENCH Open_Database(argv[3]);

#define DB_SETUP_REFERENCES_BENCH Setup_References();
			       
#define DB_END_TRANSACTION_BENCH END_TRANSACTION

#define DB_FINALIZE_BENCH  BenchFinalize();

#define DESIGNLIB DesignLib

#define DEFINE_TRANSACTION  os_transaction* xact;
#define BEGIN_TRANSACTION xact = os_transaction::begin();
#define END_TRANSACTION os_transaction::commit(xact);

#endif __ODIDEF_H_
