#include <math.h>
#include "macrodef.h"

#include "GenList.h"
#include "GenVHSet.h"
#include "GenVHBag.h"
#include "GenBBag.h"
#include "GenAVLIndex.h"

#include "OO7.h"
#include "BenchParams.h"
#include "GenParams.h"
#include "VarParams.h"

#include <fstream.h>

os_database* oo7db;
os_segment* single_seg;

long NumMallocs;
long NumBytes;
long NumDerefs;
char *dummyptr;
extern DBPoolRoot *DBRoot;
extern int debugMode;
extern int printHeap;
extern int TotalDBSize;

extern struct rusage startUsage, endUsage;
extern struct timeval startWallTime;
extern struct timeval tempWallTime;
extern struct timeval endWallTime;
extern struct timezone ignoreTimeZone;

extern double ComputeWallClockTime(struct timeval *startWallTime, 
				   struct timeval *endWallTime);
				   
extern double ComputeUserTime(struct rusage *startUsage, 
			      struct rusage *endUsage);
			      
extern double ComputeSystemTime(struct rusage *startUsage, 
	           struct rusage *endUsage);

extern void Compute_Database_Sizes();

// Containers for the database and indexes.
// All variables are ***pointers***  to sets and indexes
// Design library = Set of Composite Parts

extern void *DesignLib;
extern GenAVLIndex<int, CompositePart *> *DesignLibIdIndex;

// Set of Atomic Parts
extern GenVHSet<AtomicPart *> *AtomicDB;
extern GenAVLIndex<int, AtomicPart *> *AtomicIdIndex;
extern GenAVLIndex<int, AtomicPart *> *AtomicDateIndex;

// Set of Documents
extern GenVHSet<Document *> *DocumentDB;
extern GenAVLIndex<int, Document *> *DocumentIdIndex;
extern GenAVLIndex<char *, Document *> *DocumentTitleIndex;

// Set of Assemblies
extern GenVHSet<BaseAssembly *> *BaseAssemblyDB;
extern GenAVLIndex<int, BaseAssembly *> *BaseAssemblyIdIndex;

// Data structures to hold the database
// Set of Modules
extern GenVHSet<Module *> *ModuleDB;
extern GenAVLIndex<int, Module *> *ModuleIdIndex;

extern GenList<BaseAssembly *> *private_cp;
extern GenList<BaseAssembly *> *shared_cp;

// the function IncrementDerefCount  counts (or will eventually count) 
// the total number of dereferences made to all objects in the program
void
IncrementDerefCount()
{
  ++NumDerefs;
}

void
PrintHeapInfo(char* initstr)
{
  printf("%s\n", initstr); fflush(stdout);
  printf("Persistent heap stats\n"); fflush(stdout);
  printf("****** Total number of mallocs in program = %ld******\n", NumMallocs);
  printf("****** Total size of mallocs in program = %ld Bytes******\n", NumBytes);
  fflush(stdout);
}


void
GenDBInitialize()
{ 

  Compute_Database_Sizes(); 
  char *dbname = getenv("DBNAME");
  if (! dbname) {
    dbname = "/tmp/os_db";
  }
  oo7db = os_database::create(dbname, 0664, 1);
  if (oo7db == NULL) {
    fprintf(stderr, "ERROR: Cannot open OO7 Database.\n");
    os_transaction::abort();
    exit(1);
  }
  oo7db->set_read_whole_segment(0);
  
  single_seg = oo7db->create_segment();
  single_seg->set_size((int) (1.0 * TotalDBSize));

  dummyptr = new (single_seg) char;
}

void
GenDBSetRoot()
{
  os_database_root *dbroot = oo7db->create_root("DBROOT");
  dbroot->set_value(DBRoot);
}


void
GenDBFinalize()
{
  gettimeofday(&endWallTime, &ignoreTimeZone);
  printf("Wall-Clock time to generate database: %f seconds.\n",
	 ComputeWallClockTime(&startWallTime, &endWallTime));
  getrusage(RUSAGE_SELF, &endUsage);
  printf("System time: %f seconds, User time: %f seconds\n", 
	 ComputeSystemTime(&startUsage, &endUsage),
	 ComputeUserTime(&startUsage, &endUsage));
  cout.flush();
  // Close the Database
  oo7db->close();
}

void
ResolveSegment()
{
  os_database_root *dbroot = oo7db->find_root("DBROOT");
  DBRoot = (DBPoolRoot*)dbroot->get_value();
}

void
GetSetsAndIndexes()
{
  DesignLib = (GenVHSet<CompositePart *> *)DBRoot->DesignLib;
  AtomicDB = (GenVHSet<AtomicPart *> *)DBRoot->AtomicDB;

  DocumentDB = (GenVHSet<Document *> *)DBRoot->DocumentDB;

  DesignLibIdIndex = (GenAVLIndex<int, CompositePart *> *)
    DBRoot->DesignLibIdIndex;
  AtomicIdIndex = (GenAVLIndex<int, AtomicPart *> *)DBRoot->AtomicIdIndex;
  AtomicDateIndex = (GenAVLIndex<int, AtomicPart *> *)
    DBRoot->AtomicDateIndex;

  DocumentIdIndex = (GenAVLIndex<int, Document *> *)DBRoot->DocumentIdIndex;
  DocumentTitleIndex = (GenAVLIndex<char *, Document *> *)
    DBRoot->DocumentTitleIndex;

  BaseAssemblyDB = (GenVHSet<BaseAssembly *> *)DBRoot->BaseAssemblyDB;
  BaseAssemblyIdIndex = (GenAVLIndex<int, BaseAssembly *> *)
    DBRoot->BaseAssemblyIdIndex;
  ModuleDB = (GenVHSet<Module *> *)DBRoot->ModuleDB;
  ModuleIdIndex = (GenAVLIndex<int, Module *> *)DBRoot->ModuleIdIndex;
  private_cp = (GenList<BaseAssembly *> *)DBRoot->private_cp;
  shared_cp = (GenList<BaseAssembly *> *)DBRoot->shared_cp;
}

void
Open_Database(char *arg)
{
  arg = arg;

  objectstore::set_cache_size(CachePages * 8192);
  printf("Initialized system with call objectstore::set_cache_size(%d)\n", CachePages * 8192);
  // Now open the database, disable whole-segment transfers (in favor
  // of cluster-level ones), and start up a transaction

  char *dbname = getenv("DBNAME");
  if (! dbname) {
    dbname = "/tmp/os_db";
  }
  oo7db = os_database::open(dbname);
  if (oo7db == NULL) {
    fprintf(stderr, "ERROR: Cannot open OO7 Database.\n");
    exit(1);
  }
  oo7db->set_read_whole_segment(0);
}

void Setup_References()
{
  ResolveSegment();
  single_seg = os_segment::of(DBRoot);
  single_seg->set_fetch_policy(os_fetch_page, 8192);
//  single_seg->set_fetch_policy(os_fetch_segment, 8192);
  GetSetsAndIndexes();
}

void
BenchInitialize()
{ 
  Compute_Database_Sizes(); 
}


void
BenchFinalize()
{
  oo7db->close();
  printf("*** Total number of dereferences in the program = %ld***\n", NumDerefs);
}
