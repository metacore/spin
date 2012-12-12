//////////////////////////////////////////////////////////////////
//
// Global Variables, etc., for Benchmark Database Generation
//
//////////////////////////////////////////////////////////////////
#include <iostream.h>

#include "macrodef.h"
#include <fstream.h>
extern void *DesignLib;
#include "GenList.h"
#include "GenVHSet.h"
#include "GenVHBag.h"
#include "GenBBag.h"
#include "GenAVLIndex.h"

#include "OO7.h"
#include "BenchParams.h"
#include "GenParams.h"
#include "VarParams.h"

DBPoolRoot *DBRoot;

extern struct rusage startUsage, endUsage;
extern struct timeval startWallTime;
extern struct timeval tempWallTime;
extern struct timeval endWallTime;
extern struct timezone ignoreTimeZone;


extern void SetParams(char* configFileName);



extern double ComputeWallClockTime(struct timeval *startWallTime, 
				   struct timeval *endWallTime);
				   
extern double ComputeUserTime(struct rusage *startUsage, 
			      struct rusage *endUsage);
			      
extern double ComputeSystemTime(struct rusage *startUsage, 
	           struct rusage *endUsage);

extern void Bench(int, char **);


extern int	nextAtomicId;
extern int	nextCompositeId;
extern int	nextComplexAssemblyId;
extern int	nextBaseAssemblyId;
extern int	nextModuleId;

extern int	debugMode;
extern int	clusterLevel;
extern int	printHeap;

extern char    *types[NumTypes];
extern char *dummyptr;

int cmpint(int,int);
int cmpstr(char *, char *);

typedef GenAVLIndex<int, CompositePart*> intCPPIndex;
typedef GenAVLIndex<int, AtomicPart*> intAPPIndex;
typedef GenAVLIndex<int, Document*> intDPIndex;
typedef GenAVLIndex<char *, Document*> charDPIndex;
typedef GenAVLIndex<int, BaseAssembly*> intBAPIndex;
typedef GenAVLIndex<int, Module*> intMPIndex;

/************************************************************************/
// The following containers  are in PERSISTENT segments
/************************************************************************/

// Containers for the database and indexes.
// All variables are ***pointers***  to sets and indexes
// Design library = Set of Composite Parts

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

/****************************************************************/
// comparison functions to be used in Indexes
int
cmpint(int x, int y)
{
  return (x==y) ? 0  : ( x < y ? -1 : 1);
}

int
cmpstr(char *x, char *y)
{
  return strcmp(x,y);
}
/****************************************************************/

void
GenDB(int argc, char **argv)
{
  // declares local variables for this function 
  GENDB_LOCAL_VARS
  
  SetParams(argv[0]);

  DB_BEGIN_TRANSACTION_GENDB
  // create segment(s)
  DB_INITIALIZE_GENDB(argv[0]);

  // create containers to hold the database structure
  
  DesignLib = BEGIN_NEW(dummyptr,GenVHSet<CompositePart *>)
  END_NEW
  AtomicDB = BEGIN_NEW(DesignLib, GenVHSet<AtomicPart *>)
  END_NEW
  DesignLibIdIndex = BEGIN_NEW(DesignLib, intCPPIndex) (0,cmpint)
  END_NEW
  AtomicIdIndex = BEGIN_NEW(DesignLib, intAPPIndex) (0,cmpint)
  END_NEW
  AtomicDateIndex = BEGIN_NEW(DesignLib, intAPPIndex) (0,cmpint)
  END_NEW
  DocumentDB = BEGIN_NEW(DesignLib, GenVHSet<Document *>)
  END_NEW
  DocumentIdIndex = BEGIN_NEW(DesignLib, intDPIndex)(0,cmpint)
  END_NEW
  DocumentTitleIndex = BEGIN_NEW(DesignLib, charDPIndex) (0,cmpstr)
  END_NEW
  BaseAssemblyDB = BEGIN_NEW(DesignLib, GenVHSet<BaseAssembly *>)
  END_NEW
  BaseAssemblyIdIndex = BEGIN_NEW(DesignLib, intBAPIndex)(0,cmpint)
  END_NEW
  ModuleDB = BEGIN_NEW(DesignLib, GenVHSet<Module *>)
  END_NEW
  ModuleIdIndex = BEGIN_NEW(DesignLib, intMPIndex)(0,cmpint)
  END_NEW
  private_cp = BEGIN_NEW(DesignLib, GenList<BaseAssembly *>[TotalCompParts+1])
  END_NEW
  shared_cp = BEGIN_NEW(DesignLib, GenList<BaseAssembly *>[TotalCompParts+1])
  END_NEW

  DBRoot
    = BEGIN_NEW(DesignLib, DBPoolRoot)((GenVHSet<CompositePart *> *)DesignLib, AtomicDB, DocumentDB,
		     DesignLibIdIndex, AtomicIdIndex, AtomicDateIndex,
		     DocumentIdIndex, DocumentTitleIndex,
		     BaseAssemblyDB, BaseAssemblyIdIndex,
		     ModuleDB, ModuleIdIndex,
		     private_cp, shared_cp)
  END_NEW

  SETROOT

  if (printHeap) PrintHeapInfo("CREATED CONTAINERS");
  
  // get wall clock time
  gettimeofday(&startWallTime, &ignoreTimeZone);
  
  
  // Start the transaction
    
  // get starting usage values.
      getrusage(RUSAGE_SELF, &startUsage);
  
  
  //////////////////////////////////////////////////////////////////
  //
  // Do Benchmark Data Generation
  //
  //////////////////////////////////////////////////////////////////
  
  // set random number seed.
  srandom(1);

  // First generate the desired number of modules, each in its own
  // segment
  while (nextModuleId <= TotalModules) 
    {
      //ModulePool[nextModuleId-1]->PrintStats(cout);
      cout << "Generating module " << nextModuleId << "\n"; 
      int id = nextModuleId++;
      (ModuleDB)->add(BEGIN_NEW(DesignLib,Module) (id))
      END_NEW
    }

  gettimeofday(&tempWallTime, &ignoreTimeZone);
  printf("Wall clock time to generate modules: %f seconds.\n", 
	 ComputeWallClockTime(&startWallTime, &tempWallTime)); fflush(stdout);
  if(printHeap) PrintHeapInfo("CREATED ASSEMBLIES");  
  // Generate the desired number of composite parts in a design
  // library segment, with each part being in its own cluster
  
  int id;
  CompositePart* comp;
  
  while (nextCompositeId <= TotalCompParts) 
    {
      id = nextCompositeId++;
      if(debugMode)
	cout << ">>> creating composite part " << id << " itself\n";
      
      comp = BEGIN_NEW(DesignLib, CompositePart) (id)
      END_NEW
      
      ((GenVHSet<CompositePart *>*)(DesignLib))->add(comp);
      
      if (debugMode) {
	cout << ">>> done creating composite part " << id << "\n";
      }
      
    }
  gettimeofday(&tempWallTime, &ignoreTimeZone);
  printf("Wall clock time to generate composite parts: %f seconds.\n", 
	 ComputeWallClockTime(&startWallTime, &tempWallTime)); fflush(stdout);

  if(printHeap) PrintHeapInfo("CREATED DESIGNLIB");  

  // Create indexes on ...
  Pix cp;
  // Index the CompositeParts by ID
  for(cp = ((GenVHSet<CompositePart *>*)(DesignLib))->first();cp;
      ((GenVHSet<CompositePart *>*)(DesignLib))->next(cp))
    {
      if(!cp)
	{ cout << "cp is null\n"; cout.flush(); exit(1);}
      (*DesignLibIdIndex)[(*((GenVHSet<CompositePart *>*)DesignLib))(cp)->id] = 
	(*((GenVHSet<CompositePart *>*)DesignLib))(cp);
    }

  gettimeofday(&tempWallTime, &ignoreTimeZone);
  printf("Wall clock time to generate composite part index: %f seconds.\n", 
	 ComputeWallClockTime(&startWallTime, &tempWallTime)); fflush(stdout);

  if(printHeap) PrintHeapInfo("CREATED COMPOSITE INDEX");  
  // ... Parts
  Pix ap;
  // Index the AtomicParts (both id and date fields)

  for(ap = (AtomicDB)->first();ap;(AtomicDB)->next(ap))
    {
      if(!ap)
      	{ cout << "ap is null\n"; cout.flush(); exit(1);}
      (*AtomicIdIndex)[(*AtomicDB)(ap)->id] = 
	(*AtomicDB)(ap);
      (*AtomicDateIndex)[(*AtomicDB)(ap)->buildDate] = 
	(*AtomicDB)(ap);      
    }

  gettimeofday(&tempWallTime, &ignoreTimeZone);
  printf("Wall clock time to generate atomic part index: %f seconds.\n", 
	 ComputeWallClockTime(&startWallTime, &tempWallTime)); fflush(stdout);

  if(printHeap) PrintHeapInfo("CREATED ATOMIC INDEX");  
  // .... and Documents
  Pix dp;
  // Index the Documents

  for(dp = (DocumentDB)->first();dp;(DocumentDB)->next(dp))
    {
      if(!dp)
      	{ cout << "dp is null\n"; cout.flush(); exit(1);}
      (*DocumentIdIndex)[(*DocumentDB)(dp)->id] = 
	(*DocumentDB)(dp);
      (*DocumentTitleIndex)[(char *&)(*DocumentDB)(dp)->title] = 
	(*DocumentDB)(dp);
    }

  gettimeofday(&tempWallTime, &ignoreTimeZone);
  printf("Wall clock time to generate document index: %f seconds.\n", 
	 ComputeWallClockTime(&startWallTime, &tempWallTime)); fflush(stdout);

  if(printHeap) PrintHeapInfo("CREATED DOC INDEX");    
  // ....  Base Assemblies 
  Pix ba;
  // Index the BaseAssembly by id
  // also need to index priv->doc->title

  for(ba = (BaseAssemblyDB)->first();ba;(BaseAssemblyDB)->next(ba))
    {
      (*BaseAssemblyIdIndex)[(*BaseAssemblyDB)(ba)->id] = 
	(*BaseAssemblyDB)(ba);
    }

  gettimeofday(&tempWallTime, &ignoreTimeZone);
  printf("Wall clock time to generate base assembly index: %f seconds.\n", 
	 ComputeWallClockTime(&startWallTime, &tempWallTime)); fflush(stdout);

  if(printHeap) PrintHeapInfo("CREATED BA INDEX");    

  // ..... and finally, Modules
  Pix mod;
  // Index the Modules

  for(mod = (ModuleDB)->first();mod;(ModuleDB)->next(mod))
    {
      (*ModuleIdIndex)[(*ModuleDB)(mod)->id] = 
	(*ModuleDB)(mod);
    }
  gettimeofday(&tempWallTime, &ignoreTimeZone);
  printf("Wall clock time to generate module index: %f seconds.\n", 
	 ComputeWallClockTime(&startWallTime, &tempWallTime)); fflush(stdout);

  // do whatever is necessary to ensure persistence

  DB_END_TRANSACTION_GENDB

  // and close the database
  DB_FINALIZE_GENDB

  // Print out some useful information about what happened
  
  cout << "=== DONE CREATING DATABASE, TOTALS WERE ===\n";
  cout << " # atomic parts\t\t " <<  nextAtomicId-1 << "\n";
  cout << " # composite parts\t\t " <<  nextCompositeId << "\n";
  cout << " # complex assemblies\t " <<  nextComplexAssemblyId << "\n";
  cout << " # base assemblies\t\t " <<   nextBaseAssemblyId-1 <<"\n";
  cout << " # modules\t\t\t" << nextModuleId-1 << "\n";
  cout.flush();

	 
  // Exit
  exit(0);
}

