#include <iostream.h>

#include "macrodef.h"
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

#include <fstream.h>

extern int RealWork;         // set to one to make DoNothings do work.
extern int WorkAmount;		// controls how much work DoNothings do

extern void SetParams(char* configFileName);

extern int  query1();
extern int  query2();
extern int  query3();
extern int  query4();
extern int  query5();
extern int  query7();
extern int  query8();
extern void insert1();
extern void delete1();

extern double ComputeWallClockTime(struct timeval *startWallTime, 
			           struct timeval *endWallTime);

extern double ComputeUserTime(struct rusage *startUsage, 
	        	      struct rusage *endUsage);

extern double ComputeSystemTime(struct rusage *startUsage, 
	 	                struct rusage *endUsage);


extern long ComputeNumPageFaults(struct rusage *startUsage,
                                struct rusage *endUsage);

extern long ComputeNumSignals(struct rusage *startUsage,
                                struct rusage *endUsage);

extern long ComputeNumSwaps(struct rusage *startUsage,
                                struct rusage *endUsage);


//////////////////////////////////////////////////////////////////
//
// Global Variables, etc., for Benchmarking ODB Operations
//
/////////////////////////////////////////////////////////////////
extern struct rusage startUsage, endUsage;
extern struct timeval startWallTime;
extern struct timeval endWallTime;
extern struct timezone ignoreTimeZone;
extern struct timeval startWarmTime;

extern int	nextAtomicId;
extern int	nextCompositeId;
extern int	nextComplexAssemblyId;
extern int	nextBaseAssemblyId;
extern int	nextModuleId;
extern int	debugMode;
extern int	clusterLevel;

extern char    *types[NumTypes];

BENCH_LOCAL_VARS

// Containers for the database and indexes.
// All variables are ***pointers***  to sets and indexes
// Design library = Set of Composite Parts

// extern GenVHSet<CompositePart *> *DesignLib;
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

void SetupPrefetch(Module* module)
{
  module = module;
  printf("Prefetching done\n");
}

///////////////////////////////////////////////////////////////////////////
// ParseCommandLine parses the original shell call to "bench", determining 
// which operation to run, how many times to run it, and whether the
// individual runs are distinct transactions or are lumped together
// into one large transaction.
//////////////////////////////////////////////////////////////////////////
void ParseCommandLine(int argc, 
		      char**argv, 
		      int opIndex,
		      int& repeatCount,
                      BenchmarkOp& whichOp,
	              int& manyXACTS) 
{

  repeatCount = repeatCount;
  manyXACTS = manyXACTS;

  cout << "Call was: ";
  for (int foo = 0; foo < argc; foo++) {
    cout << "  " << argv[foo];
  }
  cout << "\n";
  
  cout << "opIndex = " << opIndex << "\n";
  
  if (strcmp(argv[opIndex], "t1") == 0) {
    whichOp = Trav1;
  } else if (strcmp(argv[opIndex], "t1ww") == 0) {
    whichOp = Trav1WW;
    if (debugMode == TRUe) WorkAmount = atoi(argv[argc-2]);
    else WorkAmount = atoi(argv[argc-1]);
    cout << "WorkAmount = " <<  WorkAmount << "\n";
  } else if (strcmp(argv[opIndex], "t2a") == 0) {
    whichOp = Trav2a;
  } else if (strcmp(argv[opIndex], "t2b") == 0) {
    whichOp = Trav2b;
  } else if (strcmp(argv[opIndex], "t2c") == 0) {
    whichOp = Trav2c;
  } else if (strcmp(argv[opIndex], "t3a") == 0) {
    whichOp = Trav3a;
  } else if (strcmp(argv[opIndex], "t3b") == 0) {
    whichOp = Trav3b;
  } else if (strcmp(argv[opIndex], "t3c") == 0) {
    whichOp = Trav3c;
  } else if (strcmp(argv[opIndex], "t4") == 0) {
    whichOp = Trav4;
  } else if (strcmp(argv[opIndex], "t5do") == 0) {
    whichOp = Trav5do;
  } else if (strcmp(argv[opIndex], "t5undo") == 0) {
    whichOp = Trav5undo;
  } else if (strcmp(argv[opIndex], "t6") == 0) {
    whichOp = Trav6;
  } else if (strcmp(argv[opIndex], "t7") == 0) {
    whichOp = Trav7;
  } else if (strcmp(argv[opIndex], "t8") == 0) {
    whichOp = Trav8;
  } else if (strcmp(argv[opIndex], "t9") == 0) {
    whichOp = Trav9;
  } else if (strcmp(argv[opIndex], "t10") == 0) {
    whichOp = Trav10;
  } else if (strcmp(argv[opIndex], "q1") == 0) {
    whichOp = Query1;
  } else if (strcmp(argv[opIndex], "q1ww") == 0) {
    whichOp = Query1WW;
  } else if (strcmp(argv[opIndex], "q2") == 0) {
    whichOp = Query2;
  } else if (strcmp(argv[opIndex], "q3") == 0) {
    whichOp = Query3;
  } else if (strcmp(argv[opIndex], "q4") == 0) {
    whichOp = Query4;
  } else if (strcmp(argv[opIndex], "q5") == 0) {
    whichOp = Query5;
  } else if (strcmp(argv[opIndex], "q6") == 0) {
    whichOp = Query6;
  } else if (strcmp(argv[opIndex], "q7") == 0) {
    whichOp = Query7;
  } else if (strcmp(argv[opIndex], "q8") == 0) {
    whichOp = Query8;
  } else if (strcmp(argv[opIndex], "i") == 0) {
    whichOp = Insert;
  } else if (strcmp(argv[opIndex], "d") == 0) {
    whichOp = Delete;
  } else if (strcmp(argv[opIndex], "r1") == 0) {
    whichOp = Reorg1;
  } else if (strcmp(argv[opIndex], "r2") == 0) {
    whichOp = Reorg2;
  } else if (strcmp(argv[opIndex], "m1") == 0) {
    whichOp = MultiTrav1;
  } else if (strcmp(argv[opIndex], "m2") == 0) {
    whichOp = MultiTrav2;
  } else if (strcmp(argv[opIndex], "m3") == 0) {
    whichOp = MultiTrav3;
  } else if (strcmp(argv[opIndex], "m4") == 0) {
    whichOp = MultiTrav4;
  } else if (strcmp(argv[opIndex], "m5") == 0) {
    whichOp = MultiTrav5;
  } else if (strcmp(argv[opIndex], "m6") == 0) {
    whichOp = MultiTrav6;
  } else if (strcmp(argv[opIndex], "wu") == 0) {
    whichOp = WarmUpdate;
  } else {
    cout <<  "ERROR: Illegal OO7 operation specified " 
      << argv[opIndex] << "\n";
    cout <<  "Currently supported <op>s: t1, t1ww, t2a, t2b, t2c, t3a, t3b, t3c\n";
    cout <<  "\t t4, t5do, t5undo, t6, t7, t8, t9, q1, q1ww, q2, q3, q4, q5, q6, q7, q8\n";
    cout <<  "\t i, d, r1, r2, m1, m2, m3, m4, m5, m6.\n";
    exit(1);
  }
}                

void
FindNumOps(int argc, char **argv, int& numops)
{
    numops = argc - 4;
}

int
Bench(int argc, char** argv)
{
  char resultText[200];  // to hold result message in order to avoid
  // printf in timing section.
  
  //////////////////////////////////////////////////////////////////
  //
  // Initialize
  //
  //////////////////////////////////////////////////////////////////
  
  // Compute structural info needed by the update operations,
  // since these operations need to know which id's should
  // be used next.

  int baseCnt = NumAssmPerAssm;
  int complexCnt = 1;
  for (int i = 1; i < NumAssmLevels-1; i++) {
    baseCnt = baseCnt * NumAssmPerAssm;
    complexCnt += complexCnt * NumAssmPerAssm;
  }
  nextBaseAssemblyId = TotalModules*baseCnt + 1;
  nextComplexAssemblyId = TotalModules*complexCnt + 1;
  nextAtomicId = TotalAtomicParts + 1;
  nextCompositeId = TotalCompParts + 1;
  
  int opIndex=3;
  int repeatCount = 1;
  BenchmarkOp whichOp = Trav1;
  int manyXACTS = 0;
  
  // first find out number of operations requested
  int NumOps;
  FindNumOps(argc,argv,NumOps);
  cout << "Number of Ops = " << NumOps << "\n";
  cout.flush();

  // see if debug option is specified
  // also see if single/many transactions
  if (strcmp(argv[argc-1], "many") == 0) {
      manyXACTS = 1;
  } 

  SetParams(argv[0]); // read config file

  if ((repeatCount = atoi(argv[2])) <= 0) 
    {
      cout <<  "repeatCount should be positive\n";
      cout << "taking a default value of 1 for repeatCount\n";
      repeatCount = 1;
    } 

  // get access to the segments containing the database containers

  DB_OPEN_DATABASE_BENCH(argv[1]);
  DB_SETUP_REFERENCES_BENCH
  DB_INITIALIZE_BENCH
  for(int i = 0; i< NumOps;i++)
    {
      ParseCommandLine(argc, argv, opIndex+i,repeatCount, whichOp, manyXACTS); 

      // Actually run the darn thing.
      for (int iter = 0; iter < repeatCount; iter++) 
	{

	  //////////////////////////////////////////////////////////////////
	  //
	  // Run an OO7 Benchmark Operation
	  //
	  //////////////////////////////////////////////////////////////////
	  cout << "RUNNING OO7 BENCHMARK OPERATION " 
	    <<  argv[opIndex+i] << " iteration = " <<  iter << "\n";
	  cout.flush();

	  // get wall clock time
	  gettimeofday(&startWallTime, &ignoreTimeZone);
	  
	  // get starting usage values.
	  getrusage(RUSAGE_SELF, &startUsage);
	  
	  // Start a new transaction if either this is the first iteration
	  // of a multioperation transaction or we we are running each
	  // operation as a separate transaction
	  
	  if ((iter == 0) || (manyXACTS)) {
	    cout << "begin_transaction()\n";
	    BEGIN_TRANSACTION
	    cout.flush();
	}
	  
	  
	  // set random seed so "hot" runs are truly hot
	  srandom(1);
	  for (Pix mod1 = (ModuleDB)->first(); mod1; (ModuleDB)->next(mod1)) {
	      Module** module = (Module**)mod1;

	      if(!module)
		{
		    cout << "module not found\n";
		    exit(0);
		}
	      // Perform the requested operation on the chosen module
	  
	      int count = 0;
	      RealWork  = 0;    // default is not to do work.

	      if (clusterLevel == 2) SetupPrefetch(*module);
      
	  switch (whichOp) {
	  case Trav1:
	    count = (*module)->traverse(whichOp);
	    sprintf(resultText, "Traversal 1 DFS visited %d parts.\n",
		    count);
	    break;
	  case Trav1WW:
	    RealWork = 1;
	    whichOp = Trav1;    // so traverse methods work correctly
	    count = (*module)->traverse(whichOp);
	    whichOp = Trav1WW;  // for next (hot) iteration
	    sprintf(resultText, "Traversal 1 WW DFS visited %d atomic parts.\n",
		    count);
	    break;
	  case Trav2a:
	    count = (*module)->traverse(whichOp);
	    sprintf(resultText, "Traversal 2A swapped %d pairs of (X,Y) coordinates.\n",
		    count);
	    break;
	  case Trav2b:
	    count = (*module)->traverse(whichOp);
	    sprintf(resultText, "Traversal 2B swapped %d pairs of (X,Y) coordinates.\n",
		    count);
	    break;
	  case Trav2c:
	    count = (*module)->traverse(whichOp);
	    sprintf(resultText, "Traversal 2C swapped %d pairs of (X,Y) coordinates.\n",
		    count);
	    break;
	  case Trav3a:
	    count = (*module)->traverse(whichOp);
	    sprintf(resultText, "Traversal 3A toggled %d dates.\n",
		    count);
	    //cout << "running query2 from here\n";
	    //query2();
	    break;
	  case Trav3b:
	    count = (*module)->traverse(whichOp);
	    sprintf(resultText, "Traversal 3B toggled %d dates.\n",
		    count);
	    break;
	  case Trav3c:
	    count = (*module)->traverse(whichOp);
	    sprintf(resultText, "Traversal 3C toggled %d dates.\n",
		    count);
	    break;
	  case Trav4:
	            count = (*module)->traverse(whichOp);
	    sprintf(resultText, "Traversal 4: %d instances of the character found\n",
		    count);
	    break;
	  case Trav5do:
	    count = (*module)->traverse(whichOp);
	    sprintf(resultText, "Traversal 5(DO): %d string replacements performed\n",
		    count);
	    break;
	  case Trav5undo:
	    count = (*module)->traverse(whichOp);
	    sprintf(resultText, "Traversal 5(UNDO): %d string replacements performed\n",
		    count);
	    break;
	  case Trav6:
	    count = (*module)->traverse(whichOp);
	    sprintf(resultText, "Traversal 6: visited %d atomic root parts.\n",
		    count);
	    break;
	    /*
	       case Trav7:
	       count = traverse7();
	       sprintf(resultText, "Traversal 7: found %d assemblies using random atomic part.\n",
	       count);
	       break;
	       */
	  case Trav8:
	    count = (*module)->scanManual();
	    sprintf(resultText, "Traversal 8: found %d instances of char in manual.\n",
		    count);
	    break;
	  case Trav9:
	    count = (*module)->firstLast();
	    sprintf(resultText, "Traversal 9: match was %d.\n",
		    count);
	    break;
	  case Query1:
	    cout << "calling query1\n"; cout.flush();
	    count = query1();
	    sprintf(resultText, "Query one retrieved %d atomic parts.\n",
		    count);
	    break;
	  case Query2:
	    cout << "calling query2\n"; cout.flush();
	    count = query2();
	    sprintf(resultText, "Query two retrieved %d qualifying atomic parts.\n",
		    count);
	    break;
	  case Query3:
	    count = query3();
	    sprintf(resultText, "Query three retrieved %d qualifying atomic parts.\n",
		    count);
	    break;
	  case Query4:
	    count = query4();
	    sprintf(resultText, "Query four retrieved %d (document, base assembly) pairs.\n",
		    count);
	    break;
	  case Query5:
	count = query5();
	    sprintf(resultText, "Query five retrieved %d out-of-date base assemblies.\n",
		    count);
	    break;
	  case Query8:
	    count = query8();
	    sprintf(resultText, "Query eight found %d atomic part/document matches.\n",
		    count);
	    break;
	  case Query7:
	    count = query7();
	sprintf(resultText, "Query seven iterated through %d atomic parts.\n",
		count);
	    break;
	  case Insert:
	    insert1();
	    sprintf(resultText, "Inserted %d composite parts (a total of %d atomic parts.)\n",
		    NumNewCompParts, NumNewCompParts*NumAtomicPerComp);
	    break;
	  case Delete:
	    delete1();
	    sprintf(resultText, "Deleted %d composite parts (a total of %d atomic parts.)\n",
		    NumNewCompParts, NumNewCompParts*NumAtomicPerComp);
	    break;
	  default:
	    cout <<  "Sorry, that operation isn't available yet.\n";
	    // need an abort_transaction() here
	    exit(1);
	  }
          }

	  if ((iter == repeatCount-1) || (manyXACTS)) {
	    END_TRANSACTION
	    // end this transaction
	    cout << "commit_transaction()\n";
	  }

	  // compute and report wall clock time
	  gettimeofday(&endWallTime, &ignoreTimeZone);
	  // Compute and report CPU time.
	  getrusage(RUSAGE_SELF, &endUsage);

	  printf("operation= %s, iteration= %d.\n", argv[opIndex+i], iter);
	  printf("Wallclock elapsedTime = %f seconds\n",
		 ComputeWallClockTime(&startWallTime, &endWallTime));
	  
	  if (iter == 0) startWarmTime = startWallTime;
	  
	  // now print result string
	  cout <<  resultText;
	  printf("CPU time: %f seconds",
		 ComputeUserTime(&startUsage, &endUsage) 
		 + ComputeSystemTime(&startUsage, &endUsage));

	  printf(" %f seconds user, %f seconds system.\n", 
		 ComputeUserTime(&startUsage, &endUsage), 
		 ComputeSystemTime(&startUsage, &endUsage));

          printf(" %ld page faults %ld signals %ld swaps.\n",
                  ComputeNumPageFaults(&startUsage, &endUsage),
                  ComputeNumSignals(&startUsage, &endUsage),
                  ComputeNumSwaps(&startUsage, &endUsage));

	  if ((repeatCount > 1) && (iter == repeatCount-1))
	    {
	      printf("OS, operation= %s, average hot elapsedTime= %f seconds\n",
		     argv[opIndex+i], ComputeWallClockTime(&startWarmTime, 
					   &endWallTime)/(repeatCount-1));
	      
	    }
	}
    }
  
  //////////////////////////////////////////////////////////////////
  //
  // Shutdown
  //
  //////////////////////////////////////////////////////////////////

  //  anything to be done before closing up 
  DB_FINALIZE_BENCH;
  
}

