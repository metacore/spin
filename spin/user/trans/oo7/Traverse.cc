//////////////////////////////////////////////////////////////////
//
// Traverse - DFS traverse module.  Upon reaching a base assembly,
// visit all referenced composite parts.  At each composite part,
// take an action that depends on which traversal variant has been
// requested.
//
///////////////////////////////////////////////////////////////////
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

#ifdef LOG
#include "Logger.h"
#endif

extern int	debugMode;

extern GenAVLIndex<int, AtomicPart *> *AtomicDateIndex;
extern void 	PrintOp(BenchmarkOp op);

//////////////////////////////////////////////////////////////////
//
// Module Method for Traversal
//
//////////////////////////////////////////////////////////////////


int Module::traverse(BenchmarkOp op)
{
#ifdef LOG
  Logger l(this, 0);
#endif
  if (debugMode) {
    cout << "Module::traverse id = " << id << " type = " << type << " op = ";
    PrintOp(op);
    cout << "\n";
    cout.flush();
  }
  
  // now traverse the assembly hierarchy
  if(!designRoot) 
    {
      cout << " designRoot is null\n";
      cout.flush();
      exit(1);
    }

  // set the threads memorypool to designlibindexpool
  // because that is the only index for which a new and delete are required
  // in the OO7 traversals
  int ct =(this->designRoot)->traverse(op);
  return ct;
}


//////////////////////////////////////////////////////////////////
//
// ComplexAssembly Method for Traversal
//
//////////////////////////////////////////////////////////////////

int ComplexAssembly::traverse(BenchmarkOp op)
{
#ifdef LOG
  Logger l(this, 0);
#endif
  if (debugMode) {
    cout << "\tComplexAssembly::traverse id = " << id << " op = ";
    cout.flush();
    PrintOp(op);
    cout << "\n";
    cout.flush();
  }

  // prepare for and execute the traversal
  int count = 0;
  Pix p;

  if (op < MultiTrav1) 
    {
      //cout << "making full traversal of assembly hierarchy\n";
      cout.flush();
      // fully traverse the assembly hierarchy
      for(p = (Assembly **)subAssemblies->first(); p; subAssemblies->next(p))
	{
	  //cout << "inside complex assembly traversal loop\n";
	  cout.flush();
	  count += (*(Assembly **)p)->traverse(op);    
	}
    } 
  else 
    {
      //cout << "making a random walk of the assm hierarchy\n";
      //cout.flush();
      // randomly walk down the assembly hierarchy
      int curPath = 0;
      int randPath = (int) (random() % NumAssmPerAssm);
      for(p = (Assembly **)subAssemblies->first(); p; subAssemblies->next(p))
	{
	  if (curPath++ == randPath) 
	    {
	      count += (*(Assembly **)p)->traverse(op);
	      break;
	    }
	}
    }
  
  // lastly, return the result
  return count;
}



int BaseAssembly::traverse(BenchmarkOp op)
{
#ifdef LOG
  Logger l(this, 0);
#endif
  if(debugMode) 
    {
      cout << "\t\tBaseAssembly::traverse id = " << id << " op = ";
      PrintOp(op);
      cout << "\n";
    }
  
  
  int count = 0;
  CompositePart* comp;
  Pix p;
  
  if(op<MultiTrav1)
    {
      for( p = (CompositePart **)componentsPriv->first(); p;
	      componentsPriv->next(p))
	count += (*(CompositePart**)p)->traverse(op);
    }
  else
    {
      // first, traverse private and/or shared composite parts
      
      if (op < MultiTrav3 || op > MultiTrav4) {
	for(p = (CompositePart **)componentsPriv->first();
	    p;componentsPriv->next(p))
	  count += (*(CompositePart**)p)->traverse(Trav1);
      }
      
      if (op > MultiTrav2) {
	for(p = (CompositePart **)componentsShar->first();
	    p;componentsShar->next(p))
	  count += (*(CompositePart**)p)->traverse(Trav1);
      }
    
      // next, sleep for awhile to reduce server disk contention
      
      // sleep(MultiSleepTime);
      
      // also, perform an update traversal if one is desired
      
      if (op == MultiTrav2 || op == MultiTrav5 || op == MultiTrav6) {
	for(p = (CompositePart **)componentsPriv->first();
	    p;componentsPriv->next(p))
	  count += (*(CompositePart**)p)->traverse(Trav2b);
      } else if (op == MultiTrav4) {
	for(p = (CompositePart **)componentsShar->first();
	    p;componentsShar->next(p))
	  count += (*(CompositePart**)p)->traverse(Trav2b);
      }
    
    }
  
  return count;
}

//////////////////////////////////////////////////////////////////
//
// CompositePart Method for Traversal
//
//////////////////////////////////////////////////////////////////


int CompositePart::traverse(BenchmarkOp op)
{
#ifdef LOG
  Logger l(this, 0);
#endif
  if (debugMode) {
    cout << "\t\t\tCompositePart::traverse(id = " << id << " op = ";
    PrintOp(op);
    cout << ")\n";
  }
  
  if ((op >= Trav1) && (op <= Trav3c)) {
    
    // do parameterized DFS of atomic part graph
    PartIdSet visitedIds;
    return rootPart->traverse(op, visitedIds);
  } else if (op == Trav4) {
    
    // search document text for a certain character
    return documentation->searchText('I');
    
  } else if (op == Trav5do) {
    
    // conditionally change initial part of document text
    return documentation->replaceText("I am", "This is");
    
  } else if (op == Trav5undo) {
    
    // conditionally change back initial part of document text
    return documentation->replaceText("This is", "I am");
    
  } else if (op == Trav6) {
    
    // visit the root part only (it knows how to handle this)
    PartIdSet visitedIds;
    return rootPart->traverse(op, visitedIds);
    
  } else {    
  
    cout << "*** CompositePart::PANIC -- illegal traversal!!! ***\n";
    exit(1);
  }
}


//////////////////////////////////////////////////////////////////
//
// AtomicPart Method for Traversal
//
//////////////////////////////////////////////////////////////////


int AtomicPart::traverse(BenchmarkOp op, PartIdSet& visitedIds)
{
#ifdef LOG
  Logger l(this, 0);
#endif
  if (debugMode) {
    cout << " \t\t\t\tAtomicPart::traverse id = " << id << " op = ";
    PrintOp(op);
    cout << "\n"; fflush(stdout);
  }
  
  int i;
  int count = 0;
  
  switch (op) 
    {    
    case Trav1: 
      count  += 1;
      DoNothing();
      break;     
    case Trav2a:
      if (visitedIds.empty()) 
	{ 
	  swapXY(); 
	  count += 1;
	}
      break;    
    case Trav2b:
      swapXY();
      count += 1;
      break;
      
    case Trav2c:
      for (i = 0; i < UpdateRepeatCnt; i++) 
	{ 
	  swapXY(); 
	  count += 1;
	}
    break;
      
    case Trav3a:
      if (visitedIds.empty()) 
	{ 
	  // update the AtomicDateIndex, i.e. delete old version add new one
	  AtomicDateIndex->del(this->buildDate);
	  toggleDate();
	  (*AtomicDateIndex)[this->buildDate] = this;
	  count += 1;
	}
      break;
      
    case Trav3b:
      // update the AtomicDateIndex, i.e. delete old version add new one
      AtomicDateIndex->del(this->buildDate);
      toggleDate();
      (*AtomicDateIndex)[this->buildDate] = this;
      count += 1;
      break;
      
    case Trav3c:
      //cout << "reached here in Trav3c\n"; cout.flush();
      for (i = 0; i < UpdateRepeatCnt; i++) 
	{ 
	  // update the AtomicDateIndex, i.e. delete old version add new one
	  AtomicDateIndex->del(this->buildDate);
	  toggleDate();
	  (*AtomicDateIndex)[this->buildDate] = this;
	  count += 1;
	}
      break;
    
    case Trav6:
      count += 1;
      DoNothing();
      return count;
      
    default:
      cout << "*** AtomicPart::PANIC -- illegal traversal!!! ***\n";
    
    }

  if (debugMode) printf("id = %d\n", id); fflush(stdout);
  visitedIds.insert(id);

  Pix conn;
  for(conn = (Connection **)to->first();conn;to->next(conn))
    {
      if (!visitedIds.contains(((*(Connection **)conn)->To())->id))
	{
	  count += ((*(Connection**)conn)->To())->traverse(op, visitedIds);
	}
    }
  return count;
}
  
