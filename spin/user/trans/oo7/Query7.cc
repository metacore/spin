//////////////////////////////////////////////////////////////////
//
// Query #7 --- iterate through all atomic parts.  Checks scan
// speed.  Some duplication with other queries, especially in
// systems that lack indices and force the other queries to be
// done by iterating through sets.
//
//////////////////////////////////////////////////////////////////
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


extern int	debugMode;
extern GenVHSet<AtomicPart *> *AtomicDB;
//////////////////////////////////////////////////////////////////
//
// Routine to do Query #7
//
//////////////////////////////////////////////////////////////////


int query7()
{
  int count = 0;
  
  
  Pix ap;
  for (ap = (AtomicDB)->first(); ap; (AtomicDB)->next(ap)) {
    if (debugMode) {
      cout << " In Query 7 " << " id = " << 
	(*AtomicDB)(ap)->id << "\n";
    }
    
    (*AtomicDB)(ap)->DoNothing();
    count++;
  }
  return count;
}

