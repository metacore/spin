//////////////////////////////////////////////////////////////////
//
// Query #1 - randomly choose "Query1RepeatCnt" atomic parts by 
// lookup on their id field.  An index can be used for the
// actual lookup.  
//
/////////////////////////////////////////////////////////////////
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
extern GenAVLIndex<int, AtomicPart *> *AtomicIdIndex;
//////////////////////////////////////////////////////////////////
//
// Routine to do Query #1
//
//////////////////////////////////////////////////////////////////


int query1()
{
  // set random seed so "hot" runs are truly hot
  srandom(1);
  
  // randomly select parts and process them
  
  int partId;
  cout << "inside query1\n" ; cout.flush();
  for (int i = 0; i < Query1RepeatCnt; i++) {
    
    // generate part id and lookup part
    
    partId = (int) (random() % TotalAtomicParts) + 1;

    AtomicPart *& atom = (*AtomicIdIndex)[partId];

    if (debugMode) {
      cout << " In Query1, partId = " <<  partId << "\n";
      cout.flush();
    }
    
    // process part by invoking DoNothing method
    if(!atom)
      {
	cout << "****error atom not found\n";
	cout.flush();
	return 0;
      }
    atom->DoNothing();
  }
  
  return Query1RepeatCnt;
}

