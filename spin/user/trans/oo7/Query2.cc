//////////////////////////////////////////////////////////////////
//
// Query #2 - 1% selection on AtomicParts via build date (the
// most recent 1% of AtomicParts.)
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

extern GenAVLIndex<int, AtomicPart *> *AtomicDateIndex;

//////////////////////////////////////////////////////////////////
//
// Routine to do Query #2
//
//////////////////////////////////////////////////////////////////


int query2()
{
  // choose a range of dates with the appropriate selectivity
  
  int dateRange = (Query2Percent * (MaxAtomicDate - MinAtomicDate)) / 100;
  int lowerDate = MaxAtomicDate - dateRange;
  
  if (debugMode) {
    cout << "    Running Query2 with dateRange = " << dateRange <<  
      "lowerDate = " << lowerDate << "\n";
    cout.flush();

  }
  if(!AtomicDateIndex)
    {
      cout << "AtomicDateIndex is NULL\n"; cout.flush();
      exit(1);
    }
//  cout << "address of AtomicDateIndex = " << hex((Address) AtomicDateIndex)  << "\n";
  cout.flush();
  
  // first find the first part and process it
  Pix ap = (AtomicDateIndex)->seek(lowerDate);
  int i =0;
  while(!ap)
    {
      cout << " couldn't find atomic part with Date = " << 
	lowerDate+i << "\n";      cout.flush();
      i++;
      int tmp = lowerDate+i;
      ap = (AtomicDateIndex)->seek(tmp);
    }
  AtomicPart *& atom = (AtomicDateIndex)->contents(ap);
  if(!atom)
    {
      cout << "atom is NULL\n"; cout.flush();
    }
  cout << "Date of atom = " << atom->buildDate << "\n";
  cout.flush();
  atom->DoNothing();
  cout << "called DoNothing on atom\n"; cout.flush();
  // now process successive parts until MaxAtomicDate
  int count = 1;
  for(;ap;(AtomicDateIndex)->next(ap))
    {
      if(!ap)
	{
	  cout << "Oops ap is null\n"; cout.flush();
	  exit(1);
	}
      atom = (AtomicDateIndex)->contents(ap);
      if(!atom)
	{ cout << "atom is null\n"; cout.flush(); exit(1);}
      atom->DoNothing();      
      count++;
      cout << "Date of atom = " << atom->buildDate << "\n"; cout.flush();
    }
  return count;
}
