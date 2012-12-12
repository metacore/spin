//////////////////////////////////////////////////////////////////
//
// Query #8 --- value join between Documents and AtomicParts
// on Document.id and AtomicPart.docId.  This is a key - foreign
// key join.
//
// The main drawback is that we have no semantics whatsoever for
// this query.
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

extern int debugMode;

extern int	debugMode;
extern GenVHSet<AtomicPart *> *AtomicDB;
extern GenAVLIndex<int, Document *> *DocumentIdIndex;

extern void JoinDoNothing(int x, int y);


//////////////////////////////////////////////////////////////////
//
// Routine to do Query #8
//
//////////////////////////////////////////////////////////////////


int query8()
{
  int count = 0;
  
  for(Pix a = (AtomicDB)->first();a;(AtomicDB)->next(a))
    {
      AtomicPart *ap = (*AtomicDB)(a);
      //cout << "Atomic part id = " << ap->id << "\n";
      // now find all docs whose id is = ap->id
      // find first and keep finding successors until a value bigger than
      // ap->id is encountered
      Document *doc;
      Pix d = (DocumentIdIndex)->seek(ap->id);
      if(d)
	{
	  while(d && ((doc=(DocumentIdIndex)->contents(d))->id == ap->id))
	    {
	      // call the JoinDoNothing function
	      JoinDoNothing(ap->id, doc->id);
	      count++;
	      (DocumentIdIndex)->next(d);
	    }
	}
    }
  return count;
}  


