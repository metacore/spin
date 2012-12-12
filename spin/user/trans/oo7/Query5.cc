
//////////////////////////////////////////////////////////////////
//
// Query #5 - find all base assemblies B that "use" a composite
// part with a more recent build date than B's build date.
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

extern int debugMode;
extern GenVHSet<BaseAssembly *> *BaseAssemblyDB;

extern int	debugMode;

int PrintBaseId(int id)
{
    printf("PrintBaseId(base id = %d)\n", id);
    return TRUe;
}


int PrintBaseCompIds(int bid, int cid)
{
    printf("PrintBaseCompIds(base id = %d, comp id = %d)\n", bid, cid);
    return TRUe;
}


//////////////////////////////////////////////////////////////////
//
// Routine to do Query #5
//
//////////////////////////////////////////////////////////////////


int query5()
{
  // find the base assemblies that use more recently build-dated
  // composite parts;  uses nested foreach loops since 
  
  if (debugMode) {
    cout << "    In Query5, about to execute the query...\n";
  }
  
  int count = 0;
  int total_count = 0;
  // first find the desired base assembly
  for(Pix bap = (BaseAssemblyDB)->first();bap;
      (BaseAssemblyDB)->next(bap))
    {
      BaseAssembly *ba = (*BaseAssemblyDB)(bap);
      // find all private composite parts used by this BaseAssembly
      for(Pix c = (ba->componentsPriv)->first();c; 
	  (ba->componentsPriv)->next(c))
	{
	  CompositePart *cp = (*(ba->componentsPriv))(c);
	  // check if cp is newer than ba
	  //cout << " ba date = " << ba->buildDate <<
	  //  " cp date = " << cp->buildDate << "\n";
	  if(cp->buildDate > ba->buildDate)
	    {
	      ba->DoNothing();
	      count++;
	    }
	  total_count++;
	}
    }
  cout << "Query5: total count = " << total_count << "\n";
  return count;
}


