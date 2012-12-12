
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

//////////////////////////////////////////////////////////////////
//
// Delete - Delete NumNewCompParts randomly chosen composite parts. 
//
//////////////////////////////////////////////////////////////////

extern int debugMode;
extern int TotalCompParts;
extern void *DesignLib;
extern GenAVLIndex<int, CompositePart *> *DesignLibIdIndex;

//////////////////////////////////////////////////////////////////
//
// Routine to do Delete
//
//////////////////////////////////////////////////////////////////


void delete1()
{
  
  // delete the desired number of new composite parts
  
  int compId;

  for (int i = 1; i <= NumNewCompParts; i++) {
    compId = TotalCompParts + i;
    
    //if (debugMode) {
      cout <<"In Delete, looking up composite part " << compId << "\n"; 
    //}
    CompositePart*& comp = 
      (*DesignLibIdIndex)[compId]; // update index
    cout << " updated index " << "\n";
    Pix c = ((GenVHSet<CompositePart*> *)(DesignLib))->seek(comp);
    if(!c)
      {
	cout << "unable to find cp \n";
	exit(1);
      }
    comp = (* ((GenVHSet<CompositePart *> *)DesignLib))(c);
    if(!comp)
      {
	cout << "comp is nul\n";
	exit(1);
      }
    else
      cout << " found composite part \n";

    BaseAssembly *ba;
    // update all base assemblies pointing to this cp
    for(Pix p = (comp->usedInPriv)->first();p;(comp->usedInPriv)->next(p))
      {
	ba = (*(comp->usedInPriv))(p);
	if(!ba) { cout << " ba is null -- exiting \n"; exit(1); }
	(ba->componentsPriv)->del(comp);
      }
    // now delete the comp part
    delete comp;
    
  }
}

