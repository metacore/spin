//////////////////////////////////////////////////////////////////
//
// Insert - Create NumNewCompParts new composite parts (which
// means also creating a set of AtomicParts for each.)  Add
// a reference to each new composite part from a randomly chosen
// base assembly (a different random choice of base assembly for
// each new composite part.)
//
//////////////////////////////////////////////////////////////////
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


extern int	debugMode;
extern int	nextAtomicId;
extern int	nextCompositeId;
extern int	nextBaseAssemblyId;

extern void *DesignLib;
extern GenAVLIndex<int, CompositePart *> *DesignLibIdIndex;
extern GenAVLIndex<int, BaseAssembly *> *BaseAssemblyIdIndex;

//////////////////////////////////////////////////////////////////
//
// Routine to do Insert
//
//////////////////////////////////////////////////////////////////


CompositePart *
add_comp()
{
  CompositePart* comp;
  int compId = nextCompositeId++;
  comp = BEGIN_NEW(DesignLib, CompositePart) (compId)
  END_NEW
  ((GenVHSet<CompositePart*> *)(DesignLib))->add(comp); // add comp to database
  return comp;
}

void insert1()
{
  
  // create the desired number of new composite parts, adding each
  // one as a (private) composite part that a randomly selected base
  // assembly now wishes to use
  
  int compId, assmId;
  CompositePart* comp;
  
  if (debugMode) {
    cout << "In Insert, nextCompositeId = " << nextCompositeId << "\n" ;
    cout << "In Insert, nextAtomicId = " << nextAtomicId << "\n" ;
    cout << "In Insert, nextBaseAssmId = " << nextBaseAssemblyId << "\n" ;
  }
  
  for (int i = 0; i < NumNewCompParts; i++) 
    {
      
      // add a new composite part to the design library
      
      //if (debugMode) {
      cout << "In Insert, making composite part " <<  nextCompositeId 
	<< "\n";
      //}
      comp = add_comp();
      (*DesignLibIdIndex)[compId]  =  comp; // update index
      
      // randomly select a base assembly that should use it
      
      assmId = (int) (random() % (nextBaseAssemblyId-1)) + 1;
      
      // locate the appropriate base assembly object
      
      Pix bap = (BaseAssemblyIdIndex)->seek(assmId);
      if(!bap)
	{
	  cout << "fatal error: couldn't find ba \n";
	  exit(1);
	}
      BaseAssembly *assm = (BaseAssemblyIdIndex)->contents(bap);
      if(!assm)
	{
	  cout << "fatal error: couldn't find assm \n";
	  exit(1);
	}
      
      // finally, add the newly created composite part as a privately used
      // member of the randomly selected base assembly
      
      assm->componentsPriv->add(comp);
      
      if (debugMode) {
	cout << "[just made it be used by base assembly " << assmId << "]\n";
      }
    }
}

