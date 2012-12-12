////////////////////////////////////////////////////////////////////////////
//
// Assembly Methods
//
////////////////////////////////////////////////////////////////////////////
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

extern int	nextComplexAssemblyId;
extern int	nextBaseAssemblyId;
extern int	debugMode;
extern char*	types[NumTypes];

int    numCompositePartsLinked = 0;


extern GenVHSet<BaseAssembly *> *BaseAssemblyDB;
extern GenList<BaseAssembly *> *private_cp;
extern GenList<BaseAssembly *> *shared_cp;


////////////////////////////////////////////////////////////////////////////
//
// Assembly Constructor
//
////////////////////////////////////////////////////////////////////////////

Assembly::Assembly(int asId) { 
    SETRANGE((char *)this, sizeof(*this));
    id = asId;
}


//////////////////////////////////////////////////////////////////////////
// 
// DoNothing method for Assemblies 
// 
//////////////////////////////////////////////////////////////////////////

void Assembly::DoNothing ()
{
    if (id < 0) 
      cout << "DoNothing - negative id" ;

    if (debugMode) {
        cout << "==> DoNothing id = " << id << " type = " << type << "  buildDate = " << buildDate << "\n";
    }
}

////////////////////////////////////////////////////////////////////////////
//
// ComplexAssembly Constructor
//
////////////////////////////////////////////////////////////////////////////

ComplexAssembly::ComplexAssembly(int asId, ComplexAssembly* parentAssembly,
		                 int levelNo, Module *mod)
 :Assembly(asId)
{
    SETRANGE((char *)this, sizeof(*this));

  // initialize internal state of new assembly (some of it randomly)
  
  int typeNo = (int) (random() % NumTypes);
  strncpy(type, types[typeNo], TypeSize);
  buildDate = MinAssmDate + (int) (random() % (MaxAssmDate-MinAssmDate+1));
  superAssembly = parentAssembly;
  module = mod;

  // create "small" bag for subassemblies
  subAssemblies = BEGIN_NEW(mod,GenVHSet<Assembly *>)(2*NumAssmPerAssm)
  END_NEW

  
  // now recursively create subassemblies for this complex assembly
  
  int i, nextId;
  
  for (i = 0; i < NumAssmPerAssm; i++) {
    
    if (levelNo < NumAssmLevels-1) {
      nextId = nextComplexAssemblyId++;
      ComplexAssembly* subAssm = BEGIN_NEW(mod, ComplexAssembly)
	(nextId, this, levelNo+1, mod)
      END_NEW
      subAssemblies->add(subAssm);
    } else {
      nextId = nextBaseAssemblyId++;
      BaseAssembly* subAssm = BEGIN_NEW(mod, BaseAssembly)
	(nextId, this, mod)
      END_NEW
      (BaseAssemblyDB)->add(subAssm);
      subAssemblies->add(subAssm);
    }
  }
}


////////////////////////////////////////////////////////////////////////////
//
// BaseAssembly Constructor
//
////////////////////////////////////////////////////////////////////////////

BaseAssembly::BaseAssembly(int asId,
    	                   ComplexAssembly* parentAssembly,
			   Module *mod
			  ) :Assembly(asId)
{
    SETRANGE((char *)this, sizeof(*this));

    if (debugMode) {
      cout << "BaseAssembly::BaseAssembly(asId = " << asId << 
	" levelNo = " << NumAssmLevels << "\n";
    }
    
    // initialize internal state of new assembly (some of it randomly)
    int typeNo = (int) (random() % NumTypes);
    strncpy(type, types[typeNo], TypeSize);
    buildDate = MinAssmDate + (int) (random() % (MaxAssmDate-MinAssmDate+1));
    superAssembly = parentAssembly;
    module = mod;

    // create "small" bags for composite part pointers
    componentsPriv = BEGIN_NEW(mod, GenVHBag<CompositePart *>)
      (2*NumCompPerAssm)
    END_NEW
    componentsShar = BEGIN_NEW(mod, GenVHBag<CompositePart *>)
      (2*NumCompPerAssm)
    END_NEW


    // select the private composite parts for this assembly

    int i;
    int compId;
    int lowCompId = (module->id - 1) * NumCompPerModule + 1;
    int compIdLimit = NumCompPerModule;
    CompositePart* comp;

    for (i = 0; i < NumCompPerAssm; i++) 
    {
	compId = lowCompId + (int) (random() % compIdLimit);
	(private_cp)[compId].Append(this);  // keep track of which
		// composite parts this base assembly uses as private
    }

    // now select the shared composite parts for this assembly
    for (i = 0; i < NumCompPerAssm; i++) 
    {
	compId = (int) (random() % TotalCompParts) + 1;
	(shared_cp)[compId].Append(this);  // keep track of which
		// composite parts this base assembly uses as shared

    }
}


////////////////////////////////////////////////////////////////////////////
//
// BaseAssembly Destructor
//
////////////////////////////////////////////////////////////////////////////

//BaseAssembly::~BaseAssembly()
//{
//    if (debugMode) {
//        cout << "BaseAssembly::~BaseAssembly id = " << id << "\n";
//    }
//}


