

////////////////////////////////////////////////////////////////////////////
//
// Module Methods
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


extern void *DesignLib;

extern int	nextComplexAssemblyId;
extern int	debugMode;
extern char*	types[NumTypes];

////////////////////////////////////////////////////////////////////////////
//
// Module Constructor
//
////////////////////////////////////////////////////////////////////////////

Module::Module(int modId)
{
#ifndef NO_SETRANGE
  rvm_tid_t tid;

  BeginTransaction(&tid);
  SETRANGE((char *)this, sizeof(*this));
#endif
  if (debugMode) {
    cout << "Module::Module(modId = " << modId << "\n";
  }
  
  id = modId;
  cout << " setting module id to " <<  id << "\n";
  // initialize the new module's data members (some randomly)
  
  int typeNo = (int) (random() % NumTypes);
  strncpy(type, types[typeNo], TypeSize);
  cout << "type of module is " <<  type;
  buildDate = MinModuleDate +
    (int) (random() % (MaxModuleDate - MinModuleDate + 1));
  
  int assmId;
  
  man = BEGIN_NEW(DesignLib, Manual)(modId)
  END_NEW

  cout << " allocated Manual for module\n";
  cout.flush();

  // create a set of for holding assembly pointers
  // this set is "large"
  assemblies = BEGIN_NEW(DesignLib, GenVHSet<Assembly *>)
  END_NEW


  if (NumAssmLevels > 1) {
    assmId = nextComplexAssemblyId++;
    cout << "creating design root\n";
    cout.flush();
    ComplexAssembly *assm = BEGIN_NEW(DesignLib, ComplexAssembly) 
     (assmId, NULL, 1, this)
    END_NEW
    designRoot = assm;
    if(designRoot)
      cout << "designRoot set\n";
    else
      cout << " unable to set designRoot\n";
    cout.flush();
  }
  
#ifndef NO_SETRANGE
  Commit(&tid);
#endif
}


////////////////////////////////////////////////////////////////////////////
//
// Module Destructor
//
////////////////////////////////////////////////////////////////////////////

Module::~Module()
{
  if (debugMode) {
    cout << "Module::~Module id = " << id << "\n";
  }
}


///////////////////////////////////////////////////////////////////////////
//
// Module::scanManual() returns the number of occurrences
// of the character 'I' in the module's manual.
//
///////////////////////////////////////////////////////////////////////////

int Module::scanManual()
{
  return man->searchText('I');
}


///////////////////////////////////////////////////////////////////////////
//
// Module::firstLast() returns one if the first and last character
// of the manual are the same, zero otherwise.
//
///////////////////////////////////////////////////////////////////////////

int Module::firstLast()
{
  return man->firstLast();
}

