
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

extern int	nextAtomicId;
extern int	debugMode;
extern char*	types[NumTypes];

////////////////////////////////////////////////////////////////////////////
// Iterator functions 
////////////////////////////////////////////////////////////////////////////
void
SharedBAAdd(BaseAssembly *& ba, void *arg)
{
  ba->componentsShar->add((CompositePart *) arg);
}

void
PrivateBAAdd(BaseAssembly *& ba, void *arg)
{
  ba->componentsPriv->add((CompositePart *) arg);
}

void
SharedCPAdd(BaseAssembly *& ba, void *arg)
{
  ((CompositePart *)arg)->usedInShar->add(ba);
}

void
PrivateCPAdd(BaseAssembly *& ba, void *arg)
{
  ((CompositePart *)arg)->usedInPriv->add(ba);
}

