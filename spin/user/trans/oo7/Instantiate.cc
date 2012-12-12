#include <iostream.h>
#include "rvmdef.h"
#include "macrodef.h"

extern void *DesignLib;
#include "GenVHSet.h"
#include "GenList.h"
#include "GenVHBag.h"
#include "GenBBag.h"
#include "GenAVLIndex.h"

#include "GenVHSet.cc"
#include "GenList.cc"
#include "GenVHBag.cc"
#include "GenBBag.cc"
#include "GenAVLIndex.cc"
#include "OO7.h"

#include "BenchParams.h"
#include "GenParams.h"
#include "VarParams.h"

/* Instantiate all the craps. */

template class GenBBag<Connection *>;
template class GenVHBag<CompositePart *>;
template class GenVHBag<BaseAssembly *>;

template class GenVHSet<AtomicPart *>;
template class GenVHSet<Assembly *>;
template class GenVHSet<BaseAssembly *>;
template class GenVHSet<CompositePart *>;

template class GenAVLIndex<int, CompositePart *>;
template class GenAVLIndex<int, AtomicPart *>;
template class GenAVLIndex<int, Document *>;
template class GenAVLIndex<char *, Document *>;
template class GenAVLIndex<int, BaseAssembly *>;
template class GenVHSet<Module *>;
template class GenAVLIndex<int, Module *>;
template class GenList<BaseAssembly *>;
template class Element<AtomicPart *>;
template class Element<BaseAssembly *>;
template class Element<Document *>;
template class Element<Module *>;
template class Element<CompositePart *>;

GenAVLIndex<int, CompositePart *> *DesignLibIdIndex;

// Set of Atomic Parts

GenVHSet<AtomicPart *> *AtomicDB;
GenAVLIndex<int, AtomicPart *> *AtomicIdIndex;
GenAVLIndex<int, AtomicPart *> *AtomicDateIndex;

// Set of Documents
template class GenVHSet<Document *>;
GenVHSet<Document *> *DocumentDB;
GenAVLIndex<int, Document *> *DocumentIdIndex;
GenAVLIndex<char *, Document *> *DocumentTitleIndex;

// Set of Assemblies

GenVHSet<BaseAssembly *> *BaseAssemblyDB;
GenAVLIndex<int, BaseAssembly *> *BaseAssemblyIdIndex;

// Data structures to hold the database
// Set of Modules
GenVHSet<Module *> *ModuleDB;
GenAVLIndex<int, Module *> *ModuleIdIndex;

GenList<BaseAssembly *> *private_cp;
GenList<BaseAssembly *> *shared_cp;

