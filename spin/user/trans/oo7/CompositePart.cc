////////////////////////////////////////////////////////////////////////////
//
// CompositePart Methods
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

extern int	nextAtomicId;
extern int	debugMode;
extern int	clusterLevel;

extern char*	types[NumTypes];

extern void SharedBAAdd(BaseAssembly *&, void *);
extern void PrivateBAAdd(BaseAssembly *&, void *);
extern void SharedCPAdd(BaseAssembly *&, void *);
extern void PrivateCPAdd(BaseAssembly *&, void *);

extern void *DesignLib;
extern GenVHSet<AtomicPart *> *AtomicDB;
extern GenVHSet<Document *> *DocumentDB;
extern GenList<BaseAssembly *> *private_cp;
extern GenList<BaseAssembly *> *shared_cp;

#define PAGE_SIZE 8192

////////////////////////////////////////////////////////////////////////////
//
// CompositePart Constructor
//
////////////////////////////////////////////////////////////////////////////

CompositePart::CompositePart(int cpId)
{
  BaseAssembly* ba;
  SETRANGE((char *)this, sizeof(*this));
  
  if (debugMode) {
    cout << "BEGIN CompositePart::CompositePart cpId = " << cpId << "\n";
    cout << "IN CompositePart::CompositePart:  init data members\n";
  }
  id = cpId;
  // initialize this composite part's simple data members (some randomly)
  
  int typeNo = (int) (random() % NumTypes);
  strncpy(type, types[typeNo], TypeSize);
  
  // for the build date, decide if this part is young or old, and then
  // randomly choose a date in the required range
  
  if (cpId % YoungCompFrac == 0) {
    // young one
    if (debugMode) {
      cout <<  "young composite part id = " << id << "\n";
    }
    buildDate = MinYoungCompDate + 
      (int)(random() % (MaxYoungCompDate - MinYoungCompDate + 1));
  } else { 
    // old one
    if (debugMode) {
      cout << "old composite part id = " << id << "\n";
    }
    buildDate = MinOldCompDate + 
      (int)(random() % (MaxOldCompDate - MinOldCompDate + 1));
  }
  if (debugMode) {
    cout << "IN CompositePart::CompositePart: create usedIn Priv & Shar bags\n";
  }
  // may create "small" or "large" depending on NumAtomicPerComp.
  // "small" for small database, "large" for medium/large database
  parts = BEGIN_NEW(DesignLib, GenVHSet<AtomicPart *>)(NumAtomicPerComp)
  END_NEW
  
  usedInPriv = BEGIN_NEW(DesignLib,GenVHBag<BaseAssembly *>)(NumCompPerAssm)
  END_NEW
  usedInShar = BEGIN_NEW(DesignLib,GenVHBag<BaseAssembly *>)(NumCompPerAssm)
  END_NEW

  if (debugMode) {
    cout << "IN CompositePart::CompositePart:  create document\n";
  }
  
  // initialize the documentation 
  // put it in the correct segment
  documentation = BEGIN_NEW(DesignLib,Document) (cpId, this)
  END_NEW
  (DocumentDB)->add(documentation);

  if (debugMode) {
    cout << "IN CompositePart::CompositePart:  create atomic parts\n";
  }
  
  // now create the atomic parts 
  
  int i, aid, from, to;
  BEGIN_TEMP_NEW
  AtomicPart** atom = new AtomicPart*[NumAtomicPerComp];
  END_TEMP_NEW
  Connection* conn;
  
  for (i = 0; i < NumAtomicPerComp; i++) {
    aid = nextAtomicId + i;
    atom[i] = BEGIN_NEW(DesignLib, AtomicPart)(aid)
    END_NEW
    parts->add(atom[i]);
    (AtomicDB)->add(atom[i]);
    if (i == 0) { rootPart = atom[i]; }
  }
  
  if (debugMode) {
    cout << "IN CompositePart::CompositePart:  wire up via connections\n";
  }
  if (debugMode) {
    cout << "IN CompositePart::CompositePart:  allocating to & from bags\n";
  }  
  // create "small" bags -- i.e. implemented as linked lists
  for (i = 0; i < NumAtomicPerComp; i++) {
      SETRANGE((char *)atom[i], sizeof(*(atom[i])));
      atom[i]->to = BEGIN_NEW(DesignLib, GenBBag<Connection *>)
	(NumConnPerAtomic)
	  END_NEW
    atom[i]->from = BEGIN_NEW(DesignLib,GenBBag<Connection *>)
      (4*NumConnPerAtomic)
    END_NEW
    }

  // ... and then wire them semi-randomly together (as a ring plus random
  // additional connections to ensure full part reachability for traversals)
  
  for (from = 0; from < NumAtomicPerComp; from++) {
    for (i = 0; i < NumConnPerAtomic; i++) {
      if (i == 0) {
	to = (from + 1) % NumAtomicPerComp;
      } else {
	to = (int) (random() % NumAtomicPerComp);
      }
      conn = BEGIN_NEW(DesignLib, Connection) (atom[from], atom[to])
      END_NEW
      atom[from]->AddToConnection(conn);
      atom[to]->AddFromConnection(conn);
    }
  }
  
  if (debugMode) {
    cout << "IN CompositePart::CompositePart:  local cleanup\n";
  }
  
  delete atom;
  nextAtomicId += NumAtomicPerComp;
  
  // finally insert this composite part as a child of the base
  // assemblies that use it
  // ... and insert the baseassembly as a parent of the composite part 
  //first add cp to ba
  (shared_cp)[cpId].Iterate(SharedBAAdd, (void *) this);
  (private_cp)[cpId].Iterate(PrivateBAAdd, (void *) this);

  // next add ba to cp
  (shared_cp)[cpId].Iterate(SharedCPAdd, (void *) this);
  (private_cp)[cpId].Iterate(PrivateCPAdd, (void *) this);

  // This is a trick to make sure that every new compositepart is allocated
  // on a new page -- idea being to make prefetching possible
  // PAGE_SIZE = 8192 for the alpha
  // the following code will make sure that one byte is allocated out of 
  // the next page.
  if (clusterLevel) {
	char *whereAmI = new char;
	int usedBytesOnPage = (((long)whereAmI) % PAGE_SIZE) ;
	int bytesToAllocate = (PAGE_SIZE - usedBytesOnPage) + 1;
	char *wastedSpace = BEGIN_NEW(DesignLib, char[bytesToAllocate])
	END_NEW
	if(debugMode) {
	  printf("next composite part will be allocated on page no. %d\n",
	  ((long)whereAmI)/PAGE_SIZE + 1);
	}
  }
    

  if (debugMode) {
    cout << "END CompositePart::CompositePart cpId = " << cpId << "\n";
  }
}


////////////////////////////////////////////////////////////////////////////
//
// CompositePart Destructor
//
////////////////////////////////////////////////////////////////////////////

CompositePart::~CompositePart()
{
    if (debugMode) {
      cout << "Entering CompositePart::~CompositePart id = " << id << "\n";
    }

    // delete associated atomic parts
    if (debugMode) {
      cout << "    In ~CompositePart, about to delete parts\n";
    }
    
    for(Pix p = parts->first(); p; parts->next(p))
      {
	delete (AtomicPart *) p;
      }

    // delete associated documentation
      if (debugMode) {
	cout << " In ~CompositePart, about to delete documentation\n";
      }
    delete (Document*) documentation;

    // remove part from CompositePart extent
    ((GenVHSet<CompositePart*> *)(DesignLib))->del(this);
    
    if (debugMode) {
        cout << "Leaving CompositePart::~CompositePart id = " << id << "\n";
    }
}

