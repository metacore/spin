////////////////////////////////////////////////////////////////////////////
//
// AtomicPart Methods
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

extern int	debugMode;
extern char*	types[NumTypes];

////////////////////////////////////////////////////////////////////////////
//
// AtomicPart Constructor
//
////////////////////////////////////////////////////////////////////////////

AtomicPart::AtomicPart(int ptId)
{
    SETRANGE((char *)this, sizeof(*this));

    if (debugMode) {
      //cout << "BEGIN AtomicPart::AtomicPart ptId = " << ptId << "\n";
    }
    id = ptId;
    // initialize internal state of new part (most of it randomly)
    int typeNo = (int) (random() % NumTypes);
    strncpy(type, types[typeNo], TypeSize);
    buildDate = MinAtomicDate +
    		(int) (random() % (MaxAtomicDate-MinAtomicDate+1));
    x = (int) (random() % XYRange);
    y = (int) (random() % XYRange);

    docId = (int) random() % TotalCompParts + 1;

    if (debugMode) {
      //cout << "MIDDLE AtomicPart::AtomicPart ptId = " << ptId << "\n";
      //cout << "\tbuildDate = " << buildDate << "\n";
      //cout << "\tx = " << x << " y = " << y << "\n";
    }

    if (debugMode) {
      //cout << "END AtomicPart::AtomicPart(ptId = " << ptId << "\n";
    }
}

////////////////////////////////////////////////////////////////////////////
//
//             Functions to set up connections 
//
////////////////////////////////////////////////////////////////////////////
void AtomicPart::AddToConnection(Connection *conn)
{
  to->add(conn);
}

void AtomicPart::AddFromConnection(Connection *conn)
{
  from->add(conn);
}

////////////////////////////////////////////////////////////////////////////
//
// AtomicPart swapXY Method for use in update traversals
//
////////////////////////////////////////////////////////////////////////////

void AtomicPart::swapXY()
{
    // exchange X and Y values

    int tmp = x;

    // XXX I assume x,y are located contiguously.
    SETRANGE((char*)&(this->x), sizeof(int)*2);

    x = y;
    y = tmp;

    if (debugMode) {
        cout << " [did swap, so x = " << x << " y = " << y << "]\n";
    }
}


////////////////////////////////////////////////////////////////////////////
//
// AtomicPart toggleDate Method for use in update traversals
//
////////////////////////////////////////////////////////////////////////////

void AtomicPart::toggleDate()
{
    // increment build date if odd, decrement it if even
    SETRANGE((char *)&(this->buildDate),sizeof(buildDate));

    if (buildDate % 2) {
	// odd case
	buildDate++;
    } else {
	// even case
	buildDate--;
    }

    if (debugMode) {
        cout << "[did toggle, so buildDate = " << buildDate << "\n";
    }
}

////////////////////////////////////////////////////////////////////////////
//
// AtomicPart DoNothing Method 
//
////////////////////////////////////////////////////////////////////////////

int RealWork; // set to one to make DoNothing do work.
int WorkAmount; // amount of work to do

void AtomicPart::DoNothing()
{
  struct timeval tv;
  struct timezone tz;
  
  int i;
  if (id < 0)
    cout << "DoNothing - negative id\n";
  
  if (debugMode) {
    cout << "==> DoNothing \n" ;
  }
  
  if (RealWork) {
    for (i = 0; i < WorkAmount; i++) {
      gettimeofday(&tv, &tz);
    }
  }
}

/*
////////////////////////////////////////////////////////////////////////////
//
// Overloaded AtomicPart Operator ->
//
////////////////////////////////////////////////////////////////////////////

AtomicPart *
AtomicPart::operator->()
{
#ifdef COUNT_DEREFS
  IncrementDerefCount();
#endif
  return this;
}

*/

////////////////////////////////////////////////////////////////////////////
//
// AtomicPart Destructor
//
////////////////////////////////////////////////////////////////////////////

AtomicPart::~AtomicPart()
{
    if (debugMode) {
        cout << "Entering AtomicPart::~AtomicPart id = " << id << "\n";
    }

// keep set of connections
// delete connections from composite part destructor.

// delete all affected connections
//      if (debugMode) {
//	cout << " In ~AtomicPart, about to delete 'to' connections\n";
//      }
//    Connection* c;
//    for(Pix c = to->first(); c; to->next(c))
//      {
//	delete (Connection *) c;
//      }
//
//if (debugMode) {
//    cout("    In ~AtomicPart, about to delete 'from' connections\n");
//}
//    while (!from.empty()) {
//	c = from.pick();
//        delete c;
//    }

}

