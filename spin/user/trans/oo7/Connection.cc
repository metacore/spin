
////////////////////////////////////////////////////////////////////////////
//
// Connection Methods
//
////////////////////////////////////////////////////////////////////////////

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

#ifdef LOG
#include "Logger.h"
#endif

extern int	debugMode;
extern char*	types[NumTypes];

////////////////////////////////////////////////////////////////////////////
//
// Connection Constructor
//
////////////////////////////////////////////////////////////////////////////

Connection::Connection(AtomicPart* fromPart, AtomicPart* toPart)
{
  SETRANGE((char *)this, sizeof(*this));
    if (debugMode) {
      //cout << "BEGIN Connection::Connection fromId = " << fromPart->id
	// << " toId = " << toPart->id << "\n";
    }
    
    // initialize internal state of new connection (some of it randomly)
      
    int typeNo = (int) (random() % NumTypes);
    strncpy(type, types[typeNo], TypeSize);
    length = (int) (random() % XYRange);
    from = fromPart;
    to = toPart;

    // if(debugMode) cout << " done connection constructor\n";

}

////////////////////////////////////////////////////////////////////////////
//
// Accessor functions
//
////////////////////////////////////////////////////////////////////////////
AtomicPart *
Connection::To()
{
#ifdef LOG
  Logger l(this, 0);
#endif
  return to;
}

AtomicPart *
Connection::From()
{
#ifdef LOG
  Logger l(this, 0);
#endif
  return from;
}
