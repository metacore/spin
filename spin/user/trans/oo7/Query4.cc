//////////////////////////////////////////////////////////////////
//
// Routine to do Query #4
//
//////////////////////////////////////////////////////////////////
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
extern GenAVLIndex<char *, Document *> *DocumentTitleIndex;


int  query4()
{
  // randomly select a document title and find the base assemblies
  // that use its associated composite part (and then repeat this
  // process the desired number of times)
  
  // set random seed so "hot" runs are truly hot
  srandom(1);
  
  int count = 0;
  char title[TitleSize];
  
  for (int i = 0; i < Query4RepeatCnt; i++) {
    
    // generate random document title
    
    int compPartId = (int) (random() % TotalCompParts) + 1;
    sprintf(title, "Composite Part %08d", compPartId);
    if (debugMode) {
      cout << " In Query4, document title = " << title << "\n";
    }
    
    // first find the document with this title

    Pix dp = (DocumentTitleIndex)->seek((char *&)title);
    if(!dp)
      {
	cout << " unable to find document with this title\n";
      }
    else
      {
	Document *doc = (DocumentTitleIndex)->contents(dp);
	if(!doc)
	  {
	    cout << " doc is null \n";
	    return 0;
	  }
	//cout << " doc title is " << doc->title << "\n";
	// find composite part using this document
	CompositePart *cp = doc->part;
	if(!cp)
	  {
	    cout << " fatal error: dp not null but cp is null \n";
	    return 0;
	  }
	// find all base assemblies using this composite part
	//cout << "cardinality of usedInPriv = " << 
	  //(cp->usedInPriv).cardinality() << "\n";
	for( Pix bap = (cp->usedInPriv)->first();
	    bap; (cp->usedInPriv)->next(bap))
	  {
	    BaseAssembly *ba = (*(cp->usedInPriv))(bap);
	    ba->DoNothing();
	    count++;
	  }
      }
  }
  return count;
}

