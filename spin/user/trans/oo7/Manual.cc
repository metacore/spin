
////////////////////////////////////////////////////////////////////////////
//
// Manual Methods
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
extern int	debugMode;

////////////////////////////////////////////////////////////////////////////
//
// Manual Constructor
//
////////////////////////////////////////////////////////////////////////////

Manual::Manual(int modId)
{
#ifndef NO_SETRANGE
  rvm_tid_t tid;

  BeginTransaction(&tid);
  SETRANGE((char *)this, sizeof(*this));
#endif

    // fill in id

    id = modId;

    // prepare and fill in the manual title

    char manTitle[TitleSize];
    sprintf(manTitle, "Module         %08d", modId);
    if (debugMode) {
        cout << "Manual::Manual(title = " << manTitle << "\n";
    }
    strncpy(title, manTitle, TitleSize);

    // prepare and fill in the manual text

    char myText[ManualTextLength];
    sprintf(myText, ManualText, modId);
    int stringLen = strlen(myText);
    cout << " allocating space for manual of size " << ManualSize << "\n";
    cout.flush();
    text = BEGIN_NEW(DesignLib, char[ManualSize])
    END_NEW
    cout << " allocated space for manual of size " << ManualSize << "\n";
    cout.flush();
    int curChar, strChar;
    curChar = strChar = 0;
    while (curChar < ManualSize-1) {
	text[curChar++] = myText[strChar++];
	if (strChar >= stringLen) { strChar = 0; }
    }
    text[curChar] = '\0';

    textLen = strlen(text);
#ifndef NO_SETRANGE
  Commit(&tid);
#endif
}


////////////////////////////////////////////////////////////////////////////
//
// Manual searchText Method for use in traversals
//
////////////////////////////////////////////////////////////////////////////

int Manual::searchText(char c)
{
    if (debugMode) {
        cout << " Manual::searchText title = " << title << "\n";
    }

    // count occurrences of the indicated letter (for busy work)

    int i = 0;
    int count = 0;
    while (text[i] != '\0') {
	if (text[i++] == c) { count++; }
    }

    if (debugMode) {
      cout << "found " << count << " " << c << "'s" << " among " << i << " characters\n";
    }

    return count;
}

////////////////////////////////////////////////////////////////////////////
//
// Manual firstLast Method for use in traversals
//
////////////////////////////////////////////////////////////////////////////

int Manual::firstLast()
{
    return (text[0] == text[textLen-1]);
}


////////////////////////////////////////////////////////////////////////////
//
// Manual replaceText Method for use in traversals
//
////////////////////////////////////////////////////////////////////////////

int Manual::replaceText(char *oldString, char *newString)
{
    if (debugMode) {
      cout << "Manual::changeText title = " << title << "\n";
    }

    // check to see if the text starts with the old string

    int oldTextSize = strlen(text) + 1;
    int oldStrLength  = strlen(oldString);
    int foundMatch = (strncmp(text, oldString, oldStrLength) == 0);

    // if so, change it to start with the new string instead

    if (foundMatch) {

        int newStrLength  = strlen(newString);
        int lengthDiff = newStrLength - oldStrLength;
        int newTextSize  = oldTextSize + lengthDiff;

	if (lengthDiff == 0) {
	    strncpy(text, newString, newStrLength);
        } else {
	    char* newText = BEGIN_NEW(DesignLib, char[newTextSize])
	    END_NEW
	    strncpy(newText, newString, newStrLength);
	    strncpy(&newText[newStrLength], &text[oldStrLength],
		    oldTextSize - oldStrLength);
            delete text;
	    text = newText;
        }
    }

    if (debugMode) {
      if (foundMatch) {
	cout << "changed " << oldString <<  " to " << newString << "\n";
      } else {
	cout << "no match, so no change was made\n" ;
      }
    }
    
    if (foundMatch) {
      return 1;
    } else {
      return 0;
    }
    
}




////////////////////////////////////////////////////////////////////////////
//
// Manual Destructor
//
////////////////////////////////////////////////////////////////////////////

Manual::~Manual()
{
  if (debugMode) {
    cout << "Manual::~Manual title = " << title << "\n";
  }
  
  // reclaim space from manual text
    delete text;
}

