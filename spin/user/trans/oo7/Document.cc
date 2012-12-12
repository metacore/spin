
////////////////////////////////////////////////////////////////////////////
//
// Document Methods
//
////////////////////////////////////////////////////////////////////////////
#include <iostream.h>

#include "macrodef.h"
extern void *DesignLib;
#include "GenList.h"
#include "GenVHSet.h"
#include "GenBBag.h"
#include "GenVHBag.h"
#include "GenAVLIndex.h"

#include "OO7.h"
#include "BenchParams.h"
#include "GenParams.h"
#include "VarParams.h"


extern void *DesignLib;
extern int DocumentSize;

extern int	debugMode;
extern char*	types[NumTypes];

////////////////////////////////////////////////////////////////////////////
//
// Document Constructor
//
////////////////////////////////////////////////////////////////////////////

Document::Document(int cpId, CompositePart *cp)
{
    SETRANGE((char *)this, sizeof(*this));

  id = cpId;
  part = cp;
  // prepare and fill in the (indexed) document title
  
  char docTitle[TitleSize];
  sprintf(docTitle, "Composite Part %08d", cpId);
  if (debugMode) {
    cout << "Document::Document title = " << docTitle << "\n";
  }
  strncpy(title, docTitle, TitleSize);
  
  // prepare and fill in the document text
  
  char myText[DocTextLength];
  sprintf(myText, DocumentText, cpId);
  int stringLen = strlen(myText);
  text = BEGIN_NEW(DesignLib, char[DocumentSize])
  END_NEW
  int curChar, strChar;
  curChar = strChar = 0;
  while (curChar < DocumentSize-1) {
    text[curChar++] = myText[strChar++];
    if (strChar >= stringLen) { strChar = 0; }
  }
  text[curChar] = '\0';

}


////////////////////////////////////////////////////////////////////////////
//
// Document searchText Method for use in traversals
//
////////////////////////////////////////////////////////////////////////////

int Document::searchText(char c)
{
  if (debugMode) {
    cout << " Document::searchText title = " << title << "\n";
  }
  
  // count occurrences of the indicated letter (for busy work)
  
  int i = 0;
  int count = 0;
  while (text[i] != '\0') {
    if (text[i++] == c) { count++; }
  }
  
  if (debugMode) {
    cout << "[found "<< count << " " << c << "'s among" 
      <<  i << "characters]\n"; 
  }
  return count;
}


////////////////////////////////////////////////////////////////////////////
//
// Document replaceText Method for use in traversals
//
////////////////////////////////////////////////////////////////////////////

int Document::replaceText(char *oldString, char *newString)
{
  if (debugMode) {
    cout << "Document :: replaceText\n";
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
  
  if (foundMatch) {
    return 1;
  } else {
    return 0;
  }
  
}


////////////////////////////////////////////////////////////////////////////
//
// Document Destructor
//
////////////////////////////////////////////////////////////////////////////

Document::~Document()
{
  if (debugMode) {
    cout << "Document::~Document(title = " << title << "\n";
  }
  
  // reclaim space from document text
  delete text;
  
}

