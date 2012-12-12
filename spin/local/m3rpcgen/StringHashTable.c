/*
 * StringHashTable.c
 *
 * Created by Marvin Theimer, December 28, 1990.
 *
 * Implementation file for the StringHashTable.h interface include file.
 */

/* $Id: StringHashTable.c,v 1.1 1996/02/09 18:13:35 mef Exp $ */

/* Copyright (c) 1990, 1991, 1992 Xerox Corporation.  All rights reserved.

   Use and copying of this software and preparation of derivative works
   based upon this software are permitted.  Any distribution of this
   software or derivative works must comply with all applicable United
   States export control laws.  This software is made available AS IS, and
   Xerox Corporation makes no warranty about the software, its performance
   or its conformity to any specification. */

#include "StringHashTable.h"

#define FALSE 0
#define TRUE 1


#define FREE_BLOCK_ELEMENT_NUMBER 64 /* Number of hash table elements in
				      a free block. */


typedef struct StringHashInternalElementRep
{
  struct StringHashInternalElementRep *next;
  struct StringHashElementRep element;
} *StringHashInternalElement;

typedef struct StringHashFreeElementsRep
{
  struct StringHashFreeElementsRep *next;
  struct StringHashInternalElementRep elements[FREE_BLOCK_ELEMENT_NUMBER];
} *StringHashFreeElements;

typedef struct StringHashTableRep
{
  StringHashFreeElements freeBlocks; /* List of free blocksl */
  StringHashInternalElement nextFree; /* List of free hash elements. */
  StringHashInternalElement *keyTable; /* Ptr to an array of
					  StringHashInternalElement */
  int tableSize;		/* Size of key table (in elements). */
  int numFinds;			/* The number of Finds done. */
  int sumFindLength;		/* Sum of the number of records examined
				   before Find returned. */
  int (*mallocFcn)();		/* Memory allocator to use. */
  void (*freeFcn)();		/* Memory allocator free function to use. */
} *StringHashTable;


void StringHashAllocateFreeBlock();


StringHashTableHandle StringHashCreate(numElements, mallocFcn, freeFcn)
     int numElements;
     int (*mallocFcn)();
     void (*freeFcn)();
{
  StringHashTable ht;
  int i, j, numFreeBlocks;
  StringHashFreeElements fp;
  extern int malloc();
  extern void free();

  if (mallocFcn == NIL) mallocFcn = malloc;
  if (freeFcn == NIL) freeFcn = free;

  ht = (StringHashTable) mallocFcn(sizeof(struct StringHashTableRep));
  if (ht == NIL)
    {
      return (NIL);
    }
  ht->mallocFcn = mallocFcn;
  ht->freeFcn = freeFcn;
  ht->tableSize = numElements * 2 - 1;
  ht->numFinds = 0;
  ht->sumFindLength = 0;
  ht->keyTable = (StringHashInternalElement *)
                 mallocFcn(ht->tableSize * sizeof(StringHashInternalElement));
  if (ht->keyTable == NIL)
    {
      ht->freeFcn(ht);
      return (NIL);
    }
  for (i = 0; i < ht->tableSize; i++)
    {
      ht->keyTable[i] = NIL;
    }
  numFreeBlocks = (numElements + FREE_BLOCK_ELEMENT_NUMBER - 1) /
                    FREE_BLOCK_ELEMENT_NUMBER;
  ht->freeBlocks = NIL;
  ht->nextFree = NIL;
  for (i = 0; i < numFreeBlocks; i++)
    {
      fp = (StringHashFreeElements)
	    mallocFcn(sizeof(struct StringHashFreeElementsRep));
      if (fp == NIL)
	{
	  StringHashDestroy(ht);
	  return (NIL);
	}
      fp->next = ht->freeBlocks;
      ht->freeBlocks = fp;
      for (j = 0; j < FREE_BLOCK_ELEMENT_NUMBER; j++)
	{
	  fp->elements[j].next = &(fp->elements[j+1]);
	}
      fp->elements[FREE_BLOCK_ELEMENT_NUMBER-1].next = ht->nextFree;
      ht->nextFree = &(fp->elements[0]);
    }
  return ((StringHashTableHandle)ht);
}


void StringHashDestroy(hashTable)
     StringHashTableHandle hashTable;
{
  StringHashTable ht = (StringHashTable) hashTable;
  StringHashFreeElements fp, nextFp;

  ht->freeFcn(ht->keyTable);
  for (fp = ht->freeBlocks; fp != NIL; fp = nextFp)
    {
      nextFp = fp->next;	/* Do this because we can't rely on the fields
				 in fp after the call to free? */
      ht->freeFcn(fp);
    }
  ht->freeFcn(ht);
}


/*
 * Compute a hash index from the key.
 * We use the sum of the char bit values modulo the table size.
 */
int StringHashIndex(ht, key)
     StringHashTable ht;
     StringHashKey key;
{
  int indx = 0;
  int cnt = 0;
  char *p = key;

  while (*p != '\0')
    {
      indx += (((int)(*p)) << cnt);
      if (cnt == 15) cnt = 0;
      else cnt++;
      p++;
    }
  indx = indx  % ht->tableSize;
  return (indx);
}


/*
 * Copy a (string) key and return a pointer to the copy.
 */
char *StringHashCopyKey(ht, key)
     StringHashTable ht;
     StringHashKey key;
{
  char *p;

  p = (char *) ht->mallocFcn(strlen(key)+1);
  strcpy(p, key);
  return (p);
}


StringHashElement StringHashFind(hashTable, key, action, inserted)
     StringHashTableHandle hashTable;
     StringHashKey key;
     StringHashAction action;
     int *inserted;
{
  StringHashTable ht = (StringHashTable) hashTable;
  unsigned hashIndex;
  StringHashInternalElement elem = NIL;
  int findLength;
  int insertFlag = FALSE;

  ht->numFinds += 1;
  /* Try to find the specified element. */
  hashIndex = StringHashIndex(ht, key);
  for (elem = ht->keyTable[hashIndex], findLength = 1;
       elem != NIL;
       elem = elem->next, findLength++)
    {
      if (strcmp(elem->element.key, key) == 0) break;
    }
  if (elem == NIL) findLength--;
  ht->sumFindLength += findLength;

  if ((elem == NIL) && (action == insert))
    {
      /* Insert a new element. */
      elem = ht->nextFree;
      if (elem == NIL)
	{
	  StringHashAllocateFreeBlock(ht);
	  elem = ht->nextFree;
	  if (elem == NIL) return (NIL); /* Out of memory. */
	}
      ht->nextFree = elem->next;
      elem->next = ht->keyTable[hashIndex];
      elem->element.key = StringHashCopyKey(ht, key);
      elem->element.userData = NIL;
      ht->keyTable[hashIndex] = elem;
      insertFlag = TRUE;
    }

  if (inserted != NIL) *inserted = insertFlag;
  return ((elem == NIL) ? NIL : &(elem->element));
}


void StringHashDelete(hashTable, key)
     StringHashTableHandle hashTable;
     StringHashKey key;
{
  StringHashTable ht = (StringHashTable) hashTable;
  unsigned hashIndex;
  StringHashInternalElement elem, prevElem;

  /* Try to find the specified element. */
  hashIndex = StringHashIndex(ht, key);
  elem = ht->keyTable[hashIndex];
  if (strcmp(elem->element.key, key) == 0)
    {
      ht->keyTable[hashIndex] = elem->next;
    }
  else
    {
      for (prevElem = elem, elem = elem->next;
	   elem != NIL;
	   prevElem = elem, elem = elem->next)
	{
	  if (strcmp(elem->element.key, key) == 0) break;
	}
      if (elem != NIL)
	{
	  prevElem->next = elem->next;
	}
    }

  /* Give the key string's space back. */
  ht->freeFcn(elem->element.key);
  /* elem points to the deleted element (or is NIL) at this point.
     Put it back on the free list. */
  elem->next = ht->nextFree;
  ht->nextFree = elem;
}


void StringHashIterate(hashTable, fcn, userArgs)
     StringHashTable hashTable;
     int (*fcn)();		/* int fcn(hashElement, userArgs)
				           StringHashElement hashElement; */
     char *userArgs;		/* Ptr to user-supplied arguments for fcn. */
{
  StringHashTable ht = (StringHashTable) hashTable;
  int i;
  StringHashInternalElement elem;

  for (i = 0; i < ht->tableSize; i++)
    {
      for (elem = ht->keyTable[i]; elem != NIL; elem = elem->next)
	{
	  if (!fcn(&(elem->element), userArgs)) return;
	}
    }
}


void StringHashStatistics(hashTable, stats)
     StringHashTable hashTable;
     StringHashStatisticsRec stats;
{
  StringHashTable ht = (StringHashTable) hashTable;
  int chainSum, chainLength, maxChainLength;
  int i;
  int numElements, numChains;
  StringHashInternalElement elem;

  stats->numFinds = ht->numFinds;
  if (ht->numFinds == 0)
      stats->avFindLength = 0.0;
  else
      stats->avFindLength = ((float)(ht->sumFindLength)) /
	                      ((float)(ht->numFinds));

  numElements = 0;
  numChains = 0;
  maxChainLength = 0;
  chainSum = 0;
  for (i = 0; i < ht->tableSize; i++)
    {
      if (ht->keyTable[i] != NIL) numChains++;
      for (elem = ht->keyTable[i], chainLength = 0;
	   elem != NIL;
	   elem = elem->next, chainLength++)
	{
	  numElements++;
	}
      chainSum += chainLength;
      if (chainLength > maxChainLength)
	maxChainLength = chainLength;
    }
  if (numChains == 0)
      stats->avChainLength = 0.0;
  else
      stats->avChainLength = ((float)chainSum) / ((float)(numChains));
  stats->numElements = numElements;
  stats->maxChainLength = maxChainLength;
}


void StringHashResetStatistics(hashTable)
     StringHashTable hashTable;
{
  StringHashTable ht = (StringHashTable) hashTable;

  ht->numFinds = 0;
  ht->sumFindLength = 0;
}


/*** Internal routines ***/


/*
 * StringHashAllocateFreeBlock
 * Allocate another free block of hash table elements.
 */
void StringHashAllocateFreeBlock(ht)
     StringHashTable ht;
{
  StringHashFreeElements fp;
  int j;

  fp = (StringHashFreeElements)
        ht->mallocFcn(sizeof(struct StringHashFreeElementsRep));
  if (fp == NIL)
    {
      return;
    }
  fp->next = ht->freeBlocks;
  ht->freeBlocks = fp;
  for (j = 0; j < FREE_BLOCK_ELEMENT_NUMBER; j++)
    {
      fp->elements[j].next = &(fp->elements[j+1]);
    }
  fp->elements[FREE_BLOCK_ELEMENT_NUMBER-1].next = ht->nextFree;
  ht->nextFree = &(fp->elements[0]);
}
