/*
 * StringHashTable.h
 *
 * Created by Marvin Theimer, January 19, 1991.
 *
 * String hash table package.
 *
 * This package stores pointers to user data in a hash table.  It uses
 * null-terminated ascii strings as keys.
 *
 * The package supplies four operations:
 *   StringHashCreate:    - Create a new hash table of a given size.
 *   StringHashDestroy:   - Deallocate a hash table and reclaim its resources.
 *   StringHashFind       - Find/insert an element into a hash table.
 *   StringHashDelete     - Delete an element from a hash table.
 *   StringHashIterate    - Apply a function to all elements in a hash table.
 * The package's StringHashFind function returns pointers to the elements stored
 * in the hash table, which consist of a field for the tuple and a field
 * where a user may put a pointer to the data they wish to store.
 * This allows a user to set the user data pointer
 * after a hash table element has already been found/created.  This is
 * useful when we don't want to malloc a new user data record until
 * we're sure that one doesn't already exist.
 *
 * The hash table can accomodate an arbitrary number of elements.  However,
 * the size of table into which keys are hashed is fixed at creation time;
 * implying that performance will deteriorate as more and more elements
 * are inserted.  The hash table itself consists of a keytable into which
 * keys are hashed and a linked list of "free blocks" that contain hash
 * table elements.  Whenever insertion uses up the elements in the free
 * blocks list, another block is allocated and added to the list.  When the
 * hash table is destroyed, both the key table and the list of free blocks
 * is freed.
 *
 * The hash table iterator function operates in time proportional to the
 * size of the table.
 */

/* $Id: StringHashTable.h,v 1.1 1996/02/09 18:13:36 mef Exp $ */

/* Copyright (c) 1991, 1992 Xerox Corporation.  All rights reserved.

   Use and copying of this software and preparation of derivative works
   based upon this software are permitted.  Any distribution of this
   software or derivative works must comply with all applicable United
   States export control laws.  This software is made available AS IS, and
   Xerox Corporation makes no warranty about the software, its performance
   or its conformity to any specification. */

#ifndef NIL
#define NIL 0
#endif


/*
 * Opaque hash table handle.
 */
typedef char *StringHashTableHandle;


/* 
 * This package uses null-terminated ascii strings as keys
 */
typedef char *StringHashKey;


/*
 * The hash table stores elements consisting of a key field and a field
 * containing a pointer to user data.
 */
typedef struct StringHashElementRep
{
  StringHashKey key;
  char *userData;
} *StringHashElement;


/*
 * Actions that StringHashFind should take.
 * If action == insert then StringHashFind will insert a new element if none is
 * found.  If action == find then StringHashFind will NOT insert a new element
 * when none is found and will return NIL.
 */
typedef enum {insert, find} StringHashAction;


/*
 * Hash table statistics record.
 */
typedef struct StringHashStatisticsRecRep
{
  int numElements;		/* Number of records in the hash table. */
  int numFinds;			/* Returns number of HashFinds executed. */
  float avFindLength;		/* Returns av. number of records examined in a
				   hash chain during HashFind. */
  float avChainLength;		/* Returns. av. chain length in hash table. */
  int maxChainLength;		/* Maximum length chain in the table. */
} *StringHashStatisticsRec;


/*
 * Function definitions.
 */


/*
 * StringHashCreate
 * Create a new hash table to hold numElements elements.
 * If there is not enough memory to create the hash table then NIL is returned.
 * The mallocFcn parameter specifies which memory allocator to use for
 * creating the hash table and any subsequent memory resources it needs.
 * Passing in NIL for this argument will result in use of the system's
 * standard malloc() function.
 * The freeFcn parameter specifies a memory free function to use when
 * freeing memory resources.  Passing in NIL for this argument will result
 * in use of the system's standard free() function
 */
StringHashTableHandle StringHashCreate(/* numElements, mallocFcn, freeFcn */);
 /* int numElements;
    int (*mallocFcn)();
    void (*freeFcn)(); */


/*
 * StringHashDestroy
 * Deallocate a hash table.  Reclaims the memory used by the hash table.
 */
void StringHashDestroy(/* hashTable */);
 /* StringHashTableHandle hashTable; */


/*
 * StringHashFind
 * Search for the specified key in a hash table and return a pointer to
 * the element found.
 * If no element is found and action specifies insert then a new element
 * is inserted into the hash table and a pointer to it is returned.
 * If no element is found and action specifies FIND then the function does
 * NOT insert a new element and returns NIL.
 * inserted  returns a boolean indicating if a new element was inserted,
 * thereby allowing one to detect when an insert action actually inserted
 * a new element.  If a value of NIL is passed in then nothing is passed
 * back.
 *
 * If inserting a new element causes the package to run out of memory then
 * the function returns NIL.
 */
StringHashElement StringHashFind(/* hashTable, key, action, inserted */);
 /* StringHashTableHandle hashTable;
    StringHashKey key;
    StringHashAction;
    int *inserted; */


/*
 * StringHashDelete
 * Delete an element from a hash table.
 */
void StringHashDelete(/* hashTable, key */);
 /* StringHashTableHandle hashTable;
    StringHashKey key; */


/*
 * StringHashIterate
 * Apply a function to each element of the hash table in turn.
 * The supplied function should take an StringHashElement and a pointer to
 * to user-supplied arguments as input and
 * should return a boolean value indicating whether or not to continue
 * iterating over the hash table elements (TRUE => continue):
 *   int fcn(hashElement, userArgs)
 *           StringHashElement hashElement;
 *           char *userArgs;
 */
void StringHashIterate(/* hashTable, fcn, userArgs */);
 /*  StringHashTableHandle hashTable;
     int (*fcn)();
     char *userArgs; */


/*
 * StringHashStatistics
 * Return statistics on hash table usage.
 */
void StringHashStatistics(/*hashTable, stats*/);
     /*StringHashTable hashTable;*/
     /*StringHashStatisticsRec stats;*/


/*
 * StringHashResetStatistics
 * Reset the statistics variables for a hash table.
 */
void StringHashResetStatistics(/*hashTable*/);
     /*StringHashTable hashTable;*/

