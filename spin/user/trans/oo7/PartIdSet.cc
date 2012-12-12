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

#include "PartIdSet.h"

extern void *DesignLib;

// Set all hash table entries to NULL

PartIdSet::PartIdSet()
{
    for (int h = 0; h < HashTableSize; h++) {
	hashTable[h] = NULL;
    }
    emptySet = TRUe;
}


// Reset all hash table entries to NULL (after deallocating contents)

void PartIdSet::clear()
{
    for (int h = 0; h < HashTableSize; h++) {
	Member* mem;
	Member* nextMem;
	mem = hashTable[h];
	hashTable[h] = NULL;
	while (mem != NULL) {
	    nextMem = mem->next;
	    delete mem;
	    mem = nextMem;
	}
    }
    emptySet = TRUe;
}


// Add an entry to a set of part ids

void PartIdSet::insert(int val)
{
    int h = hash(val);
    BEGIN_TEMP_NEW
    Member* mem  = new Member();
    END_TEMP_NEW
//    Member* mem  = BEGIN_NEW(DesignLib, Member)
//    END_NEW;
    mem->value   = val;
    mem->next    = hashTable[h];
    hashTable[h] = mem;
    emptySet 	 = FALSe;
}


// member function to look up a record in a hashed file based on its key

int PartIdSet::contains(int val)
{
    int h = hash(val);
    Member* mem = hashTable[h];
    while (mem != NULL && mem->value != val) { mem = mem->next; }
    return (mem != NULL);
}

