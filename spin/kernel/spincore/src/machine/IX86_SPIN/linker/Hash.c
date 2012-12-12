/*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 09-Feb-96  Emin Gun Sirer (egs) at the University of Washington
 *	Made hash unsigned to avoid negative indexing of the hash array.
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Hash tables for symbols.
 *
 */
#include "Interface.h"
#include "Hash.h"

/*
 * generate hash value
 */
static unsigned long
Hash_func(char *p)
{
    long          val;

    ASSERT(p);
    for(val = 0; *p; p++)
	val += (val << 1) + ((*p) ^ (val >> 28));
    return (val % MAX_SYMBOL_HASH_VALUE);
}

/*
 *  This routine assumes that there is enough space in the
 *  hash table to hold all symbols.
 *
 *  We do linear probing here, but it is not the most efficient method.
 */
extern void
Hash_add_symbol(SymbolEntry *sym, SymbolTable *st)
{
    unsigned long   hash;

    ASSERT(sym != NULL);
    ASSERT(st != NULL);
    ASSERT(st->ST_tSize);
    ASSERT(st->ST_symBlkPtr != NULL);

    hash = Hash_func(sym->SE_name);
    hash = hash % st->ST_tSize;

    /*
     * See if it's filled yet.  If it is, get a secondary
     * hash value, and start skipping by that much until an
     * empty slot is found.
     */
    for(; st->ST_hashPtr[hash] != NULL; )
	hash = ((hash + 1) % st->ST_tSize);
    
    st->ST_hashPtr[hash] = sym;
}

struct SymbolEntry *
Hash_find_symbol(char *symName, SymbolTable *st)
{
    long                     tSize;
    unsigned long                     hashValue;
    SymbolEntry            **se;

    if(st == NULL)
	return NULL;

    tSize = st->ST_tSize;
    hashValue = Hash_func(symName) % tSize;
    se        = st->ST_hashPtr;

    for(; se[hashValue] != NULL; hashValue = (hashValue+1) % tSize) {
	ASSERT(se[hashValue]->SE_name != NULL);
	if(strcmp(se[hashValue]->SE_name, symName) == 0)
	    return se[hashValue];
    }
    return NULL;
}

/*
 * Get the size of the Hash table given # of entries
 */
static long
Hash_size(long entries)
{
    return entries > 10 ? (entries * 14) / 10 : 14;
}

void *
myrealloc(void *ptr, long oldsize, long newsize) {
	char *p;

	p = (char *) spin_malloc(newsize);
	if(ptr) 
	{
		if(newsize<oldsize)
		{
			oldsize = newsize;
		}
		bcopy(ptr, p, oldsize);
	}
	return p;
}


extern void
Hash_create(SymbolTable *st)
{
    int bytes, oldsize;

    oldsize = st->ST_tSize * sizeof(SymbolEntry *);
    bytes = st->ST_tSize = Hash_size(st->ST_nSyms);
    bytes *= sizeof(SymbolEntry *);

    if(st->ST_hashPtr == NULL)
       st->ST_hashPtr = (SymbolEntry **) lalloc(bytes);
    else
       st->ST_hashPtr = (SymbolEntry **) myrealloc(st->ST_hashPtr, oldsize, bytes);
    bzero((void *)st->ST_hashPtr, bytes);
}

