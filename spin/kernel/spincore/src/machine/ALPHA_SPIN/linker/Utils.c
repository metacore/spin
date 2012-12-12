/*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Utilities to deal with coff alignment vs. machine align.
 *
 */
#include <Interface.h>

void *
coffxtrct(void *source, long size) {
    if((long)source & 7) {
	void *p = (void *) spin_malloc(size);
	bcopy(source, p, size);
	return p;
    } else
	return source;
}

void
coffdone(void *ptr, void *source) {
    if(ptr != source)
	spin_free(ptr);
}

void *
myrealloc(void *ptr, long oldsize, long newsize) {
	char *p;

	p = (char *) symlalloc(newsize);
	if(ptr) 
	{
	    if(newsize<oldsize)
	    {
		oldsize = newsize;
	    }
	    bcopy(ptr, p, oldsize);
	    free (ptr, M_DSYMBOL);
	}

	return p;
}
