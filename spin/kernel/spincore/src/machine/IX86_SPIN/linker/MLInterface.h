/*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 10-Jul-96  Tian Fung Lim (tian) at the University of Washington
 *	Created.  Defines bunch of new mallocs that allocate out of new
 *	pools.
 */
#ifndef _MLSDLDINTERFACE_H_
#define _MLSDLDINTERFACE_H_
#include <Interface.h>
#include <sys/malloc.h>

/*
   The following mallocs allocate to the following pools :
   
   sym - anything symbol or symbol table related, including hash tables.
   sec - section structure overhead.
   reloc - anything relocation related
   module - module structures, COFF section and data (basically the
            module pools)

   To add other pools, just edit malloc.h
   
   */

#define GENERICALLOC(which, A) malloc(A, BUCKETINDEX(A), which,0)

#define symlalloc(A)	GENERICALLOC(M_DSYMBOL,A)
#define seclalloc(A)	GENERICALLOC(M_DSECTION,A)
#define reloclalloc(A)	GENERICALLOC(M_DRELOC,A)
#define lalloc(A)	GENERICALLOC(M_DMODULE,A)
/*#define LALLOC(A) 	symlalloc(sizeof(struct A))*/



#endif /* _MLSDLDINTERFACE_H_ */

