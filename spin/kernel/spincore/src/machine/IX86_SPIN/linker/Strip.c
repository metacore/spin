/*
 * Copyright 1996, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 27-Jun-96  Tian Fung Lim (tian) at the University of Washington
 *	Created.  Strip relocations and internal symbols.
 *
 */

#include "Interface.h"
#include "Resolve.h"
#include "Section.h"
#include "Symbol.h"

#undef 		PRINT
#define 	PRINT if (stripDebug) printf

extern void	StripRelocations(Module * ms);
extern void	PrintStripStats(void);
extern void	StripInternalSymbols(Module * ms);


static int 	stripDebug = 1;

/* forward decl for helper */
static void	IterateAndCompact();

void		Strip(Module * ms)
{
    StripRelocations(ms);
    StripInternalSymbols(ms);
}

/* strip relocations from a single module */
void		StripRelocations(Module * ms)
{
    Reloc *	reloc;
    Section *	ss;
    int		i;
    
    
    /* need to iterate over sections, then compact the relocation
       lists */
    if (ms == NULL) 
    {
	return;
    }
    
    for (i = 0; i < ms->MS_nSec; i++)
    {
	ss = ms->MS_sectPtr[i];
	if (ss)
	{
	    IterateAndCompact(ss);
	}
	else
	{
	    /* Null section, ignore */
	}
    }
}


static long freed=0, externUnpatched=0, externPatched=0, total=0;


/* 
   perform 2 passes...one to count how large the new table should be,
   second to allocate memory and perform copy.
   */
   
static void	IterateAndCompact(Section* ss)
{
    Reloc *	reloc, * newReloc;
    int 	i,j, toBeFreed;
    
    total += ss->SS_nReloc;
    
    for (i = 0, reloc = ss->SS_relocPtr, toBeFreed=0;
	 i < ss->SS_nReloc;
	 i++)
    {
	if (!reloc[i].RS_relocInfo.r_extern && reloc[i].RS_patched)
	{
	    freed+=1;
	    toBeFreed++;
	}
	
	if (reloc[i].RS_relocInfo.r_extern && reloc[i].RS_patched)
	{
	    externPatched+=1;
	}
	
	if (reloc[i].RS_relocInfo.r_extern && !reloc[i].RS_patched)
	{
	    externUnpatched+=1;
	}
    }

    if (toBeFreed > 0)
    {
	newReloc = (Reloc*) lalloc(sizeof(Reloc) * 
				   (ss->SS_nReloc - toBeFreed));

	/* i counts in original array, j in the new one */
	for (i = j = 0; i < ss->SS_nReloc; i++)
	{
	    if (reloc[i].RS_relocInfo.r_extern || !reloc[i].RS_patched)
	    {
		memcpy (&newReloc[j], &reloc[i], sizeof(reloc[i]));
		j++;
	    }
	}

	spin_free(reloc);
	ss->SS_relocPtr = newReloc;
	ss->SS_nReloc = j;
    }
}

/* not very useful since the stats aren't counted meaningfully */
void		PrintStripStats(void)
{
    PRINT("stats \n  freed	%d\n  externPatched	%d\n  externUnpatched	%d\n  total	%d\n",
	  freed, externPatched, externUnpatched,total);
    PRINT("  percentage freed = %d\n", freed*100/total);
}



/*
   StripInternalSymbols
   
   Removes the symbol table and any associated memory.
 */

void		StripInternalSymbols(Module *ms)
{
    /* not yet implemented */
}


