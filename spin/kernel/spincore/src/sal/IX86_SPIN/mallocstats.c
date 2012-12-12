
#include <sys/types.h>
#include <sys/param.h>
#include <sys/kernel.h>
#include <vm/vm.h>
#include <sys/malloc.h>

extern struct kmembuckets kmembuckets[]; /* in kern/kern_malloc.c */
extern char **kmemnames; /* in kern/kern_malloc.c */
extern vm_offset_t kmemlimit;	/* end of       "        */
extern int *kmemtypetable;

void mallocstats(void)
{
	long totalloc=0;
	long totfree=0;
	int i,nout;

        printf("\nMemory usage by bucket:\n\n");
	printf("bucket#  elements_in_use  elements_free\n"); 
#ifdef __FreeBSD__
	for(i=0; i < MINBUCKET + 16; i++)
#else
	for(i=0; i < 16; i++)
#endif
	{
	    printf("   %2d        	%6d          %10d\n",
		   i,
		   kmembuckets[i].kb_elmpercl - kmembuckets[i].kb_totalfree,
		   kmembuckets[i].kb_totalfree);
	}

        printf("Memory usage by type: Type and Number of bytes being used\n\n");
	i=0;
	nout=0;
	while(1) {
	    if(kmemtypetable[i] == 0) {
		if(++i == M_LAST) break; 
	    } 
	    else  {
		printf("%-12s= %-9d", kmemnames[i], kmemtypetable[i]);
		nout++;
                /* if 3rd on line, put out newline */
                if(nout%3 == 0) 
		    printf("\n");
		if(++i == M_LAST) break; 
	    }
	}
	if(nout%3 != 0)
	   printf("\n");
}


