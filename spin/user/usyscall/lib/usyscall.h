#ifndef __USYSCALL_H
#define __USYSCALL_H
/*
 * HISTORY
 * 17-May-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 */

void __usyscall_bootstrap(char *domain, char *prog_name);
/* Link DOMAIN into the caller's address space. 
   PROG_NAME is used only to display error messages. */

typedef struct {
    long mhz, min, max, hits, total;
    /* Caution: up to here, the structure must be same as 
       USyscall.SpyInfo. */
    long nsamples;
    long *samples;
} usyscall_spy_info;

void __usyscall_get_spy_info(char *name, usyscall_spy_info *info);
/* Get the info about the in-kernel spy NAME. 
   All the fields in INFO are overwritten on return. 
   INFO->nsamples are an in/out parameter. 
   INFO->samples must point to valid memory region big enough
   to store INFO->nsamples of longs. */
#endif

