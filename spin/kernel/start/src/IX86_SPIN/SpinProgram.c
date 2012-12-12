/*
 * Copyright 1994, 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 */
/*
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *	Rename run_program salhook_main_program
 *	Renamed NumMallocPages to salhook_mallocpages and brought it
 *	back here
 *
 * 02-Feb-97  Marc Fiuczynski (mef) at the University of Washington
 *	Moved SpinNumMallocPages to sal/TARGET/standalone/SAL.c and
 *	renamed it to NumMallocPages.
 *
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Created from Alpha version.
 *
 */

#include <sys/param.h>
#include <sys/kernel.h>
#include <sys/types.h>
#include <vm/vm.h>
#include <i386/include/md_var.h>

/*
 * These variables control how much memory each pool gets.
 */
#ifdef REFCOUNT
int InitSpinNumRefCountPages = 400;
int PermSpinNumRefCountPages = 100;
#else
int InitSpinNumRefCountPages = 0;
int PermSpinNumRefCountPages = 0;
#endif

int salhook_mallocpages = 3072 * 2;   /* # of untraced pages */

int SpinNumPhysFreePages = 850; /* # of SPIN physical pages */
int SpinNumTracedPages;          /* Set dynamically to use all excess memory */
int SpinNumTracedPagesMin = 1024;/* min # of traced pages */

extern vm_map_t kernel_map;
extern vm_offset_t virtual_avail, virtual_end;

/*
 * variables in SAL that tell us where memory is
 */
extern vm_offset_t	avail_start, avail_end;

/* These are <*EXTERNAL*> in RTMemExternal.i3 and PhysFreeMemExtern.i3 */
vm_offset_t     refcount_start,refcount_end;
vm_offset_t     prefcount_start,prefcount_end;
vm_offset_t     irefcount_start,irefcount_end;
vm_offset_t     pirefcount_start,pirefcount_end;
vm_offset_t     traced_start,traced_end;
vm_offset_t     ptraced_start;
vm_size_t       traced_size, refcount_size, irefcount_size;
vm_offset_t     pmem_start,pmem_end;
vm_size_t	pmem_size;

#define DB /* printf */ 


/*
 * Macros to convert bytes to pages and maegabytes
 */
#define PAGES(x)	((x)/PAGE_SIZE)
#define MEGABYTES(x)	((x)/(1024*1024))

salhook_main_program()
{
	extern vm_offset_t kmembase;	/* start of untraced mem */
	extern vm_offset_t kmemlimit;	/* end of  untraced mem  */
	vm_size_t kmemsize; /* size of untraced heap */

	/*
	 * Begin by mapping in all of the physical memory.
	 */
	pmap_init_kseg();

        /* 
	 * Report size of traced heap
         */
	SpinNumTracedPages = ((avail_end-avail_start)/PAGE_SIZE)
		- SpinNumPhysFreePages;
	if (SpinNumTracedPages < SpinNumTracedPagesMin) {
	   if (SpinNumPhysFreePages <=  
	      (SpinNumTracedPagesMin - SpinNumTracedPages)) {
	      printf("      Traced heap below minimum.  No recovery possible.\n");
	      printf("      Don't expect this to work.\n");
           } else {
	      printf("      Traced heap below min. Overriding SpinNumPhysFreePages directive.\n"); 
	      printf("      Moving %d pages to traced heap.\n",
		     (SpinNumTracedPagesMin - SpinNumTracedPages));
	      SpinNumPhysFreePages -= (SpinNumTracedPagesMin
				       - SpinNumTracedPages);
	      SpinNumTracedPages += (SpinNumTracedPagesMin
				       - SpinNumTracedPages);
           }
        }

	/*
	 * Report size of untraced heap
	 */
	kmemsize = kmemlimit-kmembase;
        printf("SPIN: Untraced Heap(%ld.%ld%ld MB), ",
	       MEGABYTES(kmemsize), MEGABYTES(10*kmemsize)%10,
	       MEGABYTES(100*kmemsize)%10);
	DB("      mapped at 0x%lx to 0x%lx\n",kmembase, kmemlimit);

        refcount_size = PAGE_SIZE * PermSpinNumRefCountPages;
	if(refcount_size != 0) {
	  prefcount_start = avail_start;
	  prefcount_end = prefcount_start+refcount_size;
	  /* FIXME: refcount_start = vmap(prefcount_start, refcount_size);  */
	  refcount_end = refcount_start + refcount_size;
	  avail_start += refcount_size;
	  printf("Refcount Area (%ld.%ld%ld MB), ",
		 MEGABYTES(refcount_size), MEGABYTES(10*refcount_size)%10,
		 MEGABYTES(100*refcount_size)%10);
	}
	
        traced_size = PAGE_SIZE * SpinNumTracedPages;
	printf("Traced Heap(%ld.%ld%ld MB), ",
	       MEGABYTES(traced_size), MEGABYTES(10*traced_size)%10,
	       MEGABYTES(100*traced_size)%10);
        
	traced_start = pmem_alloc(traced_size);  
	ptraced_start = kvtop(traced_start);

   	traced_end = traced_start + traced_size;

        irefcount_size = PAGE_SIZE * InitSpinNumRefCountPages;
	if(irefcount_size != 0) {
	  pirefcount_start = avail_start;
	  pirefcount_end = pirefcount_start+irefcount_size;
	  /* FIXME: irefcount_start = vmap(pirefcount_start, irefcount_size);*/
	  irefcount_end = irefcount_start + irefcount_size;
	  avail_start += irefcount_size;
	  printf("Initial Refcount Area (%ld.%ld%ld MB), ",
		 MEGABYTES(irefcount_size), MEGABYTES(10*irefcount_size)%10,
		 MEGABYTES(100*irefcount_size)%10);
	}
	
	/*
	 * Report size of basic physical pages
         */
        pmem_size = PAGE_SIZE * SpinNumPhysFreePages;
        pmem_start = avail_start; 
        pmem_end = pmem_start+pmem_size;
        printf("PhysFreeMem(%ld.%ld%ld MB)\n",
	       MEGABYTES(pmem_size), MEGABYTES(10*pmem_size)%10,
	       MEGABYTES(100*pmem_size)%10);
	DB("      mapped at 0x%lx to 0x%lx\n",pmem_start, pmem_end);

	printf("SPIN: Traced heap mapped from 0x%lx to 0x%lx\n",
	       traced_start, traced_end);

	spin_start();
}


void *boot_stack_bottom; 

/*  
 * Starts up spin.
 */
spin_start(void)
{
	extern long hwrpb_addr;
	char *argv[] = {"spin", 0};
	char *envp[] = {0};
	int i;

	/* capture sp before we enter M3-land, used by GC */
	boot_stack_bottom = mvesp();

	printf("Starting M3 runtime\n");
	spin_init(1, argv, envp);   /* this is in start/ALPHA_SPIN/_m3main.c */
	/* NOT REACHED */

	printf("SPIN exiting\n");
	panic("Spin exited.\n");
	standalone_halt();
}
