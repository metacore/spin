/*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 */
/*
 * HISTORY
 * 28-Sep-97  Tian Fung Lim (tian) at the University of Washington
 *	Modified hardclock() to use the cycle counter to bump up the
 *	time.  It's much more accurate now.
 *
 * 18-Jan-97  Yasushi Saito (yasushi) at the University of Washington
 *	pmap_unmap_page is eliminated. Turns out it's same as pmap_remove_all. 
 *	So stupid.
 *	 
 * 25-Sep-96  Marc Fiuczynski (mef) at the University of Washington
 *	Got rid of print statements in pmap_unmap_page.
 *
 * 3-Sep-96  becker at the University of Washington
 *	Fixed valloc define for DigitalUnix 4.0 cc.  From gallatin.
 *	
 * 16-Jun-96  Yasushi Saito (yasushi) at the University of Washington
 *	Added phys_to_kseg.
 *	
 * 10-May-96 oystr at the University of Washington
 *	Added select_init call;  see miscsupport.c for details.
 *
 * 07-May-96  Stefan Savage (savage) at the University of Washington
 *	Always make checkpte() useable
 *
 * 29-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Moved malloc/free to MemoryForSAL.c (to move weird M3 upcalls 
 *	out of sal). Also defined checkpte() function.  Useful for debugging
 *	pmap.  (to use this you must change pmap_pt_access() in pmap.c
 *	to not be static).  
 *
 * 27-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Fixed fucking bug in vmap (we incorrectly called pmap_valid_page())
 *
 * 20-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Separated from upcalls.c
 *
 * 09-Mar-96  Stefan Savage (savage) at the University of Washington
 *	Added mallocstats.
 *
 * 22-Feb-96  Charles Garrett (garrett) at the University of Washington
 *	Changed ALPHA_OSF to ALPHA_SPIN.
 *
 * 08-Feb-96  Stefan Savage (savage) at the University of Washington
 *	Did easy cleanup.  Lots more needed.
 *
 * 20-Aug-95  Marc Fiuczynski (mef) at the University of Washington
 *	kmem_free kr variable not initialized.  Depending on what's
 *	on the stack it might not equal KERN_SUCCESS and panic.
 *	Fixed by just setting kr = KERN_SUCCESS if not PICCO.
 *
 * 28-Apr-95  David Becker (becker) at the University of Washington
 *	Created
 */
#include <mach/vm_param.h>
#include <vm/vm_map.h>
#include <vm/vm_kern.h>
#include <sys/malloc.h>
#include <machine/pmap.h>
#include <kern/zalloc.h>
#include <kern/macro_help.h>
#include <kern/task.h>
#include <sys/proc.h>
#include <machine/pcb.h>
#include <sal/cyclecount.h>

extern vm_offset_t	zdata;
extern vm_size_t	zdata_size;
extern pmap_t current_pmap;

static struct proc current_proc_struct;
static struct map_spoof map_spoof_struct;

static vm_offset_t     vm_managed_pages;
static struct vm_map	vm_map_list[50];

void
setup_main_proc(void)
{
	current_pmap = kernel_pmap;
	current_task.map = current_pmap;
	current_proc = &current_proc_struct;
	current_proc->map = &map_spoof_struct;
	map_spoof_struct.vm_pmap = current_pmap;
	u.u_procp = current_proc;
}


vm_offset_t
vmap(vm_offset_t paddr, vm_size_t size)
{
	extern vm_offset_t virtual_avail;
	vm_offset_t oldvaddr = virtual_avail;
	vm_size_t o;
	vm_size_t amount_to_map;
	vm_offset_t vaddr = virtual_avail;

	paddr = trunc_page(paddr);
	size = round_page(size);

	amount_to_map = round_page(size);
	for (o = amount_to_map; o != 0; o -= PAGE_SIZE) {
		while (pmap_valid_page(paddr, TRUE) != 0) {
			printf(".");
			paddr += PAGE_SIZE;
		}
		pmap_enter(kernel_pmap, vaddr, paddr,
			   VM_PROT_READ|VM_PROT_WRITE, TRUE,
			   VM_PROT_READ|VM_PROT_WRITE);
		vaddr += PAGE_SIZE;
		paddr += PAGE_SIZE;		
	}								
	blkclr((caddr_t) (vaddr - amount_to_map), amount_to_map);
	virtual_avail = vaddr;
	mtpr_tbi(-2L);
	return (oldvaddr);
}

static int typetablearray[M_LAST*sizeof(int)];
static int typetablemax[M_LAST*sizeof(int)];
vm_mem_init()
{
        extern vm_offset_t avail_start, avail_end;
	extern vm_offset_t virtual_avail, virtual_end;
	extern int *kmemtypetable;
	extern int *kmemtypemax;
	register vm_offset_t start;
	vm_offset_t vaddr;
	vm_offset_t array_start;
	vm_size_t pmap_array_size;
	vaddr = virtual_avail;
	start = round_page(avail_start);

	kmemtypetable=(int*)typetablearray;
	kmemtypemax=(int*)typetablemax;



	/*
	 *	Get physically contiguous addresses.
	 */
#define PMAP_MAP(size)							\
	MACRO_BEGIN							\
	vm_size_t	o;						\
	vm_size_t	amount_to_map;					\
									\
	amount_to_map = round_page(size);				\
	for (o = amount_to_map; o != 0; o -= PAGE_SIZE) {		\
		while (!pmap_valid_page(start))				\
			start += PAGE_SIZE;				\
		pmap_enter(kernel_pmap, vaddr, start,			\
			VM_PROT_READ|VM_PROT_WRITE, FALSE,		\
			VM_PROT_READ|VM_PROT_WRITE);			\
		vaddr += PAGE_SIZE;					\
		start += PAGE_SIZE;					\
	}								\
	blkclr((caddr_t) (vaddr - amount_to_map), amount_to_map);	\
	MACRO_END

	{
		vm_size_t       size;

		zdata_size = round_page(10 * sizeof(struct zone));
/*		zdata = (vm_offset_t) vaddr; */
		zdata = (vm_offset_t) PHYS_TO_KSEG(avail_start);
		size = zdata_size;
		start = avail_start + size; /* hack */

/*		PMAP_MAP(size); */
	}

	
	virtual_avail = vaddr;
	avail_start = start;
	
	spin_map_entry_init();
	pmap_array_size=round_page(pmap_page_array_size(avail_start,avail_end));
	pmap_page_array_init(avail_start,avail_end, PHYS_TO_KSEG(avail_start));
	avail_start += pmap_array_size;
	vm_managed_pages = atop(avail_end  - avail_start);
	zone_bootstrap();
	kmem_init(virtual_avail, virtual_end);
        pmap_init();
	zone_init((vm_size_t)round_page(avail_end - avail_start));
	select_init();
}


/* from mem.c */
void init_zero_page()
{
}
vm_offset_t vm_min_kernel_address=((vm_offset_t)0xffffffff80000000);


extern vm_offset_t	avail_start, avail_end;
extern vm_offset_t	virtual_avail, virtual_end;



/*
 *
 *	Memory management
 *
 */

#define SPIN_NUM_MAP_ENTRIES (512)

static struct vm_map_entry	vm_map_entries[SPIN_NUM_MAP_ENTRIES];
static int vm_map_list_index = 0;
static int spin_map_debug = 0;

/* 
 * simple minded map entry management here for clients of OSF/1 kernel maps.
 */
spin_map_entry_init()
{
	int i;
	for (i=0;i<SPIN_NUM_MAP_ENTRIES;i++) 
	{
		vm_map_entries[i].vme_private = 0;
	}
	if (spin_map_debug) {
		printf("spin_map_entry_init: initialized list of vm_map_entries\n");
	}
}

void gimmeabreak(void);
#define panic(x) mefDebugger(x)
static void mefDebugger(const char* message)
{
	printf("panic: %s",message);
	gimmeabreak();
}

vm_map_entry_t 
spin_get_map_entry(vm_size_t start, 
		  vm_size_t end,
		  vm_map_entry_t prev, 
		  vm_map_entry_t next)
{
	int i;
        vm_map_entry_t e = 0;

	if (spin_map_debug)
		printf("spin_get_map_entry: getting map entry\n");
	for (i=0;i<SPIN_NUM_MAP_ENTRIES;i++)  {
		if (vm_map_entries[i].vme_private == 0)  {
			e = &vm_map_entries[i];
			break;
                }
	}
        if (e)  {
		e->vme_private = 1;
		e->vme_start = start;
		e->vme_end = end;
		e->vme_prev = prev;
		e->vme_next = next;
        } else  {
		panic("spin_get_map_entries: no more map entries");
	}
	return e;
}

spin_free_map_entry(vm_map_entry_t old_entry)
{
	if (spin_map_debug) 
		printf("spin_free_map_entry: freeing map entry\n");
	if (old_entry->vme_private > 0)   {
		old_entry->vme_private = 0;
		/* for good measure */
		bzero(old_entry, sizeof(struct vm_map_entry));
	} else {
		panic("spin_free_map_entry: freeing free map entry");
	}
}

kern_return_t
kernel_memory_allocate(
        register vm_map_t       map,
	register vm_offset_t    *addrp,
	register vm_size_t      size, 
	register vm_offset_t    mask,
	int                     flags)
{
	*addrp = kmem_alloc(map, size);
	if (*addrp == 0) return KERN_RESOURCE_SHORTAGE;
	return KERN_SUCCESS;
}

/*
 *	kmem_alloc:
 *
 *	Allocate wired-down memory in the kernel's address map
 *	or a submap.  The memory is not zero-filled.
 */

vm_offset_t 
kmem_alloc_pageable(
	vm_map_t	map,
	vm_size_t	size)
{
	vm_offset_t	addrp;
	if (kernel_memory_allocate(map, &addrp, size, 0, 0) != KERN_SUCCESS)
		return 0;
	else
		return addrp;
}


/*
 * Try to coalesce the map entries surrounding the current one
 */

static void
kmem_coalesce(vm_map_entry_t entry)
{
	vm_map_entry_t next_entry;
	vm_map_entry_t prev_entry;

	next_entry = entry->vme_next;
	if (entry->vme_end == next_entry->vme_start) {
		if (spin_map_debug)	{
			printf("kmem_coalesce from 0x%lx to 0x%lx\n",
				entry->vme_start, next_entry->vme_end);
		}
		entry->vme_end = next_entry->vme_end;  
		next_entry->vme_next->vme_prev = entry; 
		entry->vme_next = next_entry->vme_next;
		spin_free_map_entry(next_entry);
	}
	prev_entry = entry->vme_prev;
	if (entry->vme_start == prev_entry->vme_end) {
		if (spin_map_debug)	{
			printf("kmem_coalese from 0x%lx to 0x%lx\n",
				prev_entry->vme_start, entry->vme_end);
		}
		entry->vme_start = prev_entry->vme_start;  
		prev_entry->vme_prev->vme_next = entry; 
		entry->vme_prev = prev_entry->vme_prev;
		spin_free_map_entry(prev_entry);
	}
}



vm_offset_t 
kmem_alloc(
	vm_map_t	map,
	vm_size_t	size)
{
	vm_offset_t	addrp;
	vm_map_entry_t	next_entry;
	vm_offset_t	start;
	vm_offset_t	end;

	vm_size_t       orig_size = size;

	spl_t s;
	s = splextreme();
	if (spin_map_debug)  {
		printf("in kmem_alloc size=%d (0x%lx 0x%lx 0x%lx)\n",
			size,
			&vm_map_entries[0],
			&vm_map_entries[SPIN_NUM_MAP_ENTRIES-1],
			map);
		printf("list entries before:\n");

		for (next_entry = vm_map_first_entry(map);
		     next_entry != vm_map_to_entry(map);
	     	     next_entry = next_entry->vme_next)  {
			printf("start = %lx, end = %lx\n",
			      next_entry->vme_start,
			      next_entry->vme_end);
		}
	}
	size = alpha_round_page(size);

	for (next_entry = vm_map_first_entry(map);
	     next_entry != vm_map_to_entry(map);
     	     next_entry = next_entry->vme_next)  {
		if ((next_entry->vme_end - next_entry->vme_start) >= size)
		{
		   start = next_entry->vme_start;
		   next_entry->vme_start+=size;
		   if (next_entry->vme_start==next_entry->vme_end)
		   {
			   vm_map_entry_t prev = next_entry->vme_prev;
                           prev->vme_next = next_entry->vme_next;
                           next_entry->vme_next->vme_prev = prev;
			   spin_free_map_entry(next_entry);
                           next_entry = prev;
                   }
		   else
			   kmem_coalesce(next_entry);
		
                   if (spin_map_debug)  {
       			   printf("list entries after:\n");
			   next_entry = vm_map_first_entry(map);
			   while (next_entry != vm_map_to_entry(map)) {
				   printf("start = %lx, end = %lx\n",
					  next_entry->vme_start,
					  next_entry->vme_end);
				   next_entry = next_entry->vme_next;
			   }
			   printf("returning memory chunk  %lx\n",start);
		   }
		   splx(s);
		   return(start);
	        }
	}
#if 0
	printf("kmem_alloc(%lx,%d,%d) is looking for an axe and found nothing\n",
	       map,size,orig_size);

	for (next_entry = vm_map_first_entry(map);
	     next_entry != vm_map_to_entry(map);
	     next_entry = next_entry->vme_next)  {
		printf("start = %lx, end = %lx\n",
		      next_entry->vme_start,
		      next_entry->vme_end);
		}

	splx(s);
	panic("kmem_alloc");
	/* NOT REACHED */
#endif
	printf("kmem_alloc(%lx,%d,%d) no memory\n",map,size,orig_size);
	splx(s);
	return (0);
}


/*
 *	kmem_suballoc:
 *
 *	Allocates a map to manage a subrange
 *	of the kernel virtual address space.
 *
 *	Arguments are as follows:
 *
 *	parent		Map to take range from
 *	size		Size of range to find
 *	min, max	Returned endpoints of map
 *	pageable	Can the region be paged
 */
vm_map_t
kmem_suballoc(register vm_map_t parent,
        vm_offset_t *min,
        vm_offset_t *max,
        vm_size_t size,
        boolean_t pageable)
{
	vm_offset_t kernel_min;
        vm_map_t result;
	extern vm_offset_t avail_start;
	vm_map_entry_t new_entry;
	spl_t s;

	s = splextreme();
	
	kernel_min = PHYS_TO_KSEG(avail_start);
	if (vm_map_list_index > 15) {
		printf("kmem_suballoc(upcalls.c): Nobody loves me, everybody hates me, now I gotta eat a worm\n");
		halt_cpu();
	}
	result=&(vm_map_list[vm_map_list_index++]);
	
        size = round_page(size);
        *min = kernel_min;
        *max = *min + size;
	
#ifdef WATCHMEM
	printf("sub map %lx, min %lx, max %lx, size %lx\n",
	       result,*min,*max,*max-*min);
#endif
	result->vm_min_offset = *min;
	result->vm_max_offset = *max;

	new_entry = spin_get_map_entry(*min,*max,
				       vm_map_to_entry(result),
				       vm_map_to_entry(result));


        vm_map_first_entry(result) = new_entry; 
	vm_map_last_entry(result) =  new_entry;

	avail_start += size;
	splx(s);
        return (vm_map_t)result;
}

/*
 *	kmem_free:
 *
 *	Release a region of kernel virtual memory allocated
 *	with kmem_alloc, kmem_alloc_wired, or kmem_alloc_pageable,
 *	and return the physical pages associated with that region.
 */

void
kmem_free(
	vm_map_t	map,
	vm_offset_t	addr,
	vm_size_t	size)
{
	kern_return_t kr;
	vm_offset_t start;
	vm_offset_t end;
	vm_map_entry_t new_entry;
	vm_map_entry_t next_entry;
	vm_map_entry_t prev_entry;
	spl_t s;

	s = splextreme();

	start = addr;
	end = addr+size;

	if  (spin_map_debug)  {
		printf("in kmem_free (0x%lx 0x%lx 0x%lx)\n",
			&vm_map_entries[0],
			&vm_map_entries[SPIN_NUM_MAP_ENTRIES-1],
			map);
		printf("list entries before:\n");


		for (next_entry = vm_map_first_entry(map);
		     next_entry != vm_map_to_entry(map);
	     	     next_entry = next_entry->vme_next)  {        
			printf("start = %lx, end = %lx\n",
			      next_entry->vme_start,
			       next_entry->vme_end);
		}
	}


	for (next_entry = vm_map_first_entry(map);
	     next_entry != vm_map_to_entry(map);
     	     next_entry = next_entry->vme_next)  {
		if (start < next_entry->vme_start)
			break;
	}
	new_entry = spin_get_map_entry(start, end, 
			next_entry->vme_prev, next_entry);


	/* we should be right before this entry */
	next_entry->vme_prev->vme_next = new_entry;
	next_entry->vme_prev = new_entry;

	/* Now coalesce entries next to each other */
	kmem_coalesce(new_entry);

	if (spin_map_debug)  {
		printf("list entries after:\n");
		for (next_entry = vm_map_first_entry(map);
		     next_entry != vm_map_to_entry(map);
	     	     next_entry = next_entry->vme_next)  {
			printf("start = %lx, end = %lx\n",
			      next_entry->vme_start,
			      next_entry->vme_end);
		}
	}

	splx(s);
	kr = KERN_SUCCESS;
	if (kr != KERN_SUCCESS)
		panic("kmem_free");
}



void 
kmem_init(vm_offset_t start, vm_offset_t end)
{
	extern vm_offset_t avail_start,avail_end;
	vm_map_entry_t new_entry;

	kernel_map=&(vm_map_list[vm_map_list_index++]);

	kernel_map->vm_min_offset = PHYS_TO_KSEG(avail_start);
	kernel_map->vm_max_offset = kernel_map->vm_min_offset + (PAGE_SIZE*250);

	new_entry = spin_get_map_entry(kernel_map->vm_min_offset,
				       kernel_map->vm_max_offset,
				       vm_map_to_entry(kernel_map),
				       vm_map_to_entry(kernel_map));

        vm_map_first_entry(kernel_map) = new_entry;
	vm_map_last_entry(kernel_map) =  new_entry;

#ifdef WATCHMEM
	printf("kmem_init: min %#lx max %#lx  avail going from %#lx to %#lx\n",
		kernel_map->vm_min_offset, kernel_map->vm_max_offset,
		avail_start, avail_start +  (PAGE_SIZE*250));
	printf("kernel map = %lx\n",kernel_map);
#endif
	
	avail_start +=  (PAGE_SIZE*250);
}


vm_offset_t phys_to_kseg(vm_offset_t phys)
{
    return (vm_offset_t) PHYS_TO_KSEG(phys);
}
vm_offset_t kseg_to_phys(vm_offset_t virt)
{
    return (vm_offset_t) KSEG_TO_PHYS(virt);
}

/*
 * vm_set_page_size()
 * Source: local 
 * Called from: upcalls.c
 */
void
vm_set_page_size(long dummy)
{
}

extern struct kmembuckets kmembuckets[16];
extern int *kmemtypetable;
extern char kmemnames [M_LAST][KMEMNAMSZ];
extern vm_offset_t kmembase;	/* start of untraced mem */
extern vm_offset_t kmemlimit;	/* end of       "        */

mallocstats()
{
	long totalloc=0;
	long totfree=0;
	int i,nout;

        printf("\nMemory usage by bucket:\n\n");
	printf("bucket#  element_size  elements_in_use  elements_free  bytes_in_use\n"); 
	for(i=0; i < 16; i++) {
		printf("   %2d        %6d        %6d          %6d      %10d\n",
				i, kmembuckets[i].kb_size,
			 	kmembuckets[i].kb_total - kmembuckets[i].kb_free,
			 	kmembuckets[i].kb_free, kmembuckets[i].kb_size *
			 	(kmembuckets[i].kb_total - kmembuckets[i].kb_free));
		totalloc += kmembuckets[i].kb_size * kmembuckets[i].kb_total;
		totfree += kmembuckets[i].kb_size * kmembuckets[i].kb_free;
	}
        printf("\nTotal memory being used from buckets = %d bytes\n",
		   totalloc - totfree);
        printf("Total free memory in buckets = %d bytes\n\n\n", totfree);

	
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
                /* if 3th on line, put out newline */
                if(nout%3 == 0) 
		    printf("\n");
		if(++i == M_LAST) break; 
	    }
	}
	if(nout%3 != 0)
	   printf("\n");
}


/*************************/
/* alpha/startup.c */
extern struct  cblock *cfree;
extern int nclist;
allocate_unix_tables(base_addr) 
	vm_offset_t    base_addr;
{
	vm_offset_t	v;

        /*
         * Allocate space for system data structures.
         */

        v = base_addr;

#define	valloc(name, type, num) \
    (name) = (type *)v; v = (vm_offset_t)((vm_offset_t)((name)+(num)))

	valloc(cfree, struct cblock, nclist);

	bzero(base_addr, v - base_addr);
	return (v - base_addr);
}
extern pmap_t current_pmap;

/* XXX These macros are copied from pmap.c.  */
   
#define UNLOCK_PAGE_TABLE_WINDOW(OLOCK) 				\
	if (OLOCK) {							\
		Root_pt[LEVEL1_PT_OFFSET(Othermap)].quadword = 0L;	\
		simple_unlock(OLOCK);					\
	}
#if NCPUS > 1
extern union pv_list_head *lock_pvh(union pv_list_head *);
extern void unlock_pvh(union pv_list_head *);
extern void unlock_pvh_data(union pv_list_head *, long);
#else
#define lock_pvh(pvh)		(pvh)
#define unlock_pvh(pvh)
#define unlock_pvh_data(pvh,xx)	((pvh)->bits = (xx))
#define	lock_pmap(pmap)
#define	unlock_pmap(pmap)
#endif /* NCPUS */

#define	pa_index(pa)	(alpha_btop((pa) - pmap_first_phys))
#define pa_to_pvh(pa)	(&pv_head_table[pa_index(pa)])
#define MAPS_TEXT(PTE)		(((PTE)->quadword \
	& (PTEQ_MASK_KWE | PTEQ_MASK_UWE | PTEQ_MASK_EXEC)) == PTEQ_MASK_EXEC)

checkpte(pmap_t map, unsigned long addr)
{
	pt_entry_t		*pte1, *pte2, *pte3;
	long		other;

	if (map == kernel_pmap) {
		printf("pmap = kernel_pmap\n");
	} else {
		printf("pmap = %lx\n",map);
	}
	other = pmap_pt_access(map, addr, &pte1, &pte2, &pte3); 
	printf("other = %lx, ",other);
	printf("pte1 = %lx, ", pte1);
	printf("pte2 = %lx, ", pte2);
	printf("pte3 = %lx\n", pte3);
	if (pte1 != 0) {
		printf("pte1->quadword = %lx, ",pte1->quadword);
	}
	if (pte2 != 0) {
		printf("pte2->quadword = %lx, ",pte2->quadword);
	}
	if (pte3 != 0) {
		printf("pte3->quadword = %lx\n",pte3->quadword);
	}
	UNLOCK_PAGE_TABLE_WINDOW(other);

}


/* get phys frame # and it's protection mode.
   If page is not there, then *PROT will be 0 */
void pmap_get_phys_and_protection(pmap_t map, unsigned long addr,
				 long *frame, int *prot)
{
    pt_entry_t *pte1, *pte2, *pte3; 
    long other;
    other = pmap_pt_access(map, addr, &pte1, &pte2, &pte3);
    *prot = 0;
    /* XXX The below codes are dependent on Alpha pte structure */
    if (pte1->pg_v && pte2->pg_v && pte3->pg_v) {
	if ((pte3->pg_prot & 0x1) == 0x1) {
	    *prot |= VM_PROT_READ|VM_PROT_EXECUTE; /* is readable */
	}
	if ((pte3->pg_prot & 0x10) == 0x10) {
	    *prot |= VM_PROT_WRITE; /* is writable */
	} 
	*frame = PTETOPHYS(pte3);
    }
    UNLOCK_PAGE_TABLE_WINDOW(other);
}

/*
 * Copyright 1994, 1995, 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 */
/*
 * HISTORY
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added drawing of a rectangle.  Used to blink as the GC is entered
 *	and exited.
 *
 * 29-Jul-96  Charles Garrett (garrett) at the University of Washington
 *	Events.h file is now found by searching the include path.
 *
 * 11-Jul-96 becker at the University of Washington
 *	Added -L boot local flag.
 *
 * 28-May-96 becker at the University of Washington
 *	Moved swap_generic.c symbols here.
 *
 * 13-May-96  Stefan Savage (savage) at the University of Washington
 *	Eliminated dead code previously needed by locore.s and trap.c 
 *
 * 10-May-96 oystr at the University of Washington
 *	Restored select_* routines for use with DEC drivers.
 *
 * 07-May-96  Stefan Savage (savage) at the University of Washington
 *	Change boot() to redirect to debugger instead of halt the processor
 *
 * 29-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Removed call to pmap_scavenge_boot() (for now) because it
 *	screwed up physical pool.
 *
 * 23-Apr-96  Emin Gun Sirer (egs) at the University of Washington
 *	Implemented the lock_* interface with ulocks.
 *
 * 23-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Reclaim scanvenged pages for general consumption.
 *
 * 21-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Separated from upcalls.c and cleaned up.
 *
 * 08-Feb-96  Stefan Savage (savage) at the University of Washington
 *	Did easy cleanup.  Lots more needed.
 *
 * 28-Apr-95  David Becker (becker) at the University of Washington
 *	Created
 */
#include <bsd_tty.h>		/* for define of BSD_TTY */
#include <sys/types.h>
#include <mach/kern_return.h>
#include <mach/vm_param.h>
#include <arch/alpha/rpb.h>
#include <arch/alpha/pmap.h>
#include <arch/alpha/pal.h>
#include <mach/machine.h>
#include <kern/macro_help.h>
#include <kern/ast.h>
#include <kern/lock.h>
#include <kern/zalloc.h>
#include <mach/exception.h>

boolean_t local_boot=FALSE;	/* -L boot flag sets this true and we
					skip bootp and tftp */

/*
 * useracc() returns whether the given yser range ia accessible
 *
 * Location in DEC UNIX: vm/vm_user.c
 * BUGBUG: This doesn't work
 */
int useracc(caddr_t addr, u_int len, int prot)
{
	return 1;
}

/*
 * kdebug_bcopy_fault() is _supposed_ to return a non-zero if there was a fault
 * while bcopying.  
 *
 * Location in DEC UNIX: dec/debug/kdebug/kdebug_main.c
 * BUGBUG: We don't correctly support nofault.
 */
int kdebug_bcopy_fault(src,dst,bytes)
char *src;
char *dst;
long bytes;
{
	return(bcopy(src,dst,bytes));
}

/*
 * boot() shuts down the processsor (needed by panic in subr_prf.c)
 *
 * Location in DEC UNIX: arch/alpha/machdep.c
 */
void gimmeabreak(void);
boot(reason, howto)
int reason, howto;
{
	printf("halting, reason = %d, howto = %d\n", reason, howto);
	printf("going to debugger\n");
	gimmeabreak();
/*	halt(); */
}

/*
 * loadable_driver_init() called to init loadable driver framework
 *
 * Location in DEC UNIX: io/common/ldbl_driver_support.c
 */
loadable_driver_init()
{
	return;
}

/*
 * subsys_bootstrap() called to allocate memory for subsystems (none in SPIN)
 *
 * Location in DEC UNIX: bsd/subsys_conf.c
 */
subsys_bootstrap(
    caddr_t                     buf,
    caddr_t                     mem)
{
	return 0;
}

/*
 * subsys_conf() called to initialize subsystems (none in SPIN)
 *
 * Location in DEC UNIX: bsd/subsys_conf.c
 */
subsys_conf(
    caddr_t                     subsys)
{
	return 0;
}

/*
 * ether_sprintf() prints ethernet address
 * 
 * Location in DEC UNIX: net/if_ethersubr.c
 */
char *
ether_sprintf(
        u_char *ap)
{
        register i;
        static char etherbuf[18];
        register char *cp = etherbuf;
        static char digits[] = "0123456789abcdef";

        for (i = 0; i < 6; i++) {
                *cp++ = digits[*ap >> 4];
                *cp++ = digits[*ap++ & 0xf];
                *cp++ = ':';
        }
        *--cp = 0;
        return (etherbuf);
}

/*
 * save_context_all()
 * 
 * Location in DEC UNIX: arch/alpha/context.s
 */
save_context_all()
{
	printf("save_context_all(): called\n");
}

/*
 *
 * Location in DEC UNIX: arch/alpha/swapgeneric.c
 */
#define BOOTDEVLEN 256
char bootdevice[BOOTDEVLEN];	/* the name of the bootdevice */
#define NODEV       (dev_t)(-1)
dev_t	rootdev = NODEV;

/**********************************************************************
 *
 *	Logging interfaces (mainly from bsd/subr_log.c & bsd/subr_binlog.c)
 *
 **********************************************************************/

/*
 * log_bootstrap allocates memory for log (called from arch/alpha/alpha_init.c)
 *
 * Location in DEC UNIX: bsd/subr_log.c
 */
int
log_bootstrap(mp)
long *mp;
{
	return 0;
}

/*
 * binlog_allocate allocates space for binlog
 * (called from arch/alpha/alpha_init.c)
 *
 * Location in DEC UNIX: bsd/subr_binlog.c
 */
int
binlog_allocate(caddr_t start)
{
	return 0;
}


/*
 * logwakeup called in bsd/subr_prf.c to log system messages
 *
 * Location in DEC UNIX: bsd/subr_log.c
 */
logwakeup()
{
	printf("logwakeup: Not supported\n");
}

/*
 * log_puts logs a message 
 *
 * Location in DEC UNIX: bsd/subr_log.c
 */
log_puts(char *buf, int num)
{
	cons_puts(buf);
}

/*
 * binlog_alloc allocates space in binlog (called by 
 * hal system code).
 *
 * Location in DEC UNIX: bsd/subr_binlog.c
 */
char *binlog_alloc(asize, pri)
int asize;
int pri;
{
	printf("binlog_alloc: Not supported\n");
	return (char *)0;
}

/*
 * binlog_valid marks binlog record as valid.  Used to mark memory checks in
 * hal system code.
 *
 * Location in DEC UNIX: bsd/subr_binlog.c
 */
binlog_valid(buf)
char *buf;
{
	printf("binlog_valid: Not supported, data at %lx.\n", buf);
	return;
}

#if select
/*
 *
 * Select interfaces (mainly from bsd/subr_select.c, used by tty code)
 *
 */


zone_t select_zone;
extern struct thread * current_thread();

void select_init()
{
    select_zone = zinit(sizeof(struct sel_queue),
			round_page( 512 * sizeof(struct sel_queue)),
			round_page( 128 * sizeof(struct sel_queue)),
			"Select queues");
}


/*
 * select_dequeue_all() dequeue all threads waiting for event
 *
 * Location in DEC UNIX: bsd/subr_select.c
 */
void
select_dequeue_all(selq)
sel_queue_t     *selq;
{
	register sel_queue_t    *qp;
	register sel_queue_t    *next;
	
	/* 
	 * this routine is called for all chars to and from the console.
	 * so it cannot print.
	 */
	while (!queue_empty(&selq->links)) {
	    qp = (sel_queue_t *) dequeue_head(&selq->links);
	    zfree(select_zone, (vm_offset_t) qp);
	}
}

/*
 * select_enqueue() adds threads to list waiting for event.
 *
 * Location in DEC UNIX: bsd/subr_select.c
 */
void
select_enqueue(selq)
sel_queue_t     *selq;
{
	sel_queue_t *qp;
	
	qp = (sel_queue_t *)zalloc(select_zone);
	qp->event.ev_thread = (struct thread *) current_thread();
	qp->event.ev_event = 0;
	qp->event.ev_waiter = 0;
	enqueue_tail(&selq->links, qp);
}


/*
 * select_dequeue() removes thread from list waiting for event.
 *
 * Location in DEC UNIX: bsd/subr_select.c
 */
void
select_dequeue(selq)
sel_queue_t     *selq;
{
	register struct thread *target;
	register sel_queue_t *qp;
	register sel_queue_t *next;
	
	target = (struct thread *)current_thread();
	qp = (sel_queue_t *) queue_first(&selq->links);
	while (!queue_end(qp, (sel_queue_t *) &selq->links)) {
	    if (qp->event.ev_thread == target) {
		remque(&qp->links);
		next = (sel_queue_t *) queue_next(&qp->links);
		zfree(select_zone, (vm_offset_t) qp);
		qp = next;
	    }
	    else {
		qp = (sel_queue_t *) queue_next(&qp->links);
	    }
	}
}
#endif select




/**********************************************************************
 *
 *	Lock interfaces (mainly from kern/lock.c)
 *	Only used by zalloc and handler (in io/common)
 *
 **********************************************************************/

/*
 * lock_bootstrap() allocates space for locks
 *
 * Location in DEC UNIX: kern/lock.c
 */
lock_bootstrap(addr)
caddr_t addr;
{
	return 0;
}

/*
 * lock_setup() initializes a lock
 *
 * Location in DEC UNIX: kern/lock.c
 */
void
lock_setup(l, lip, can_sleep)
	lock_t l;
	struct lockinfo *lip;
	boolean_t can_sleep;
{
    ulock_setup(l, lip, can_sleep);
}

/*
 * lock_write() takes a write lock
 *
 * Location in DEC UNIX: kern/lock.c
 */
void
lock_write(l)
	lock_t	l;
{
    ulock_write(l);
}

/*
 * lock_try_write() tries to take a write lock
 *
 * Location in DEC UNIX: kern/lock.c
 */
boolean_t
lock_try_write(l)
        lock_t l;
{
    ulock_try_write(l);
}

/*
 * lock_done() gives up a lock
 *
 * Location in DEC UNIX: kern/lock.c
 */
void
lock_done(l)
	register lock_t	l;
{
    ulock_done(l);
}



/**********************************************************************
 *
 *	Debugger interfaces (only SPIN specific)
 *
 **********************************************************************/
extern boolean_t ttd_enabled;

void
Debugger(const char   *message)
{
      gimmeabreak();
}

/*
 * gimmeabreak and Debugger cause a breakpoint
 */

void
gimmeabreak(void) 
{

        if (ttd_enabled) {
                asm(".globl ttd_gimmeabreak_called");
                asm("ttd_gimmeabreak_called:");
		asm("call_pal 128"); 
	}

}

/*
 * setup_main() called from alpha_init.c and starts up SPIN code
 *
 * Location in DEC UNIX: bsd/init_main.c and arch/alpha/startup.c
 */
extern char version[];
extern vm_size_t mem_size;	/* 100 ways to cook an egg */
extern int consmem;
void
setup_main(void)
{
	vm_size_t size;
	extern struct scavenge_list scavenge_info;
	extern vm_offset_t avail_start,avail_end;
	extern int NumZonePages;
	extern int salhook_mallocpages;

	setup_main_proc();

        printf(version);
	size = mem_size + ptoa(consmem);

#define MEG	(1048576)
        printf("physical memory = %ld.%ld%ld megabytes.\n", size / MEG,
               ((size * 10) / MEG) % 10, ((size * 100) / MEG) % 10);

	size = alpha_ptob(((avail_end - avail_start)/PAGE_SIZE)
			  + scavenge_info.count + NumZonePages +
			  salhook_mallocpages);

	printf("available memory = %d.%d%d megabytes.\n", size / MEG,
	       ((10 * size) / MEG) % 10,
	       ((100 * size) / MEG) % 10);

	clock_init();

	configure();

	/* The -L boot flag sets local_boot true in order to skip bootp and tftp
	   You could hardcode a target's ip address here by setting my_ip.
	 */
	if (local_boot)
		printf("Local boot (-L).  Skipping bootp. tftp not available\n");
	else	bootp_init();

	ttd_init();

	cinit();
	salhook_main_program();
	panic("setup_main does not return\n");
}


#include <sys/param.h>
#include <sys/types.h>
#include <sys/kernel.h>
#include <sys/systm.h>
#include <sys/mbuf.h>

#include <net/if.h>
#include <netinet/in.h>
#include <netinet/if_ether.h>
#include <netinet/in_systm.h>
#include <netinet/ip.h>
#include <net/route.h>
#include <net/if_types.h>
#include <netinet/in_pcb.h>
#include <netinet/ip_var.h>
#include <netinet/udp.h>
#include <netinet/udp_var.h>


/*
 * Add an Ethernet multicast address or range of addresses to the list for a
 * given interface.
 */
int
ether_addmulti(ifr, ac)
        struct ifreq *ifr;
        register struct arpcom *ac;
{
  return (EAFNOSUPPORT);
}

/*
 * Delete a multicast address record.
 */
int
ether_delmulti(ifr, ac)
        struct ifreq *ifr;
        register struct arpcom *ac;
{
  return (EAFNOSUPPORT);
}

void
arp_ifinit(ac, ifa)
        struct arpcom *ac;
        struct ifaddr *ifa;
{
#if 0
        ac->ac_ipaddr = IA_SIN(ifa)->sin_addr;
        arpwhohas(ac, &ac->ac_ipaddr);
        ifa->ifa_rtrequest = arp_rtrequest;
        ifa->ifa_flags |= RTF_CLONING;
#endif
}

struct protosw *
pffindtype(int family, int type)
{
  printf("pffindtype");
  return NULL;
}

struct protosw *
pffindproto(int family, int protocol, int type)
{
  printf("pffindproto");
  return NULL;
}

/*
 * from net/if.c
 */

/*
 * Attach an interface to the
 * list of "active" interfaces.
 */
static struct ifnet *ifnet = 0;
static unsigned long if_index = 0;

void if_attach (struct ifnet *ifp)
{
	struct ifnet **p;

	/* XXX needs to be moved into networking startup code */
	extern int max_linkhdr;
	max_linkhdr = 16;	/* usually happens in bsd/uipc_domain.c */

	/*
	printf("  %s\n",ifp->if_version);
	printf("  %s%d mtu %d mtu %d baudrate %d\n",
	       ifp->if_name, 
	       ifp->if_unit, 
	       ifp->if_mtu, 
	       ifp->if_mtu, 
	       ifp->if_baudrate);
	*/

	/* attach this ifp to the end of all configured interface list */
	ifp->if_next = ifnet;
	ifnet = ifp;
	ifp->if_index = ++if_index; /* assign next index */
}

/*****
 from net/if_ethersubr.c
 */

/*
 * Ethernet output routine.
 * Encapsulate a packet of type family for the local net.
 * Use trailer local net encapsulation if enough data in first
 * packet leaves a multiple of 512 bytes of data in remainder.
 * Assumes that ifp is actually pointer to arpcom structure.
 */
int
ether_output(struct ifnet *ifp,
	     struct mbuf *m,
	     struct sockaddr *dst,
	     struct rtentry *rt)
{
	printf("ether_output: stub called.\n");
	if (m) m_freem(m);
	return 0;
}

/*
 * Perform common duties while attaching to interface list
 */
void
ether_ifattach(ifp)
	register struct ifnet *ifp;
{
	register struct ifaddr *ifa;
	register struct sockaddr_dl *sdl;

	ifp->if_type = IFT_ETHER;
	ifp->if_addrlen = 6;
	ifp->if_hdrlen = 14;
	ifp->if_mtu = ETHERMTU;
	if (ifp->if_baudrate == 0)
	    ifp->if_baudrate = 10000000;
#if 0
	for (ifa = ifp->if_addrlist; ifa; ifa = ifa->ifa_next)
		if ((sdl = (struct sockaddr_dl *)ifa->ifa_addr) &&
		    sdl->sdl_family == AF_LINK) {
			sdl->sdl_type = IFT_ETHER;
			sdl->sdl_alen = ifp->if_addrlen;
			bcopy((caddr_t)((struct arpcom *)ifp)->ac_enaddr,
			      LLADDR(sdl), ifp->if_addrlen);
			break;
		}
#endif
}

/*****
 ether_input originally from net/if_ethersubr.c
 */

/*
   Process a received Ethernet packet;
   the packet is in the mbuf chain m without
   the ether header, which is provided separately.

   Unlike the regular ether_input stack, we throw the ether header on
   top right away.  We need it around for ether_sendback().
 */
void
ether_input(ifp, ehp, m)
	struct ifnet *ifp;
	register struct ether_header *ehp; /* no ether header in mbuf */
	struct mbuf *m;
{
	struct mbuf *n = m;
#ifdef __FreeBSD__
	ehp->ether_type = ntohs(ehp->ether_type);
#endif

	M_PREPEND(m,sizeof(struct ether_header),M_DONTWAIT);

#ifndef __FreeBSD__
	n->m_flags |= M_PKTHDR; /* XXX making sure that pkthdr flag stays set */
#endif

	bcopy(ehp, mtod(m,struct ether_header*),
	      sizeof(struct ether_header));

	/* queue packet for handling by ifp->if_recv() at the end of 
	the interrupt. ether_input is call mid-interrupt, when it may be
	ok to transmit a packet out this interface.
	if_rcv and if_recv are spin specific */
	IF_ENQUEUE(&ifp->if_rcv,m);
}

/*****
 from bsd/uipc_domain.c
 */
void
pfreclaim()
{
}

/*****
 from net/netisr.c
 */

/*
 * Add/delete isr's in input table. Isr's are specified by a small
 * integer, and an optional input queue and domain may be specified.
 * The input queue is used for isr's which receive packets, the
 * domain is used for its funnel and reference count.
 */
netisr_add(num, isr, ifq, dp, optflags)
	int num;
	void (*isr)(void);
	struct ifqueue *ifq;
	struct domain *dp;
	int optflags;			/* eg. ISRF_INCHDR */
{
}

/*****
 from net/ifether_subr.c
 */
/*
 * Initialize the part of the ether_driver structure concerned with
 * the packet filter, and tell the packet filter driver about us
 */
attachpfilter(edp)
struct ether_driver *edp;
{
}

void
rearpwhohas(ac)
        struct arpcom *ac;
{
}
/*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 */
/*
 * HISTORY
 * 29-May-96  Charles Garrett (garrett) at the University of Washington
 *	Removed explicit ALPHA_SPIN from include filename.
 *
 * 20-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Separated from upcalls.c and cleaned up
 *
 * 08-Feb-96  Stefan Savage (savage) at the University of Washington
 *	Did easy cleanup.  Lots more needed.
 *
 * 21-Sep-95  Stefan Savage (savage) at the University of Washington
 *	Updated to support tty's, OSF printf, some u area crud...
 *	beware ye who enter here
 *
 * 28-Apr-95  David Becker (becker) at the University of Washington
 *	Created
 */
#include <bsd_tty.h>
#include <sys/tty.h>
#include <sys/clist.h>

/*
 * scanc() processes special characters according to table and mask.
 * Used by bsd/tty.c.
 *
 * Location in DEC UNIX: ufs/ufs_subr.c
 * BUGBUG: Needs rewrite or replacement to be legal (stolen from DEC). 
 */
scanc(size, cp, table, mask)
	u_int size;
	register u_char *cp, table[];
	register u_char mask;
{
	register u_char *end = &cp[size];

	while (cp < end && (table[*cp] & mask) == 0)
		cp++;
	return (end - cp);
}

/*
 * cinit initialies the clists.
 *
 * Location in DEC UNIX: bsd/init_main.c (used by bsd/tty.c)
 * BUGBUG: Needs rewrite or replacement to be legal (stolen from DEC). 
 */
cinit()
{
	register vm_offset_t ccp;
	register struct cblock *cp;

	ccp = (vm_offset_t)cfree;
	ccp = (ccp+CROUND) & ~CROUND;
	for (cp = (struct cblock *)ccp; cp < &cfree[nclist-1]; cp++) {
		cp->c_next = cfreelist;
		cfreelist = cp;
		cfreecount += CBSIZE;
	}
}

/*
 * pgsignal and pgsignal_tty signal the process group
 *
 * Location in DEC UNIX: sys/signal.h
 */
void pgsignal_tty(pg, sig, checkctty, doself, smpsafe)
     long pg, sig, checkctty, doself, smpsafe;
{
	pgsignal(pg, sig, checkctty);
}

/*
 * Copyright 1994, 1995, 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 */
/*
 * HISTORY
 * 15-May-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed hardclock() to update usec value by tick count.  
 *
 * 23-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Got rid of microtime (we use DEC version again).
 *
 * 20-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Separated from upcalls.c and cleaned up
 *
 * 22-Feb-96  Charles Garrett (garrett) at the University of Washington
 *	Changed ALPHA_OSF to ALPHA_SPIN.
 *
 * 08-Feb-96  Stefan Savage (savage) at the University of Washington
 *	Did easy cleanup.  Lots more needed.
 *
 * 14-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Added get_rpb_clock for computing hertz.
 *
 * 06-Nov-95  Marc Fiuczynski (mef) at the University of Washington
 *	Updated timeout() and untimeout() to call osf_timeout() and
 *      osf_untimeout().
 *
 * 28-Apr-95  David Becker (becker) at the University of Washington
 *	Created
 */
#include <sys/types.h>
#include <sys/time.h>
#include <arch/alpha/rpb.h>

extern int fixtick;
extern int tickadj;

/*
 * fixcnt is used to figure out when to add fixtck adjustment
 *
 * Location in DEC UNIX: bsd/kern_clock.c
 */
static int fixcnt = 0;

/*
 * Location in DEC UNIX: bsd/kern_clock.c
 */

clock_init()
{
	int compute_mach_factor();

	/*
	 * Compute constants for hardclock TOD maintenance.
	 */
	tick = 1000000L / hz;
	fixtick = 1000000L - ((1000000L/hz) * hz);
	if (hz > 100) {
		tickadj = 1;	/* can adjust hz usecs/second (1 usec/tick)*/
	} else {
		tickadj = 100 / hz; /* can adjust 100 usecs per second */
	}
}


#define BUMPTIME(t, usec) { \
	(t).tv_usec += (usec); \
	if ((t).tv_usec >= 1000000) { \
		(t).tv_usec -= 1000000; \
		(t).tv_sec++; \
	} \
}

/*
 * hardclock() is called on each timer interrupt (using DEC locore.s)
 * 
 * Location in DEC UNIX: bsd/kern_clock.c
 */

static cycle_t	timestamp;

static int cnt =0;


void
hardclock() 
{
	struct timeval t;
	extern int tickdelta;
	extern long timedelta;
	cycle_t ts;
	int delta;
	
	TIME_LOAD(t);
	ts = cyclecounter();
	if (cnt != 0 )
	{
	    delta = cycleminus(ts,timestamp);
	    BUMPTIME(t,cycle_to_microsec(delta));
	}
	timestamp = ts;
	cnt++;
	TIME_STORE(t);
	lbolt++;
}

#include <machine/trap.h>
#include <machine/cpu.h>
#include <machine/reg.h>

extern cerror(), uaerror();
extern emulator_handler();
extern pmap_fault_on();

/*
 * SPIN specific functions to get access to the current interrupt rate
 * and the processor clock speed.
 */
extern long	hwrpb_addr;		/* kernel virt addr of HWRPB */
long get_rpb_clock()
{
	return ((struct rpb *)hwrpb_addr)->rpb_clock;
}

long get_rpb_counter()
{
      return ((struct rpb *)hwrpb_addr)->rpb_counter;
}

int  (*nofault_pc[NF_NENTRIES])() = {
	 0L, cerror, uaerror, emulator_handler	
	,0L, 0L, pmap_fault_on	
};


void
trap(a0,a1,a2,code,exc_frame)
unsigned long a0,a1,a2,code;
unsigned long *exc_frame;
{
   if (code == T_IFAULT) {
      if (a0 == T_IFAULT_BPT) {
         ttd_trap(exc_frame, code, a0);
      } else {
	 printf("Unhandled instruction fault @ pc 0x%lx (ra: 0x%lx), a0 = %lx, a1 = %lx, a2 = %lx.\n", exc_frame[EF_PC], exc_frame[EF_RA], a0, a1, a2);
      }
   }
}

void
fp_psig(void)
{
}


#if select
/****
  from bsd/subr_select.c
 */

/*
 * select_enqueue:  add the current thread to the list
 * of threads waiting for something to happen.
 *
 * N.B.:  The lock protecting this queue must be held
 * while calling this routine.
 */
void
select_enqueue(selq)
sel_queue_t	*selq;
{
	sel_queue_t	*qp;

	qp = (sel_queue_t *) zalloc(select_zone);
	qp->event = current_thread();
	enqueue_tail(&selq->links, qp);
}


/*
 * select_wakeup:  Wakeup any threads who've selected
 * this event.
 *
 * N.B.:  The lock protecting this queue must be held
 * while calling this routine.
 */
void
select_wakeup(selq)
sel_queue_t	*selq;
{
	sel_queue_t	*qp;
	struct queue_entry *first;

	first = selq->links.prev->next;
	while(selq->links.next != first) {
		qp = (sel_queue_t *)selq->links.next;
		thread_wakeup((vm_offset_t)qp->event);
		selq = (sel_queue_t *)selq->links.next;
	}
}
#endif select
