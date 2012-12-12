/*
  	sal.c

	Contains all the subroutines that do not appear anywhere in
	freebsd and are specific to the x86. 
	Most of it implements vm and cyclecounter code.

	created by David Becker Wed Jun 25 09:34:50 PDT 1997
 */
/*
 * HISTORY
 * 25-Jun-97  becker at the University of Washington
 *	Moved cyclecounter subroutines to cyclecount since thats all
 *	salboot needed.
 */

#include <sys/param.h>
#include <sys/kernel.h>
#include <sys/malloc.h>
#include <sys/buf.h>
#include <vm/vm.h>
#include <vm/vm_kern.h>
#include <sal/i386_freebsd/sal.h>

/*****
 standalone vm support
 */
#define KMEM_ALLOC_PAGES 1024
struct vm_map	vm_map_list[50];
struct vm_map_entry	vm_map_entries[STANDALONE_NUM_MAP_ENTRIES];
int vm_map_list_index = 0;
int standalone_map_debug = 0;
pmap_t current_pmap;
int *kmemtypetable;
int *kmemtypemax;

int typetablearray[M_LAST*sizeof(int)];
int typetablemax[M_LAST*sizeof(int)];

struct swqueue bp_queue;

/* 
 * simple minded map entry management here for clients of OSF/1 kernel maps.
 */
void standalone_map_entry_init(void)
{
	int i;
	for (i=0;i<STANDALONE_NUM_MAP_ENTRIES;i++) 
	{
		vm_map_entries[i].private = 0;
	}
	if (standalone_map_debug) {
		printf("standalone_map_entry_init: initialized list of vm_map_entries\n");
	}
}

void vm_mem_init(void)
{
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

	virtual_avail = vaddr;
	avail_start = start;
	
	standalone_map_entry_init();

#if 0
	pmap_array_size=round_page(pmap_page_array_size(avail_start,avail_end));
	pmap_page_array_init(avail_start,avail_end, PHYS_TO_KSEG(avail_start));
	avail_start += pmap_array_size;
	vm_managed_pages = atop(avail_end  - avail_start);
	zone_bootstrap();
#endif

	kmem_init(virtual_avail, virtual_end);
        pmap_init(avail_start, avail_end);
	current_pmap = kernel_pmap; /* TTD uses current_pmap */

	/*zone_init((vm_size_t)round_page(avail_end - avail_start));
	select_init();*/
}

/*
 * determine if a page is managed (memory vs. device)
 */
static inline int
pmap_is_managed(pa)
	vm_offset_t pa;
{
	int i;

	for (i = 0; phys_avail[i + 1]; i += 2) {
		if (pa >= phys_avail[i] && pa < phys_avail[i + 1])
			return 1;
	}
	return 0;
}

vm_offset_t pmem_alloc(vm_size_t size)
{
  int i;
  vm_offset_t result = virtual_avail;
  int pages = round_page(size) / PAGE_SIZE;
  
  while(pages)
    {
      if(avail_start > avail_end)
	panic("pmem_alloc: out of memory");

      if(!pmap_is_managed(avail_start))
	{
	  printf("skipping %lx...\n", avail_start);
	  avail_start += PAGE_SIZE;
	  continue;
	}
      
      pmap_enter(kernel_pmap, virtual_avail, avail_start, VM_PROT_ALL,
		 FALSE); 

      bzero((char *) virtual_avail, PAGE_SIZE);

      avail_start += PAGE_SIZE;
      virtual_avail += PAGE_SIZE;
      pages--;
    }

  return result;
}

void 
kmem_init(vm_offset_t start, vm_offset_t end)
{
	vm_map_entry_t new_entry;
	int i, pages_to_grab;

	kernel_map=&(vm_map_list[vm_map_list_index++]);
	kernel_map->min_offset = virtual_avail;
	kernel_map->max_offset = kernel_map->min_offset + 
	    (PAGE_SIZE*KMEM_ALLOC_PAGES);

	new_entry = standalone_get_map_entry(kernel_map->min_offset,
				       kernel_map->max_offset,
				       vm_map_to_entry(kernel_map),
				       vm_map_to_entry(kernel_map));

        vm_map_first_entry(kernel_map) = new_entry;
	vm_map_last_entry(kernel_map) =  new_entry;

#ifdef WATCHMEM
	printf("kmem_init: min %lx max %lx",
		kernel_map->min_offset, kernel_map->max_offset);
	printf("kernel map = %lx\n",kernel_map);
#endif

	pmem_alloc(PAGE_SIZE*KMEM_ALLOC_PAGES);
}

void standalone_free_map_entry(vm_map_entry_t old_entry)
{
	if (standalone_map_debug) 
		printf("standalone_free_map_entry: freeing map entry\n");
	if (old_entry->private > 0)   {
		old_entry->private = 0;
		/* for good measure */
		bzero(old_entry, sizeof(struct vm_map_entry));
	} else {
		panic("standalone_free_map_entry: freeing free map entry");
	}
}

vm_map_entry_t 
standalone_get_map_entry(vm_size_t start, 
		  vm_size_t end,
		  vm_map_entry_t prev, 
		  vm_map_entry_t next)
{
	int i;
        vm_map_entry_t e = 0;

	if (standalone_map_debug)
		printf("standalone_get_map_entry: getting map entry\n");
	for (i=0;i<STANDALONE_NUM_MAP_ENTRIES;i++)  {
		if (vm_map_entries[i].private == 0)  {
			e = &vm_map_entries[i];
			break;
                }
	}
        if (e)  {
		e->private = 1;
		e->start = start;
		e->end = end;
		e->prev = prev;
		e->next = next;
        } else  {
		panic("standalone_get_map_entries: no more map entries");
	}
	return e;
}

int /* int */
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
 * Try to coalesce the map entries surrounding the current one
 */

void
kmem_coalesce(vm_map_entry_t entry)
{
	vm_map_entry_t next_entry;
	vm_map_entry_t prev_entry;

	next_entry = entry->next;
	if (entry->end == next_entry->start) {
		if (standalone_map_debug)	{
			printf("kmem_coalesce from 0x%lx to 0x%lx\n",
				entry->start, next_entry->end);
		}
		entry->end = next_entry->end;  
		next_entry->next->prev = entry; 
		entry->next = next_entry->next;
		standalone_free_map_entry(next_entry);
	}
	prev_entry = entry->prev;
	if (entry->start == prev_entry->end) {
		if (standalone_map_debug)	{
			printf("kmem_coalese from 0x%lx to 0x%lx\n",
				prev_entry->start, entry->end);
		}
		entry->start = prev_entry->start;  
		prev_entry->prev->next = entry; 
		entry->prev = prev_entry->prev;
		standalone_free_map_entry(prev_entry);
	}
}

/*****
 standalone boottime
 */

#if __GNUC__ >= 2
void __main() {}
#endif

void standalone_halt(void)
{
  boot(0);
}
