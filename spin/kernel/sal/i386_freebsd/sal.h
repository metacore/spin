
#define STANDALONE_NUM_MAP_ENTRIES (512)
#define vm_map_to_entry(map)   ((struct vm_map_entry *) &(map)->header)
#define vm_map_first_entry(map) ((map)->header.next)
#define vm_map_last_entry(map)  ((map)->header.prev)

int ttd_trap(int type, int code, void *regs);
vm_offset_t pmem_alloc(vm_size_t size);
vm_map_entry_t standalone_get_map_entry(vm_size_t start, 
		  vm_size_t end, vm_map_entry_t prev, vm_map_entry_t next);
void standalone_halt();
int kernel_memory_allocate(
        register vm_map_t       map,
	register vm_offset_t    *addrp,
	register vm_size_t      size, 
	register vm_offset_t    mask,
	int                     flags);
void standalone_free_map_entry(vm_map_entry_t old_entry);
void kmem_coalesce(vm_map_entry_t entry);

extern struct swqueue bp_queue;
extern struct vm_map	vm_map_list[];
extern int vm_map_list_index;
extern struct vm_map_entry	vm_map_entries[];
extern int standalone_map_debug;
