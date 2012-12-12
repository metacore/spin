/*
 * HISTORY 
 */

#include <sal/ttd/ttd_types.h>

void ttd_break(void);

void ttd_halt_processors(void);

ttd_machine_type get_ttd_machine_type(void);

void ttd_recover_esp(ttd_machine_state *ttd_state);

void ttd_overwrite_esp(ttd_machine_state *ttd_state);

boolean_t ttd_mem_access(vm_offset_t offset,
			 vm_prot_t access);

void ttd_flush_cache(vm_offset_t offset,
		     vm_size_t length);

boolean_t ttd_insert_breakpoint(task_t task,
				vm_address_t address,
				ttd_saved_inst *saved_inst);

boolean_t ttd_remove_breakpoint(task_t task, 
				vm_address_t address,
				ttd_saved_inst saved_inst);

boolean_t ttd_set_machine_single_step(task_t task);

boolean_t ttd_clear_machine_single_step(task_t task);

void ttd_type_to_ttdtrap(int type);

int ttd_write_bytes(natural_t *addr, 
		    volatile int *len,
		    natural_t *data,
		    task_t task);

int ttd_read_bytes(natural_t *addr,
		   volatile int *len, 
		   natural_t *data,
		   task_t task);
