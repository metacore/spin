/*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 12-Mar-96  Stefan Savage (savage) at the University of Washington
 *	Created.
 *
 */
#include "ALPHA_SPIN/libmach_user.h"

typedef int task_t;
typedef int memory_object_t;
typedef int kern_return_t;

task_t
task_self()
{
	return LibMach_TaskSelf();
}

kern_return_t
task_create(task_t parent_task, int inherit_memory, task_t *child_task)
{
	return LibMach_TaskCreate(parent_task, inherit_memory, child_task);
}

kern_return_t
task_terminate(task_t target_task)
{
	return LibMach_TaskTerminate(target_task);
}

kern_return_t
vm_allocate(task_t parent_task, long *address, long size, long anywhere)
{
	return LibMach_VMAllocate(parent_task, address, size, anywhere);
}

kern_return_t
vm_deallocate(task_t parent_task, long address, long size)
{
	return LibMach_VMDeallocate(parent_task, address, size);
}

kern_return_t
vm_protect(task_t target_task, long address, long size, long set_maximum,
	   long new_protection)
{
	return LibMach_VMProtect(target_task, address, size, set_maximum,
				 new_protection);
}
/*
kern_return_t
vm_inherit(task_t target_task, long address, long size, long inheritance)
{
	return LibMach_VMInherit(target_task, address, size, inheritance);
}

kern_return_t
vm_copy(task_t target_task, long source_address, long count, long dest_address)
{
	return LibMach_VMCopy(target_task, source_address, count,
			      dest_address);
}

kern_return_t
vm_read(task_t target_task, long address, long size, long *data,
	long *data_count)
{
	return LibMach_VMRead(target_task, address, size, data, data_count);
}

kern_return_t
vm_write(task_t target_task, long address, long data,
	 long data_count)
{
	return LibMach_VMWrite(target_task, address, data, data_count);
}

kern_return_t
vm_map(task_t target_task, long *address, long size, long mask, long anywhere,
       memory_object_t memory_object, long offset, long copy, long
       cur_protection, long max_protection, long inheritance)  
{
	return LibMach_VMMap(target_task, address, size, mask,
			     anywhere, memory_object, offset, copy,
			     cur_protection, max_protection, inheritance);
}

kern_return_t
vm_region(task_t target_task, long *address, long *size, long
	  *protection, long *max_protection, long *inheritance, long
	  *shared, memory_object_t *object_name, long *offset)
	  
{
	return LibMach_VMRegion(target_task, address, size, protection,
			     max_protection, inheritance, shared,
				obejct_name, offset);
}


*/


