/*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 21-Sep-95  Stefan Savage (savage) at the University of Washington
 *	Created
 */
#ifndef _VM_VM_PAGE_H_
#define _VM_VM_PAGE_H_
#include <mach/kern_return.h>
#include <machine/pmap.h>
struct vm_page {
	vm_offset_t  element;
};

typedef vm_offset_t vm_page_t;
#endif /* _VM_VM_PAGE_H_ */
