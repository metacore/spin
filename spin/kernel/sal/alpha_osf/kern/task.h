/*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 21-Sep-95  Stefan Savage (savage) at the University of Washington
 *	Created
 */
#ifndef _KERN_TASK_H_
#define _KERN_TASK_H_
#include <machine/pmap.h>
struct task {
	pmap_t map;
};

typedef struct task *task_t;
	
#endif /* _KERN_TASK_H_ */
