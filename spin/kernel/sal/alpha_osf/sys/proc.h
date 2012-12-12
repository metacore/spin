/*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY 
 * 21-Sep-95  Stefan Savage (savage) at the University of Washington
 *	Created
 *
 */
#ifndef _SYS_PROC_H_
#define  _SYS_PROC_H_
#include <machine/pmap.h>
struct map_spoof {
	pmap_t vm_pmap;
};
struct proc {
	struct map_spoof *map;
};


#include <sys/user.h>
extern struct proc *current_proc;
#define proc_to_task(x) (current_proc)
#endif  /* _SYS_PROC_H_ */



