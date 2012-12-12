/*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 21-Sep-95  Stefan Savage (savage) at the University of Washington
 *	Created
 */
#ifndef _KERN_THREAD_H_
#define _KERN_THREAD_H_
#include <machine/thread.h>
#include <kern/task.h>
typedef long thread_t;
#define THREAD_NULL ((thread_t) 0)

extern struct task current_task;
#define current_task()          (&current_task)
#endif /* _KERN_THREAD_H_ */
