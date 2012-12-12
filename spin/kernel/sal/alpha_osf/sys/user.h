/*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 13-May-96  Stefan Savage (savage) at the University of Washington
 *	Define uthread and new u_*'s so we can use diffed DEC pcb.h
 *
 * 11-May-95  Stefan Savage (savage) at the University of Washington
 *	Created
 *	(dummy file for to make OSF code get the right includes)
 */
#ifndef _SYS_USER_H_
#define _SYS_USER_H_

#include <sys/errno.h>
#include <sys/param.h>
#include <sys/time.h>

/*
 * Fake uthread struct.  Just there for pcb.  Note that we fake the uthread
 * struct by defining u.* to refer to a pcb.  We should fix this one day.
 */
struct uthread {
	char stack_align[64];
};

#include <kern/thread.h>
#include <machine/pcb.h>
#undef u
extern struct pcb u;

/*
 * Traditional unix formula to satisfy undefined symbols.
 * Note, put uu_* symbols and proc in pcb which we use to fake uthread.
 */
#define u_ieee_fp_trigger_sum           uu_ieee_fp_trigger_sum
#define u_ieee_fp_trigger_inst          uu_ieee_fp_trigger_inst
#define u_ieee_fp_trap_pc               uu_ieee_fp_trap_pc
#define u_ieee_fp_control               uu_ieee_fp_control
#define u_ieee_set_state_at_signal      uu_ieee_set_state_at_signal
#define u_ieee_fp_control_at_signal     uu_ieee_fp_control_at_signal
#define u_ieee_fpcr_at_signal           uu_ieee_fpcr_at_signal
#define u_procp				proc

#endif /* _SYS_USER_H_ */
