/* Native-dependent definitions for LynxOS.
   Copyright 1993 Free Software Foundation, Inc.

This file is part of GDB.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#ifndef NM_LYNX_H
#define NM_LYNX_H

#include <sys/conf.h>
#include <sys/kernel.h>
/* sys/kernel.h should define this, but doesn't always, sigh. */
#ifndef __LYNXOS
#define __LYNXOS
#endif
#include <sys/mem.h>
#include <sys/signal.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/itimer.h>
#include <sys/file.h>
#include <sys/proc.h>
#include "thread.h"

/* This is the amount to subtract from u.u_ar0 to get the offset in
   the core file of the register values.  */

#define KERNEL_U_ADDR USRSTACK

#undef FLOAT_INFO	/* No float info yet */

#define PTRACE_ARG3_TYPE char *

/* Override copies of {fetch,store}_inferior_registers in infptrace.c.  */

#define FETCH_INFERIOR_REGISTERS

/* Thread ID of stopped thread.  */

#define WIFTID(x) (((union wait *)&x)->w_tid)

/* Override child_wait in inftarg.c */

#define CHILD_WAIT

/* Override child_resume in infptrace.c */

#define CHILD_RESUME

#include "target.h"

extern int child_wait PARAMS ((int pid, struct target_waitstatus *status));

/* Lynx needs a special definition of this so that we can
   print out the pid and thread number seperatly.  */

#undef target_pid_to_str

#define target_pid_to_str(PID) lynx_pid_to_str (PID)

extern char *lynx_pid_to_str PARAMS ((int pid));

#endif /* NM_LYNX_H */
