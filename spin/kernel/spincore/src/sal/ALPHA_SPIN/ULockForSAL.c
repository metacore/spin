/* 
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY 
 * 23-Apr-96  Emin Gun Sirer (egs) at the University of Washington
 *	Changed ulock_setup so that locks initialized before M3 starts 
 *	will fail reliably if used later without reinit.
 *
 * 22-Feb-96  Charles Garrett (garrett) at the University of Washington
 *	Changed ALPHA_OSF to ALPHA_SPIN.
 *
 * 19-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Added wakeup_one symbol used by some of the Digital Unix code.
 *
 * 18-Feb-96  Emin Gun Sirer (egs) at the University of Washington
 *	Changed wakeup to broadcast to all threads, and added wakeupone
 *	as per the Digital Unix internal kernel interface requirements.
 *
 * 25-Oct-95  Emin Gun Sirer (egs) at the University of Washington
 *	Changed to pass StrongRef'ed strands.
 *
 * 21-Sep-95  Stefan Savage (savage) at the University of Washington
 *	Don't call M3 function which aren't initialized.  This is a hack and
 *	should never happen.  M3 initialization should happen earlier.
 *
 * 22-Aug-95  Emin Gun Sirer (egs) at the University of Washington
 *	Changed mpsleep to support Digital Unix tcp requirements.
 *
 * 01-Aug-95  Brian Bershad (bershad) at the University of Washington
 *	extended compat layer for Mach/OSF dev drivers as per Oystr's
 *      CAM requirements.
 *
 * 13-Jul-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Compatibility layer for Mach/OSF device drivers.
 *
 */
#include <mach/std_types.h>
#include <machine/machparam.h>		/* spl definitions */
#include <mach/exception.h>
#include <spincore/ALPHA_SPIN/ULockForSAL.h>

#define ULOCK_VERBOSE 0

#define GET_CALLER(caller) void *caller = (void *)asm("subq %ra,4,%v0") 
/* ulock support for reader/writer locks used by Digital Unix */
void ulock_setup(lock_t l, void /*struct lockinfo*/ * lip, boolean_t canwait){
#if ULOCK_VERBOSE > 0 
	GET_CALLER(caller);
	printf("[ulock_setup (%lx) ra = %lx]\n",l, caller);
#endif /* ULOCK_VERBOSE */
	if (*ULockForSAL__ulock_setup)
		ULockForSAL__ulock_setup(l,canwait);
	else {
	    /*
	     * Modula-3 not ready to initialize the lock. We now bzero
	     * the structure so that a future use will fail reliably.
	     */
	    bzero(l, sizeof(lock_data_t));
	}
}

void ulock_terminate(lock_t l){
#if ULOCK_VERBOSE > 0 
	GET_CALLER(caller);
	printf("[ulock_terminate (%lx) ra = %lx]\n",l, caller);
#endif /* ULOCK_VERBOSE */
	if (*ULockForSAL__ulock_terminate)
		ULockForSAL__ulock_terminate(l);
}

void ulock_write(lock_t l){
#if ULOCK_VERBOSE > 0 
	GET_CALLER(caller);
	printf("[ulock_write (%lx) ra = %lx]\n",l, caller);
#endif /* ULOCK_VERBOSE */
	if (*ULockForSAL__ulock_write)
		ULockForSAL__ulock_write(l);
}

void ulock_read(lock_t l){
#if ULOCK_VERBOSE > 0 
	GET_CALLER(caller);
	printf("[ulock_read (%lx) ra = %lx]\n",l, caller);
#endif /* ULOCK_VERBOSE */
	if (*ULockForSAL__ulock_read)
		ULockForSAL__ulock_read(l);
}

void ulock_done(lock_t l){
#if ULOCK_VERBOSE > 0 
	GET_CALLER(caller);
	printf("[ulock_done (%lx) ra = %lx]\n",l, caller);
#endif /* ULOCK_VERBOSE */
	if (*ULockForSAL__ulock_done)
		ULockForSAL__ulock_done(l);
}

boolean_t ulock_try_write(lock_t l){
#if ULOCK_VERBOSE > 0 
	GET_CALLER(caller);
	printf("[ulock_try_write (%lx) ra = %lx]\n",l, caller);
#endif /* ULOCK_VERBOSE */
	if (*ULockForSAL__ulock_try_write)
		ULockForSAL__ulock_try_write(l);
}

boolean_t ulock_try_read(lock_t l){
#if ULOCK_VERBOSE > 0 
	GET_CALLER(caller);
	printf("[ulock_try_read (%lx) ra = %lx]\n",l, caller);
#endif /* ULOCK_VERBOSE */
	if (*ULockForSAL__ulock_try_read)
		ULockForSAL__ulock_try_read(l);
}

void ulock_write_to_read(lock_t l){
	printf("WARNING thread.c:ulock_write_to_read() not implemented.\n");
}

void ulock_set_recursive(lock_t l){
	/* NOOP */
}

void ulock_clear_recursive(lock_t l){
	/* NOOP */
}
