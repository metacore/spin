/* 
 * Spin Operating System
 */
/*
 * TTD thread management header file.
 *
 * HISTORY:
 * 09-Feb-95  Brian Bershad (bershad) at the University of Washington
 *	Create
 *
 * ttd_thread.h,v
 * Revision 1.1  1995/03/10  21:56:10  bershad
 * Created.
 *
 * 
 */

#ifndef	_TTD_THREAD_H_
#define	_TTD_THREAD_H_

#include <sal/ttd/ttd_types.h>
#include <sal/ttd/ttd_thread.h>

task_t ttd_thread_to_spintask(ttd_thread t);

/*
 * Check if a ttd_thread handle is valid
 */
extern boolean_t ttd_thread_valid_thread_id(ttd_thread thread);

extern int ttd_thread_debug;
#endif	/* _TTD_THREAD_H_ */
