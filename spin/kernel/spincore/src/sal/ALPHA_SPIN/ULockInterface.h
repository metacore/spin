/* 
 * HISTORY
 * 13-Nov-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.  Start to clean interface for SAL support.
 */

/* ULockInterface */
typedef void * lock_t;
typedef int boolean_t;

struct ULockInterface { 
	void (*setup)(lock_t l, void /*struct lockinfo*/ * lip, boolean_t canwait);

	void (*terminate)(lock_t l);

	void (*write)(lock_t l);

	void (*read)(lock_t l);

	void (*done)(lock_t l);

	boolean_t (*try_write)(lock_t l);

	boolean_t (*try_read)(lock_t l);

	void (*write_to_read)(lock_t l);

	void (*set_recursive)(lock_t l);

	void (*clear_recursive)(lock_t l);
};
