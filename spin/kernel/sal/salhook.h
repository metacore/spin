
typedef void* machine_state;

void salhook_main_program();

/* salhook_clock_intr is a function pointer so that higher level OS
   can override this functionality directly, rather than testing at
   runtime whether the vtable exported entry is NIL or not . */
void (*salhook_clock_intr)(machine_state frame);

/* DebuggerInterface
  
   Defines the interface that SAL expects the operating system
   define for TTD support.
 
   NOTE: Some of the interface arguments purposely use "void*" types,
   as they are both architecture and operating specific.  For example,
   "machine_state" is specific to the architecture and "m3array"
   currently is specific to the OS.

 */


unsigned int salhook_current_thread();
/* returns the ID of the current thread that is running */

void salhook_thread_name(unsigned int thread,char* bp,unsigned long len);
/* sets bp to the name of the current thread */

int salhook_is_user_thread(unsigned int thread);
/* returns TRUE if thread is in user space. Otherwise, returns FALSE. */

void salhook_get_state(unsigned int thread, int get_boundary_regs, machine_state machine_state);
/* gets the registers in "machine_state" */

void salhook_set_state(unsigned int thread,machine_state machine_state);
/* sets the registers in "machine_state" */

int salhook_next_domain(long domain_cnt, void *buf, int bytes, void* textaddr);

int salhook_next_thread(unsigned int * thread);
/* ??? */

int salhook_valid_thread(unsigned int t);
/* ??? */

extern int salhook_mallocpages;

#if 0

This is here for information.

/***
  typedefs from sys/systm.h
  */

/* Timeouts */
typedef void (timeout_t)(void *); /* actual timeout function type */
typedef timeout_t *timeout_func_t; /* a pointer to this type */


void timeout(timeout_func_t fun, void *arg, int t);
void untimeout(timeout_func_t fun, void *arg);
void wakeup(void *chan) ;
void thread_wakeup(int chan) ;
int tsleep(void *chan, int pri, char *wmesg, int timo) ;
#endif
