
#if 0 /* when x86 cc args updated */
#include <machine/cpu_func.h>
#else
#include <sal/i386_freebsd/machine/cpufunc.h>
#endif

long mvesp(void)
{
	asm("movl %esp,%eax");
}

long get_cr2(void)
{
    return rcr2(); /* this is an inline from i386/include/cpufunc.h */
}
