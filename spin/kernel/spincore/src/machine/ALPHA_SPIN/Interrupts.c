#include <arch/machine/cpu.h>


int IsLow(unsigned level)
{
    if(level)
	return 0;
    else
	return 1;
}
