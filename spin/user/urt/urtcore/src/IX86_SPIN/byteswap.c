
#include <sys/param.h>
#include <sys/systm.h>
#include <sys/types.h>

/* refer to i386_freebsd/machine/endian.h */
unsigned int byte_swap_long(unsigned int i)
{
	return __byte_swap_long(i);
}

unsigned short byte_swap_word(unsigned short s)
{
	return __byte_swap_word(s);
}
