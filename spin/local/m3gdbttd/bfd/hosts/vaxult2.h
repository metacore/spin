#include <ansidecl.h>
#include <fcntl.h>
#include <errno.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>
#include <string.h>
#include <sys/file.h>

#ifndef	O_ACCMODE
#define O_ACCMODE (O_RDONLY | O_WRONLY | O_RDWR)
#endif
#define SEEK_SET 0
#define SEEK_CUR 1

#ifndef	DONTDECLARE_MALLOC
extern PTR	malloc	PARAMS ((unsigned));
extern PTR	realloc	PARAMS ((PTR, unsigned));
extern int	free	PARAMS ((PTR));
#endif
extern int	abort	PARAMS ((void));
extern void	bcopy	PARAMS ((char*, char*, int));
extern void	exit	PARAMS ((int));
extern void	bzero	PARAMS ((char *, int));
extern int strtol();
 
#include <machine/param.h>
#include <machine/vmparam.h>
#define	HOST_PAGE_SIZE		(NBPG*CLSIZE)
#define	HOST_MACHINE_ARCH	bfd_arch_vax

#define	HOST_TEXT_START_ADDR	USRTEXT
#define	HOST_STACK_END_ADDR	USRSTACK
#undef	HOST_BIG_ENDIAN_P

#include "fopen-same.h"
