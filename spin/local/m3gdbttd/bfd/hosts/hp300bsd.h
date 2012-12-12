#include <fcntl.h>
#include <errno.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>
#include <string.h>
#include <sys/file.h>

void *malloc();
void *realloc();
void free();

#ifndef	O_ACCMODE
#define O_ACCMODE (O_RDONLY | O_WRONLY | O_RDWR)
#endif

#define SEEK_SET 0
#define SEEK_CUR 1

#include <sys/param.h>
#ifdef BSD4_4
#include <stdlib.h>
#define NO_CORE_COMMAND
#endif

#define	HOST_PAGE_SIZE		NBPG
#define	HOST_SEGMENT_SIZE	NBPG	/* Data seg start addr rounds to NBPG */
#define	HOST_MACHINE_ARCH	bfd_arch_m68k
/* #define	HOST_MACHINE_MACHINE	 */

#define	HOST_TEXT_START_ADDR		0
#define	HOST_STACK_END_ADDR		0xfff00000
#define	HOST_BIG_ENDIAN_P

#include "fopen-same.h"
