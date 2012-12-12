#include <stddef.h>
#include <fcntl.h>
#include <errno.h>
#undef NULL /* XXX */
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <sys/file.h>

#ifndef	O_ACCMODE
#define O_ACCMODE (O_RDONLY | O_WRONLY | O_RDWR)
#endif

#define SEEK_SET 0
#define SEEK_CUR 1

#include <machine/param.h>
#include <machine/vmparam.h>
#undef ALIGN

#define	HOST_PAGE_SIZE		NBPG
/* #define	HOST_SEGMENT_SIZE	NBPG -- we use HOST_DATA_START_ADDR */
#define	HOST_MACHINE_ARCH	bfd_arch_mips
/* #define	HOST_MACHINE_MACHINE	 */

#define	HOST_TEXT_START_ADDR		USRTEXT
#define	HOST_STACK_END_ADDR		USRSTACK
#define NO_CORE_COMMAND

#include "fopen-same.h"
