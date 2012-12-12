# SPIN SPECIFIC STUFF
THISTREE:=$(shell cd ../../../../../../.. ; pwd)
include $(THISTREE)/make.conf   # sets FULLTREE and PATH
CC=${TARGETCC}
FREEBSDSRC=$(THISTREE)/kernel/sal/i386_freebsd

# REGULAR MAKEFILE STUFF
LIBS=	liblmm.a

SRCS=	lmm_add_free.c      \
	lmm_add_region.c    \
	lmm_alloc.c         \
	lmm_alloc_aligned.c \
	lmm_alloc_gen.c     \
	lmm_avail.c         \
	lmm_avail_in.c      \
	lmm_dump.c          \
	lmm_find_free.c     \
	lmm_free.c          \
	lmm_init.c          \
	lmm_remove_free.c

NOTYET=	lmm_alloc_page.c    \
	lmm_free_page.c     \
	ENDOFLIST

OBJS= $(patsubst %.c,%.o,$(SRCS))

INCLUDES=       -nostdinc -I. 
INCLUDES+=	-I$(FREEBSDSRC)
INCLUDES+=	-I$(FREEBSDSRC)/include
DEFINES=	-DKERNEL -Di386 -DNDEBUG
COPTFLAGS=	-O2 # -fno-builtin

COPTS=		${INCLUDES} ${DEFINES}
CFLAGS=		${COPTFLAGS} ${COPTS}

CLEANFILES+=	${OBS}

all: ${LIBS}

liblmm.a: $(OBJS)
	$(TARGETAR) r $@ $(OBJS) 

$(OBJS): Makefile


clean :
	rm -f ${OBJS} ${LIBS} *~

depend: $(SRCS)
	${CC} -M ${COPTS} ${SRCS} > fullpath.tmp
	sed -e 's,$(FREEBSDSRC),../../sal/${M3ARCH},' < fullpath.tmp > dependencies.tmp 
	cp dependencies.tmp dependencies
	rm dependencies.tmp fullpath.tmp

#
# Dependencies
#

# gmake halts if this does not already exists.  create an empty file and
# 	then run make deps
include dependencies

# this line allows dir names to be used as Makefile targets
.PHONY: $(LIBS) 
