
#
# HISTORY
# 28-Dec-97  Przemek Pardyak (pardy) at the University of Washington
#	Improved clean-up.
#
# 16-Jul-97  Tian Fung Lim (tian) at the University of Washington
#	added writebarrier platform
#
# 13-Jun-97  David Becker at the University of Washington
#      renamed sal dirs.  include sal/i386_freebsd
# 28-Feb-97  Wilson Hsieh (whsieh) at the University of Washington
#	fixed to handle separate build targets
#
# 11-Feb-97  Przemek Pardyak (pardy) at the University of Washington
#	Added ref-counting targets.
#
# 29-May-96  Charles Garrett (garrett) at the University of Washington
#	Work with either profiled or unprofiled version of DRIVER code.
#
# 24-May-96  Marc Fiuczynski (mef) at the University of Washington
#	Created.


THISTREE:=$(shell cd ../../..; pwd)
include $(THISTREE)/make.conf	# sets FULLTREE and PATH to m3build

CC = ${TARGETCC}

LDFLAGS=-L$(THISTREE)/user/$(BUILD_DIR)/lib -L$(FULLTREE)/user/$(BUILD_DIR)/lib
SRCS := $(wildcard *.c)
OBJS := $(SRCS:.c=.o) 

include ../../make.extension

all: extension 

extension: m3sources $(BUILD_DIR)/net_upcalls.o $(BUILD_DIR)/socksupport.o

install: all 

clobber clobber_all:: clean_dep

clean_dep::
	rm -f make.dep

#



ifeq ($(M3ARCH),ALPHA_SPIN)
SALDIR=kernel/sal/alpha_osf
includes = $(DRIVER_TARGET) . include include/alpha arch/alpha arch
INCLUDES=-I $(patsubst %,-I$(THISTREE)/$(SALDIR)/%, $(includes))	\
	 $(patsubst %,-I$(FULLTREE)/$(SALDIR)/%, $(includes))
DEFINES=-DIDENT=SPIN -DDEC3000_300 -DDEC3000_500 -DSWAPTYPE=1 		\
	-DRELEASE='"'0.0'"' -DVERSION='"'0'"' -DMACH -DOSF -DCOMPAT_43	\
	-DSPIN -DUFS -DKERNEL -D_KERNEL -D_BSD -DTCP_SPINDLE
CFLAGS = \
	$(INCLUDES) -I$(SYSDIR) -I$(SYSDIR)/glue		\
	$(DEFINES) -O2 -Wco,-nofloat -Olimit 1500 -D__alpha 
else
SALDIR=kernel/sal/i386_freebsd
includes = $(DRIVER_TARGET) . include
INCLUDES=-I $(patsubst %,-I$(THISTREE)/$(SALDIR)/%, $(includes))	\
	 $(patsubst %,-I$(FULLTREE)/$(SALDIR)/%, $(includes))
DEFINES=-DIDENT=SPIN -DCOMPAT_43 -DSPIN -DUFS -DKERNEL -D_KERNEL -D_BSD \
        -DTCP_SPINDLE
CFLAGS = $(INCLUDES) $(DEFINES) -O2 -g
endif

ASFLAGS = -I $(INCLUDES) $(DEFINES)

depend : $(SRCS)
	-${CC} -M ${CFLAGS} $? > make.dep

$(BUILD_DIR)/net_upcalls.o: src/$(M3ARCH)/net_upcalls.c
	${CC} ${CFLAGS} -c -o $@ $<

$(BUILD_DIR)/socksupport.o: src/$(M3ARCH)/socksupport.c
	${CC} ${CFLAGS} -c -o $@ $<

make.dep :
	touch make.dep
	$(MAKE) depend
include make.dep

.PHONY:
# DO NOT DELETE THIS LINE -- make depend depends on it.
