#
# HISTORY
# 28-Dec-97  Przemek Pardyak (pardy) at the University of Washington
#	Improved clean-up.
#

include ../.target
THISTREE:=$(shell cd ../../..; pwd)
include $(THISTREE)/make.conf
ifeq ($(TRANSTARGET), osf)
  M3ARCH=osf
endif

%:%.c
	$(CC) -non_shared -o $@ $^ -L. -ltrans

BENCHDIR=$(THISTREE)/user/sphinx/libbench
CFLAGS += -g3 -D$(M3ARCH) -D$(TRANSTARGET) -I$(BENCHDIR)
all::
	-if [ ! -d $(M3ARCH) ]; then mkdir $(M3ARCH); fi

all:: $(M3ARCH)/libtrans.a

clean clobber:: clean_lib

clean clobber clean_all clobber_all:: clean_this

clean_this::
	rm -f *.o *.a *~
	rm -rf spin osf

clean_all clobber_all:: clean_all_lib

clobber clobber_all:: clean_dep

clean_lib::
	-rm -rf $(M3ARCH) 

clean_all_lib::
	-rm -rf $(M3ARCHES)

clean_dep::
	-rm -f make.dep make.dep.bak

SRCS = libtrans.c xmalloc.c rvm_emulator.c top-down-size-splay.c
OBJS = $(patsubst %.c,$(M3ARCH)/%.o,$(SRCS))

$(M3ARCH)/%.o : %.c
	$(CC) -o $@ -c $(CFLAGS) $^

ifeq ($(M3ARCH),osf) 
$(M3ARCH)/libtrans_osf.o: ../ALPHA_OSF/CSupport.h
  OBJS += libtrans_osf.o
else
  CFLAGS += -O4
  OBJS +=  ../$(BUILD_DIR)/TransSyscallUser.o \
    $(THISTREE)/user/usyscall/$(BUILD_DIR)/USyscallUser.o \
    $(THISTREE)/user/usyscall/lib/usyscall.o
endif

$(M3ARCH)/libtrans.a: $(OBJS)
	rm -f $@
	ar q $@ $^

include ../make.conf
include make.dep
