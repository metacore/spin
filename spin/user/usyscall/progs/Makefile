
#
# HISTORY
# 28-Dec-97  Przemek Pardyak (pardy) at the University of Washington
#	Improved clean-up.
#
# 23-May-97  Charles Garrett (garrett) at the University of Washington
#	null removed from TARGETS because there appears to be no such
#	 file
#
# 28-Feb-97  Wilson Hsieh (whsieh) at the University of Washington
#	fixed to handle separate build targets
#

THISTREE:=$(shell cd ../../..; pwd)
include $(THISTREE)/make.conf   # sets FULLTREE and PATH to m3build

CFLAGS = -g -I..
#TARGETS = hello self null
TARGETS = hello self

ifeq ($(M3ARCH),IX86_SPIN)
all : $(TARGETS)
CC = $(TARGETCC)
LDFLAGS=-Bstatic -e start -nostdlib -L$(STDLIB) -L$(THISTREE)/user/$(BUILD_DIR)/lib -L$(FULLTREE)/user/$(M3ARCH)/lib
$(TARGETS):%:%.o ../$(M3ARCH)/USyscallUser.o
	$(TARGETLD) $(LDFLAGS) -o $@  $(STDLIB)/crt0.o $^ -lc 
endif

ifeq ($(M3ARCH),ALPHA_SPIN)
all : crt0.o $(TARGETS)
$(TARGETS):%:%.o ../$(BUILD_DIR)/USyscallUser.o
	$(LD) -o $@  crt0.o $^ 
endif

clean clobber clean_all clobber_all:
	rm -f *.o *~ $(TARGETS)
