#
# HISTORY
# 28-Dec-97  Przemek Pardyak (pardy) at the University of Washington
#	Improved clean-up.
#
# 28-Jun-96  Marc Fiuczynski (mef) at the University of Washington
#	Added Jan Sanislo's dlib test programs.
#

THISTREE=$(shell cd ../../../; pwd)
include $(THISTREE)/make.conf
CFLAGS = -g
#M3ARCH = IX86_SPIN
NOTYET = auxv

TARGETS := $(patsubst %.c,%,$(wildcard *.c))
TARGETS := $(filter-out $(NOTYET), $(TARGETS))

all : $(TARGETS)

ifeq ($(M3ARCH), IX86_SPIN)
CC=bsd-newcc
$(TARGETS):%:%.o
	$(CC) -static -o $@ $^

else
$(TARGETS):%:%.o
	cc -non_shared -o $@ $^
endif

clean clobber clean_all clobber_all:
	rm -f *.o *~ $(TARGETS)
