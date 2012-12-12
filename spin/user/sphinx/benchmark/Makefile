#
# HISTORY
# 28-Dec-97  Przemek Pardyak (pardy) at the University of Washington
#	Improved clean-up.
#

THISTREE:=$(shell cd ../../..; pwd)
include $(THISTREE)/make.conf

TARGETS := test_getpid small_tcp appel fault ls micro pcctime \
	big_tcp
CFLAGS = -I../libbench -g

UNAME=$(shell uname)
ifeq ($(UNAME), Linux)
 STATIC=-static
else
 STATIC=-non_shared
endif

all: $(TARGETS) 

$(TARGETS):%:%.o libnet_subr.a
	cc $(STATIC) -o $@ $^  ../libbench/$(M3ARCH)/libbench.a \
		../../usyscall/$(M3ARCH)/USyscallUser.o \
		libnet_subr.a

libnet_subr.a: net_subr.o
	rm -f $@
	ar q $@ $^

clean clobber clean_all clobber_all::
	rm -f *.o *~
	$(MAKE) -C osf $@
	$(MAKE) -C spin $@

