#
# HISTORY
# 28-Dec-97  Przemek Pardyak (pardy) at the University of Washington
#	Improved clean-up.
#

TARGETS := test_getpid small_tcp appel fault ls micro pcctime \
	big_tcp
CFLAGS += -DSPIN
VPATH=..

all: $(TARGETS)
$(TARGETS): doredirect.o Measure.o \
	../../libbench/libbench.a \
	/spin/$(USER)/spin/user/usyscall/lib/usyscall.o \
	/spin/$(USER)/spin/user/usyscall/ALPHA_SPIN/USyscallUser.o


clean clobber clean_all clobber_all::
	rm -f $(TARGETS) *.o
