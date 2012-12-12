#
# HISTORY
# 28-Dec-97  Przemek Pardyak (pardy) at the University of Washington
#	Improved clean-up.
#

TARGETS := test_getpid small_tcp appel fault ls micro pcctime \
	big_tcp
CFLAGS += -g
VPATH=..
foo := $(shell echo $(TARGETS) foo fofo >/dev/tty)
all: $(TARGETS)
$(TARGETS): doredirect.o Measure.o ../../libbench/libbench.a


clean clobber clean_all clobber_all::
	rm -f $(TARGETS) *.o
