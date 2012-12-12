
#
# HISTORY
# 28-Dec-97  Przemek Pardyak (pardy) at the University of Washington
#	Improved clean-up.
#
# 28-Feb-97  Wilson Hsieh (whsieh) at the University of Washington
#	fixed to handle separate build targets
#
#
#


THISTREE:=$(shell cd ../../..; pwd)
include $(THISTREE)/make.conf

OBJS = ufs.o extend_ufs.o encap_ufs.o

all: extension

ufs_domain: $(OBJS)
	$(LD) $(LDFLAGS) -o $@ $^ -lusyscall

extension: m3sources	# make.extension has the rule to build m3sources

install: all
	#cp ufs_domain $(THISTREE)/user/$(BUILD_DIR)/bin

clean clobber clean_all clobber_all::
	rm -f $(OBJS)

include ../../make.extension
