
#
# HISTORY
# 28-Dec-97  Przemek Pardyak (pardy) at the University of Washington
#	Improved clean-up.
#
# 20-Apr-97  Wilson Hsieh (whsieh) at the University of Washington
#	fix so that progs and lib get built
#
# 28-Feb-97  Wilson Hsieh (whsieh) at the University of Washington
#	fixed to handle separate build targets
#
#
#

THISTREE:=$(shell cd ../..; pwd)

include $(THISTREE)/make.conf	# sets FULLTREE and PATH to m3build

include ../make.extension

all: extension $(BUILD_DIR)/libusyscall_user.a misc

misc:
ifeq ($(M3ARCH),ALPHA_SPIN)
	$(MAKE) -Cprogs
	$(MAKE) -Clib
endif

$(BUILD_DIR)/libusyscall_user.a: $(BUILD_DIR)/USyscallUser.o
	$(TARGETAR) r $@ $^

$(BUILD_DIR)/USyscallUser.s: extension

install: all ../$(M3ARCH)/lib/libusyscall.a
	echo $(M3ARCH)

../$(M3ARCH)/lib/libusyscall.a:  $(BUILD_DIR)/libusyscall_user.a
	cp $(BUILD_DIR)/libusyscall_user.a ../$(M3ARCH)/lib/libusyscall.a

extension: m3sources misc # make.extension has the rule to build m3sources

clean clean_all clobber clobber_all:: clean_subs

clean_subs::
	$(MAKE) -Cprogs clean
	$(MAKE) -Clib clean




