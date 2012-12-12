#
# HISTORY
# 28-Dec-97  Przemek Pardyak (pardy) at the University of Washington
#	Improved clean-up.
#

THISTREE:=$(shell cd ../..; pwd)
include $(THISTREE)/make.conf	# sets FULLTREE and PATH to m3build

ifeq ($(M3ARCH),ALPHA_SPIN)

extension: m3sources	# make.extension has the rule to build m3sources

install: extension

clean clobber::
	-rm -rf $(M3ARCH) m3.deps

clean_all clobber_all::
	-rm -rf $(M3ARCHES) $(ALLM3DEPS)

include ../make.extension

else

extension:
install:
all:

endif
