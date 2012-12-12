#
# Copyright 1996, University of Washington
# All rights reserved.
# See COPYRIGHT file for a full description
#
# HISTORY
# 28-Dec-97  Przemek Pardyak (pardy) at the University of Washington
#	Improved clean-up.
#
# 31-May-97  David Becker at the University of Washington
#      Added netdev
#
# 20-May-96 oystr at the University of Washington
#	Added ws to PROGRAMS
#
#

THISTREE:=$(shell cd ../..; pwd)
include $(THISTREE)/make.conf

# we use the wildcard so only the subtrees that exist here are built
ifeq ($(M3ARCH), IX86_SPIN)
PROGRAMS= $(wildcard devcore console extent netdev bdev)
else
PROGRAMS= $(wildcard devcore console ws cam extent netdev)
endif

# for clean-up, the sum of all architectures
ALL_PROGRAMS= $(wildcard devcore console extent netdev bdev ws cam)

install: $(PROGRAMS)
$(PROGRAMS):
	$(MAKE) -C $@ install

clean clean_all clobber clobber_all::
	@for i in $(ALL_PROGRAMS); do \
		echo -n "$$i:	" ;\
		$(MAKE) -C $$i $@; \
		done

# this line allows dir names to be used as Makefile targets
.PHONY: $(ALL_PROGRAMS)
