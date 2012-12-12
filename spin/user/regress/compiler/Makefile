#
# Copyright 1995, University of Washington
# All rights reserved.
# See COPYRIGHT file for a full description
#
# HISTORY
# 29-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
#	make sure bitfield gets compiled for IX86_SPIN
#
# 28-Mar-96  Przemek Pardyak (pardy) at the University of Washington
#	Converted to new build environment.
#
# 04-Jan-96  Przemek Pardyak (pardy) at the University of Washington
#	Created.
#

THISTREE:=$(shell cd ../../..; pwd)
include $(THISTREE)/make.conf   # sets FULLTREE and PATH to m3build

TESTS = view bitfield

all: $(TESTS)
$(TESTS):
	$(MAKE) -C $@

clean clean_all clobber clobber_all::
	@for i in $(TESTS); do \
		echo -n "$$i:	" ;\
		$(MAKE) -C $$i $@; \
		done

.PHONY: ${TESTS}
