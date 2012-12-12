#
# Copyright 1995, University of Washington
# All rights reserved.
# See COPYRIGHT file for a full description
#
# HISTORY
#
# 11-Nov-97  David Becker (becker) at the University of Washington
#	Changed tests so make will succede when all tests are not checked out.
#
# 03-Sep-97  Robert Grimm (rgrimm) at the University of Washington
#       Added preemption test
#
# 06-Aug-97  Robert Grimm (rgrimm) at the University of Washington
#       Added security regression tests
#
# 28-Mar-96  Przemek Pardyak (pardy) at the University of Washington
#	Converted to new build environment.
#
# 09-Mar-96  Stefan Savage (savage) at the University of Washington
#	Added memory tests
#
# 04-Jan-96  Przemek Pardyak (pardy) at the University of Washington
#	Created.
#

TESTS = $(wildcard misc dynlink compiler disp memory security preempt)

install: $(TESTS)
$(TESTS):
	$(MAKE) -C $@

clean clean_all clobber clobber_all::
	@for i in $(TESTS); do \
		echo -n "$$i:	" ;\
		$(MAKE) -C $$i $@; \
		done

.PHONY: ${TESTS}

