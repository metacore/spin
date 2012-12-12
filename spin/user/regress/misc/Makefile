#
# Copyright 1995, University of Washington
# All rights reserved.
# See COPYRIGHT file for a full description
#
# HISTORY
# 28-Mar-96  Przemek Pardyak (pardy) at the University of Washington
#	Converted to new build environment.
#
# 04-Jan-96  Przemek Pardyak (pardy) at the University of Washington
#	Created.
#

TESTS = hello

all: $(TESTS)
$(TESTS):
	$(MAKE) -C $@

clean clean_all clobber clobber_all::
	@for i in $(TESTS); do \
		echo -n "$$i:	" ;\
		$(MAKE) -C $$i $@; \
		done

.PHONY: ${TESTS}
