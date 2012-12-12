#
# Copyright 1995, University of Washington
# All rights reserved.
# See COPYRIGHT file for a full description
#
# HISTORY
#
# 06-Aug-97  Robert Grimm (rgrimm) at the University of Washington
#	Created.
#

TESTS = accessmode manager dyncontrol

all: $(TESTS)
$(TESTS):
	$(MAKE) -C $@

clean clean_all clobber clobber_all::
	@for i in $(TESTS); do \
		echo -n "$$i:	" ;\
		$(MAKE) -C $$i $@; \
		done

.PHONY: ${TESTS}
