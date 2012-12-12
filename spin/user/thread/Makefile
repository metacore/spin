#
# Copyright 1996, University of Washington
# All rights reserved.
# See COPYRIGHT file for a full description
#
# HISTORY
#

# we use the wildcard so only the subtrees that exist here are built
PROGRAMS= $(wildcard threadcore exec cthread)

install: $(PROGRAMS)
$(PROGRAMS):
	$(MAKE) -C $@ install

clean clean_all clobber clobber_all::
	@for i in $(PROGRAMS); do \
		echo -n "$$i:	" ;\
		$(MAKE) -C $$i $@; \
		done


# this line allows dir names to be used as Makefile targets
.PHONY: $(PROGRAMS)

