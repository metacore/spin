#
# Copyright 1996, University of Washington
# All rights reserved.
# See COPYRIGHT file for a full description
#
# HISTORY
# 09-Sep-96  Marc Fiuczynski (mef) at the University of Washington
#	Changed defaultpager to filepager.
#
#

# we use the wildcard so only the subtrees that exist here are built
PROGRAMS= $(wildcard vmcore mach space filepager inodepager)

install extension: $(PROGRAMS)
$(PROGRAMS):
	$(MAKE) -C $@ extension
	$(MAKE) -C $@ install

clean clean_all clobber clobber_all::
	@for i in $(PROGRAMS); do \
		echo -n "$$i:	" ;\
		$(MAKE) -C $$i $@; \
		done

document:
	-$(MAKE) -Cvmcore/doc
	-$(MAKE) -Cspace/doc
	-$(MAKE) -Cfilepager/doc
	-$(MAKE) -Cinodepager/doc

# this line allows dir names to be used as Makefile targets
.PHONY: $(PROGRAMS)

