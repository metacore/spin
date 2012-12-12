#
# Copyright 1996, University of Washington
# All rights reserved.
# See COPYRIGHT file for a full description
#
#

# HISTORY
# 28-Dec-97  Przemek Pardyak (pardy) at the University of Washington
#	Improved clean-up.
#

# we use the wildcard so only the subtrees that exist here are built
PROGRAMS= $(wildcard sync touch read write)

install: $(PROGRAMS)
$(PROGRAMS):
	$(MAKE) -C $@ all

clean clobber clean_all clobber_all::
	@for i in $(PROGRAMS); do \
		echo -n "$$i:	" ;\
		$(MAKE) -C $$i $@; \
		done

# this line allows dir names to be used as Makefile targets
.PHONY: $(PROGRAMS)









