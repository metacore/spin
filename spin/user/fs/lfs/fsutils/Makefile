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
PROGRAMS= $(wildcard makefs bigwrite cleanse getfreelist getsegdesc setcurrentseg)

install: $(PROGRAMS)
$(PROGRAMS):
	$(MAKE) -C $@ all

clean clean_all clobber clobber_all::
	@for i in $(PROGRAMS); do \
		echo -n "$$i:	" ;\
		$(MAKE) --no-print-directory -C $$i $@; \
		done


# this line allows dir names to be used as Makefile targets
.PHONY: $(PROGRAMS)









