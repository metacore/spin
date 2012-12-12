#
# Copyright 1996, University of Washington
# All rights reserved.
# See COPYRIGHT file for a full description
#
# HISTORY
# 20-Apr-97  Wilson Hsieh (whsieh) at the University of Washington
#	added stcpfs
#
# 29-Jul-96  Marc Fiuczynski (mef) at the University of Washington
#	Added nullfs and tmpfs
#
#

# we use the wildcard so only the subtrees that exist here are built
PROGRAMS= $(wildcard fscore fscache tftp ufs efs nfs nullfs tmpfs lfs stcpfs)
NOTYET= wfs

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

