#
# Copyright 1996, University of Washington
# All rights reserved.
# See COPYRIGHT file for a full description
#
# HISTORY
# 23-Jun-96  Eric Christoffersen (ericc) at the University of Washington
#       Moved benchmarks from /user/shell to /user as per ddion merge
#       instructions.
#
# 11-Jun-96  Brian Bershad (bershad) at the University of Washington
#	Introduced utils and expr
#
# 17-Apr-96  Przemek Pardyak (pardy) at the University of Washington
#	Enabled benchmarks.
#

# we use the wildcard so only the subtrees that exist here are built
PROGRAMS= $(wildcard shellcore basiccmds regress consoleshell expr utils)

ifeq ($(M3ARCH),ALPHA_SPIN)
  PROGRAMS += $(wildcard pfm)
endif

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

