#
# Copyright 1996, University of Washington
# All rights reserved.
# See COPYRIGHT file for a full description
#
# HISTORY
# 29-Jul-96  Wilson Hsieh (whsieh) at the University of Washington
#	added cross-package dependencies


THISTREE:=$(shell cd ../../..; pwd)
include $(THISTREE)/make.conf

# inter-package dependencies
FSCORE:=$(THISTREE)/user/fs/fscore

extension: $(FSCORE) m3sources	# make.extension has the rule to build m3sources

# inter-package build rules
$(FSCORE):
	$(MAKE) PROFILE=$(PROFILE) --no-print-directory -C $@ install

install: extension

include ../../make.extension

.PHONY: $(DEVCORE) $(FSCORE)
