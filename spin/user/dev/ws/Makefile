
#
# HISTORY
# 29-Jul-96  Wilson Hsieh (whsieh) at the University of Washington
#	added cross-package dependencies
#
#

THISTREE:=$(shell cd ../../..; pwd)
include $(THISTREE)/make.conf

# inter-package dependencies
VMCORE:=$(THISTREE)/user/vm/vmcore
SPACE:=$(THISTREE)/user/vm/space

extension: $(VMCORE) $(SPACE) m3sources	# make.extension has the rule to build m3sources

# inter-package build rules
$(VMCORE):
	$(MAKE) PROFILE=$(PROFILE) --no-print-directory -C $@ install

$(SPACE):
	$(MAKE) PROFILE=$(PROFILE) --no-print-directory -C $@ install

install: extension

include ../../make.extension

.PHONY: $(VMCORE) $(SPACE)
