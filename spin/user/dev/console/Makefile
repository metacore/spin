# HISTORY
# 29-May-96  Charles Garrett (garrett) at the University of Washington
#	Handle either profiled or unprofiled version of DRIVER.
#

THISTREE:=$(shell cd ../../..; pwd)
include $(THISTREE)/make.conf

extension: m3sources # make.extension has the rule to build m3sources
install: extension

include ../../make.extension

.PHONY: sal
