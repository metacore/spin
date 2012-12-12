THISTREE:=$(shell cd ../../..; pwd)
include $(THISTREE)/make.conf

install: extension docs
extension: m3sources	# make.extension has the rule to build m3sources

include ../../make.extension

