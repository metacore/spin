THISTREE:=$(shell cd ../../..; pwd)
include $(THISTREE)/make.conf

all: extension

extension: m3sources	# make.extension has the rule to build m3sources

install: all

include ../../make.extension
