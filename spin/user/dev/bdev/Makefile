THISTREE:=$(shell cd ../../..; pwd)
include $(THISTREE)/make.conf

all: extension $(BUILD_DIR)/bdev_domain

extension: m3sources	# make.extension has the rule to build m3sources
install: all

include ../../make.extension
