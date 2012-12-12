#
# Copyright 1995, University of Washington
# All rights reserved.
# See COPYRIGHT file for a full description
#

#
# HISTORY
# 23-Feb-97  Wilson Hsieh (whsieh) at the University of Washington
#	fix dependency on heap_domain
#
# 28-Jan-97  Przemek Pardyak (pardy) at the University of Washington
#	Created.
#


THISTREE:=$(shell cd ../..; pwd)
include $(THISTREE)/make.conf

include ../make.extension

all:	m3sources extension $(BUILD_DIR)/heap_domain

install: all install_domain


