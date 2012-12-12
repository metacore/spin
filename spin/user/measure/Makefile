#
# Copyright 1995, University of Washington
# All rights reserved.
# See COPYRIGHT file for a full description
#
# HISTORY
# 28-Mar-96  Przemek Pardyak (pardy) at the University of Washington
#	Converted to new build environment.
#

THISTREE:=$(shell cd ../..; pwd)
include $(THISTREE)/make.conf

extension: m3sources	# make.extension has the rule to build m3sources
install: extension

include ../make.extension

