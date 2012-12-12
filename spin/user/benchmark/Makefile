#
# Copyright 1995, University of Washington
# All rights reserved.
# See COPYRIGHT file for a full description
#
# HISTORY
# 23-Jun-96  Eric Christoffersen (ericc) at the University of Washington
#
# 17-Apr-96  Przemek Pardyak (pardy) at the University of Washington
#	Created
#

THISTREE:=$(shell cd ../..; pwd)
include $(THISTREE)/make.conf

extension: m3sources	# make.extension has the rule to build m3sources
install: extension

include ../make.extension
