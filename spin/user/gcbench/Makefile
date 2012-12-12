#
# Copyright 1996, University of Washington
# All rights reserved.
# See COPYRIGHT file for a full description
#
# HISTORY
# 03-Dec-97  Przemek Pardyak (pardy) at the University of Washington
#	Created.
#

THISTREE:=$(shell cd ../..; pwd)
include $(THISTREE)/make.conf

extension: m3sources	# make.extension has the rule to build m3sources

all: extension

install: all install_domain

include ../make.extension
