
THISTREE:=$(shell cd ../..; pwd)
include $(THISTREE)/make.conf

extension: m3sources	# make.extension has the rule to build m3sources

all: extension

install: all install_domain

include ../make.extension
