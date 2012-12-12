
THISTREE:=$(shell cd ../..; pwd)
include $(THISTREE)/make.conf	# sets FULLTREE and PATH to m3build

extension: m3sources	# make.extension has the rule to build m3sources

install: extension

include ../make.extension
