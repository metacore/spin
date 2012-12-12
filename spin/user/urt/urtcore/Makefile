THISTREE:=$(shell cd ../../..; pwd)
include $(THISTREE)/make.conf	# sets FULLTREE and PATH to m3build

LDFLAGS=-L$(THISTREE)/user/lib -L$(FULLTREE)/user/lib

all: extension $(OBJS)

extension: m3sources	# make.extension has the rule to build m3sources

install: all 

clean::

include ../../make.extension

.PHONY: 
# DO NOT DELETE THIS LINE -- make depend depends on it.
