#
# getfreelist m3 deps
#

THISTREE:=$(shell cd ../../../../..; pwd)
include $(THISTREE)/make.conf

LDFLAGS=-L$(THISTREE)/user/lib -L$(FULLTREE)/user/lib
vpath crt0.o    $(THISTREE)/user/lib:$(FULLTREE)/user/lib

all: extension
getfreelist_domain: crt0.o getfreelist.o extend_getfreelist.o encap_getfreelist.o
	$(LD) $(LDFLAGS) -o $@ $^ -ldomain -lc

extension: m3sources	# make.extension has the rule to build m3sources

install: extension

include $(THISTREE)/user/make.extension

