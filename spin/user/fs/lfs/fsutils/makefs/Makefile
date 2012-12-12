#
# makefs m3 deps
#

THISTREE:=$(shell cd ../../../../..; pwd)
include $(THISTREE)/make.conf

LDFLAGS=-L$(THISTREE)/user/lib -L$(FULLTREE)/user/lib
vpath crt0.o    $(THISTREE)/user/lib:$(FULLTREE)/user/lib

all: extension #makefs_domain
makefs_domain: crt0.o makefs.o extend_makefs.o encap_makefs.o
	$(LD) $(LDFLAGS) -o $@ $^ -ldomain -lc

extension: m3sources	# make.extension has the rule to build m3sources

install: extension #makefs_domain

include $(THISTREE)/user/make.extension

