
THISTREE:=$(shell cd ../..; pwd)
include $(THISTREE)/make.conf	# sets FULLTREE and PATH to m3build

all: extension libmach_user libmach_domain

libmach_user: LibMach.a

LibMach.a: MachCalls.o
	rm -f libmach.a
	$(TARGETAR) q libmach.a $^	
	ranlib libmach.a

extension: m3sources	# make.extension has the rule to build m3sources

install: all
	cp $(M3ARCH)/libmach_domain ../$(M3ARCH)/bin
	if cmp  libmach.a ../lib/$(M3ARCH)/libmach.a; then true; else cp libmach.a ../$(M3ARCH)/lib; fi

include ../make.extension
