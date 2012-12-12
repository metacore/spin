
THISTREE:=$(shell cd ../../../..; pwd)
include $(THISTREE)/make.conf

LDFLAGS=-L$(THISTREE)/user/lib -L$(FULLTREE)/user/lib
vpath crt0.o    $(THISTREE)/user/lib:$(FULLTREE)/user/lib

all: extension #freelist_domain
freelist_domain: crt0.o freelist.o extend_freelist.o encap_freelist.o
	$(LD) $(LDFLAGS) -o $@ $^ -ldomain -lc


extension: m3sources	# make.extension has the rule to build m3sources
install: extension #freelist_domain
	cp freelist_domain ../../$(M3ARCH)/bin

include $(THISTREE)/user/make.extension

