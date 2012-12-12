#
# getsegdesc m3 deps
#

THISTREE:=$(shell cd ../../../../..; pwd)
include $(THISTREE)/make.conf

LDFLAGS=-L$(THISTREE)/user/lib -L$(FULLTREE)/user/lib
vpath crt0.o    $(THISTREE)/user/lib:$(FULLTREE)/user/lib

all: extension
getsegdesc_domain: crt0.o getsegdesc.o extend_getsegdesc.o encap_getsegdesc.o
	$(LD) $(LDFLAGS) -o $@ $^ -ldomain -lc

extension: m3sources	# make.extension has the rule to build m3sources

install: extension

include $(THISTREE)/user/make.extension

