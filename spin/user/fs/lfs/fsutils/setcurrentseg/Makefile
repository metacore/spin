#
# setcurrentseg m3 deps
#

THISTREE:=$(shell cd ../../../../..; pwd)
include $(THISTREE)/make.conf

LDFLAGS=-L$(THISTREE)/user/lib -L$(FULLTREE)/user/lib
vpath crt0.o    $(THISTREE)/user/lib:$(FULLTREE)/user/lib

all: extension
setcurrentseg_domain: crt0.o setcurrentseg.o extend_setcurrentseg.o encap_setcurrentseg.o
	$(LD) $(LDFLAGS) -o $@ $^ -ldomain -lc

extension: m3sources	# make.extension has the rule to build m3sources

install: extension

include $(THISTREE)/user/make.extension

