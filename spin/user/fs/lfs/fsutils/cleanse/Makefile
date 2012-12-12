#
# cleanse m3 deps
#

THISTREE:=$(shell cd ../../../../..; pwd)
include $(THISTREE)/make.conf

LDFLAGS=-L$(THISTREE)/user/lib -L$(FULLTREE)/user/lib
vpath crt0.o    $(THISTREE)/user/lib:$(FULLTREE)/user/lib

all: extension #cleanse_domain
cleanse_domain: crt0.o cleanse.o extend_cleanse.o encap_cleanse.o
	$(LD) $(LDFLAGS) -o $@ $^ -ldomain -lc

extension: m3sources	# make.extension has the rule to build m3sources

install: extension #cleanse_domain

include $(THISTREE)/user/make.extension

