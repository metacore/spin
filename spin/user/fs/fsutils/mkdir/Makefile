#
# mkdir
#

THISTREE:=$(shell cd ../../../..; pwd)
include $(THISTREE)/make.conf

LDFLAGS=-L$(THISTREE)/user/lib -L$(FULLTREE)/user/lib
vpath crt0.o    $(THISTREE)/user/lib:$(FULLTREE)/user/lib

all: extension #mkdir_domain
mkdir_domain: crt0.o mkdir.o extend_mkdir.o encap_mkdir.o
	$(LD) $(LDFLAGS) -o $@ $^ -ldomain -lc

extension: m3sources	# make.extension has the rule to build m3sources

install: extension #mkdir_domain

include $(THISTREE)/user/make.extension

