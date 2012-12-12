#
# write m3 deps
#

THISTREE:=$(shell cd ../../../..; pwd)
include $(THISTREE)/make.conf

LDFLAGS=-L$(THISTREE)/user/lib -L$(FULLTREE)/user/lib
vpath crt0.o    $(THISTREE)/user/lib:$(FULLTREE)/user/lib

all: extension #write_domain
write_domain: crt0.o write.o extend_write.o encap_write.o
	$(LD) $(LDFLAGS) -o $@ $^ -ldomain -lc

extension: m3sources	# make.extension has the rule to build m3sources

install: extension #write_domain

include $(THISTREE)/user/make.extension

