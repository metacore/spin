#
# cleaner m3 deps
#

THISTREE:=$(shell cd ../../../..; pwd)
include $(THISTREE)/make.conf

LDFLAGS=-L$(THISTREE)/user/lib -L$(FULLTREE)/user/lib
vpath crt0.o    $(THISTREE)/user/lib:$(FULLTREE)/user/lib

all: extension #cleaner_domain
cleaner_domain: crt0.o cleaner.o extend_cleaner.o encap_cleaner.o
	$(LD) $(LDFLAGS) -o $@ $^ -ldomain -lc

extension: m3sources	# make.extension has the rule to build m3sources

install: extension #cleaner_domain
	#cp cleaner_domain ../../bin

include $(THISTREE)/user/make.extension
