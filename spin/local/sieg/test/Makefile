THISTREE:=$(shell cd ../..; pwd)
include $(THISTREE)/make.conf	# sets FULLTREE and PATH to m3build

CFLAGS=-I
LDFLAGS=-L$(THISTREE)/user/lib -L$(FULLTREE)/user/lib

all: extension testhello

testhello : crt0.o testhello.o
	$(LD) crt0.o testhello.o

extension: m3sources	# make.extension has the rule to build m3sources

clean::
	rm -f *.o encap* dlib_domain

include ../make.extension
