#
# HISTORY
# 28-Dec-97  Przemek Pardyak (pardy) at the University of Washington
#	Improved clean-up.
#

THISTREE:=$(shell cd ../..; pwd)
include $(THISTREE)/make.conf	# sets FULLTREE and PATH to m3build
export M3ARCH

all: m3sources	# make.extension has the rule to build m3sources
	$(MAKE) -Clibbench

install: all

clean clobber clean_all clobber_all::
	rm -f *.o encap*
	- $(MAKE) -Cprogs $@
	- $(MAKE) -Clibbench $@
	- $(MAKE) -Cbenchmark $@

include ../make.extension




