#
# Makefile for the SPIN version of the Modula-3 compiler
# This program runs on the local host.
#
# History
#
# 24-Aug-97  Przemek Pardyak (pardy) at the University of Washington
#	Enable building user-level and kernel targets in local.
#
# 11-Feb-97  Przemek Pardyak (pardy) at the University of Washington
#	Better clean-up and multiple platforms.
#

LOCAL=TRUE

THISTREE:=$(shell cd ../..; pwd)
include $(THISTREE)/make.conf	# sets FULLTREE and DECSRCM3BUILD

CHUNKS= m3middle m3linker m3front m3
all: $(CHUNKS)
	cp m3/$(LOCALTARGET)/m3 ../$(LOCALTARGET)/lib/m3/$(LOCALTARGET)

$(CHUNKS):
	$(MAKE) --no-print-directory -C $@

clean clobber clean_all clobber_all::
	@for i in $(CHUNKS); do \
		echo -n "$$i:	" ;\
		$(MAKE) --no-print-directory -C $$i $@; \
		done
clobber_all::
	@for i in $(LOCALTARGETS); do \
		rm -rf $(THISTREE)/local/$$i/lib/m3/$$i/m3 ; \
	done

# this line allows dir names to be used as Makefile targets
.PHONY: $(CHUNKS)
