#
# Makefile for the SPIN version of the Modula-3 rpc stub generator
# This program runs on the local host.
#
# History
#
# 11-Feb-97  Przemek Pardyak (pardy) at the University of Washington
#	Better clean-up and multiple platforms.
#

THISTREE:=$(shell cd ../..; pwd)
include $(THISTREE)/make.conf	# sets FULLTREE and DECSRCM3BUILD

all: $(LOCALTARGET)/m3rpcgen

$(LOCALTARGET)/m3rpcgen: $(LOCALTARGET)/Makefile
	$(MAKE) -C $(LOCALTARGET) install

$(LOCALTARGET)/Makefile: $(LOCALTARGET)
	cp Makefile.in $(LOCALTARGET)/Makefile

$(LOCALTARGET):
	mkdir -p $(LOCALTARGET)

clean::
	-rm -rf $(M3ARCH) $(LOCALTARGET)
clobber:: clean
	-rm -rf $(THISTREE)/local/$(LOCALTARGET)/bin/m3rpcgen

clean_all clobber_all::
	-rm -rf $(M3ARCHES) $(LOCALTARGETS)
	@for i in $(LOCALTARGETS); do \
		rm -rf $(THISTREE)/local/$$i/bin/m3rpcgen; \
		rm -rf $(THISTREE)/local/$$i/man/man1/m3rpcgen.1 ; \
	done

