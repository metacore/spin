#
# Makefile for script tools
#
# History
#
# 11-Feb-97  Przemek Pardyak (pardy) at the University of Washington
#	Better clean-up and multiple platforms.
#


THISTREE:=$(shell cd ../..; pwd)
include $(THISTREE)/make.conf   # sets FULLTREE and DECSRCM3BUILD

SCRIPTS=FlatProfile.perl GraphProfile.perl InsertMcount.perl \
	profile.perl m3tohtml.perl

install: 
	for i in $(SCRIPTS); do \
		echo "#!$(PERL)"  > ../$(LOCALTARGET)/bin/$$i; \
		cat $$i >> ../$(LOCALTARGET)/bin/$$i; \
		chmod +x ../$(LOCALTARGET)/bin/$$i; \
	done
	cp encap.$(LOCALTARGET) ../$(LOCALTARGET)/bin/encap

clobber::
	-rm -f ../$(LOCALTARGET)/bin/encap

clobber_all::
	@for i in $(LOCALTARGETS); do \
		rm -f ../$$i/bin/encap ; \
	done


