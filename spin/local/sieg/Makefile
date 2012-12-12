#
# Makefile for the SPIN Interface to Extension Generator
# This program runs on the local host.
#
# 11-Nov-97  David Becker (becker) at the University of Washington
#	Removed unused variables
#
# 11-Feb-97  Przemek Pardyak (pardy) at the University of Washington
#	Better clean-up and multiple platforms.
#

THISTREE:=$(shell cd ../..; pwd)
include $(THISTREE)/make.conf	# sets FULLTREE

all: m3sources ../$(LOCALTARGET)/bin/sieg

$(LOCALTARGET)/sieg: m3sources 

../$(LOCALTARGET)/bin/sieg : $(LOCALTARGET)/sieg
	cp -p $(LOCALTARGET)/sieg ../$(LOCALTARGET)/bin

include $(THISTREE)/local/make.local

