#
# Makefile for the SPIN version of m3tk
# This program runs under Digital Unix on the local host.
#
# 11-Nov-97  David Becker (becker) at the University of Washington
#	Removed unused variables
#
# 11-Feb-97  Przemek Pardyak (pardy) at the University of Washington
#	Better clean-up and multiple platforms.
#


THISTREE:=$(shell cd ../..; pwd)
include $(THISTREE)/make.conf	# sets FULLTREE and DECSRCM3BUILD

M3FLAGS=-DINSTALL_ROOT=$(THISTREE)/local/${LOCALTARGET}/

all: m3sources

include $(THISTREE)/local/make.local
