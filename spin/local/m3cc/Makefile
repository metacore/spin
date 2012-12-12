#
# Makefile for the SPIN version of the Modula-3 compiler backend
# This program runs on the local host.
#
# History
#
# 11-Nov-97  David Becker (becker) at the University of Washington
#	Changed M3BUILD definition to search explicity rather than
#	through a PATH variable set somewhere in the makefiles.
#	Use new make.conf vars and removed unused targets
#
# 11-Feb-97  Przemek Pardyak (pardy) at the University of Washington
#	Better clean-up and multiple platforms.
#

THISTREE:=$(shell cd ../..; pwd)
include $(THISTREE)/make.conf	# sets FULLTREE

BINDIR=$(THISTREE)/local/$(LOCALTARGET)/bin
LIBDIR=$(THISTREE)/local/$(LOCALTARGET)/lib/m3/$(LOCALTARGET)

m3cc: m3sources
	cp -p $(LOCALTARGET)/m3cgc1 $(BINDIR)
ifeq ($(LOCALTARGET), ALPHA_OSF)
	cp -p $(LOCALTARGET)/mips-tfile $(LIBDIR)
endif



ifeq ($(LOCALTARGET), LINUXELF)
M3CC_TARGET=FreeBSD2
endif
ifeq ($(LOCALTARGET), ALPHA_OSF)
M3CC_TARGET=ALPHA_OSF
endif

M3FLAGS=-DGNU_MAKE=$(MAKE) -DM3CC_TARGET=$(M3CC_TARGET)
M3FLAGS+=-DGCCDIR=gcc-2.7.2.1
M3extrasrc=gcc-2.7.2.1	# this tells make.m3 to put the files in gcc/ into m3.deps
M3BUILD = $(ORIGINAL_M3BUILD)

include $(THISTREE)/local/make.local

#
# Make wants to rerun yacc and makeinfo when it sees the sources in m3.deps.
# The following lines remove the builtin implicit rules that cause this.
%.c: %.y;
%.info: ;

