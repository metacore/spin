#
# Copyright 1995, University of Washington
# All rights reserved.
# See COPYRIGHT file for a full description
#
# HISTORY
# 28-Dec-97  Przemek Pardyak (pardy) at the University of Washington
#	Improved clean-up.
#
# 11-Nov-97  David Becker (becker) at the University of Washington
#	Changed M3BUILD definition to search explicity rather than
#	through a PATH variable set somewhere in the makefiles.
#
# 16-Jul-97  Tian Fung Lim (tian) at the University of Washington
#	Added writebarrier platform
#
# 29-Jan-97  Wilson Hsieh (whsieh) at the University of Washington
#	refcount platform, fix clean/clobber to only delete BUILD_DIR
#
# 07-Aug-96  Marc Fiuczynski (mef) at the University of Washington
#	Cleanup.
#
# 29-May-96  Charles Garrett (garrett) at the University of Washington
#	Enabled cleaning of profiled of unprofiled version.
#
# 05-May-96  Charles Garrett (garrett) at the University of Washington
#	Set M3ARCH to either SPIN or SPIN_PROF depending on whether
#	 PROFILE is defined or not.
#
# 18-Apr-96  Przemek Pardyak (pardy) at the University of Washington
#	Added copyright.  Force recreation of _m3main.c by removing
#	libstart.a, this is done to make sure that module and type
#	information avaialble though _m3main.c is up-to-date with
#	other kernel libraries.

# start is special: we cannot ever skip m3build since it has the code to
#   check if the static spindles need to be re-encapsulated.  So this
#   Makefile does not include make.m3
#
#

THISTREE:=$(shell cd ../..; pwd)
include $(THISTREE)/make.conf	# sets FULLTREE and PATH to m3build

#  Look for m3build first in THISTREE, then in FULLTREE.
M3BUILD:=$(firstword $(wildcard $(patsubst %, %/local/$(LOCALTARGET)/bin/m3build, $(THISTREE) $(FULLTREE))))

M3FLAGS=-b $(BUILD_DIR) $(TREEFLAGS) -DBSD_PREFIX=$(BSD_PREFIX) -DMOUNT_POINT=$(MOUNT_POINT) -DHTTP_ADDR=$(HTTP_ADDR) -DHTTP_PORT=$(HTTP_PORT) -DMOUNT_PAD=$(MOUNT_PAD) -DFETCH_METHOD=$(FETCH_METHOD)

all: 
	-rm -f $(BUILD_DIR)/libstart.a  # force _m3main.c to be regenerated
	$(M3BUILD) $(M3FLAGS)

clean clobber::
	-rm -rf $(BUILD_DIR) $(M3DEPS)
	-rm m3statics*

clean_all clobber_all::
	-rm -rf $(M3ARCHES) $(ALLM3DEPS)
	-rm -f $(ALLM3STATICS)
