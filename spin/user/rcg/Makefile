# HISTORY
# 26-Nov-96  Przemek Pardyak (pardy) at the University of Washington
#	Add empty clean and clobber for PCs.
#
# 16-Oct-96  Wilson Hsieh (whsieh) at the University of Washington
#	rcg only builds for Alphas
#
# 05-May-96  Charles Garrett (garrett) at the University of Washington
#	Set M3ARCH to either SPIN or SPIN_PROF depending on whether
#	 PROFILE is defined or not.
#

THISTREE:=$(shell cd ../..; pwd)
include $(THISTREE)/make.conf

ifeq ($(M3ARCH),ALPHA_SPIN)
extension: m3sources	# make.extension has the rule to build m3sources
install: extension

include ../make.extension

else

all clean clobber install clobber_all clean_all::
	@echo "rcg only builds for ALPHA_SPIN "
endif

