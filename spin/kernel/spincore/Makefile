

# HISTORY
# 28-Dec-97  Przemek Pardyak (pardy) at the University of Washington
#	Improved clean-up.
#
# 16-Jul-97  Tian Fung Lim (tian) at the University of Washington
#	added writebarrier platform
#
# 29-Jan-97  Wilson Hsieh (whsieh) at the University of Washington
#	refcount platform
#
# 05-May-96  Charles Garrett (garrett) at the University of Washington
#	Set M3ARCH to either SPIN or SPIN_PROF depending on whether
#	 PROFILE is defined or not.
#

THISTREE:=$(shell cd ../..; pwd)
include $(THISTREE)/make.conf	# sets FULLTREE and PATH to m3build

all: m3sources

M3FLAGS=-v -b $(BUILD_DIR) $(TREEFLAGS)

include $(THISTREE)/make.m3

