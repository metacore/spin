#
# HISTORY
# 16-Jul-97  Tian Fung Lim (tian) at the University of Washington
#	writebarrier platform
#
# 29-Jan-97  Wilson Hsieh (whsieh) at the University of Washington
#	refcount platform
#
# 13-Aug-96  Frederick Gray (fgray) at the University of Washington
#	Multi-architecture cleanup.
#
# 05-May-96  Charles Garrett (garrett) at the University of Washington
#	Define M3ARCH to be either ALPHA_SPIN of ALPHA_SPIN_PROF
#	 depending on whether PROFILE is defined or not.
#

THISTREE:=$(shell cd ../..; pwd)
include $(THISTREE)/make.conf	# sets FULLTREE and PATH to m3build

all: m3sources

ifdef PROFILE
    BUILD_DIR:=$(M3ARCH)_PROF
else
    BUILD_DIR:=$(M3ARCH)
endif
ifdef REFCOUNT
    BUILD_DIR:=$(BUILD_DIR)_REF
endif
ifdef WRITEBARRIER
    BUILD_DIR:=$(BUILD_DIR)_WB
endif

# HACK overrides everything else
ifdef HACK
    BUILD_DIR:=$(M3ARCH)_HACK
endif
M3FLAGS=-b $(BUILD_DIR) $(TREEFLAGS)

include $(THISTREE)/make.m3
