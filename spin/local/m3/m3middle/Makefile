#
# Makefile for the SPIN version of the Modula-3 compiler
# This program runs on the local host.
#

# HISTORY
#
# 24-Aug-97  Przemek Pardyak (pardy) at the University of Washington
#	Enable building user-level and kernel targets in local.
#
# 28-Feb-97  Wilson Hsieh (whsieh) at the University of Washington
#	fix platforms by setting BUILD_DIR
#

LOCAL:=TRUE

THISTREE:=$(shell cd ../../..; pwd)
include $(THISTREE)/make.conf	# sets FULLTREE and DECSRCM3BUILD

BUILD_DIR:=$(LOCALTARGET)

all: m3sources

include ../make.compiler
