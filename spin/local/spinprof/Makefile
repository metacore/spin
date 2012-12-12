#
# Makefile for the SPIN tools
# These are programs that run on the local host.
#
# HISTORY
# 11-Feb-97  Przemek Pardyak (pardy) at the University of Washington
#	Better clean-up and multiple platforms.
#
# 29-Jul-96  Charles Garrett (garrett) at the University of Washington
#	Changed to use diffs from OSF files.
#
# 24-Jun-96  Charles Garrett (garrett) at the University of Washington
#	Created.
#

THISTREE:=$(shell cd ../..; pwd)
include $(THISTREE)/make.conf	# sets FULLTREE and DECSRCM3BUILD

# Directory where OSF sources for gprof are located.
GPROFDIR = $(DIGITALUNIX)/src/usr/ccs/bin/gprof

CFLAGS   = -g 

HEADERS  = gprof.h gmon.h prof_common.h gprofblurbs.h
SRCS	= arcs.c dfn.c getobjinfo.c gprof.c lookup.c printgprof.c \
	   printlist.c

ifeq ($(LOCALTARGET), ALPHA_OSF)
LIBS     = -lmld 
endif

spinprof:	$(SRCS)
	$(CC) -o spinprof -g $(SRCS) $(LIBS)




clean clean_all clobber clobber_all::
	rm -f *.o

# dependencies among .c and .h files

arcs.c:	gprof.h gmon.h prof_common.h 

dfn.c:	gprof.h gmon.h prof_common.h 

getobjinfo.c:	prof_common.h

gprof.c:	gprof.h gmon.h prof_common.h 

lookup.c:	gprof.h gmon.h prof_common.h 

printgprof.c:	gprof.h gmon.h gprofblurbs.h prof_common.h 

printlist.c:	gprof.h gmon.h prof_common.h 

