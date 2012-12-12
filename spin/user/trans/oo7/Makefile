#
# HISTORY
# 28-Dec-97  Przemek Pardyak (pardy) at the University of Washington
#	Improved clean-up.
#

# OO7 benchmark.
# I think this one was originally written for RVM,
# and was hacked for Opal when I got it. 
# I hacked it further for the SPIN transaction. -- yasushi@cs.washington.edu
#
# This makefile is for GNU make.

include ../.target
THISTREE:=$(shell cd ../../..; pwd)
include $(THISTREE)/make.conf

BENCHDIR=$(THISTREE)/user/sphinx/libbench
BENCHLIB=$(BENCHDIR)/$(M3ARCH)/libbench.a
OPAL_FLAGS = -DSANITY -DLOCAL_SANITY -DNO_OPAL -DNO_MP $(OPAL_IFLAGS)
CXXFLAGS += -g -O2 -D$(TRANSTARGET) $(OPAL_FLAGS) -I../lib -I../malloc -I$(BENCHDIR)


OO7_SRCS = Instantiate.cc AtomicPart.cc Assembly.cc Connection.cc \
	CompositePart.cc CompPartIter.cc Module.cc Document.cc Manual.cc \
	PartIdSet.cc Support.cc SetParams.cc Insert.cc Delete.cc Traverse.cc\
	Query1.cc Query2.cc Query3.cc Query4.cc Query5.cc Query7.cc Query8.cc\
	dbsize.cc  Bench.cc GenDB.cc OO7.cc \
        rds_interface.cc file_utils.cc 

CXX=g++

$(M3ARCH)/%.o : %.cc
	$(CXX) $(CXXFLAGS) -c -o $@ $^

all::	
	-if [ ! -d $(M3ARCH) ]; then mkdir $(M3ARCH); fi

ifeq ($(TRANSTARGET),spin)
  all::  $(M3ARCH)/OO7
  RVMLIBS=../malloc/librds.a ../lib/$(M3ARCH)/libtrans.a $(BENCHLIB)
  OO7_SRCS += SPINDep.cc
endif

ifeq ($(TRANSTARGET),rvmunix)
  M3ARCH=rvmunix
  all::  $(M3ARCH)/OO7
  RVMLIBS=../malloc/librds.a ../seg/libseg.a ../rvm/librvm.a $(BENCHLIB)
  OO7_SRCS += RVMDep.cc
endif

ifeq ($(TRANSTARGET),osf)
  M3ARCH=osf
  all:: $(M3ARCH)/liboo7.a
  OO7_SRCS += SPINDep.cc
endif

COMMON_OBJS = $(patsubst %.cc, $(M3ARCH)/%.o, $(COMMON_SRCS))
OO7_OBJS = $(patsubst %.cc, $(M3ARCH)/%.o, $(OO7_SRCS))

SRCS = $(RDSTEST_SRCS) $(OO7_SRCS) $(COMMON_SRCS)


$(M3ARCH)/OO7: $(OO7_OBJS) $(RVMLIBS)
	$(CXX) -static $(CFLAGS) $(OPAL_FLAGS) -o $@ $(OO7_OBJS) \
	$(COMMON_OBJS) $(RVMLIBS) $(THREADSLIB) $(LIBFLAGS)
	cp $(M3ARCH)/OO7 $(M3ARCH)/oo7.sym
	strip $(M3ARCH)/OO7

$(M3ARCH)/liboo7.a: $(OO7_OBJS) $(RDSTEST_OBJS) $(COMMON_OBJS)
	rm -f $@
	ar q $@ $^

clean clobber:: clean_lib
	rm -f *.o rdstest OO7 *~
	rm -f spin/* osf/*

clean_all clobber_all:: clean_all_lib

clobber clobber_all:: clean_dep

clean_lib::
	-rm -rf $(M3ARCH) 

clean_all_lib::
	-rm -rf $(M3ARCHES)

clean_dep::
	-rm -f make.dep make.dep.bak

tidy:
	rm -f core swapfile* rds_data* .__afs* rdstest.address

spotless: tidy clean
	rm -f log_file
clean::
	rm -f db.* *_data_file rds_data
	rm -f *init.* 

installfiles:
	$(INSTALL) testrvm $(BINDIR)
	-chmod 0755 $(BINDIR)/testrvm
	$(INSTALL) rvm_basher $(BINDIR)
	-chmod 0755 $(BINDIR)/rvm_basher

install:
	gmake clean
	gmake reinstall

reinstall:
	gmake all
	gmake installfiles

include ../make.conf
include make.dep

