include ../.target
SRCS=trans.cc work.cc spin_def.c getopt.c getopt1.c
THISTREE:=$(shell cd ../../..; pwd)
include $(THISTREE)/make.conf
BENCHDIR=$(THISTREE)/user/sphinx/libbench
CFLAGS=-g -I../lib -DSPIN -D$(TRANSTARGET) -I$(BENCHDIR)
LIB=../lib/$(M3ARCH)/libtrans.a $(BENCHDIR)/$(M3ARCH)/libbench.a

$(M3ARCH)/%.o : %.cc
	$(CXX) $(CFLAGS) -o $@ -c $*.cc
$(M3ARCH)/%.o : %.c
	$(CC) $(CFLAGS) -o $@ -c $*.c
$(M3ARCH)/%.o : %.s
	as -o $@ $^

all:: $(M3ARCH)/rvmbench

OBJS:=$(patsubst %.cc, $(M3ARCH)/%.o, $(SRCS))
OBJS:=$(patsubst %.c, $(M3ARCH)/%.o, $(OBJS))
OBJS:=$(patsubst %.s, $(M3ARCH)/%.o, $(OBJS))

$(M3ARCH)/rvmbench:  $(OBJS) $(LIB)
	$(CXX) -static -o $@ $(CFLAGS) $^
$(M3ARCH)/librvmbench.a: $(OBJS)
	rm -f $@
	ar q $@ $^
	
clean::
	rm -f librvmbench.a

include ../make.conf
include make.dep
