#
# HISTORY
# 28-Dec-97  Przemek Pardyak (pardy) at the University of Washington
#	Improved clean-up.
#

# rds definitions
TRANSTARGET := $(shell cat ../.target)
HFILES = trans_malloc.h
SRCS = trans_malloc.c trans_free.c trans_queue.c trans_load_heap.c \
	trans_checkheap.c trans_prealloc.c
OBJS = $(SRCS:.c=.o)
CFLAGS=-I. -I../lib -g

all: librds.a seg_create rdstest

seg_create: seg_create.o
	$(CC) $(STATIC_FLAG) $(CFLAGS) -o $@ $^

librds.a:  $(OBJS)
	ar rv librds.a $?
	ranlib librds.a

$(OFILES):%.o: %.c $(HFILES)
	$(CC) -c $(CFLAGS) $*.c

rdstest: librds.a rdstest.o
	$(CC) -o $@ $^ librds.a
	
clean clobber:: clean_lib

clean clobber clean_all clobber_all:: clean_this

clean_this::
	rm -f *.o *.a rdstest seg_create

clean_all clobber_all:: clean_all_lib

clobber clobber_all:: clean_dep

clean_lib::
	-rm -rf $(M3ARCH) 

clean_all_lib::
	-rm -rf $(M3ARCHES)

clean_dep::
	-rm -f make.dep make.dep.bak

installfiles:
	$(INSTALL) librds.a $(LIBDIR)
	-chmod 0644 $(LIBDIR)/librds.a
	$(INSTALL) rdsinit $(BINDIR)
	-chmod 0755 $(BINDIR)/rdsinit
	$(INSTALL) $(RDSDIR)/rds.h $(INCLDIR)
	-chmod 0644 $(INCLDIR)/rds.h

install:
	gmake reinstall

reinstall:
	gmake all
	gmake installfiles

include ../make.conf
include make.dep
