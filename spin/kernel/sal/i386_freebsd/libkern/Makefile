#	@(#)Makefile	7.9 (Berkeley) 6/1/93
# $Id: Makefile,v 1.3 1997/06/03 16:35:39 becker Exp $

all: libkern.a

THISTREE:=$(shell cd ../../../..; pwd)
include $(THISTREE)/make.conf	# sets FULLTREE and DECSRCM3BUILD

CC=$(TARGETCC)

CFLAGS+= -nostdinc -I../include -I. -I.. -DKERNEL
SRCS=	adddi3.c anddi3.c ashldi3.c ashrdi3.c bcmp.c cmpdi2.c divdi3.c \
	ffs.c iordi3.c inet_ntoa.c locc.c lshldi3.c lshrdi3.c \
	moddi3.c muldi3.c negdi2.c notdi2.c qdivrem.c random.c rindex.c \
	scanc.c skpc.c strcat.c strcmp.c strcpy.c strlen.c strncmp.c \
	strncpy.c subdi3.c ucmpdi2.c udivdi3.c umoddi3.c xordi3.c \
	memcmp.c memset.c index.c
OBJS=$(patsubst %.c,%.o,$(SRCS))

libkern.a: $(OBJS)
	$(TARGETAR) r $@ $(OBJS) 

install:

clean:
	rm -f $(OBJS) libkern.a

clobber: clean
