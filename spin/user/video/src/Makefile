#
# HISTORY
CC=cc
OFILES = video.o fli.o
LDFLAGS= -non_shared
ALL=

SALDIR	= ../../../sal
includedirs = . config include include/alpha arch/alpha arch 
INCLUDEDIRS = $(patsubst %, $(SALDIR)/%, $(includedirs))
INCLUDEDIRS += ../../dev
INCLUDES = -I  $(patsubst %, -I%, $(INCLUDEDIRS))
DEFINES	= -DKERNEL=1 -D_KERNEL=1 -DMACH_KERNEL=1 -DBINARY -DSPIN
CFLAGS= -O2 $(DEFINES) $(INCLUDES)

ALL = video.o fli.o 

all: ${ALL}

video.o: video.c
	$(CC) $(CFLAGS) -c $< -o $@

fli.o: fli.c
	$(CC) $(CFLAGS) -c $< -o $@

clean::
	rm -f *.o *.so *~ ${ALL}

clobber:	clean
	rm -f rpc.c
	rm -rf ../ALPHA_OSF

cthreads.o: cthreads.s
	cc -c cthreads.s
