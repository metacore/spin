#
# HISTORY
# 28-Dec-97  Przemek Pardyak (pardy) at the University of Washington
#	Improved clean-up.
#

THISTREE:=$(shell cd ../../..; pwd)
include $(THISTREE)/make.conf

OBJS = $(M3ARCH)/spy.o 
CFLAGS = -g

all: preparation $(M3ARCH)/libbench.a

preparation:
	-mkdir $(M3ARCH)

$(M3ARCH)/%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $^

$(M3ARCH)/libbench.a : $(OBJS) 
	rm -f $@
	ar qc $@ $(OBJS)

clean::
	rm -rf *~ *.o $(M3ARCH)

clean_all::
	rm -rf *~ *.o $(M3ARCHES)

clobber:: clean

clobber_all:: clean_all

clobber clobber_all::
	rm -f libbench.a
