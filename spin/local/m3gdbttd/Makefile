#
# Makefile for the SPIN version of the Modula-3 gdb debugger m3gdbttd
# This program runs on the local host.
#

THISTREE:=$(shell cd ../..; pwd)
include $(THISTREE)/make.conf	# sets FULLTREE and DECSRCM3BUILD

all: $(LOCALTARGET)/gdb/m3gdbttd

$(LOCALTARGET)/gdb/m3gdbttd: $(LOCALTARGET)/Makefile
	$(MAKE) -C $(LOCALTARGET)
	cp $(LOCALTARGET)/gdb/gdb $(THISTREE)/local/$(LOCALTARGET)/bin/m3gdbttd

ifeq ($(LOCALTARGET), ALPHA_OSF)
GDBHOST=alpha-dec-osf3.2
endif

ifeq ($(LOCALTARGET), LINUXELF)
GDBHOST=i486--linuxelf
endif

ifeq ($(M3ARCH), ALPHA_SPIN)
GDBTARGET=alpha-dec-osf3.2
endif

ifeq ($(M3ARCH), IX86_SPIN)
GDBTARGET=freebsd386
endif

$(LOCALTARGET)/Makefile: $(LOCALTARGET)
	# Run the configure script for gdb
	cd $(LOCALTARGET) && ../configure --host=$(GDBHOST) --target=$(GDBTARGET)

$(LOCALTARGET):
	mkdir -p $(LOCALTARGET)

clean clobber::
	-rm -rf $(M3ARCH) $(LOCALTARGET)
clean_all clobber_all::
	-rm -rf $(M3ARCHES) $(LOCALTARGETS)
