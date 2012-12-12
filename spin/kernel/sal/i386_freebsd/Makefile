#
# Makefile for the SPIN system
#
# HISTORY
# 28-Dec-97  Przemek Pardyak (pardy) at the University of Washington
#	Improved clean-up.
#
# 13-Jun-97  David Becker at the University of Washington
#	updated clean and clobber targets
#
# 02-Jul-96  Frederick Gray (fgray) at the University of Washington
#	Created.
#

THISTREE:=$(shell cd ../../..; pwd)
include $(THISTREE)/make.conf

all: compile/$(MAIN_TARGET)

compile/$(MAIN_TARGET): compile/$(MAIN_TARGET)/Makefile
	ln -fs ../dependencies compile/$(MAIN_TARGET)/dependencies
	$(MAKE) -C compile/$(MAIN_TARGET) 

salboot: compile/SALBOOT
compile/SALBOOT: compile/SALBOOT/Makefile
	$(MAKE) -C $@

deps:
	$(MAKE) -C compile/$(MAIN_TARGET) $@

machine:
	-rm -f $@
	ln -s i386/include $@

configure: machine compile/$(MAIN_TARGET)/Makefile
	rm -f $(DRIVER_TARGET)
	ln -s compile/$(MAIN_TARGET) $(DRIVER_TARGET)
	rm -f compile/$(MAIN_TARGET)/dependencies
	(cd compile/$(MAIN_TARGET) && ln -s ../dependencies dependencies)

compile/$(MAIN_TARGET)/Makefile: conf/files conf/defines i386/conf/files.i386 i386/conf/devices.i386 i386/conf/Makefile.i386 i386/conf/$(MAIN_TARGET)
	(cd i386/conf && $(TARGETCONFIG) -n $(MAIN_TARGET))

compile/SALBOOT/Makefile: i386/bootconf/files.i386 i386/bootconf/devices.i386 i386/bootconf/Makefile.i386 i386/bootconf/SALBOOT
	(cd i386/bootconf && bsd-config -n SALBOOT)

clean::
	@if [ -d compile/$(MAIN_TARGET) ]; then $(MAKE) -C compile/$(MAIN_TARGET) clean; fi
	$(MAKE) -C libkern clean
	$(MAKE) -C i386/boot/salboot/lmm clean

clean_all::
	@if [ -d compile/SPIN ]; then $(MAKE) -C compile/SPIN clean; fi
	@if [ -d compile/SPIN_PROF ]; then $(MAKE) -C compile/SPIN_PROF clean; fi
	@if [ -d compile/SALBOOT ]; then $(MAKE) -C compile/SALBOOT clean; fi
	$(MAKE) -C libkern clean
	$(MAKE) -C i386/boot/salboot/lmm clean

clobber::
	-rm -rf compile/$(MAIN_TARGET)
	-rm -f $(DRIVER_TARGET)
	-rm -f machine
	$(MAKE) -C libkern clean
	$(MAKE) -C i386/boot/salboot/lmm clean

clobber_all::
	-rm -rf compile/SPIN* compile/DRIVER* compile/SALBOOT
	-rm -f DRIVER_*
	-rm -f machine
	$(MAKE) -C libkern clean
	$(MAKE) -C i386/boot/salboot/lmm clean

# Normal driver files can be built using just the Makefile from config.
$(DRIVER_TARGET):
	-ln -fs compile/$(MAIN_TARGET) $(DRIVER_TARGET)
	make -C $(DRIVER_TARGET) fbsd.tcp

.PHONY: compile/SPIN compile/SPIN_PROF $(DRIVER_TARGET)

