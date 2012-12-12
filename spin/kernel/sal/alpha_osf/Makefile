#
# Makefile for the SPIN system
#
# HISTORY
# 28-Dec-97  Przemek Pardyak (pardy) at the University of Washington
#	Improved clean-up.
#
# 17-Nov-97  David Becker at the University of Washington
#	Use new make.conf variables
#
# 16-Jul-97  Tian Fung Lim (tian) at the University of Washington
#	added writebarrier platform
#
# 13-Jun-97  David Becker at the University of Washington
#	updated clean and clobber targets
# 28-Feb-97  Wilson Hsieh (whsieh) at the University of Washington
#	fix HACK
#
# 18-Feb-97  Wilson Hsieh (whsieh) at the University of Washington
#	fix clean, clobber, clean_all, clobber_all
# 
# 11-Feb-97  Przemek Pardyak (pardy) at the University of Washington
#	Added targets for ref-counting and hack.
#
# 07-Jan-97  becker at the University of Washington
#	Stop building osf.tt osf.tal osf.fore in DRIVER
#
# 23-Jul-96  becker at the University of Washington
#	Removed osf32 check, since that was afs dependent and the osf2.0
#	machines are gone.
#
# 30-May-96  David Dion (ddion) at the University of Washington
#	Added arch/alpha/pcb.h to spincorefiles for PmapActivate.c
#
# 29-May-96  Charles Garrett (garrett) at the University of Washington
#	Can optionally build profiled driver target, DRIVER_PROF.
#
# 21-May-96  David Dion (ddion) at the University of Washington
#	added arch/alpha/pmap.h to spincorefiles (needed by MemoryForSAL.c)
#
# 05-May-96  Charles Garrett (garrett) at the University of Washington
#	Handle both profiled and unprofiled versions of sal. The profiled
#	version is called SPIN_PROF.
#
# 21-Sep-95  Stefan Savage (savage) at the University of Washington
#	Created (ugh) to prepare the system for config
#
#

THISTREE:=$(shell cd ../../..; pwd)
include ../../../make.conf	# sets DIGITALUNIX and PATH to find 'config' 

all: $(MAIN_TARGET)

#
# These links needed by everyone to correctly build on an alpha
#
machinelinks = mach/machine arch/machine 

${machinelinks}:
	-rm -f $@
	ln -s alpha $@

#
# These files are needed by the config program to correctly create all
# the happy little .h files
#
files =		conf/copyright conf/filesystems conf/newvers.sh \
		conf/template.mk conf/version.banner conf/version.baselevel \
		conf/version.build conf/version.major conf/version.minor \
		conf/version.patch conf/version.type conf/version.variant \
		conf/alpha/devices conf/alpha/template.mk \
		io/common/conf.c data/tc_option_data.c io/dec/tc/data_format \
		data/pci_option_data.c io/dec/pci/data_format \
		data/isa_option_data.c io/dec/isa/data_format 

FILES = $(patsubst %, ../../sal/alpha_osf/%, $(files))

../../sal/alpha_osf/%: $(DIGITALUNIX)/src/kernel/%
	@mkdir -p $(dir $@); 
	@if [ -f  $@ ]; then exit 0; fi; \
		if [ -f  $@.diff ]; \
		then patch -o $@ $< $@.diff ;\
		else cp $< $@; \
		fi; \
		echo cp $< $@;
../sal/conf/template.mk: ../sal/conf/template.mk.diff

import conf/template.mk: ${FILES} ${machinelinks}

unimport:  
	-rm -f ${FILES} $(spincorefiles)
	@if [ -d $(MAIN_TARGET) ]; then $(MAKE) -C $(MAIN_TARGET) unimport; fi
	@if [ -d $(MAIN_TARGET) ]; then rm $(MAIN_TARGET)/Makefile; fi

# we go to $(DRIVER_TARGET) to build the deps since it has a more complete
# set of config generated .h files.
deps:	$(DRIVER_TARGET)/Makefile
	$(MAKE) -C $(DRIVER_TARGET) import deps

# Normal driver files can be built using just the Makefile from config.
$(DRIVER_TARGET) spindle_support: $(DRIVER_TARGET)/Makefile 
	gmake -C $(DRIVER_TARGET) osf.tcp osf.cam # osf.tt osf.tal osf.fore

$(DRIVER_TARGET)/Makefile: conf/files conf/$(DRIVER_TARGET) conf/template.mk
	cd conf && env PATH=$(OSF_TOOLBIN) config -s $(DRIVER_TARGET)
	-rm -f $(DRIVER_TARGET)/dependencies
	-rm -f $(DRIVER_TARGET)/assym.s
	-ln -s ../conf/dependencies $(DRIVER_TARGET)
	-ln -s ../conf/assym.s $(DRIVER_TARGET)


spincorefiles=arch/alpha/asm.h arch/alpha/regdef.h arch/alpha/reg.h \
	arch/alpha/pal.h vm/pmap.h arch/alpha/pmap.h arch/alpha/pcb.h \
	sys/malloc.h arch/alpha/cpu.h

configure: import $(MAIN_TARGET)/Makefile
	gmake -C $(MAIN_TARGET) $(patsubst %, ../%, $(spincorefiles))
	
#
# This recipe must be in sync with configure recipe.
#
$(MAIN_TARGET)/Makefile: conf/files conf/alpha/files conf/$(MAIN_TARGET) conf/template.mk
	cd conf && env PATH=$(OSF_TOOLBIN) config -s $(MAIN_TARGET)
	-rm -f $(MAIN_TARGET)/dependencies
	-rm -f $(MAIN_TARGET)/assym.s
	-ln -s ../conf/dependencies $(MAIN_TARGET)
	-ln -s ../conf/assym.s $(MAIN_TARGET)
	-@echo $(MAIN_TARGET) >$(MAIN_TARGET)/vers.config

$(MAIN_TARGET): $(MAIN_TARGET)/Makefile
	$(MAKE) -C $(MAIN_TARGET)

clean::
	@if [ -d $(MAIN_TARGET) ]; then $(MAKE) -C $(MAIN_TARGET) clean; fi

clobber:: unimport
	-rm -f $(machinelinks)
	-rm -rf $(MAIN_TARGET) $(DRIVER_TARGET)

clean_all::
	@if [ -d SPIN ]; then $(MAKE) -C SPIN clean; fi
	@if [ -d SPIN_PROF ]; then $(MAKE) -C SPIN_PROF clean; fi
	@if [ -d SPIN_HACK ]; then $(MAKE) -C SPIN_HACK clean; fi
	@if [ -d SPIN_REF ]; then $(MAKE) -C SPIN_REF clean; fi
	@if [ -d SPIN_WB ]; then $(MAKE) -C SPIN_WB clean; fi
	@if [ -d SPIN_PROF_REF ]; then $(MAKE) -C SPIN_PROF_REF clean; fi

clobber_all:: unimport
	-rm -rf SPIN* DRIVER*

.PHONY: SPIN SPIN_PROF SPIN_PROF_REF SPIN_REF SPIN_WB SPIN_HACK DRIVER DRIVER_PROF DRIVER_PROF_REF DRIVER_REF DRIVER_WB DRIVER_HACK
