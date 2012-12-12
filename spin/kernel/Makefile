#
# Makefile for the SPIN kernel
#
# HISTORY
# 28-Dec-97  Przemek Pardyak (pardy) at the University of Washington
#	Removed creation and descruction of the bin directory because
#	it was not used.
#
# 11-Nov-97  David Becker (becker) at the University of Washington
#	Restored the "all" target.  Removed lots of commented out variables.
#
# 20-Mar-97  owa at the University of Washington
#	'stcp' is one of static extensions on PC as well as alpha.
#
# 12-Feb-97  owa at the University of Washington
#	added stcp (simple tcp) as a static on ALPHA_SPIN.
#
# 22-Nov-96  becker at the University of Washington
#	Switched to just the init extension as a static
#
# 25-Sep-96  Marc Fiuczynski (mef) at the University of Washington
#	Added tmpfs as a encap-dynamic domain.
#
# 14-Aug-96  Frederick Gray (fgray) at the University of Washington
#	Makefile changes to support Intel x86 port.
#
# 28-May-96  David Becker (becker) at the University of Washington
#	removed 'kernel' as an alias for sal
#
# 27-May-96  Stefan Savage (savage) at the University of Washington
#	Added vmcore and space to the set of static extensions
#
# 05-May-96  Charles Garrett (garrett) at the University of Washington
#	Changed to handle both profiled and unprofiled versions.
#

THISTREE:=$(shell cd ..; pwd)
include $(THISTREE)/make.conf	# sets FULLTREE and PATH to m3build

STATIC_EXTENSIONS=stcp init
STATIC_C_EXTENSIONS=  # none at this time (eg. dev/cam/osf.cam urt/net/osf.tcp)


LIBS=$(wildcard m3core_sa libm3_sa spincore start)
     # we use the wildcard so only the subtrees that exist here are built

all:  $(LIBS) sal

ifeq ($(M3ARCH),ALPHA_SPIN)
ifdef PROFILE
    SALDIR:=sal/alpha_osf/SPIN_PROF
else
    SALDIR:=sal/alpha_osf/SPIN
endif
else
ifdef PROFILE
    SALDIR:=sal/i386_freebsd/compile/SPIN_PROF
else
    SALDIR:=sal/i386_freebsd/compile/SPIN
endif
endif


sal:
	# Stand Alone Layer and kernel images
	# we use the wildcard so only the subtrees that exist here are built
	$(MAKE) -C sal

$(SALDIR)/Makefile:
	$(MAKE) -C sal configure

start: static_list
	# Starts core SPIN services and loads initial extensions
	$(MAKE) -C $@

spincore: $(SALDIR)/Makefile
	# The core SPIN services
	$(MAKE) -C $@

m3core_sa:	
	# Modula-3 runtime for SPIN
	$(MAKE) -C $@

libm3_sa:
	# SPINized subset of the standard m3 library
	$(MAKE) -C $@

# describe the static extensions in a form for m3build
STATIC_FILE=start/m3statics.$(M3ARCH)
static_list: build_statics 
	@echo "STATIC_EXTENSIONS = [" >  $(STATIC_FILE)
	@comma=no; for i in $(STATIC_C_EXTENSIONS) $(STATIC_EXTENSIONS); do \
		ext=`basename $$i`; \
		if [ $$comma = yes ] ; then echo "," >>  $(STATIC_FILE); fi; \
		comma=yes; \
		echo "\"$$ext\"" >>  $(STATIC_FILE); \
		done;
	@echo " ]" >>  $(STATIC_FILE)
	@echo "STATIC_DATA = {" >>  $(STATIC_FILE)
	@comma=no; for i in $(STATIC_C_EXTENSIONS) $(STATIC_EXTENSIONS); do \
		ext=`basename $$i`; \
		if [ $$comma = yes ] ; then echo "," >>  $(STATIC_FILE); fi; \
		comma=yes; \
		echo "\"$$ext\":[\"$$ext\", \"user/$$i\"]" >>  $(STATIC_FILE); \
		done;
	@echo " }" >>  $(STATIC_FILE)
	@echo $(STATIC_C_EXTENSIONS) > start/m3statics2
	@echo $(STATIC_EXTENSIONS) >> start/m3statics2

LOCALSTATICS:=$(wildcard $(patsubst %,../user/%, $(STATIC_EXTENSIONS) ))
build_statics:$(LOCALSTATICS)
$(LOCALSTATICS):
	$(MAKE) --no-print-directory -C $@ extension

clean clobber clean_all clobber_all::
	# Remove the object files
	for i in $(LIBS) sal; do $(MAKE) -C $$i $@; done
	rm -f $(STATIC_FILE)

# this line allows dir names to be used as Makefile targets
.PHONY: $(LIBS) sal $(LOCALSTATICS)
