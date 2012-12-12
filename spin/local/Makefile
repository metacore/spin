#
# Makefile for the SPIN tools
# These are programs that run on the local host.
#
# bin/ lib/ and man/ will be created to contain the programs built here.
#
# HISTORY
# 11-Feb-97  Przemek Pardyak (pardy) at the University of Washington
#	Better clean-up and multiple platforms.
#
# 25-Sep-96  Marc Fiuczynski (mef) at the University of Washington
#	Took out dependency information for sieg to enable local builds
#	without requiring a local copy of m3tk.
#
# 14-Aug-96  Frederick Gray (fgray) at the University of Washington
#	Makefile changes to support Intel x86 port.
#
# 18-Jul-96  Wilson Hsieh (whsieh) at the University of Washington
#	added m3core and libm3
#
# 11-Jul-96  becker at the University of Washington
#	restored ability to just check out only sieg and build it locally.
#
# 27-Jun-96  Wilson Hsieh (whsieh) at the University of Washington
#	added m3tk
#
# 29-May-96  Charles Garrett (garrett) at the University of Washington
#	More profiling stuff. InsertMcount.perl actually massages
#	 assembly language files to permit profiling of sal code that is
#	 compiled with the DEC C compiler but not linked with their linker.
#
# 05-May-96  Charles Garrett (garrett) at the University of Washington
#	Added profiling support programs. Run these on a Unix machine to
#	 extract profile info from a crash machine.
#

THISTREE:=$(shell cd ..; pwd)
include $(THISTREE)/make.conf	# sets FULLTREE and DECSRCM3BUILD

# we use the wildcard so only the subtrees that exist here are built
ifeq ($(LOCALTARGET), ALPHA_OSF)
PROGRAMS= $(wildcard script m3support m3 m3cc m3gdbttd m3rpcgen \
	m3core libm3 m3tk sieg spinprof)
endif

#
# FIXME: As the side of the SPIN compiler being unable to generate correct
#	 code for LINUX, we cannot build m3core and libm3 in the SPIN
#	 tree because they use EPHEMERAL.
#
ifeq ($(LOCALTARGET), LINUXELF)
PROGRAMS= $(wildcard script m3support m3 m3cc m3gdbttd m3rpcgen \
	m3tk sieg spinprof)
endif

all: $(PROGRAMS)

m3support $(LOCALTARGET)/lib/m3:
	# Modula-3 build tools: m3build m3ship
	$(MAKE) -C m3support

m3: $(LOCALTARGET)/lib/m3
	# Modula-3 compiler
	$(MAKE) -C $@

m3cc: $(LOCALTARGET)/lib/m3
	# Modula-3 C compiler backend
	$(MAKE) -C $@	

m3gdbttd:
	# Modula-3 GNU debugger for Topaz TeleDebug protocol
	$(MAKE) -C $@

m3rpcgen:
	# Modula-3 remote procedure call generator
	$(MAKE) -C $@

sieg: #$(LOCALTARGET)/lib/m3 m3tk
	# SPIN Interface to Extension Generator
	$(MAKE) -C $@	

m3tk:
	# SPIN version of m3tk
	$(MAKE) -C $@

m3core:
	# SPIN version of m3core for user-level programs
	# 
	# m3core cannot be build on LINUXELF
	#
	$(MAKE) -C $@

libm3:
	# SPIN version of libm3 for user-level programs
	#
	# this libm3 does not work on LINUXELF
	#
	$(MAKE) -C $@

script:
	# perl and shell scripts used to build the system
	$(MAKE) -C $@

encap:
	# Encapsulates binaries in a form for extraction inside the kernel
	cp encap.$(LOCALTARGET) $(LOCALTARGET)/bin/encap

mkextender:
	# Encapsulates binaries in a form for extraction inside the kernel
	cp mkextender.$(LOCALTARGET) $(LOCALTARGET)/bin/mkextender

profile.perl:
	# Processes output from m3gdbttd "profile" command
	cp profile.perl $(LOCALTARGET)/bin

FlatProfile.perl:
	# A script which runs m3gdbttd and profile.perl for you.
	# Only gives you PC sample output.
	cp FlatProfile.perl $(LOCALTARGET)/bin

GraphProfile.perl:
	# A script which runs m3gdbttd and spinprof for you.
	# Returns copious call graph information.
	cp GraphProfile.perl $(LOCALTARGET)/bin

InsertMcount.perl:
	# Runs /bin/cc and inserts profiling calls.
	# Returns copious call graph information.
	cp $@ bin

spinprof:	
	# SPIN's version of gprof. 
	$(MAKE) -C spinprof
	mv spinprof/spinprof $(LOCALTARGET)/bin

osfbenchmarks:	
	# I wish.

$(PROGRAMS): $(LOCALTARGET)/bin
$(LOCALTARGET)/bin:
	-mkdir -p $(LOCALTARGET)/bin

clean clobber clean_all clobber_all::
	@for i in $(PROGRAMS); do \
		echo -n "$$i:	" ;\
		$(MAKE) --no-print-directory -C $$i $@; \
		done

clobber::
	-rm -rf $(LOCALTARGET)

clobber_all::
	-rm -rf $(LOCALTARGETS)

# this line allows dir names to be used as Makefile targets
.PHONY: $(PROGRAMS)
