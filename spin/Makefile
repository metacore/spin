#
# Makefile for the SPIN world
#

# HISTORY
# 28-Dec-97  Przemek Pardyak (pardy) at the University of Washington
#	Improved clean-up.
#
# 02-Jul-96  Frederick Gray (fgray) at the University of Washington
#	Removed Alpha-centric comments.
#
# 05-May-96  Charles Garrett (garrett) at the University of Washington
#	Added the PROFILE option. Say "gmake PROFILE=TRUE <target>" to
#	build a target with call graph profiling support. Just saying
#	"gmake <target>" should build the unprofiled version just like
#	before.
#


WORLD=$(wildcard local kernel user)
world: $(WORLD)
# we use the wildcard so only the subtrees that exist here are built


######
# SPIN kernel lives in kernel/
#
kernel:
 	# kernel plus encapsulated extensions from user
	$(MAKE) -C kernel

######
# Spin programs and their kernel extensions live in user/
#
user:
	# make spin programs and extensions	
	$(MAKE) -C user

######
# The compiler, debugger, etc live in local/
# These are programs that run on the local host.
#
local:
	# tools that run on local host
	$(MAKE) -C local

clean clobber clean_all clobber_all::
	for i in $(WORLD); do $(MAKE) -C $$i $@; done

# this line allows dir names to be used as Makefile targets
.PHONY: $(WORLD)
