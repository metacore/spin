#
# Make support for M3
#
# HISTORY
# 28-Dec-97  Przemek Pardyak (pardy) at the University of Washington
#	Improved clean-up.
#
# 11-Nov-97  David Becker (becker) at the University of Washington
#	Changed M3BUILD definition to search explicity rather than
#	through a PATH variable set somewhere in the makefiles.
#	Also changed M3BUILD to not include the m3build arguments.
#	That means now when M3BUILD is invoked, the M3FLAGS var must be
#	dereferenced as well.
#	Renamed SYSBIN to BSD_PREFIX.
#
# 11-Feb-97  Przemek Pardyak (pardy) at the University of Washington
#	Better clean-up and multiple platforms.
#
# 29-Jan-97  Wilson Hsieh (whsieh) at the University of Washington
#	fix clean,clobber to reference BUILD_DIR instead of M3ARCH
#
# 23-Jul-96  becker at the University of Washington
#	Allow M3BUILD to be defined by including makefile.  Prompted by
#	needs of local/m3.  See local/m3/make.compiler.
#	Added M3extrasrc variable because local/m3cc/ has sources under 
#	src/ and gcc/
#
# 05-May-96  Charles Garrett (garrett) at the University of Washington
#	M3ARCH is now a variable which can be either ALPHA_SPIN or 
#       ALPHA_SPIN_PROF.
#
# With M3, make does know all the possible dependencies.  Also, the M3
# compiler is called once for all the files in a particular library.
#
# What make can determine is if the build can be skipped.  This is the
# case if none of the files under foo/src/ are newer than ALPHA_SPIN/libfoo.a
# _and_ none of the libs imported by foo are newer than ALPHA_SPIN/libfoo.a
# Make can judge these conditions much faster than the M3 compiler.
#
# To use these rules, put a Makefile in foo/ that with a target that
# depends on m3sources and include this file.  For instance:
#
#	all: m3sources
#	M3ARCH=ALPHA_SPIN
#	M3FLAGS=-b $(M3ARCH) $(TREEFLAGS)
#	include $(THISTREE)/make.m3
#
#  m3build is searched for in the PATH variable. PATH is set by make.conf
#

ifndef M3BUILD
# Most Makefile assume make.m3 will set M3BUILD to the spin version of m3build.
# This happens here.    Look first in THISTREE, then in FULLTREE.
M3BUILD:=$(firstword $(wildcard $(patsubst %, %/local/$(LOCALTARGET)/bin/m3build, $(THISTREE) $(FULLTREE))))
endif

ifeq ($(LOCALTARGET),LINUXELF)
M3FLAGS+=-DBSD_PREFIX=$(BSD_PREFIX)
endif

M3TARGET=$(BUILD_DIR)/build.stamp
m3sources: $(M3TARGET)

M3DEPS=m3.deps.$(M3ARCH)
-include $(M3DEPS)

$(M3DEPS):
	@rm -f $(M3TARGET)
	@touch $@


#
# this rule first compiles the m3sources and the makes a new m3.deps files
#
# the sed command was changed to let M3ARCH be evaluated after m3.deps is
# included, rather than when it is written.
# 
$(M3TARGET):  # these deps are in the m3.deps file
	#
	# $(shell basename `pwd`) m3 sources
	#
	$(M3BUILD) $(M3FLAGS)
	@-rm -f $@
	@touch $@
	@grep '^@/.*src.*' $(BUILD_DIR)/.M3IMPTAB \
		| sed -e 's/^@//' \
			-e 's,/src.*,/$$(BUILD_DIR)/build.stamp,' \
			-e '/lib.m3.pkg/d' \
			-e 's,^,$$(BUILD_DIR)/build.stamp: ,' \
		| sort -u > $(M3DEPS)
	@find src $(M3extrasrc) -name CVS -prune \
		-o -name "*~" -prune \
		-o -name ".*" -prune \
		-o -type f -print | \
		sed 's,^.*,$$(BUILD_DIR)/build.stamp: $$(wildcard &),' >> $(M3DEPS)


# clobber attempts to return everything under spin/ to its unbuilt state
# clean removes intermediate files but not final programs or bin and lib dirs
clobber:: clean
clean:: clean_lib
clobber_all:: clean_all_lib
clean_all:: clean_all_lib

clean_lib::
	-rm -rf $(BUILD_DIR) $(M3DEPS)

clean_all_lib::
	-rm -rf $(M3ARCHES) $(ALLM3DEPS)

.PHONY: m3sources
