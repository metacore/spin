#
# HISTORY
# 28-Dec-97  Przemek Pardyak (pardy) at the University of Washington
#	Improved clean-up.
#

THISTREE:=$(shell cd ../..; pwd)
include $(THISTREE)/make.conf

# TRANSTARGET is either osf or spin. 
#export TRANSTARGET=osf
export TRANSTARGET=spin
export M3ARCH

# Write the target name to ".target"
foo := $(shell echo TRANSTARGET=$(TRANSTARGET) > .target)

SUBDIRS = $(wildcard lib rvmbench malloc oo7 microbench)

ifeq ($(M3ARCH),IX86_SPIN)
bar:
	@echo No x86 transaction yet.
	@true
install: bar
else

ifeq ($(TRANSTARGET),osf) 
# OSF emulation.
all ::
	$(MAKE) -Csrc/osf setup
	for dir in $(SUBDIRS); do $(MAKE) -C$$dir; done
	(cd src; m3build.osf)
install:
	@echo trans is configured for osf now. Edit Makefile, and try again.
else
  # real SPIN version.
all:: m3sources
	$(MAKE) -Clib
	$(MAKE) -Cmalloc
	if [ -e rvmbench/Makefile ]; then $(MAKE) -Crvmbench; fi
	if [ -e oo7/Makefile ]; then $(MAKE) -Coo7; fi
	if [ -e microbench/Makefile ]; then $(MAKE) -Cmicrobench; fi
install: all
include ../make.extension
endif
endif


clean clean_all clobber clobber_all:: clean_data clean_docs

clean:: clean_only_c
clean_all:: clean_only_c_all
clobber:: clobber_only_c
clobber_all:: clobber_only_c_all

clean_data:
	rm -rf `find . -name *.washington.edu -print`

clean_only_c:
	for dir in $(SUBDIRS); do $(MAKE) -C$$dir clean; done

clean_only_c_all:
	for dir in $(SUBDIRS); do $(MAKE) -C$$dir clean_all; done

clobber_only_c:
	for dir in $(SUBDIRS); do $(MAKE) -C$$dir clobber; done
	rm -f .target

clobber_only_c_all:
	for dir in $(SUBDIRS); do $(MAKE) -C$$dir clobber_all; done
	rm -f .target


depend::
	for dir in $(SUBDIRS); do $(MAKE) -C$$dir depend; done

DOCS = LockMode StorageRep Storage StorageProxy StorageLocal \
	RawIO TransMode Transaction WAL LockMan TransGroup index oo7 rvmbench \
	Buffer ActiveTrans ActiveTransRep BufferKey BufferPool BufferPurge \
	BufferRep CheckPoint DiffExtern FastIntRefTbl HostID HostSet \
	LockRecord SID ShadowPager StorageProtected StorageRemote TID \
	TransCommands TransDaemon TransError TransFlags TransLocal \
	TransGroupPrivate TransGroupRep TransOS TransPrivate TransProxy \
	TransRPC TransRemote TransRep TransService TransServiceRep \
	TransSyscall TransT TransUtils WALPrivate Pin\

clean_docs:
	for doc in $(DOCS); do rm -f doc/$$doc.html; done

