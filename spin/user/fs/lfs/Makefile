#
#
# Makefile for lfs stuff
#
#
# HISTORY
# 28-Dec-97  Przemek Pardyak (pardy) at the University of Washington
#	Improved clean-up.
#
# 6-11-96 Merged buffer domain with segment domain
# 6-10-96 Made fsutils dir for lfs shell commands
# 5-14-96 Added support for subdiretories
#

THISTREE:=$(shell cd ../../..; pwd)
include $(THISTREE)/make.conf

LDFLAGS=-non_shared -L$(THISTREE)/user/lib -L$(FULLTREE)/user/lib
LD=cc

PROGRAMS=$(wildcard segment lfscore cleaner fsutils)

all: $(PROGRAMS) extension #lfs_domain

$(PROGRAMS):
	$(MAKE) --no-print-directory -C $@ install

extension: m3sources	# make.extension has the rule to build m3sources

install: all extension install_domain

clean clobber clean_all clobber_all::
	@for i in $(PROGRAMS); do \
		echo -n "$$i:	" ;\
		$(MAKE) --no-print-directory -C $$i $@; \
		done

clobber_all clobber:: clean_this

clean_this::
	-rm -f lfs_domain *.o

include ../../make.extension

lfscore: segment
extension: segment lfscore
fsutils : extension
.PHONY: $(PROGRAMS)
