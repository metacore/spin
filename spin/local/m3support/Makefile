#
# Makefile for the SPIN version of the Modula-3 build tools
# These programs run on the local host.
#
# HISTORY
# 24-Aug-97  Przemek Pardyak (pardy) at the University of Washington
#	Enable building user-level and kernel targets in local.
#
# 11-Feb-97  Przemek Pardyak (pardy) at the University of Washington
#	Better clean-up and multiple platforms.
#

THISTREE:=$(shell cd ../..; pwd)
include $(THISTREE)/make.conf	# sets FULLTREE and DECSRCM3BUILD

INSTALL_ROOT=$(THISTREE)/local

m3support:
	LOCALTARGET=$(LOCALTARGET) MAKE=$(MAKE) ./m3boot $(INSTALL_ROOT)
	LOCALTARGET=$(LOCALTARGET) ./m3ship $(INSTALL_ROOT)


clean clobber::
	-rm -rf m3build/$(LOCALTARGET) quake/$(LOCALTARGET)
	-rm -rf quake/src/parser.c
	-rm -rf $(THISTREE)/local/$(BUILD_DIR)/bin/m3build
	-rm -rf $(THISTREE)/local/$(BUILD_DIR)/bin/quake
	-rm -rf $(THISTREE)/local/$(BUILD_DIR)/lib/pkg/m3build

clean_all clobber_all::
	@for i in $(LOCALTARGETS); do \
		echo -n "cleaning target" $$i "\n" ; \
		rm -rf m3build/$$i quake/$$i ;  \
		rm -rf quake/src/parser.c ; \
		rm -rf $(THISTREE)/local/$$i/bin/m3build ; \
		rm -rf $(THISTREE)/local/$$i/bin/quake ; \
		rm -rf $(THISTREE)/local/$$i/bin/m3ship ; \
		rm -rf $(THISTREE)/local/$$i/bin/m3where ; \
		rm -rf $(THISTREE)/local/$$i/man/man1/m3build.1 ; \
		rm -rf $(THISTREE)/local/$$i/man/man1/quake.1 ; \
		rm -rf $(THISTREE)/local/$$i/man/man1/m3ship.1 ; \
		rm -rf $(THISTREE)/local/$$i/man/man1/m3where.1 ; \
		rm -rf $(THISTREE)/local/$$i/lib/pkg/m3build ; \
	done

