#
# HISTORY
# 28-Dec-97  Przemek Pardyak (pardy) at the University of Washington
#	Improved clean-up.
#

DIR := $(shell cd ..; basename `pwd`)

ifeq ($(DIR),rvm)
  PLATFORM=rvm
else
  ifeq ($(DIR),trans)
    PLATFORM=spin
  else
    PLATFORM=ostore
  endif
endif

FOO := $(shell echo building for $(PLATFORM) >/dev/tty)
all::
	-if [ ! -d rvm ]; then mkdir rvm ALPHA_SPIN IX86_SPIN ostore osf; fi

include make.$(PLATFORM)

clean clobber:: clean_lib

clean clobber clean_all clobber_all:: clean_this

clean_this::
	rm -rf spin osf

clean_all clobber_all:: clean_all_lib

clobber clobber_all:: clean_dep

clean_lib::
	-rm -rf $(M3ARCH) rvm

clean_all_lib::
	-rm -rf $(M3ARCHES) rvm

clean_dep::
	-rm -f make.dep make.dep.bak

