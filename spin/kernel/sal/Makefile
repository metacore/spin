
THISTREE:=$(shell cd ../..; pwd)
include $(THISTREE)/make.conf	# sets FULLTREE and PATH to m3build

ifeq ($(M3ARCH),ALPHA_SPIN)
    SALDIR:=alpha_osf
else
    SALDIR:=i386_freebsd
endif


all configure unimport DRIVER DRIVER_PROF DRIVER_PROF_REF DRIVER_REF DRIVER_WB DRIVER_PROF_WB DRIVER_HACK clean clobber deps salboot:
	$(MAKE) -C $(SALDIR) $@

ARCHES=alpha_osf i386_freebsd
clean_all::
	for i in $(ARCHES); do $(MAKE) -C $$i clean_all; done
clobber_all::
	for i in $(ARCHES); do $(MAKE) -C $$i clobber_all; done
