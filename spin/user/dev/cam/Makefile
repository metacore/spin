THISTREE:=$(shell cd ../../..; pwd)
include $(THISTREE)/make.conf

ifeq ($(M3ARCH),ALPHA_SPIN)
all: extension #$(M3ARCH)/cam_domain
install: all #install_domain
include ../../make.extension
else
install:
all:
endif
