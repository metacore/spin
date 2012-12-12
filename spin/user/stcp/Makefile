# 28-Dec-97  Przemek Pardyak (pardy) at the University of Washington
#	Got rid of unnecessary handing of target names.
#

THISTREE:=$(shell cd ../..; pwd)
include $(THISTREE)/make.conf

extension: $(DRIVER_TARGET)
$(DRIVER_TARGET):
	# grab low level code from unix
	if [ -d $(THISTREE)/kernel/sal ] ; then \
	    $(MAKE) -C $(THISTREE)/kernel/sal $(DRIVER_TARGET); fi;

extension: $(DRIVER_TARGET) m3sources	# make.extension has the rule to build m3sources
install: extension


include ../make.extension
