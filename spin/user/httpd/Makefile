THISTREE:=$(shell cd ../..; pwd)
include $(THISTREE)/make.conf
all: extension #$(M3ARCH)/httpd_domain
extension: m3sources	# make.extension has the rule to build m3sources
install: all install_domain
include ../make.extension
