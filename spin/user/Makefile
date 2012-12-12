#
#
# Makefile for SPIN user applications and their kernel extensions
# These are programs that run in user space on the SPIN system
#
# bin/ and lib/ will be created to contain the programs built here.
#
#
# HISTORY
# 28-Dec-97  Przemek Pardyak (pardy) at the University of Washington
#	Improved clean-up.
#
# 16-Jul-97  Tian Fung Lim (tian) at the University of Washington
#	Added writebarrier platform, and fixed dependencies for fs
#
# 20-Apr-97  Wilson Hsieh (whsieh) at the University of Washington
#	trans depends on usyscall
#
# 03-Mar-97  Wilson Hsieh (whsieh) at the University of Washington
#	fix platforms
#
# 27-Feb-97  Emin Gun Sirer (egs) at the University of Washington
#	Added benchmark.
#
# 11-Feb-97  Przemek Pardyak (pardy) at the University of Washington
#	Added heap.
#
# 29-Jan-97  Wilson Hsieh (whsieh) at the University of Washington
#	fix clean,clobber to reference BUILD_DIR instead of M3ARCH
#
# 14-Aug-96  Frederick Gray (fgray) at the University of Washington
#	Makefile changes to support Intel x86 port.
#
# 29-Jul-96  Wilson Hsieh (whsieh) at the University of Washington
#	removed dev:vm dependency
#	lower level Makefiles have the right dependencies
#
# 23-Jun-96  Eric Christoffersen (ericc) at the University of Washington
#       Made dev depend on vm
#       Moved benchmarks from /user/shell to /user as per ddion merge
#       instructions.
#
# 29-May-96  Charles Garrett (garrett) at the University of Washington
#	Can build either profiled or unprofiled version of DRIVER code.
#
# 23-May-96  Wilson Hsieh (whsieh) at the University of Washington
#	dlib needs to depend on usyscall
#
# 21-May-96  David Dion (ddion) at the University of Washington
#	Added dependencies for extent (dlib, libdomain) and dlib (sieg)
#
# 05-May-96  Charles Garrett (garrett) at the University of Washington
#	Handle profiled and unprofiled versions of extensions.
#
# 17-Apr-96  Emin Gun Sirer (egs) at the University of Washington
#	Added extents.
#
# 17-Apr-96  Emin Gun Sirer (egs) at the University of Washington
#	Added interextension dependencies for correct compilation.
#
# 10-Apr-96  Brian Bershad (bershad) at the University of Washington
#	Added threat (again).
#
# 29-Mar-96  Brian Bershad (bershad) at the University of Washington
#	Added whist.  Added nanny, ns, and made fs after urt.
#
#

THISTREE:=$(shell cd ..; pwd)
include $(THISTREE)/make.conf   # sets FULLTREE and PATH to m3build

# we use the wildcard so only the subtrees that exist here are built
PROGRAMS= $(wildcard	\
	dev/devcore	\
	dev/console	\
	vm/vmcore	\
	fs/fscore	\
	fs/tmpfs	\
	shell/shellcore \
	vm/space	\
	hello		\
	stcp		\
	init		\
	vm		\
	dev/netdev	\
	urt		\
	dev		\
	thread		\
	thread/threadcore \
	shell		\
	net		\
	net/dns		\
	fs		\
	sieg		\
	usyscall	\
	sphinx	        \
	oncrpc		\
	sync		\
	httpd		\
	socketRW	\
	threat		\
	nanny		\
	mach		\
	regress		\
	measure		\
	rcg		\
	trans           \
	heap		\
	security        \
	fastbuffer      \
	gcbench		\
	blur		\
	benchmark)

#SMURT	autoexec	\

all: $(BUILD_DIR)/bin $(BUILD_DIR)/lib $(PROGRAMS)

$(PROGRAMS):
	$(MAKE) --no-print-directory -C $@ install

stcp dev/cam urt: $(DRIVER_TARGET)
$(DRIVER_TARGET):
	# grab low level code from unix
	if [ -d ../kernel/sal ] ; then \
	    $(MAKE) -C ../kernel/sal $(DRIVER_TARGET); fi;

$(BUILD_DIR)/bin:
	-mkdir -p $(BUILD_DIR)/bin
$(BUILD_DIR)/lib:
	-mkdir -p $(BUILD_DIR)/lib

clean clobber clean_all clobber_all::
	@for i in $(PROGRAMS); do \
		echo -n "$$i:	" ;\
		$(MAKE) --no-print-directory -C $$i $@; \
		done

clobber::
	-rm -rf $(BUILD_DIR)

clobber_all::
	-rm -rf $(M3ARCHES)

RM_BIN= $(patsubst %, %/bin/*, $(M3ARCHES))
RM_LIB= $(patsubst %, %/lib/*, $(M3ARCHES))
clobber_all::
	-rm -rf $(RM_BIN) $(RM_LIB)

# this line allows dir names to be used as Makefile targets
.PHONY: $(PROGRAMS) DRIVER DRIVER_PROF DRIVER_REF DRIVER_PROF_REF DRIVER_HACK DRIVER_WB DRIVER_PROF_WB

# Inter-extension dependencies.
httpd: urt net fs dev/cam dev/extent nanny sync
thread/threadcore: dev/console 
thread: shell/shellcore nanny vm/space 
dev/extent: dev/devcore shell/shellcore urt/urtcore nanny
dev: usyscall
fs: fs/fscore dev nanny sync
net: urt dev nanny
socketRW: net nanny
sphinx: urt sieg usyscall nanny vm/inodepager fastbuffer
shell: measure vm thread nanny fs/tmpfs fs/tftp 
fs: oncrpc nanny
oncrpc: urt dev/netdev net/dns
vm/space: vm/vmcore
mach: net urt thread dev nanny
usyscall: sieg fastbuffer
sieg: thread
benchmark: measure
rcg: net
shell/basiccmds: fs/tmpfs
trans: usyscall fastbuffer
startup: fs/tftp fs/tmpfs vm thread shell tftp fs/fscore dev/console dev/devcore shell/expr shell/utils 
init: stcp
