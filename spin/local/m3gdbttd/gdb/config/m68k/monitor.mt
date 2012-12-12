# Target: Motorola rom68k boot monitor (from Integrated Systems)
# Target: Motorola bug monitor.
# These defines should give you a gdb running on anything that will be able to
# communicate with a m68k based debug monitor.  Communications
# is facilitated via either a serial line, or a TCP or TELNET connection to
# a serial line on a terminal multiplexor.
TDEPFILES= exec.o m68k-pinsn.o m68k-tdep.o remote-mon.o 
TM_FILE= tm-monitor.h
