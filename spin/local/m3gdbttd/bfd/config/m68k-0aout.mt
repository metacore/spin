# Target: Motorola 68000 using a.out
# This version sets the toolversion field of the a.out header to 0, as
# is required by Cisco.

DEFAULT_VECTOR=aout0_big_vec
SELECT_ARCHITECTURES=bfd_m68k_arch
# We include this here, rather than making a separate cisco configuration, so 
# that cisco-core.c gets routinely tested at least for compilation.
SELECT_VECS=cisco_core_vec
