# Target:  88k using ELF.

DEFAULT_VECTOR=bfd_elf32_m88k_vec

# Include COFF because SVR4 systems can run COFF binaries, and GDB should be 
# able to debug them.
SELECT_VECS=m88kbcs_vec

SELECT_ARCHITECTURES=bfd_m88k_arch
