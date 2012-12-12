# Target: Big endian MIPS running ELF
# Includes ECOFF support for Irix 5, which can run ECOFF executables
# Includes little endian vectors because they cost nothing extra

DEFAULT_VECTOR=bfd_elf32_bigmips_vec
SELECT_VECS=bfd_elf32_littlemips_vec ecoff_big_vec ecoff_little_vec
SELECT_ARCHITECTURES=bfd_mips_arch
