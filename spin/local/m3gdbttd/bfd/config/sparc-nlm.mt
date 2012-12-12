# Target:  SPARC using Novell's NetWare Loadable Module (NLM) format.

# Note that we produce elf output and only convert it at the last
# moment to NLMs.  We could in principle generate NLMs directly, but I
# don't think the format is really rich enough to do this, and anyway,
# this can be more convenient for the user.  We can convert a.out too.

DEFAULT_VECTOR=bfd_elf32_sparc_vec

SELECT_VECS=nlm32_sparc_vec sunos_big_vec
SELECT_ARCHITECTURES=bfd_sparc_arch
