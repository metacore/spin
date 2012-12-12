# Target:  Alpha using Novell's NetWare Loadable Module (NLM) format.

# Note that we produce ECOFF output and only convert it at the last
# moment to NLMs.

DEFAULT_VECTOR=ecoffalpha_little_vec

SELECT_VECS=nlm32_alpha_vec
SELECT_ARCHITECTURES=bfd_alpha_arch
