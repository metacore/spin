typedef u_int64_t cycle_t;

/* 
 * Read function: read hardware timestamp (cycle) counter 
 */
#define read_cycle_counter(res_p) \
  __asm __volatile (						\
	 ".byte 0xf; .byte 0x31	# RDTSC instruction		\n\
         movl %%edx, %1		# High order 32 bits		\n\
         movl %%eax, %0		# Low order 32 bits"		\
	: "=g" (*(int *)(res_p)), "=g" (*(((int *)res_p)+1)) 	\
	: /* no input regs */					\
	: "eax", "edx")

long diff_cycle_counter(cycle_t start_clk, cycle_t stop_clk)
{
	/* Sanity checking; prevent "negative" results */
	if (stop_clk <= start_clk)
		return (0);
	else 
		return (((long)(stop_clk - start_clk)));
}
