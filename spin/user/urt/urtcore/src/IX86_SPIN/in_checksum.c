
/*
 * This routine does all the checksum computations that don't
 * require anything special (like copying or special headers).
 */

unsigned short in_checksum(unsigned char * buff, int len, unsigned long sum)
{
	/* Do the first multiple of 4 bytes and convert to 16 bits. */
	if (len > 3)
	{
		__asm__("clc\n"
		"1:\t"
		"lodsl\n\t"
		"adcl %%eax, %%ebx\n\t"
		"loop 1b\n\t"
		"adcl $0, %%ebx\n\t"
		"movl %%ebx, %%eax\n\t"
		"shrl $16, %%eax\n\t"
		"addw %%ax, %%bx\n\t"
		"adcw $0, %%bx"
		: "=b" (sum) , "=S" (buff)
		: "0" (sum), "c" (len >> 2) ,"1" (buff)
		: "ax", "cx", "si", "bx" );
	}
	if (len & 2)
	{
		__asm__("lodsw\n\t"
		"addw %%ax, %%bx\n\t"
		"adcw $0, %%bx"
		: "=b" (sum), "=S" (buff)
		: "0" (sum), "1" (buff)
		: "bx", "ax", "si");
	}
	if (len & 1)
	{
		__asm__("lodsb\n\t"
		"movb $0, %%ah\n\t"
		"addw %%ax, %%bx\n\t"
		"adcw $0, %%bx"
		: "=b" (sum), "=S" (buff)
		: "0" (sum), "1" (buff)
		: "bx", "ax", "si");
	}
	sum =~sum;
	return(sum & 0xffff);
}
