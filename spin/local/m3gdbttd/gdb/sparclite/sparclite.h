/* SPARClite defs */

/* Macros for reading and writing to arbitrary address spaces.  Note that ASI
   must be a constant (sorry, but the SPARC can only specify ASIs as part of an
   instruction.  */

#define read_asi(ASI, LOC, VAL) \
  __asm__ volatile ("lda [%r1]%2,%0" : "=r" (VAL) : "rJ" (LOC), "I" (ASI))

#define write_asi(ASI, LOC, VAL) \
  __asm__ volatile ("sta %0,[%r1]%2" : : "r" (VAL), "rJ" (LOC), "I" (ASI));

/* Use this when modifying registers that cause memory to be modified.  This
   will cause GCC to reload all values after this point.  */

#define write_asi_volatile(ASI, LOC, VAL) \
  __asm__ volatile ("sta %0,[%r1]%2" : : "r" (VAL), "rJ" (LOC), "I" (ASI) \
		    : "memory");
