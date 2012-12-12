typedef unsigned long cycle_t;
extern long asm();
#pragma intrinsic(asm)
#define read_cycle_counter(p) *p = (cycle_t)asm("rpcc %v0")
unsigned int 
diff_cycle_counter (cycle_t start, cycle_t stop)
{
    stop &= 0xffffffff;
    start &= 0xffffffff;

    if (stop<start) /* correct for wraparound */
      return 0x100000000-start+stop;
    
    return stop-start;

}	


