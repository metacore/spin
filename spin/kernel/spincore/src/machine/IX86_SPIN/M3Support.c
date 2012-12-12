/*
 * HISTORY
 * 24-Aug-97  Przemek Pardyak (pardy) at the University of Washington
 *	Allow zero-length openarrays.  Add abort.
 *
 * 16-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Created from Alpha version.
 *
 *
 */

#include "spincore/src/sal/OpenArray.h"

/* This function returns an UNTRACED REF ARRAY type */
/* The buffer passed to subarray MUST be UNTRACED as well */
/* Zero length arrays are legal */
struct openarray *
subarray(char *start, unsigned long len,unsigned long offset){
    struct openarray *oa;
    if(len < 0){
	printf("m3support.subarray(%lx,%ld,%ld) should produce RTE.",
	       start, len, offset);
	panic("m3support");
    }
    oa =(struct openarray *)spin_malloc(sizeof(struct openarray));
    oa->size  = len;
    oa->start = start+offset;
    return oa;
}

void subfree(struct openarray* oa) {
    spin_free(oa);
}

/*
 * This is called from the runtime's ex_frame/RTStackC.c
 */
void abort(void) {
  printf("ERROR >> abort called, go to the debugger\n");
  Debugger();
}
