/*
 * HISTORY
 * 24-Aug-97  Przemek Pardyak (pardy) at the University of Washington
 *	Allow zero-length openarrays.  Add abort.
 *
 * 31-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Fixed the signature of subarray.
 *      Added comment on correct usage.
 *
 * 21-Dec-95  Charles Garrett (garrett) at the University of Washington
 *	Added subfree which frees the header objects made by subarray.
 *
 * 29-Jun-95  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
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
