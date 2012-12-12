/*
 * HISTORY
 * 05-Jul-95  Emin Gun Sirer (egs) at the University of Washington
 *	New swap context routine that only swaps the ptbr and
 *	keeps ksp the same.
 *
 */
#include <machine/asm.h>
#include <machine/regdef.h>
#include <machine/reg.h>
#include <machine/pal.h>

        .align 4
LEAF(spinswpctx)
	stq	sp, 0(a1)
        call_pal PAL_swpctx
        ret zero,(ra)
END(spinswpctx)

	/*
	 * XXX 0xb0 is unprivileged - change to a privileged number
	 */
LEAF(spinsetptbr)
        call_pal 0xb0
	mb
        ret zero,(ra)
END(spinsetptbr)
