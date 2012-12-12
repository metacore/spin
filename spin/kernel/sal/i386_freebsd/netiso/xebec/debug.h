/* $Header: /afs/cs/project/spin/cvsroot/spin/kernel/sal/i386_freebsd/netiso/xebec/debug.h,v 1.1.1.1 1996/08/15 03:24:01 fgray Exp $ */
/* $Source: /afs/cs/project/spin/cvsroot/spin/kernel/sal/i386_freebsd/netiso/xebec/debug.h,v $ */

#define OUT stdout

extern int	debug[128];

#ifdef DEBUG
extern int column;

#define IFDEBUG(letter) \
	if(debug['letter']) {
#define ENDDEBUG  ; (void) fflush(stdout);}

#else

#define STAR *
#define IFDEBUG(letter)	 //*beginning of comment*/STAR
#define ENDDEBUG	 STAR/*end of comment*//

#endif DEBUG

