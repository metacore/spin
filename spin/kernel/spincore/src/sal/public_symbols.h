/*
 * Copyright 1994-1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * SYMBOLS FOR SPIN PUBLIC.
 *
 * HISTORY
 * 16-Jul-97  Tian Fung Lim (tian) at the University of Washington
 *	Exported AssignTracedToGlobal (writebarrier)
 *
 * 13-Jun-97  David Becker at the University of Washington
 *      Readded TEXT_VB for x86 mcount.  It was in old sal SpinPublic.h
 *
 * 31-May-97  David Becker at the University of Washington
 *	Renamed SpinPublic.h to public_symbols.h
 *	Exports addresses of underlying kernel code via kernel_symbols.c
 *
 * 05-May-96  Charles Garrett (garrett) at the University of Washington
 *	Export _mcount which is called by every procedure that is
 *	 compiled with the -pg option.
 *
 * 19-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Added bcopy, memmove and memset symbols, which are used by the m3
 *	compiler to copy large arrays and datastrutures.
 *
 *
 */

DATA(RTThread__handlerStack)


TEXT(m3_div)
TEXT(m3_mod)
TEXT(bcopy)
TEXT(memmove)
TEXT(memset)

#ifdef WRITEBARRIER
TEXT(AssignTracedToGlobal)
#endif
     
TEXT(RT0u__inCritical)
TEXT(RT0u__types)
TEXT(RT0u__RuntimeEpoch)

#ifdef CALL_GRAPH
#ifdef OSF
TEXT(_mcount)
#endif
#ifdef __FreeBSD__
TEXT_VB(mcount)
#endif
#endif


#ifdef __FreeBSD__
TEXT(setjmp)
TEXT(__setjmp)
#endif

#ifdef OSF
TEXT(_setjmp)
TEXT(rpcc)
TEXT(__divqu)
TEXT(__reml)
TEXT(__divq)
TEXT(nuxi_16)
TEXT(nuxi_32)
#endif
