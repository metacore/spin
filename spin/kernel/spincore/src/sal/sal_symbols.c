/*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * This file should go away.
 *
 * HISTORY
 * 13-Jun-97  David Becker at the University of Washington
 *      Readded TEXT_VB for x86 mcount.  It was in old x86 sal CoreSymbols.c
 *
 * 31-May-97  David Becker at the University of Washington
 *	Renamed CoreSymbols.c to kernel_symbols.c.
 *	This module exports address of underlying kernel code
 *	as listed in public_symbols.h and trusted_symbols.h
 *
 * 22-Feb-96  Charles Garrett (garrett) at the University of Washington
 *	Changed ALPHA_OSF to ALPHA_SPIN.
 *
 * 11-Oct-95  Marc Fiuczynski (mef) at the University of Washington
 *	Removed the declaration of Modula3 interfaces.  This information
 *	is now generated automatically as part of the m3build process of
 *	the spin kernel.
 *      Renamed SpinPublic and SpinTrusted with  suffix.
 *
 * 07-Aug-95  Brian Bershad (bershad) at the University of Washington
 *	Added Rofs and CAM.
 *
 * 01-Aug-95  Brian Bershad (bershad) at the University of Washington
 *	Changed to work with M3 generic "Interface" interfaces.
 *
 * 07-Jul-95  Marc Fiuczynski (mef) at the University of Washington
 *	Changed to use macros to define the core symbols
 *	
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Definition of visible symbols from core.
 */

#ifdef __FreeBSD__
#include <spincore/src/machine/IX86_SPIN/linker/Interface.h>
#endif
#ifdef OSF
#include <symconst.h>
#define HIDEMALLOCS /* tell Interface.h to not #include sys/malloc.h */
#include <spincore/src/machine/ALPHA_SPIN/linker/Interface.h>
#undef tsleep
#undef wakeup
#endif

#ifdef quote
#undef quote
#endif
#ifdef min
#undef min
#endif
#ifdef max
#undef max
#endif

/* TEXT and DATA define procedures and globals (resp.) that are in the core. */
#define TEXT(symbol) extern void * symbol();
#define TEXT_VB TEXT
#define DATA(symbol) extern void * symbol;
#include "public_symbols.h"	
#include "trusted_symbols.h"
#undef TEXT
#undef TEXT_VB
#undef DATA
#undef MODULE
#define quote(s) #s
#ifdef __FreeBSD__
#define TEXT(sym) {(void *)sym, quote(_##sym), (long) sym, scText},
#define TEXT_VB(sym) {(void *)sym, quote(sym), (long) sym, scText},
#define DATA(sym)  {(void *)&sym, quote(_##sym), (long) &sym, scData},
#endif
#ifdef OSF
#define TEXT(sym) {(void *)sym, quote(sym), (long) sym, scText},
#define DATA(sym)  {(void *)&sym, quote(sym), (long) &sym, scData},
#endif

#define DefineSymbolEntries(I) SymbolEntry I##Interface__domainSymbolsData[] =

#define DefineInterfaceInfo(I) \
SymbolEntry *##I##Interface__domainSymbols = \
	(SymbolEntry*)##I##Interface__domainSymbolsData;\
int I##Interface__domainSize = asize(I##Interface__domainSymbolsData);\
char *##I##Interface__name = quote(I);

DefineSymbolEntries(SpinPublic)
{
{(void *)0, "_fpdata", (long)0, scData},
#include "public_symbols.h"	
};	

DefineSymbolEntries(SpinTrusted)
{
#include "trusted_symbols.h"
};


/************************************************************************/

DefineInterfaceInfo(SpinTrusted)	/* Gens "SpinTrusted" */
DefineInterfaceInfo(SpinPublic)	/* Gens "SpinPublic" */

