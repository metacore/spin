/*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 06-Nov-95  Emin Gun Sirer (egs) at the University of Washington
 *    Got rid of empty module.
 *
 * 10-Aug-95  Emin Gun Sirer (egs) at the University of Washington
 *    Changed to pick a gp that matches the fake coff gp picked
 *    by the compiler, so that instruction fields will not overflow.
 *
 * 07-Jul-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added a special debug domain that identifies unresolved symbols
 *      when linked against.
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Coff linking, module at a time.
 *
 */
#include "Interface.h"
#include "Module.h"
#include "Section.h"
#include "Symbol.h"
#include "Resolve.h"

/*
 * linking against this module prints out things that 
 * go wrong.
 */
Module dm, *debugModule = &dm;

/*
 * Link ms1 into ms2.
 */
void
Link(Module *ms1, Module *ms2)
{
    Section *ss;
    int      i, done;

    if(ms1 == NULL || ms2 == NULL)
	return;
    switch(ms1->MS_flags) {
    case Resolved_Fully:
	return;
    case Resolved_Not:
    case Resolved_Partially:
	done = 1;
	for(i = 0; i < ms1->MS_nSec; ++i) {
	    ss = ms1->MS_sectPtr[i];
	    if(ss && ss->SS_flags != Resolved_Fully)
		if(Resolve_section(ss, ms1, ms2) != Resolved_Fully)
		    done = 0;
	}
	ms1->MS_flags = (done ? Resolved_Fully : Resolved_Partially);
    }
}

/*
 * Unlink ms2 from ms1.
 */
void
Unlink(Module *ms1, Module *ms2)
{
    Section *ss;
    int i;
    long undid = 0;

    for(i = 0; i < ms1->MS_nSec; ++i) {
	ss = ms1->MS_sectPtr[i];
	if(ss)
	    undid |= Unresolve_section(ss, ms1, ms2);
    }
    if(ms1->MS_flags == Resolved_Fully && undid)
	ms1->MS_flags = Resolved_Partially;
}
