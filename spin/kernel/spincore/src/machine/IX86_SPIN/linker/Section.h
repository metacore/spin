/*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created.
 *
 */
#ifndef _SECTION_H_
#define _SECTION_H_


Section *Section_create(char *source, char *name);

void   Section_load(Module *ms, Section *ss, char *source);

extern Section *Section_find_by_voffset(Module *ms, long offset);

extern Section *Section_find_by_name(Module *ms, char *name);

extern Section *Section_find_by_reloc_ref(Module *ms, int offset);

#endif /* _SECTION_H_ */
