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
#ifndef _SYMBOL_H_
#define _SYMBOL_H_

extern void Symbol_create_table(Module *ms, char *source);

extern void Symbol_fake_table(Module *ms, SymbolEntry *syms, int symsize);

extern void Symbol_assign_space(Module *ms);

extern long Symbol_get_value(SymbolEntry *se);

extern void *Symbol_find(Module *ms, char *name);

#endif /* _SYMBOL_H_ */
