/*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 25-Jul-95  Brian Bershad (bershad) at the University of Washington
 *	Removed Core_* functions.
 *
 * 19-Jun-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added setspace to help calculate size of bss segment.
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Object file resolution, symbol locaiton, etc. management.
 *
 */
#ifndef _MODULE_H_
#define _MODULE_H_

/* public */
extern Module *MCreate(char *source);

extern void Link(Module *m1, Module *m2);

extern void Unlink(Module *ms1, Module *ms2);

extern long FullyResolved(Module *m);

Module *Core_init(SymbolEntry *syms, int symsize);

extern void Add_Fake_Symbol(Module *corem, char *name, void *ptr, int text);

extern void *Symbol_findlinkinfo(Module *ms, long *found);

extern char *ExractModuleName(SymbolEntry *sym);

/* private */
extern void *Module_pool_allocate(Module *ms, PoolType pool, int size);

extern void  Module_pool_setspace(Module *ms, PoolType pool, int size);
#endif /* _MODULE_H_ */
