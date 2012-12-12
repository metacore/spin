/*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Hash tables for symbols.
 *
 */
#ifndef _HASH_H_
#define _HASH_H_

#define  MAX_SYMBOL_HASH_VALUE    (1<<31)

extern void Hash_add_symbol(SymbolEntry *sym, SymbolTable *st);

struct SymbolEntry *Hash_find_symbol(char *symName, SymbolTable *st);

extern void Hash_create(SymbolTable *st);

#endif
