/*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 07-Jul-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added a special debug domain that identifies unresolved symbols
 *      when linked against.
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Defs for the linker.
 *
 */

#ifndef _SDLDINTERFACE_H_
#define _SDLDINTERFACE_H_

#define  NULL 0

#include <a.out.h>

extern void *spin_malloc(long size);

#define lalloc(A)    spin_malloc(A)
#define LALLOC(A)    lalloc(sizeof(struct A))

#define min(a,b)     (((a) < (b)) ? (a) : (b))
#define asize(A)     (sizeof(A) / sizeof(A[0]))

#ifdef KERNEL
#define ASSERT(x)    {if((x) == 0) { printf("Assertion failed in %s:%d\n", __FILE__, __LINE__); }}
#define OUTPUTFLUSH()
#define PRINT          if(0)printf
#else
#include <assert.h>
/* these should be defined in well-known places, but are not */
extern int disassembler();
extern long __reml();
extern void *myrealloc();
#define ASSERT(x)      assert(x)
#define OUTPUTFLUSH()  fflush(stdout)
#define PRINT          if(0)printf
#endif

/*
 * The following two routines are used to go from the a.out structure
 * to an in-memory structure. They make sure the structures are aligned
 * properly.
 */
void *coffxtrct(void *source, long size);
void  coffdone(void *ptr, void *source);

#define error(A)     ASSERT(A != A);

#define SSF_ALLOCATE          0x0001    /* Need to allocate a block        */
#define SSF_ALLOCATED         0x0002    /* Block is allocated for Section  */
#define SSF_LOAD              0x0004    /* Need to load a block            */
#define SSF_LOADED            0x0008    /* Block has contents of a.out file*/
#define SSF_RELOC_LOAD        0x0010    /* Reloc's not loaded              */
#define SSF_RELOCED_NONE      0x0020    /* No Ref's resolved               */
#define SSF_RELOCED_INTERNAL  0x0040    /* All internal Ref's resolved     */
#define SSF_RELOCED_PARTIAL   0x0080    /* Some External Relocs's resolved */
#define SSF_RELOCED_ALL       0x0100    /* All Reloc's resolved            */
#define SSF_READ_ONLY         0x1000    /* Read only block                 */

typedef struct Module Module;

#define Invalid_Address   0xdeadbeef

extern Module *debugModule;

/* 
 * Every module is split into three three memory sections,
 * which are contiguous so base indexed access off of gp
 * can be made to work.
 */
typedef enum {
    POOL_TEXT,
    POOL_DATA,
    POOL_BSS,
    MAXPOOL
} PoolType;

/*
 * Link state of various fragments
 */
typedef enum {
    Resolved_Not,
    Resolved_Partially,
    Resolved_Fully
} LinkState;

typedef struct Reloc {
    struct relocation_info  RS_relocInfo;  /* Raw, straight from a.out file*/
    unsigned int            RS_patched:1;     /* module has been patched */
    void                    *RS_patchAddr;  /* addr to patch */
    long                    RS_patchValue; /* value in sect before patch  */
} Reloc;

typedef struct SymbolEntry {
    void         *SE_ptr;       /* ptr to this entity            */
    char         *SE_name;      /* ptr to symbol in StringList   */
    long          SE_nvalue;    /* n_value of symbol             */
    long          SE_nsclass;   /* n_sclass of symbol            */
    Module       *SE_module;    /* module in which it is defined */
} SymbolEntry;

typedef struct SymbolTable {
    SymbolEntry **ST_hashPtr;   /* ptr to Hash table             */
    SymbolEntry  *ST_symBlkPtr; /* ptr to symbol summaries       */
    long          ST_nSyms;     /* # of symbols in table         */
    long          ST_tSize;     /* # of entries in table         */
} SymbolTable;

typedef struct StringList {
    char         *SL_strBlkPtr; /* ptr to string block      */
    long          SL_size;      /* size of list (in bytes)  */
} StringList;

typedef struct Section {
    Reloc        *SS_relocPtr;   /* ptr to relocation block     */
    char         *SS_name;       /* name of the section         */
    void         *SS_ptr;        /* ptr to Section content      */
    long          SS_fileBase;   /* address of section in file  */
    long          SS_linkBase;   /* base address for which section is linked */
    long          SS_size;       /* # of bytes in section       */
    long          SS_nReloc;     /* # of relocations total      */
    LinkState     SS_flags;      /* Link state of the Section   */
    PoolType      SS_poolindx;   /* memory pool that backs this section */
} Section;

struct Module {
    LinkState     MS_flags;     /* relocation state of the module */
    /* sections */
    Section     **MS_sectPtr;   /* array of Sections in Module  */
    long          MS_nSec;      /* # of sections in Module      */
    /* symbols */
    SymbolTable  *MS_symPtr; /* symbol table       */
    StringList   *MS_strPtr; /* strings            */
    /* module memory */
    long          MS_poolsize[MAXPOOL]; /* pool sizes */
    char         *MS_pool[MAXPOOL];     /* memory pools */
    char         *MS_poolfree[MAXPOOL]; /* ptr to next free area in pool */
};

enum SymbolClass {
    scText = 1,
    scData,
    scBss,
    scAbs,
    scUndefined,
    scCommon,
    scIndirect,
    scSize,
    scFilename,
    scTextInternal,
    scDataInternal,
    scBssInternal,
    scAbsInternal,
    scUndefinedInternal,
    scCommonInternal,
    scIndirectInternal,
    scSizeInternal,
    scWarning,
    scOther
};

#define LastExternalClass scFilename

#endif /* _SDLDINTERFACE_H_ */
