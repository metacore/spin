/* Copyright (C) 1994, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */
/*                                                             */
/* Last modified on Tue Jan 10 15:49:19 PST 1995 by kalsow     */
/*      modified on Mon Apr 26 14:29:15 PDT 1993 by muller     */

/*
 * HISTORY
 * 14-Jun-96  Przemek Pardyak (pardy) at the University of Washington
 *	Got rid of typeidx.
 *
 * 02-Apr-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added interface link information (a copy of RTBuiltin.m3x
 *	put into a string, it could be generated automatically from that
 *	file).
 *
 * 19-Nov-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added procedure type information.
 *
 * 13-Nov-95  Przemek Pardyak (pardy) at the University of Washington
 *	Two new fields added to the typecell record (see RT0.i3)
 */


/*----------- types from RT0, for now ... -----------------*/

#define _INTEGER long
#define _INT32   int
#define _ADDRESS char*
typedef void (*_PROC)();

#define WSZ  sizeof(_ADDRESS)

typedef struct _typecell {
    _INTEGER          typecode;
    _INTEGER          subTypeCode;
    _INTEGER          lastSubTypeCode;
    _INTEGER          selfID;
    unsigned char     fp[8];
    _INTEGER          traced;       
#ifdef FIXME
    _INTEGER          class;       
#endif FIXME
    _INTEGER          dataOffset;       
    _INTEGER          dataSize;   
    _INTEGER          dataAlignment;     
    _INTEGER          methodOffset;
    _INTEGER          methodSize;
    _INTEGER          nDimensions;
    _INTEGER          elementSize;
    _ADDRESS          defaultMethods;
    _ADDRESS          type_map;
    _ADDRESS          gc_map;
    _ADDRESS          type_desc;
    _PROC             initProc;
    _PROC             linkProc;
    _INTEGER          parentID;   
    _ADDRESS          parent;
    _ADDRESS          children;
    _ADDRESS          sibling;
    _ADDRESS          brand;
    _ADDRESS          name;
    struct _typecell *next;
  } _TYPECELL;

typedef struct {
    char*        file;
    _TYPECELL   *type_cells;
    _ADDRESS     type_cell_ptrs;
    _ADDRESS     full_rev;
    _ADDRESS     partial_rev;
    _ADDRESS     proc_info;
    _ADDRESS     proc_types;
    _ADDRESS     try_scopes;
    _ADDRESS     var_map;
    _ADDRESS     gc_map;
    _PROC        link;
    _PROC        main;
  } _MODULE_INFO;

/*------------------------------------------------------------------------*/
/* the "builtin" interface record */

struct _IR_ {
  _MODULE_INFO module_info;
  _TYPECELL    untraced_root;
  _TYPECELL    root;
  _TYPECELL    refany;
  _TYPECELL    address;
  _TYPECELL    null;
};

/*------------------------------------------------------------------------*/

#define SELF MI_M3_BUILTIN
struct _IR_ SELF = {

  /*--------- module_info -----------------*/
  {
  "M3_BUILTIN.ic",      /* file */
  &SELF.untraced_root,  /* type_cells */
  0,                    /* type_cell_ptrs */
  0,                    /* full_rev */
  0,                    /* partial_rev */
  0,                    /* proc_info */
  0,                    /* try_scopes */
  0,                    /* var_map */
  0,                    /* gc_map */
  0,                    /* link */
  0                     /* main */
  },

  /*--------- untraced root -----------------*/
  /* FP ("$objectadr") ==> 16_f80919c87187be41 => 16_898ea789 = -1987139703 */
  { 0, 0, 0,        /* typecode */
    -1987139703, /* selfID */
    { 0xf8, 0x09, 0x19, 0xc8, 0x71, 0x87, 0xbe, 0x41 },    /* fingerprint */
    0,           /* traced */
#ifdef FIXME
    -2,          /* class */
#endif FIXME
    0, WSZ, WSZ, /* data offset, size, alignment */
    0, WSZ,      /* method offset, size */
    0, 0,        /* nDimensions, elementSize */
    0,           /* default methods */
    0,           /* type map */
    0,           /* gc map */
    0,           /* type desc */
    0,           /* initProc */
    0,           /* linkProc */
    0,           /* parent ID */
    0, 0, 0,     /* parent, children, sibling */
    0,           /* brand */
    "UNTRACED ROOT", /* name */
    &SELF.root       /* next */
  },

  /*--------- root -----------------*/
  /* FP ("$objectref") ==> 16_f80919c86586ad41 => 16_9d8fb489 = -1651526519 */
  { 0, 0, 0,        /* typecode */
    -1651526519, /* selfID */
    { 0xf8, 0x09, 0x19, 0xc8, 0x65, 0x86, 0xad, 0x41 },    /* fingerprint */
    1,           /* traced */
#ifdef FIXME
    -2,          /* class */
#endif FIXME
    0, WSZ, WSZ, /* data offset, size, alignment */
    0, WSZ,      /* method offset, size */
    0, 0,        /* nDimensions, elementSize */
    0,           /* default methods */
    0,           /* type map */
    0,           /* gc map */
    0,           /* type desc */
    0,           /* initProc */
    0,           /* linkProc */
    0,           /* parent ID */
    0, 0, 0,     /* parent, children, sibling */
    0,           /* brand */
    "ROOT",      /* name */
    &SELF.refany /* next */
  },

  /*--------- refany -----------------*/
  /* FP ("$refany") ==> 16_65722480796e6166 => 16_1c1c45e6 = 471614950 */
  { 0, 0, 0,        /* typecode */
    471614950,   /* selfID */
    { 0x65, 0x72, 0x24, 0x80, 0x79, 0x6e, 0x61, 0x66 },    /* fingerprint */
    1,           /* traced */
#ifdef FIXME
    -2,          /* class */
#endif FIXME
    0, WSZ, WSZ, /* data offset, size, alignment */
    0, 0,        /* method offset, size */
    0, 0,        /* nDimensions, elementSize */
    0,           /* default methods */
    0,           /* type map */
    0,           /* gc map */
    0,           /* type desc */
    0,           /* initProc */
    0,           /* linkProc */
    0,           /* parent ID */
    0, 0, 0,     /* parent, children, sibling */
    0,           /* brand */
    "REFANY",    /* name */
    &SELF.address /* next */
  },

  /*--------- address -----------------*/
  /* FP ("$address") ==> 16_628a21916aca01f2 => 16_8402063 = 138420323 */
  { 0, 0, 0,        /* typecode */
    138420323,   /* selfID */
    { 0x62, 0x8a, 0x21, 0x91, 0x6a, 0xca, 0x01, 0xf2 },    /* fingerprint */
    0,           /* traced */
#ifdef FIXME
    -2,          /* class */
#endif FIXME
    0, WSZ, WSZ, /* data offset, size, alignment */
    0, 0,        /* method offset, size */
    0, 0,        /* nDimensions, elementSize */
    0,           /* default methods */
    0,           /* type map */
    0,           /* gc map */
    0,           /* type desc */
    0,           /* initProc */
    0,           /* linkProc */
    0,           /* parent ID */
    0, 0, 0,     /* parent, children, sibling */
    0,           /* brand */
    "ADDRESS",   /* name */
    &SELF.null   /* next */
  },

  /*--------- null -----------------*/
  /* FP ("$null") ==> 16_248000006c6c756e => 16_48ec756e = 1223456110 */
  { 0, 0, 0,        /* typecode */
    1223456110,  /* selfID */
    { 0x24, 0x80, 0x00, 0x00, 0x6c, 0x6c, 0x75, 0x6e },    /* fingerprint */
    0,           /* traced */
#ifdef FIXME
    -2,          /* class */
#endif FIXME
    0, 0, 0,     /* data offset, size, alignment */
    0, 0,        /* method offset, size */
    0, 0,        /* nDimensions, elementSize */
    0,           /* default methods */
    0,           /* type map */
    0,           /* gc map */
    0,           /* type desc */
    0,           /* initProc */
    0,           /* linkProc */
    0,           /* parent ID */
    0, 0, 0,     /* parent, children, sibling */
    0,           /* brand */
    "NULL",      /* name */
    0            /* next */
  }

};


char MLink_I_M3_BUILTIN[] =
"\
\000\000\000\000\
M3 v4.2\n\
N0 M3_BUILTIN\n\
N1 RTHooks\n\
N2 Word\n\
N3 Text\n\
N4 Thread\n\
N5 RTType\n\
N6 RTMisc\n\
N7 RTHeap\n\
N8 RTException\n\
N9 RTLinker\n\
N10 TEXT\n\
N11 MUTEX\n\
/FIXME I0 1 1 9 0 0 0 0 0 2 0 0 24 0\n\
I0 1 9 0 0 0 0 0 2 0 0 24 0\n\
A0\n\
B1\n\
B2\n\
B3\n\
B4\n\
B5\n\
B6\n\
B7\n\
B8\n\
B9\n\
V0 0 10 bbef933ad0c2139d\n\
V1 0 11 be62b766fb8a92d5\n\
E0\n\
E1\n\
T1541f475\n\
Q1541f475 9d8fb489\n\
T50f86574\n\
Q50f86574 1c1c45e6\n\
T898ea789\n\
T9d8fb489\n\
T1c1c45e6\n\
T08402063\n\
T48ec756e\n\
T195c2a74\n\
T1e59237d\n\
T56e16863\n\
T97e237e2\n\
T48e16572\n\
T94fe32f6\n\
T9ee024e3\n\
T2da6581d\n\
T2fa3581d\n\
T9c9de465\n\
T20ad399f\n\
T3ce4d13b\n\
Tfa03e372\n\
T509e4c68\n\
Tdc1b3625\n\
Tee17df2c\n\
Tb740efd0\n\
";




