/* @(#)rpc_util.h	2.1 88/08/01 4.0 RPCSRC */
/*
 * Sun RPC is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify Sun RPC without charge, but are not authorized
 * to license or distribute it to anyone else except as part of a product or
 * program developed by the user.
 * 
 * SUN RPC IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 * 
 * Sun RPC is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY SUN RPC
 * OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even if
 * Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */
/* @(#)rpc_util.h 1.6 87/06/24 (C) 1987 SMI */

/*
 * rpc_util.h, Useful definitions for the RPC protocol compiler 
 * Copyright (C) 1987, Sun Microsystems, Inc. 
 */
/*
 * Modified to generate Modula-3 output by Marvin Theimer, Xerox PARC.
 */

#include "StringHashTable.h"

extern char *malloc();

#define FALSE 0
#define TRUE 1

#define alloc(size)		malloc((unsigned)(size))
#define ALLOC(object)   (object *) malloc(sizeof(object))

#define s_print	(void) sprintf
#define f_print (void) fprintf

struct list {
	char *val;
	struct list *next;
};
typedef struct list list;

struct ImportModuleRec {
    char *moduleName;		/* Module name for the associated name. */
    char *defnName;		/* String of the form foo.bar, where
				   foo is the module name. */
};
typedef struct ImportModuleRec ImportModuleRec;

/*
 * Global variables 
 */

#define MAXLINESIZE 1024

typedef struct Context {
    char curline[MAXLINESIZE];	/* current read line */
    char *where;		/* current point in line */
    int linenum;		/* current line number */
    char *infilename;		/* current input filename */
    FILE *fin;			/* file pointer of current input */
    list *defined;		/* list of defined things */
    int printDirectives;	/* TRUE => emit directives when encountered */
} Context;

extern Context *CurrentContext;

extern FILE *fout;

extern char *svcName;

extern char *RPCExceptions;

extern list *Imports;
extern StringHashTableHandle ImportNames;

extern StringHashTableHandle SymbolNames;

extern int ExpandProcArgs;
extern int RecoveryFlag;
extern int PFlag;

/*
 * rpc_util routines 
 */
void storeval();

#define STOREVAL(list,item)	\
	storeval(list,(char *)item)

int streq();
char *strdup();
void error();
void expected1();
void expected2();
void expected3();
void record_open();
char *ModuleName();
char *GetPutModuleName();
char *MapToM3Name();
char *MapToM3Type();
char *rpcgenBaseType();
char *ArrayMaxMinus1();
char *FindUniqueName();
char *StripPrefix();

/*
 * rpc_cout routines 
 */
void emit();

/*
 * rpc_hout routines 
 */
void print_datadef();

/*
 * rpc_svcout routines 
 */
void write_server_prog();
