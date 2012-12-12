/* @(#)rpc_util.c	2.1 88/08/01 4.0 RPCSRC */
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
/*
 * Modified to generate Modula-3 output by Marvin Theimer, Xerox PARC.
 */

#ifndef lint
static char sccsid[] = "@(#)rpc_util.c 1.5 87/06/24 (C) 1987 SMI";
#endif

/*
 * rpc_util.c, Utility routines for the RPC protocol compiler 
 * Copyright (C) 1987, Sun Microsystems, Inc. 
 */
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "rpc_scan.h"
#include "rpc_parse.h"
#include "rpc_util.h"

#define NFILES 5
char *outfiles[NFILES];		/* output file names */
int nfiles;

FILE *fout;			/* file pointer of current output */

/*
 * Reinitialize the world 
 */
reinitialize()
{
	bzero(CurrentContext->curline, MAXLINESIZE);
	CurrentContext->where = CurrentContext->curline;
	CurrentContext->linenum = 0;
	CurrentContext->defined = NULL;
	CurrentContext->printDirectives = TRUE;
}

/*
 * string equality 
 */
streq(a, b)
	char *a;
	char *b;
{
	return (strcmp(a, b) == 0);
}

#ifndef linux
/*
 * copy a string
 */
char *
strdup(s)
	char *s;
{
	char *new = alloc(strlen(s)+1);

	strcpy(new, s);
	return new;
}
#endif

/*
 * store a value in a list 
 */
void
storeval(lstp, val)
	list **lstp;
	char *val;
{
	list **l;
	list *lst;

	for (l = lstp; *l != NULL; l = (list **) & (*l)->next);
	lst = ALLOC(list);
	lst->val = val;
	lst->next = NULL;
	*l = lst;
}

static
findit(def, type)
	definition *def;
	char *type;
{
	return (streq(def->def_name, type));
}

static
typedefed(def, type)
	definition *def;
	char *type;
{
	if (def->def_kind != DEF_TYPEDEF || def->def.ty.old_prefix != NULL) {
		return (0);
	} else {
		return (streq(def->def_name, type));
	}
}

static char *
locase(str)
	char *str;
{
	char c;
	static char buf[100];
	char *p = buf;

	while (c = *str++) {
		*p++ = (c >= 'A' && c <= 'Z') ? (c - 'A' + 'a') : c;
	}
	*p = 0;
	return (buf);
}

/*
 * print a useful (?) error message, and then die 
 */
void
error(msg)
	char *msg;
{
	printwhere();
	f_print(stderr, "%s, line %d: ",
		CurrentContext->infilename, CurrentContext->linenum);
	f_print(stderr, "%s\n", msg);
	crash();
}

/*
 * Something went wrong, unlink any files that we may have created and then
 * die. 
 */
crash()
{
	int i;

	for (i = 0; i < nfiles; i++) {
		(void) unlink(outfiles[i]);
	}
	exit(1);
}

void
record_open(file)
	char *file;
{
	if (nfiles < NFILES) {
		outfiles[nfiles++] = file;
	} else {
		f_print(stderr, "too many files!\n");
		crash();
	}
}

static char expectbuf[100];
static char *toktostr();

/*
 * error, token encountered was not the expected one 
 */
void
expected1(exp1)
	tok_kind exp1;
{
	s_print(expectbuf, "expected '%s'",
		toktostr(exp1));
	error(expectbuf);
}

/*
 * error, token encountered was not one of two expected ones 
 */
void
expected2(exp1, exp2)
	tok_kind exp1, exp2;
{
	s_print(expectbuf, "expected '%s' or '%s'",
		toktostr(exp1),
		toktostr(exp2));
	error(expectbuf);
}

/*
 * error, token encountered was not one of 3 expected ones 
 */
void
expected3(exp1, exp2, exp3)
	tok_kind exp1, exp2, exp3;
{
	s_print(expectbuf, "expected '%s', '%s' or '%s'",
		toktostr(exp1),
		toktostr(exp2),
		toktostr(exp3));
	error(expectbuf);
}


static token tokstrings[] = {
			     {TOK_IDENT, "identifier"},
			     {TOK_CONST, "const"},
			     {TOK_RPAREN, ")"},
			     {TOK_LPAREN, "("},
			     {TOK_RBRACE, "}"},
			     {TOK_LBRACE, "{"},
			     {TOK_LBRACKET, "["},
			     {TOK_RBRACKET, "]"},
			     {TOK_STAR, "*"},
			     {TOK_COMMA, ","},
			     {TOK_EQUAL, "="},
			     {TOK_COLON, ":"},
			     {TOK_SEMICOLON, ";"},
			     {TOK_UNION, "union"},
			     {TOK_STRUCT, "struct"},
			     {TOK_SWITCH, "switch"},
			     {TOK_CASE, "case"},
			     {TOK_DEFAULT, "default"},
			     {TOK_ENUM, "enum"},
			     {TOK_TYPEDEF, "typedef"},
			     {TOK_INT, "int"},
			     {TOK_SHORT, "short"},
			     {TOK_LONG, "long"},
			     {TOK_UNSIGNED, "unsigned"},
			     {TOK_DOUBLE, "double"},
			     {TOK_FLOAT, "float"},
			     {TOK_CHAR, "char"},
			     {TOK_STRING, "string"},
			     {TOK_OPAQUE, "opaque"},
			     {TOK_BOOL, "bool"},
			     {TOK_VOID, "void"},
			     {TOK_PROGRAM, "program"},
			     {TOK_VERSION, "version"},
			     {TOK_EOF, "??????"}
};

static char *
toktostr(kind)
	tok_kind kind;
{
	token *sp;

	for (sp = tokstrings; sp->kind != TOK_EOF && sp->kind != kind; sp++);
	return (sp->str);
}

static
printbuf()
{
	char c;
	int i;
	int cnt;

#	define TABSIZE 4

	for (i = 0; c = CurrentContext->curline[i]; i++) {
		if (c == '\t') {
			cnt = 8 - (i % TABSIZE);
			c = ' ';
		} else {
			cnt = 1;
		}
		while (cnt--) {
			(void) fputc(c, stderr);
		}
	}
}

static
printwhere()
{
	int i;
	char c;
	int cnt;

	printbuf();
	for (i = 0; i < CurrentContext->where - CurrentContext->curline; i++) {
		c = CurrentContext->curline[i];
		if (c == '\t') {
			cnt = 8 - (i % TABSIZE);
		} else {
			cnt = 1;
		}
		while (cnt--) {
			(void) fputc('^', stderr);
		}
	}
	(void) fputc('\n', stderr);
}

/*
 * Return the module name for a given defn.
 */
char *ModuleName(defn)
     char *defn;
{
    StringHashElement importElem;
    ImportModuleRec *m;

    importElem = StringHashFind(ImportNames, defn, find, NULL);
    if (importElem != NULL) {
	m = (ImportModuleRec *) importElem->userData;
	return (m->moduleName);
    }
    else
	return (svcName);
}

/*
 * Return the getput module name for a given defn, followed by a period.
 * If it is the name of the getput module for the service we are currently
 * compiling then return the "" string if defaultFlag is TRUE.
 */
char *GetPutModuleName(defn, defaultFlag)
     char *defn;
     int defaultFlag;
{
    StringHashElement importElem;
    ImportModuleRec *m;
    char buf[256];

    importElem = StringHashFind(ImportNames, defn, find, NULL);
    if (importElem != NULL) {
	m = (ImportModuleRec *) importElem->userData;
	sprintf(buf, "%s_x.", m->moduleName);
	return (strdup(buf));
    }
    else if (defaultFlag)
	return ("");
    else {
	sprintf(buf, "%s_x.", svcName);
	return (strdup(buf));
    }
}

/*
 * Map from an rpcgen name to an M3 name.
 */
char *MapToM3Name(name, defaultFlag)
     char *name;
     int defaultFlag;
{
    StringHashElement importElem;
    ImportModuleRec *m;
    char buf[256];
    char *newName;

    importElem = StringHashFind(ImportNames, name, find, NULL);
    if (importElem != NULL) {
	m = (ImportModuleRec *) importElem->userData;
	newName = m->defnName;
    }
    else if (defaultFlag) {
	newName = name;
    }
    else {
	sprintf(buf, "%s.%s", svcName, name);
	newName = strdup(buf);
    }
    return (newName);
}

/*
 * Map from an rpcgen type name to an M3 type name.
 */
char *MapToM3Type(type, defaultFlag)
     char *type;
     int defaultFlag;
{
    if (streq(type, "int") || streq(type, "long"))
	return ("INTEGER");
    else if (streq(type, "string"))
	return ("TEXT");
    else if (streq(type, "bool"))
	return ("BOOLEAN");
    else if (streq(type, "char"))
	return ("CHAR");
    else if (streq(type, "u_int") ||
	     streq(type, "uint") ||
	     streq(type, "u_long"))
	return ("Ctypes.unsigned_int");
    else if (streq(type, "u_short") ||
	     streq(type, "ushort"))
	return ("Ctypes.unsigned_short");
    else if (streq(type, "u_char"))
	return ("Ctypes.unsigned_char");
    else if (streq(type, "short"))
	return ("Ctypes.short");
    else if (streq(type, "float"))
	return ("REAL");
    else if (streq(type, "double"))
	return ("LONGREAL");
    else if (streq(type, "opaque"))
	return ("REF ARRAY OF CHAR");
    else if (streq(type, "~0"))
	return ("LAST(INTEGER)");
    else {
	if (isdigit(type[0]))
	    return type;
	else
	    return (MapToM3Name(type, defaultFlag));
    }
}

/*
 * Return the M3 source/sink method type for an rpcgen type.
 * Returns NULL if there is none.
 */
char *rpcgenBaseType(type)
     char *type;
{
    if (streq(type, "int") || streq(type, "long"))
	return ("Integer");
    else if (streq(type, "string"))
	return ("Text");
    else if (streq(type, "bool"))
	return ("Boolean");
    else if (streq(type, "char"))
	return ("Char");
    else if (streq(type, "u_int") ||
	     streq(type, "uint") ||
	     streq(type, "u_long") ||
	     streq(type, "ulong"))
	return ("Integer");
    else if (streq(type, "u_short") ||
	     streq(type, "ushort"))
	return ("Short");
    else if (streq(type, "u_char") ||
	     streq(type, "uchar"))
	return ("Char");
    else if (streq(type, "short"))
	return ("Short");
    else if (streq(type, "float"))
	return ("Real");
    else if (streq(type, "double"))
	return ("LongReal");
    else
	return (NULL);
}

/*
 * Return the appropriate array index number string, decremented by 1.
 * If array_max is a number then return that.
 * If array_max is an identifier then return the appropriate M3
 * identifier name (which may include a module name).
 */
char *ArrayMaxMinus1(numStr, defaultFlag)
     char *numStr;
     int defaultFlag;
{
    char *str;
    char buf[256];
    int n = atoi(numStr);
    if (n == 0) {
	str = MapToM3Type(numStr, defaultFlag);
	sprintf(buf, "%s - 1", str);
    }
    else {
	sprintf(buf, "%d", n - 1);
    }
    return (strdup(buf));
}


/*
 * Return the base type of a definition.
 */
definition *BaseType(def)
     definition *def;
{
    StringHashElement elem;

    while (def->def_kind == DEF_TYPEDEF) {
	elem = StringHashFind(SymbolNames, def->def.ty.old_type, find, NULL);
	if (elem == NULL) break;
	def = (definition *) (elem->userData);
    }
    return def;
}


/*
 * Return the base type of a struct definition.
 * If typeName is not the name of a struct defn then return NULL.
 */
definition *FindStructDefn(typeName)
{
    StringHashElement elem;
    definition *defStruct, *defStructOld;

    elem = StringHashFind(SymbolNames, typeName, find, NULL);
    if (elem != NULL) {
	defStruct = (definition *) (elem->userData);
	defStructOld = BaseType(defStruct);
	if (defStructOld->def_kind == DEF_STRUCT) {
	    if ((defStruct->def_kind == DEF_TYPEDEF) &&
		(defStruct->def.ty.rel != REL_ALIAS))
		return NULL;
	    else
		return (defStructOld);
	}
    }

    return NULL;
}


/*
 * Return a unique name that is different from any of the names in dl.
 */
char *FindUniqueName(name, dl)
     char *name;
     decl_list *dl;
{
    char *newname = name;
    int indx = 0;
    char buf[128];

  L:
    for (; dl != NULL; dl = dl->next) {
	if (streq(newname, dl->decl.name)) {
	    sprintf(buf, "%s%d", name, indx++);
	    newname = buf;
	    goto L;
	}
    }
    return (strdup(newname));
}


/*
 * Strip module name prefix off name if there is one.
 */
char *
StripPrefix(module, str)
     char *module;
     char *str;
{
    char *p;
    char namePrefix[128];
    int namePrefixLen;

    if (PFlag) {
	sprintf(namePrefix, "%s_", module);
	namePrefixLen = strlen(namePrefix);
	
	if (strncmp(str, namePrefix, namePrefixLen) == 0)
	    return (str + namePrefixLen);
	else
	    return str;
    }
    else {
	return str;
    }
}
