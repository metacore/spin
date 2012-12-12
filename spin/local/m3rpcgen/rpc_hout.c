/* @(#)rpc_hout.c	2.1 88/08/01 4.0 RPCSRC */
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
static char sccsid[] = "@(#)rpc_hout.c 1.6 87/07/28 (C) 1987 SMI";
#endif

/*
 * rpc_hout.c, Header file outputter for the RPC protocol compiler 
 * Copyright (C) 1987, Sun Microsystems, Inc. 
 */
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "rpc_util.h"
#include "rpc_parse.h"


void
print_datadef(def)
	definition *def;
{
	switch (def->def_kind) {
	case DEF_STRUCT:
		pstructdef(def);
		break;
	case DEF_UNION:
		puniondef(def);
		break;
	case DEF_ENUM:
		penumdef(def);
		break;
	case DEF_TYPEDEF:
		ptypedef(def);
		break;
	case DEF_PROGRAM:
		pprogramdef(def);
		break;
	case DEF_CONST:
		pconstdef(def);
		break;
	}
}

static
pconstdef(def)
	definition *def;
{
	f_print(fout, "CONST %s = %s;\n", def->def_name, def->def.co);
}

static
pstructdef(def)
	definition *def;
{
	decl_list *l;
	char *name = def->def_name;

	f_print(fout, "\nTYPE %s = \n", name);
	f_print(fout, "\tRECORD\n");
	for (l = def->def.st.decls; l != NULL; l = l->next) {
		pdeclaration(name, &l->decl, 8);
	}
	f_print(fout, "\tEND;\n");
}

static
puniondef(def)
	definition *def;
{
	case_list *l;
	char *name = def->def_name;
	declaration *decl;
	char *defaultPrefix = "";

	f_print(fout, "\nTYPE %s = OBJECT\n", name);
	decl = &def->def.un.enum_decl;
	if (streq(decl->type, "bool")) {
		f_print(fout, "\t%s: BOOLEAN;\n", decl->name);
	} else {
		f_print(fout, "\t%s: %s;\n",
			decl->name, MapToM3Type(decl->type, TRUE));
	}
	f_print(fout, "\tEND;\n");
	for (l = def->def.un.cases; l != NULL; l = l->next) {
	    f_print(fout, "\t%s_%s =\n", name, l->case_name);
	    f_print(fout, "\t    %s BRANDED \"%s_%s_%s\" OBJECT\n",
		    name, svcName, name, l->case_name);
	    if (!streq(l->case_decl.type, "void"))
		pdeclaration(name, &l->case_decl, 16);
	    f_print(fout, "\t\tEND;\n");
	    if (streq(l->case_name, "Default"))
		defaultPrefix = "_";
	}
	decl = def->def.un.default_decl;
	f_print(fout, "\t%s_%sDefault =\n", name, defaultPrefix);
	f_print(fout, "\t    %s BRANDED \"%s_%s_%sDefault\" OBJECT\n",
		name, svcName, name, defaultPrefix);
	if (decl && !streq(decl->type, "void"))
	    pdeclaration(name, decl, 16);
	f_print(fout, "\t\tEND;\n");
}

static
pprogramdef(def)
	definition *def;
{
    char *progName;
    char progNum[64];
    version_list *vers;
    proc_list *proc;
    char progNumName[128];
    definition *defStruct;
    decl_list *dl;
    extern definition *FindStructDefn();

    strcpy(progNum, def->def.pr.prog_num);
    strcpy(progNumName, def->def_name);
    if (streq(progNumName, svcName))
	strcpy(progNumName, "prognum");
    else
	strcat(progNumName, "_prognum");
    if ((progNum[0] == '0') && (progNum[1] == 'x'))
	f_print(fout, "\nCONST %s = 16_%s;\n", progNumName, &(progNum[2]));
    else
	f_print(fout, "\nCONST %s = %s;\n", progNumName, progNum);
    for (vers = def->def.pr.versions; vers != NULL; vers = vers->next) {
	f_print(fout, "\nCONST %s_versnum = %s;\n", vers->vers_name, vers->vers_num);
	progName = vers->vers_name;
	f_print(fout, "\nTYPE %s = OBJECT\n", progName);
	f_print(fout, "\tMETHODS\n");
	for (proc = vers->procs; proc != NULL; proc = proc->next) {
	    f_print(fout, "\t  %s(", proc->proc_name);
	    if (ExpandProcArgs) {
		defStruct = FindStructDefn(proc->arg_type);
	    }
	    if (ExpandProcArgs && defStruct != NULL &&
		defStruct->def_kind == DEF_STRUCT) {
		f_print(fout, "\n");
		for (dl = defStruct->def.st.decls;
		     dl != NULL;
		     dl = dl->next) {
		    pdeclaration(dl->decl.name, &(dl->decl), 16);
		}
		f_print(fout, "\t\t)");
	    }
	    else {
		if (!streq(proc->arg_type, "void"))
		    f_print(fout, "READONLY inParm: %s)",
			    MapToM3Type(proc->arg_type, TRUE));
		else
		    f_print(fout, ")");
	    }
	    if (!streq(proc->res_type, "void"))
		f_print(fout, ": %s", MapToM3Type(proc->res_type, TRUE));
	    f_print(fout, "\n\t\tRAISES {%s};\n", RPCExceptions);
	}
	f_print(fout, "\tEND;\n\n");

	f_print(fout,
		"TYPE %sClient = %s OBJECT\n", progName, progName);
	f_print(fout, "\tMETHODS\n\t  GetClient(): RPCSun.Client;\n\tEND;\n\n");
	if (RecoveryFlag) {
	    f_print(fout,
		    "PROCEDURE Import%s(b: RPCSun.BindingInfo;\n",
		    progName);
	    f_print(fout, "\t\trProc: RecoveryProc := NIL): %s\n", progName);
	    f_print(fout, "\t\tRAISES {%s};\n\n", RPCExceptions);
	}
	else {
	    f_print(fout,
		    "PROCEDURE Import%s(b: RPCSun.BindingInfo): %s\n",
		    progName, progName);
	    f_print(fout, "\t\tRAISES {%s};\n\n", RPCExceptions);
	}
	f_print(fout,
		"PROCEDURE Get%sServerProc(o: %s): RPCSun.ServerProc;\n\n",
		progName, progName);
    }
}

static
penumdef(def)
	definition *def;
{
	char *name = def->def_name;
	enumval_list *l = def->def.en.vals;
	char buf[64];
	int i, val, minVal, maxVal, nVals, prevVal, col, ncol, w;
	int nonfirst = 0;

	if (l->assignment == NULL) {
	    l->assignment = "0";
	}
	prevVal = 0;
	minVal = maxVal = atoi(l->assignment);
	nVals = 1;
	for (; l != NULL; l = l->next) {
	    if (l->assignment == NULL) {
		sprintf(buf, "%d", prevVal + 1);
		l->assignment = strdup(buf);
	    }
	    prevVal = atoi(l->assignment);
	    if (prevVal < minVal) minVal = prevVal;
	    if (prevVal > maxVal) maxVal = prevVal;
	    nVals++;
	}

	f_print(fout, "\nTYPE %s = [%d..%d];\n", name, minVal, maxVal);
	f_print(fout, "CONST\n");
	for (l = def->def.en.vals; l != NULL; l = l->next) {
	    f_print(fout, "\t%s_%s = %s;\n", name, l->name, l->assignment);
	}
	
	if ( nVals / (maxVal - ((float)minVal) + 1) < 0.1 ) return;
	
	f_print(fout, "\n\t%s_names = ARRAY [%d..%d] OF TEXT{",
	    name, minVal, maxVal);
	col = 100;
	for (i = minVal; i <= maxVal; i++) {
	    for (l = def->def.en.vals; l != NULL; l = l->next) {
		val = atoi(l->assignment);
		if (val == i) {
		    if (nonfirst) {f_print(fout, ", "); col += 2; }
		    w = strlen(l->name);
		    ncol = col + 2 + w;
		    if (ncol > 75) {f_print(fout, "\n\t\t"); ncol = 18 + w;}
		    f_print(fout, "\"%s\"", l->name);
		    goto gotit;
		}
	    }
	    if (nonfirst) {f_print(fout, ", "); col += 2; }
	    ncol = col + 3;
	    if (ncol > 75) {f_print(fout, "\n\t\t"); ncol = 19;}
	    f_print(fout, "NIL");
	    gotit:
	    col = ncol; nonfirst = 1;
	}
	f_print(fout, "};\n");
}

static
ptypedef(def)
	definition *def;
{
	char *name = def->def_name;
	char *old = def->def.ty.old_type;
	relation rel = def->def.ty.rel;


	f_print(fout, "\nTYPE ");
	if (!streq(name, old)) {
		if (streq(old, "string")) {
			old = "TEXT";
			rel = REL_ALIAS;
		} else if (streq(old, "opaque")) {
			old = "CHAR";
		} else if (streq(old, "bool")) {
			old = "BOOLEAN";
		}
		else {
		        old = MapToM3Type(old, TRUE);
		}
		switch (rel) {
		case REL_ARRAY:
			f_print(fout, "%s = REF ARRAY OF %s;\n",
				name, old);
			break;
		case REL_POINTER:
			f_print(fout, "%s = REF %s;\n", name, old);
			break;
		case REL_VECTOR:
			f_print(fout, "%s = ARRAY [0..%s] OF %s;\n",
				name,
				ArrayMaxMinus1(def->def.ty.array_max, TRUE),
				old);
			break;
		case REL_ALIAS:
			f_print(fout, "%s = %s;\n", name, old);
			break;
		}
	}
}

/* ARGSUSED */
pdeclaration(name, dec, blank)
	char *name;
	declaration *dec;
	int blank;
{
	char *type;

	if (streq(dec->type, "void")) {
		return;
	}
	while (blank--) {
	    (void) fputc(' ', fout);
	}
	if (streq(dec->type, "string")) {
		f_print(fout, "%s: TEXT;\n", dec->name);
	} else {
		if (streq(dec->type, "opaque")) {
			type = "char";
		} else {
			type = dec->type;
		}
		type = MapToM3Type(type, TRUE);
		switch (dec->rel) {
		case REL_ALIAS:
			f_print(fout, "%s: %s;\n",
				dec->name, type);
			break;
		case REL_VECTOR:
			f_print(fout, "%s: ARRAY [0..%s] OF %s;\n",
				dec->name, ArrayMaxMinus1(dec->array_max, TRUE),
				type);
			break;
		case REL_POINTER:
			f_print(fout, "%s: REF %s;\n",
				dec->name, type);
			break;
		case REL_ARRAY:
			f_print(fout, "%s: REF ARRAY OF %s;\n",
				dec->name, type);
			break;
		}
	}
}
