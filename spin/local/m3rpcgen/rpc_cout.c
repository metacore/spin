/* @(#)rpc_cout.c	2.1 88/08/01 4.0 RPCSRC */
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
static char sccsid[] = "@(#)rpc_cout.c 1.8 87/06/24 (C) 1987 SMI";
#endif

/*
 * rpc_cout.c, XDR routine outputter for the RPC protocol compiler 
 * Copyright (C) 1987, Sun Microsystems, Inc. 
 */
#include <stdio.h>
#include <strings.h>
#include "rpc_util.h"
#include "rpc_parse.h"

char *XDRExceptions = "XDR.Failed, Thread.Alerted";

/*
 * Emit the interface for the given definition 
 */
void
emitDefn(def)
     definition *def;
{
    if (def->def_kind == DEF_PROGRAM || def->def_kind == DEF_CONST) {
	return;
    }
    f_print(fout,
	    "PROCEDURE Get_%s(so: XDR.Source; VAR v: %s.%s)\n\t\tRAISES {%s};\n",
	    def->def_name, svcName,
	    def->def_name, XDRExceptions);
    f_print(fout,
	    "PROCEDURE Put_%s(si: XDR.Sink; READONLY v: %s.%s)\n\t\tRAISES {%s};\n\n",
	    def->def_name, svcName,
	    def->def_name, XDRExceptions);
}

/*
 * Emit the implementation for the given definition 
 */
void
emitImpl(def)
     definition *def;
{
    if (def->def_kind == DEF_PROGRAM || def->def_kind == DEF_CONST) {
	return;
    }
    f_print(fout,
	    "PROCEDURE Get_%s(so: XDR.Source; VAR v: %s.%s)\n",
	    def->def_name, svcName, def->def_name);
    f_print(fout, "\t\tRAISES {%s} =\n", XDRExceptions);
    switch (def->def_kind) {
      case DEF_UNION:
	emit_union_get(def);
	break;
      case DEF_ENUM:
	emit_enum_get(def);
	break;
      case DEF_STRUCT:
	emit_struct_get(def);
	break;
      case DEF_TYPEDEF:
	emit_typedef_get(def);
	break;
    }
    f_print(fout, "  END Get_%s;\n\n", def->def_name, def->def_name);

    f_print(fout,
	    "PROCEDURE Put_%s(si: XDR.Sink; READONLY v: %s.%s)\n",
	    def->def_name, svcName, def->def_name);
    f_print(fout, "\t\tRAISES {%s} =\n", XDRExceptions);
    switch (def->def_kind) {
      case DEF_UNION:
	emit_union_put(def);
	break;
      case DEF_ENUM:
	emit_enum_put(def);
	break;
      case DEF_STRUCT:
	emit_struct_put(def);
	break;
      case DEF_TYPEDEF:
	emit_typedef_put(def);
	break;
    }
    f_print(fout, "  END Put_%s;\n\n", def->def_name, def->def_name);
}


/* ARGSUSED */
static
emit_enum_get(def)
	definition *def;
{
    f_print(fout, "  BEGIN\n");
    f_print(fout, "    v := XDR.GetInteger(so);\n");
}

/* ARGSUSED */
static
emit_enum_put(def)
	definition *def;
{
    f_print(fout, "  BEGIN\n");
    f_print(fout, "    XDR.PutInteger(si, v);\n");
}

static
emit_union_get(def)
	definition *def;
{
    case_list *cl;
    declaration *cs, *dflt;
    char *srcMethod;
    char *defaultPrefix = "";
    char *firstFlag = " ";

    f_print(fout, "  VAR\n");
    f_print(fout, "    disc: %s;\n",
	    MapToM3Type(def->def.un.enum_decl.type, FALSE));
    f_print(fout, "  BEGIN\n");

    srcMethod = rpcgenBaseType(def->def.un.enum_decl.type);
    if (srcMethod != NULL)
	f_print(fout, "    disc := XDR.Get%s(so);\n", srcMethod);
    else
	f_print(fout, "    %sGet_%s(so, disc);\n",
		GetPutModuleName(def->def.un.enum_decl.type, TRUE),
		def->def.un.enum_decl.type);

    f_print(fout, "    CASE disc OF\n");
    for (cl = def->def.un.cases; cl != NULL; cl = cl->next) {
	cs = &cl->case_decl;
	if (streq(cl->case_name, "Default")) defaultPrefix = "_";
	if (srcMethod != NULL) {
	    if (streq(srcMethod, "Integer") || streq(srcMethod, "Short"))
		f_print(fout, "      %s %s =>\n", firstFlag, cl->case_name);
	    else
		f_print(fout, "      %s %s =>\n", firstFlag, cl->case_name);
	}
	else
	    f_print(fout, "      %s %s.%s_%s =>\n",
		    firstFlag, svcName,
		    def->def.un.enum_decl.type, cl->case_name);
	f_print(fout, "          VAR w := NEW(%s.%s_%s);\n",
		svcName, def->def_name, cl->case_name);
	f_print(fout, "          BEGIN\n");
	if (!streq(cs->type, "void"))
	    emit_get_stat(cs, "        ", "w", TRUE, "so", "len");
	f_print(fout, "          v := w;\n");
	f_print(fout, "          END;\n");
	firstFlag = "|";
    }
    dflt = def->def.un.default_decl;
    if (dflt != NULL) {
	f_print(fout, "      ELSE\n");
	f_print(fout, "        VAR w := NEW(%s.%s_%sDefault);\n",
		svcName, def->def_name, defaultPrefix);
	f_print(fout, "        BEGIN\n");
	if (!streq(dflt->type, "void"))
	    emit_get_stat(dflt, "      ", "w", TRUE, "so", "len");
	f_print(fout, "        v := w;\n");
	f_print(fout, "        END;\n");
    } else {
	f_print(fout, "        v := NEW(%s.%s);\n", svcName, def->def_name);
    }
    
    f_print(fout, "    END;\n");
    f_print(fout, "    v.%s := disc;\n", def->def.un.enum_decl.name);
}

static
emit_union_put(def)
	definition *def;
{
    case_list *cl;
    declaration *cs, *dflt;
    char *srcMethod;
    char *defaultPrefix = "";
    char *firstFlag = " ";

    f_print(fout, "  BEGIN\n");
    
    srcMethod = rpcgenBaseType(def->def.un.enum_decl.type);
    if (srcMethod != NULL)
	f_print(fout, "    XDR.Put%s(si, v.%s);\n",
		srcMethod, def->def.un.enum_decl.name);
    else
	f_print(fout, "    %sPut_%s(si, v.%s);\n",
		GetPutModuleName(def->def.un.enum_decl.type, TRUE),
		def->def.un.enum_decl.type,
		def->def.un.enum_decl.name);

    f_print(fout, "    TYPECASE v OF\n");
    for (cl = def->def.un.cases; cl != NULL; cl = cl->next) {
	cs = &cl->case_decl;
	firstFlag = "|";
	if (!streq(cs->type, "void")) {
	    f_print(fout, "      %s %s.%s_%s (v) =>\n",
		    firstFlag, svcName,
		    def->def_name, cl->case_name);
	    emit_put_stat(cs, "      ", "v", TRUE, "si");
	}
	else {
	    f_print(fout, "      %s %s.%s_%s =>\n",
		    firstFlag, svcName,
		    def->def_name, cl->case_name);
	}
    }
    dflt = def->def.un.default_decl;
    if (dflt != NULL) {
	if (!streq(dflt->type, "void")) {
	    f_print(fout, "      %s %s.%s_%sDefault (v) =>\n",
		    firstFlag, svcName,
		    def->def_name, defaultPrefix);
	    emit_put_stat(dflt, "      ", "v", TRUE, "si");
	}
	else {
	    f_print(fout, "      %s %s.%s_%sDefault =>\n",
		    firstFlag, svcName,
		    def->def_name, defaultPrefix);
	}
    }
    f_print(fout, "    ELSE\n");
    f_print(fout, "      RAISE XDR.Failed(NEW(XDR.Failure,\n");
    f_print(fout, "          info := \"Bad object subtype encountered.\"));\n");
    f_print(fout, "    END;\n");
}

static
emit_struct_get(def)
	definition *def;
{
    decl_list *dl;
    
    f_print(fout, "  BEGIN\n");
    for (dl = def->def.st.decls; dl != NULL; dl = dl->next) {
	emit_get_stat(&(dl->decl), "", "v", TRUE, "so", "len");
    }
}

emit_get_stat(dec, indent, varName, defaultFlag, so, len)
     declaration *dec;
     char *indent;
     char *varName;
     int defaultFlag;		/* TRUE => svcName is default in the output
				   file. */
     char *so, *len;		/* source and temp. length variable names */
{
    char decName[128];
    char *srcMethod;
    char modifier[8];
    int endFlag = FALSE;
    relation rel;

    if (dec->name == NULL || streq(dec->name, ""))
	strcpy(decName, "");
    else if (streq(varName, ""))
	strcpy(decName, dec->name);
    else
	sprintf(decName, ".%s", dec->name);

    if (streq(dec->type, "opaque")) {
	if (dec->rel == REL_VECTOR) {
	    f_print(fout, "%s    XDR.GetBytes(%s, %s%s);\n",
		    indent, so, varName, decName);
	}
	else {
	    f_print(fout, "%s    WITH %s = XDR.GetInteger(%s) DO\n",
		    indent, len, so);
	    f_print(fout, "%s      IF %s < 0 OR %s > %s THEN\n",
		    indent, len, len, MapToM3Type(dec->array_max, FALSE));
	    f_print(fout, "%s        RAISE XDR.Failed(NEW(XDR.Failure,\n",
		    indent);
	    f_print(fout, "%s                             info := \"Array bounds error\"));\n",
		    indent);
	    f_print(fout, "%s      END;\n", indent);
	    f_print(fout, "%s      %s%s := NEW(REF ARRAY OF CHAR, %s);\n",
		    indent, varName, decName, len);
	    f_print(fout, "%s    END;\n", indent);
	    f_print(fout, "%s    XDR.GetBytes(%s, %s%s^);\n",
		    indent, so, varName, decName);
	}
	return;
    }

    srcMethod = rpcgenBaseType(dec->type);
    rel = streq(dec->type, "string") ? REL_ALIAS : dec->rel;
    switch (rel) {
      case REL_POINTER:
	strcpy(modifier, "^");
	f_print(fout, "%s    IF NOT XDR.GetBoolean(%s) THEN\n", indent, so);
	f_print(fout, "%s      %s%s := NIL;\n", indent, varName, decName);
	f_print(fout, "%s    ELSE\n", indent);
	f_print(fout, "%s      %s%s := NEW(REF %s);\n",
		indent, varName, decName,
		MapToM3Type(dec->type, !defaultFlag));
	endFlag = TRUE;
	break;
      case REL_VECTOR:
	strcpy(modifier, "[i]");
	f_print(fout, "%s    FOR i := 0 TO %s DO\n",
		indent, ArrayMaxMinus1(dec->array_max, FALSE));
	endFlag = TRUE;
	break;
      case REL_ARRAY:
	strcpy(modifier, "^[i]");
	f_print(fout, "%s    WITH %s = XDR.GetInteger(%s) DO\n",
		indent, len, so);
	f_print(fout, "%s      IF %s < 0 OR %s > %s THEN\n",
		indent, len, len, MapToM3Type(dec->array_max, FALSE));
	f_print(fout, "%s        RAISE XDR.Failed(NEW(XDR.Failure,\n", indent);
	f_print(fout, "%s                             info := \"Array bounds error\"));\n",
		indent);
	f_print(fout, "%s      END;\n", indent);
	f_print(fout, "%s      %s%s := NEW(REF ARRAY OF %s, %s);\n",
		indent, varName, decName,
		MapToM3Type(dec->type, !defaultFlag), len);
	f_print(fout, "%s    END;\n", indent);
	f_print(fout, "%s    FOR i := 0 TO NUMBER(%s%s^) - 1 DO\n",
		indent, varName, decName);
	endFlag = TRUE;
	break;
      case REL_ALIAS:
	strcpy(modifier, "");
	break;
    }

    if (srcMethod != NULL) {
	if (streq(dec->type, "u_char"))
	    f_print(fout, "%s%s    %s%s%s := ORD(XDR.Get%s(%s));\n",
		indent, endFlag ? "  " : "",
		varName, decName, modifier, srcMethod, so);
	else
	    f_print(fout, "%s%s    %s%s%s := XDR.Get%s(%s);\n",
		    indent, endFlag ? "  " : "",
		    varName, decName, modifier, srcMethod, so);
    }
    else {
	f_print(fout, "%s%s    %sGet_%s(%s, %s%s%s);\n",
		indent, endFlag ? "  " : "",
		GetPutModuleName(dec->type, defaultFlag), dec->type,
		so, varName, decName, modifier);
    }

    if (endFlag) f_print(fout, "%s    END;\n", indent);
}

static
emit_struct_put(def)
	definition *def;
{
    decl_list *dl;
    
    f_print(fout, "  BEGIN\n");
    for (dl = def->def.st.decls; dl != NULL; dl = dl->next) {
	emit_put_stat(&(dl->decl), "", "v", TRUE, "si");
    }
}

emit_put_stat(dec, indent, varName, defaultFlag, si)
     declaration *dec;
     char *indent;
     char *varName;
     int defaultFlag;		/* TRUE => svcName is default in the
				   output file. */
     char *si;
{
    char decName[128];
    char *srcMethod;
    char modifier[8];
    char *endFlag = "";
    relation rel;

    if (dec->name == NULL || streq(dec->name, ""))
	strcpy(decName, "");
    else if (streq(varName, ""))
	strcpy(decName, dec->name);
    else
	sprintf(decName, ".%s", dec->name);

    if (streq(dec->type, "opaque")) {
	if (dec->rel == REL_VECTOR)
	    f_print(fout, "%s    XDR.PutBytes(%s, %s%s);\n",
		    indent, si, varName, decName);
	else {
	    f_print(fout, "%s    IF %s%s = NIL THEN\n",
		    indent, varName, decName);
	    f_print(fout, "%s      XDR.PutInteger(%s, 0);\n", indent, si);
	    f_print(fout, "%s    ELSIF NUMBER(%s%s^) > %s THEN\n",
		    indent, varName, decName,
		    MapToM3Type(dec->array_max, FALSE));
	    f_print(fout, "%s      RAISE XDR.Failed(NEW(XDR.Failure,\n", indent);
	    f_print(fout, "%s                           info := \"Array bounds error\"));\n",
		    indent);
	    f_print(fout, "%s    ELSE\n", indent);
	    f_print(fout, "%s      XDR.PutInteger(%s, NUMBER(%s%s^));\n",
		    indent, si, varName, decName);
	    f_print(fout, "%s      XDR.PutBytes(%s, %s%s^);\n",
		    indent, si, varName, decName);
	    f_print(fout, "%s    END;\n", indent);
	}
	return;
    }

    srcMethod = rpcgenBaseType(dec->type);
    rel = streq(dec->type, "string") ? REL_ALIAS : dec->rel;
    switch (rel) {
      case REL_POINTER:
	strcpy(modifier, "^");
	f_print(fout, "%s    IF %s%s = NIL THEN\n", indent, varName, decName);
	f_print(fout, "%s      XDR.PutBoolean(%s, FALSE);\n", indent, si);
	f_print(fout, "%s    ELSE\n", indent);
	f_print(fout, "%s      XDR.PutBoolean(%s, TRUE);\n", indent, si);
	endFlag = "  ";
	break;
      case REL_VECTOR:
	strcpy(modifier, "[i]");
	f_print(fout, "%s    FOR i := 0 TO %s DO\n",
		indent, ArrayMaxMinus1(dec->array_max, FALSE));
	endFlag = "  ";
	break;
      case REL_ARRAY:
	strcpy(modifier, "^[i]");
	f_print(fout, "%s    IF %s%s = NIL THEN\n", indent, varName, decName);
	f_print(fout, "%s      XDR.PutInteger(%s, 0);\n", indent, si);
	f_print(fout, "%s    ELSIF NUMBER(%s%s^) > %s THEN\n",
		indent, varName, decName,
		MapToM3Type(dec->array_max, FALSE));
	f_print(fout, "%s      RAISE XDR.Failed(NEW(XDR.Failure,\n", indent);
	f_print(fout, "%s                           info := \"Array bounds error\"));\n",
		indent);
	f_print(fout, "%s    ELSE\n", indent);
	f_print(fout, "%s      XDR.PutInteger(%s, NUMBER(%s%s^));\n",
		indent, si, varName, decName);
	f_print(fout, "%s      FOR i := 0 TO NUMBER(%s%s^) - 1 DO\n",
		indent, varName, decName);
	endFlag = "    ";
	break;
      case REL_ALIAS:
	strcpy(modifier, "");
	break;
    }

    if (srcMethod != NULL) {
	if (streq(dec->type, "u_char"))
	    f_print(fout, "%s%s    XDR.Put%s(%s, VAL(%s%s%s, CHAR));\n",
		indent, endFlag, srcMethod,
		si, varName, decName, modifier);
	else
	    f_print(fout, "%s%s    XDR.Put%s(%s, %s%s%s);\n",
		    indent, endFlag, srcMethod,
		    si, varName, decName, modifier);
    }
    else {
	f_print(fout, "%s%s    %sPut_%s(%s, %s%s%s);\n",
		indent, endFlag,
		GetPutModuleName(dec->type, defaultFlag), dec->type, si,
		varName, decName, modifier);
    }

    if (strlen(endFlag) == 2) f_print(fout, "%s    END;\n", indent);
    else if (strlen(endFlag) == 4) {
	f_print(fout, "%s      END;\n", indent);
	f_print(fout, "%s    END;\n", indent);
    }
}

static
emit_typedef_get(def)
	definition *def;
{
    declaration decRec;

    decRec.prefix = def->def.ty.old_prefix;
    decRec.type = def->def.ty.old_type;
    decRec.name = NULL;
    decRec.rel = def->def.ty.rel;
    decRec.array_max = def->def.ty.array_max;
    f_print(fout, "  BEGIN\n");
    emit_get_stat(&decRec, "", "v", TRUE, "so", "len");
}

static
emit_typedef_put(def)
	definition *def;
{
    declaration decRec;

    decRec.prefix = def->def.ty.old_prefix;
    decRec.type = def->def.ty.old_type;
    decRec.name = NULL;
    decRec.rel = def->def.ty.rel;
    decRec.array_max = def->def.ty.array_max;
    f_print(fout, "  BEGIN\n");
    emit_put_stat(&decRec, "", "v", TRUE, "si");
}
