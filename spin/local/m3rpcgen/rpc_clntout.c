/* @(#)rpc_clntout.c	2.1 88/08/01 4.0 RPCSRC */
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
static char sccsid[] = "@(#)rpc_clntout.c 1.2 87/06/24 (C) 1987 SMI";
#endif

/*
 * rpc_clntout.c, Client-stub outputter for the RPC protocol compiler
 * Copyright (C) 1987, Sun Microsytsems, Inc.
 */
#include <stdio.h>
#include <strings.h>
#include "rpc_parse.h"
#include "rpc_util.h"

/* ARGSUSED */
void
write_stubs(svcName)
     char *svcName;
{
	list *l;
	definition *def;

	for (l = CurrentContext->defined; l != NULL; l = l->next) {
		def = (definition *) l->val;
		if (def->def_kind == DEF_PROGRAM) {
			write_program(def);
		}
	}
}


static
EmitProcSignature(vp, proc, pvName, defStruct, o, recoverySuffix)
     version_list *vp;
     proc_list *proc;
     char *pvName;
     definition *defStruct;
     char *o;
     char *recoverySuffix;
{
    decl_list *dl;

    f_print(fout, "\n");
    f_print(fout,
	    "PROCEDURE %s%s%s(%s: %sPrivate",
	    proc->proc_name, vp->vers_num, recoverySuffix, o, pvName);
    if (ExpandProcArgs && defStruct != NULL &&
	defStruct->def_kind == DEF_STRUCT) {
	f_print(fout, ";\n");
	for (dl = defStruct->def.st.decls;
	     dl != NULL;
	     dl = dl->next) {
	    pdeclaration(dl->decl.name, &(dl->decl), 16);
	}
	f_print(fout, "\t\t)");
    }
    else {
	if (!streq(proc->arg_type, "void"))
	    f_print(fout, "; READONLY inParm: %s)",
		    MapToM3Type(proc->arg_type, TRUE));
	else
	    f_print(fout, ")");
    }
    if (!streq(proc->res_type, "void"))
	f_print(fout, ": %s\n", MapToM3Type(proc->res_type, TRUE));
    else
	f_print(fout, "\n");
    f_print(fout, "\t\tRAISES {%s} =\n", RPCExceptions);
}


EmitRecoveryProc(vp, proc, pvName, defStruct, o,
		 recoverySuffix, inArg, outArg)
     version_list *vp;
     proc_list *proc;
     char *pvName;
     definition *defStruct;
     char *o;
     char *recoverySuffix;
     int inArg, outArg;
{
    decl_list *dl;
    char *n = "n";

    if (ExpandProcArgs && (defStruct != NULL))
	n = FindUniqueName("n", defStruct->def.st.decls);

    EmitProcSignature(vp, proc, pvName, defStruct, o, recoverySuffix);
    f_print(fout, "  VAR %s: CARDINAL := 0;\n", n);
    f_print(fout, "  BEGIN\n");
    f_print(fout, "    LOOP\n");
    f_print(fout, "      INC(n);\n");
    f_print(fout, "      TRY\n");
    if (outArg)
	f_print(fout, "        RETURN ");
    else
	f_print(fout, "        ");
    f_print(fout, "%s%s(%s",
	    proc->proc_name, vp->vers_num, o);
    if (ExpandProcArgs && defStruct != NULL &&
	defStruct->def_kind == DEF_STRUCT) {
	f_print(fout, ",\n");
	for (dl = defStruct->def.st.decls;
	     dl != NULL;
	     dl = dl->next) {
	    f_print(fout, "\t\t%s", dl->decl.name);
	    if (dl->next != NULL)
		f_print(fout, ",\n");
	    else
		f_print(fout, ");\n");
	}
    }
    else {
	if (inArg)
	    f_print(fout, ", inParm);\n");
	else
	    f_print(fout, ");\n");
    }
    if (!outArg)
	f_print(fout, "        RETURN;\n");
    f_print(fout, "      EXCEPT\n");
    f_print(fout, "        RPC.Failed(arg) =>\n");
    f_print(fout, "          o.GetClient().Destroy();\n");
    f_print(fout, "          IF %s.Recovery # NIL THEN\n", o);
    f_print(fout, "            %s.cl := %s.Recovery(%s, n, arg);\n",
	    o, o, proc->proc_num);
    f_print(fout, "          ELSE\n");
    f_print(fout, "            RAISE RPC.Failed(arg);\n");
    f_print(fout, "          END;\n");
    f_print(fout, "      END;\n");
    f_print(fout, "    END;\n");
    f_print(fout, "  END %s%s%s;\n",
	    proc->proc_name, vp->vers_num, recoverySuffix);
}


static
write_program (def)
	definition *def;
{
    version_list *vp;
    proc_list *proc;
    char *pvName;
    int inArg, outArg;
    char *srcMethod;
    definition *defStruct;
    decl_list *dl;
    int expandFlag;
    extern definition *FindStructDefn();
    char *o, *tr, *so, *outParm;
    char si[128];
    char *recoverySuffix = "";
    char *mName;
    
    for (vp = def->def.pr.versions; vp != NULL; vp = vp->next) {
	pvName = vp->vers_name;
	f_print(fout, "\nTYPE %sPrivate =\n", pvName);
	f_print(fout, "\t%sClient OBJECT\n", pvName);
	f_print(fout, "\t\t  mu: MUTEX := NIL;\n");
	f_print(fout, "\t\t  cl: RPCSun.Client := NIL;\n");
	if (RecoveryFlag) {
	    f_print(fout, "\t\t  Recovery: RecoveryProc := NIL;\n");
	    recoverySuffix = "_Recoverable";
	}
	f_print(fout, "\tOVERRIDES\n");
	for (proc = vp->procs; proc != NULL; proc = proc->next) {
	    f_print(fout, "\t\t  %s := %s%s%s;\n",
		    proc->proc_name,
		    proc->proc_name, vp->vers_num,
		    recoverySuffix);
	}
	f_print(fout, "\t\t  GetClient := %sGetClient;\n",pvName);
	f_print(fout, "\t\tEND;\n\n");

	if (RecoveryFlag) {
	    f_print(fout,
		    "PROCEDURE Import%s(b: RPCSun.BindingInfo;\n",
		    pvName);
	    f_print(fout, "\t\trProc: RecoveryProc := NIL): %s\n",
		    pvName);
	    f_print(fout, "  RAISES {%s} =\n", RPCExceptions);
	    f_print(fout, "  BEGIN\n");
	    f_print(fout,
		    "    RETURN NEW(%sPrivate,\n",
		    pvName);
	    f_print(fout, "\t\tmu := NEW(MUTEX),\n");
	    f_print(fout, "\t\tcl := RPCSun.ImportService(b),\n");
	    f_print(fout, "\t\tRecovery := rProc);\n");
	    f_print(fout, "  END Import%s;\n\n", pvName);
	}
	else {
	    f_print(fout,
		    "PROCEDURE Import%s(b: RPCSun.BindingInfo): %s\n",
		    pvName,	pvName);
	    f_print(fout, "  RAISES {%s} =\n", RPCExceptions);
	    f_print(fout, "  BEGIN\n");
	    f_print(fout,
		    "    RETURN NEW(%sPrivate,\n", pvName);
	    f_print(fout, "\t\tmu := NEW(MUTEX),\n");
	    f_print(fout, "\t\tcl := RPCSun.ImportService(b));\n");
	    f_print(fout, "  END Import%s;\n\n", pvName);
	}
	
	f_print(fout,
		"PROCEDURE %sGetClient(o: %sPrivate): RPCSun.Client =\n",
		pvName,	pvName);
	f_print(fout, "  BEGIN\n");
	f_print(fout, "    RETURN o.cl;\n");
	f_print(fout, "  END %sGetClient;\n",	pvName);
	
	for (proc = vp->procs; proc != NULL; proc = proc->next) {
	    expandFlag = FALSE;
	    o = "o";
	    tr = "tr";
	    so = "so";
	    outParm = "outParm";
	    if (ExpandProcArgs) {
		defStruct = FindStructDefn(proc->arg_type);
		if (defStruct != NULL) {
		    o = FindUniqueName("o", defStruct->def.st.decls);
		    tr = FindUniqueName("tr", defStruct->def.st.decls);
		    so = FindUniqueName("so", defStruct->def.st.decls);
		    outParm = FindUniqueName("outParm", defStruct->def.st.decls);
		}
	    }
	    sprintf(si, "%s.sink", tr);
	    if (ExpandProcArgs && defStruct != NULL &&
		defStruct->def_kind == DEF_STRUCT) {
		inArg = TRUE;
		expandFlag = TRUE;
	    }
	    else {
		if (!streq(proc->arg_type, "void"))
		    inArg = TRUE;
		else
		    inArg = FALSE;
	    }
	    if (!streq(proc->res_type, "void"))
		outArg = TRUE;
	    else
		outArg = FALSE;

	    if (RecoveryFlag) {
		EmitRecoveryProc(vp, proc, pvName, defStruct, o,
				 recoverySuffix,
				 inArg, outArg);
	    }

	    EmitProcSignature(vp, proc, pvName, defStruct, o, "");
	    f_print(fout, "  <* FATAL RPCSun.Erred *>\n");
	    f_print(fout, "  VAR\n");
	    f_print(fout, "    %s: RPCSun.Transaction;\n", tr);
	    f_print(fout, "    %s: XDR.Source;\n", so);
	    if (outArg)
		f_print(fout, "    %s: %s;\n", outParm,
			MapToM3Type(proc->res_type, TRUE));
	    f_print(fout, "  BEGIN\n");
	    f_print(fout, "    LOCK %s.mu DO\n", o);
	    f_print(fout, "      %s := %s.cl.StartCall(%s);\n", tr, o,
		    proc->proc_num);
	    if (inArg) {
		f_print(fout, "      TRY\n");
		if (expandFlag) {
		    for (dl = defStruct->def.st.decls;
			 dl != NULL;
			 dl = dl->next) {
			emit_put_stat(&(dl->decl), "    ", "", FALSE, si);
		    }
		}
		else {
		    srcMethod = rpcgenBaseType(proc->arg_type);
		    if (srcMethod != NULL)
			f_print(fout,
				"      XDR.Put%s(%s, inParm);\n",
				srcMethod, si);
		    else {
			mName = ModuleName(proc->arg_type);
			f_print(fout,
				"      %s_x.Put_%s(%s, inParm);\n",
				mName, StripPrefix(mName, proc->arg_type), si);
		    }
		}
		f_print(fout, "      EXCEPT\n");
		f_print(fout, "        XDR.Failed(e) =>\n");
		f_print(fout, "          RAISE\n");
		f_print(fout, "            RPC.Failed(NEW(RPC.ZeroTimesFailure,\n");
		f_print(fout, "              info := \"arg marshalling failed\",\n");
		f_print(fout, "              subArg := e));\n");
		f_print(fout, "      END;\n");
	    }
	    f_print(fout, "      %s := %s.cl.SendCall(%s);\n", so, o, tr);
	    if (outArg) {
		f_print(fout, "      TRY\n");
		srcMethod = rpcgenBaseType(proc->res_type);
		if (srcMethod != NULL)
		    f_print(fout,
			    "        %s := XDR.Get%s(%s);\n", outParm, srcMethod,
			    so);
		else {
		    mName = ModuleName(proc->res_type);
		    f_print(fout,
			    "        %s_x.Get_%s(%s, %s);\n",
			    mName, StripPrefix(mName,proc->res_type),
			    so, outParm);
		}
		f_print(fout, "      EXCEPT\n");
		f_print(fout, "        XDR.Failed(e) =>\n");
		f_print(fout, "          RAISE\n");
		f_print(fout, "            RPC.Failed(NEW(RPC.Failure,\n");
		f_print(fout, "              info := \"result marshalling failed\",\n");
		f_print(fout, "              subArg := e));\n");
		f_print(fout, "      END;\n");
	    }
	    f_print(fout, "      %s.cl.EndCall(%s);\n", o, tr);
	    f_print(fout, "    END;\n");
	    if (outArg)
		f_print(fout, "    RETURN %s;\n", outParm);
	    f_print(fout, "  END %s%s;\n",
		    proc->proc_name, vp->vers_num);
	}
    }
}
