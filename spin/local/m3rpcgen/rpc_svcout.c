/* @(#)rpc_svcout.c	2.1 88/08/01 4.0 RPCSRC */
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
static char sccsid[] = "@(#)rpc_svcout.c 1.6 87/06/24 (C) 1987 SMI";
#endif

/*
 * rpc_svcout.c, Server-skeleton outputter for the RPC protocol compiler
 * Copyright (C) 1987, Sun Microsytsems, Inc. 
 */
#include <stdio.h>
#include <strings.h>
#include <string.h>
#include "rpc_parse.h"
#include "rpc_util.h"


/*
 * Write the objects and procedures for a particular version of a particular
 * program.
 */
/* ARGSUSED */
void
write_server_prog(svcName, progName, vers)
     char *svcName;
     char *progName;
     version_list *vers;
{
    proc_list *proc;
    char *firstFlag = "|";
    char *srcMethod;
    char *inParm;
    int nullProcFlag = TRUE;	/* Assume we need to generate our own
				   NullProc. */
    definition *defStruct;
    decl_list *dl;
    int expandFlag;
    extern definition *FindStructDefn();
    char *serviceRoutines, *s, *so, *outParm, *outParmEq, *si, *len;
    char buf[128];
    char *mName;

    f_print(fout, "TYPE %sServerProc = RPCSun.ServerProc\n",
	    progName);
    f_print(fout, "\tOBJECT\n");
    f_print(fout, "\t\tserviceRoutines: %s;\n", progName);
    f_print(fout, "\tOVERRIDES\n");
    f_print(fout, "\t\tHandleCall := %s_proc;\n", progName);
    f_print(fout, "\tEND;\n\n");

    f_print(fout, "PROCEDURE Get%sServerProc(o: %s): RPCSun.ServerProc =\n",
	    progName, progName);
    f_print(fout, "  BEGIN\n");
    f_print(fout, "    RETURN NEW(%sServerProc, serviceRoutines := o);\n",
	    progName);
    f_print(fout, "  END Get%sServerProc;\n\n", progName);

    f_print(fout,
	    "PROCEDURE %s_proc(self: %sServerProc; s: RPCSun.Server; proc: RPCOS.UINT32; so: XDR.Source)\n",
	    progName, progName);
    f_print(fout, "\t\tRAISES {%s} =\n", RPCExceptions);
    f_print(fout, "  BEGIN\n");
    f_print(fout, "    TRY\n");
    f_print(fout, "      CASE proc OF\n");
    for (proc = vers->procs; proc != NULL; proc = proc->next) {
	if (streq(proc->proc_num, "0")) {
	    nullProcFlag = 0;
	    break;
	}
    }
    if (nullProcFlag) {
	f_print(fout, "	     0 => %sNullProc(self.serviceRoutines, s, so);\n",
		progName);
    }
    for (proc = vers->procs; proc != NULL; proc = proc->next) {
	f_print(fout, "	  %s %s => %s%s(self.serviceRoutines, s, so);\n",
		firstFlag, proc->proc_num,
		proc->proc_name, vers->vers_num);
    }
    f_print(fout, "      ELSE RAISE RPC.Failed(NEW(RPC.Failure,\n");
    f_print(fout, "                                info := \"Bad procedure number\"));\n");
    f_print(fout, "      END;\n");
    f_print(fout, "    EXCEPT\n");
    f_print(fout, "      XDR.Failed (e) =>\n");
    f_print(fout, "        RAISE RPC.Failed(NEW(RPC.Failure,\n");
    f_print(fout, "          info := \"Marshalling failure\", subArg := e));\n");
    f_print(fout, "    END;\n");
    f_print(fout, "  END %s_proc;\n\n", progName);

    if (nullProcFlag) {
	f_print(fout,
		"PROCEDURE %sNullProc(<*UNUSED*>serviceRoutines: %s;\n",
		progName, progName);
	f_print(fout, "    s: RPCSun.Server; <*UNUSED*>so: XDR.Source)\n");
	f_print(fout, "  RAISES {%s} =\n", RPCExceptions);
	f_print(fout, "  BEGIN\n");
	f_print(fout, "    EVAL s.StartReply();\n");
	f_print(fout, "  END %sNullProc;\n\n", progName);
    }
    for (proc = vers->procs; proc != NULL; proc = proc->next) {
	expandFlag = FALSE;
	serviceRoutines = "serviceRoutines";
	s = "s";
	so = "so";
	outParm = "outParm";
	si = "si";
	len = "len";
	if (ExpandProcArgs) {
	    defStruct = FindStructDefn(proc->arg_type);
	    if (defStruct != NULL) {
		serviceRoutines = FindUniqueName("serviceRoutines",
						 defStruct->def.st.decls);
		s = FindUniqueName("s", defStruct->def.st.decls);
		so = FindUniqueName("so", defStruct->def.st.decls);
		outParm = FindUniqueName("outParm", defStruct->def.st.decls);
		si = FindUniqueName("si", defStruct->def.st.decls);
		len = FindUniqueName("len", defStruct->def.st.decls);
	    }
	}
	f_print(fout,
		"PROCEDURE %s%s(%s: %s; %s: RPCSun.Server; %s%s: XDR.Source)\n",
		proc->proc_name, vers->vers_num, serviceRoutines, progName,
		s, streq(proc->arg_type, "void") ? "<*UNUSED*>" : "", so);
	f_print(fout, "  RAISES {XDR.Failed, %s} =\n", RPCExceptions);
	f_print(fout, "  VAR\n");
	if (ExpandProcArgs && defStruct != NULL &&
	    defStruct->def_kind == DEF_STRUCT) {
	    expandFlag = TRUE;
	    for (dl = defStruct->def.st.decls;
		 dl != NULL;
		 dl = dl->next) {
		pdeclaration(dl->decl.name, &(dl->decl), 4);
	    }
	}
	else if (!streq(proc->arg_type, "void")) {
	    f_print(fout, "    inParm: %s;\n",
		    MapToM3Type(proc->arg_type, TRUE));
	}
	if (!streq(proc->res_type, "void"))
	    f_print(fout, "    %s: %s;\n", outParm,
		    MapToM3Type(proc->res_type, TRUE));
	f_print(fout, "    %s: XDR.Sink;\n", si);
	f_print(fout, "  BEGIN\n");
	if (expandFlag) {
	    for (dl = defStruct->def.st.decls;
		 dl != NULL;
		 dl = dl->next) {
		emit_get_stat(&(dl->decl), "", "", FALSE, so, len);
	    }
	}
	else {
	    if (!streq(proc->arg_type, "void")) {
		srcMethod = rpcgenBaseType(proc->arg_type);
		if (srcMethod != NULL)
		    f_print(fout, "    inParm := XDR.Get%s(%s);\n",
			    srcMethod, so);
		else {
		    mName = ModuleName(proc->arg_type);
		    f_print(fout, "    %s_x.Get_%s(%s, inParm);\n",
			    mName, StripPrefix(mName, proc->arg_type), so);
		}
		inParm = "inParm";
	    }
	    else
		inParm = "";
	}
	if (streq(proc->res_type, "void"))
	    outParmEq = "";
	else {
	    sprintf(buf, "%s := ", outParm);
	    outParmEq = strdup(buf);
	}
	if (expandFlag) {
	    f_print(fout, "    %s%s.%s(\n",
		    outParmEq, serviceRoutines, proc->proc_name);
	    for (dl = defStruct->def.st.decls;
		 dl != NULL;
		 dl = dl->next) {
		f_print(fout, "\t\t\t%s", dl->decl.name);
		if (dl->next != NULL) f_print(fout, ",\n");
		else f_print(fout, ");\n");
	    }
	}
	else {
	    f_print(fout, "    %s%s.%s(%s);\n",
		    outParmEq, serviceRoutines, proc->proc_name, inParm);
	}
	f_print(fout, "    %s := %s.StartReply();\n", si, s);
	if (!streq(proc->res_type, "void")) {
	    srcMethod = rpcgenBaseType(proc->res_type);
	    if (srcMethod != NULL)
		f_print(fout, "    XDR.Put%s(%s, %s);\n", srcMethod, si, outParm);
	    else {
		mName = ModuleName(proc->res_type);
		f_print(fout, "    %s_x.Put_%s(%s, %s);\n",
			mName, StripPrefix(mName, proc->res_type),
			si, outParm);
	    }
	}
	f_print(fout, "  END %s%s;\n\n",
		proc->proc_name, vers->vers_num);
    }
    f_print(fout, "\n");
}
