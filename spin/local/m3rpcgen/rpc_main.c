/* @(#)rpc_main.c	2.2 88/08/01 4.0 RPCSRC */
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
static char sccsid[] = "@(#)rpc_main.c 1.7 87/06/24 (C) 1987 SMI";
#endif

/*
 * rpc_main.c, Top level of the RPC protocol compiler. 
 * Copyright (C) 1987, Sun Microsystems, Inc. 
 */

#include <stdio.h>
#include <strings.h>
#include <string.h>

#ifdef linux
#include <unistd.h>
#else
#include <sys/file.h>
#endif
#include "rpc_util.h"
#include "rpc_parse.h"
#include "rpc_scan.h"

#define EXTEND	1		/* alias for TRUE */

struct commandline {
	int cflag;		/* Get/Put interface and module */
	int hflag;		/* interface */
	int lflag;		/* client stubs */
	int sflag;		/* server stubs */
	char *infile;
	char *outfile;
};

static char *cmdname;
static char CPP[] = "/lib/cpp";
static char CPPFLAGS[] = "-C";

int PFlag = FALSE;		/* Strip prefix module names */
int ExpandProcArgs = FALSE;	/* TRUE => expand proc struct parameters */
int EmitAllFiles = FALSE;	/* TRUE => produce all 5 output files, even
				   if some are empty. */
int RecoveryFlag = FALSE;	/* TRUE => add RPC.Failed recovery loop to
				   client stubs and RecoveryProc parm. to
				   client import routines. */

int debugflag;		/* Turn on debugging printfs */

char *svcName;			/* Name of service being compiled. */
char *ServiceName();
void MakeSymbolTables();

char *RPCExceptions = "RPC.Failed, Thread.Alerted"; 
				/* List of exceptions that can be raised
				   by routines. */

Context *CurrentContext = NULL;

int TypeDefsExist = FALSE;	/* Used to signal if an XDR file should
				   exist for the .x file. */
int ProgramsExist = FALSE;	/* Used to signal if a program is defined
				   in the .x file. */

enum {noFile, svcInterface, clientImpl, serverImpl, xdrInterface, xdrImpl}
OutFileType;

list *Imports;			/* List of IMPORT module names. */
list *GetPutImports;		/* List of IMPORT GetPut module names. */
StringHashTableHandle ImportNames; /* Hash table of imported names.
				      Hash table elem values are module
				      names. */
/* IMPORTed .x files are searched for on a path defined by an environment var */
#define PATH_ENV_VAR	"DOTXPATH"
#define PATH_DEFAULT	".:/spin/build/latest"

StringHashTableHandle SymbolNames; /* Hash table of struct definitions.
				      Used to decide whether or not to expand
				      procedure input parameters. */


main(argc, argv)
	int argc;
	char *argv[];
{
	struct commandline cmd;

#ifdef NEW
	bzero((char *)&cmd, sizeof (struct commandline));
#endif
	if (!parseargs(argc, argv, &cmd)) {
		f_print(stderr,
			"usage: %s infile\n", cmdname);
		f_print(stderr,
			"\t%s [-p] [-e] [-f] [-r] infile\n",
			cmdname);
		f_print(stderr,
			"\t%s [-p] [-e] [-f] [-r] [-c | -h | -l | -s] [-o outfile] [infile]\n",
			cmdname);
		exit(1);
	}

	CurrentContext = (Context *) malloc(sizeof(Context));
	reinitialize();
	svcName = ServiceName(cmd.infile);
	
	MakeSymbolTables(cmd.infile);

	if (cmd.cflag) {
		c_output(cmd.infile, "-DRPC_XDR",
			 !EXTEND, cmd.outfile);
	} else if (cmd.hflag) {
		h_output(cmd.infile, "-DRPC_HDR",
			 !EXTEND, cmd.outfile);
	} else if (cmd.lflag) {
		l_output(cmd.infile, "-DRPC_CLNT",
			 !EXTEND, cmd.outfile);
	} else if (cmd.sflag) {
		s_output(cmd.infile, "-DRPC_SVC", !EXTEND, cmd.outfile);
	} else {
		c_output(cmd.infile, "-DRPC_XDR", EXTEND, "_x");
		reinitialize();
		h_output(cmd.infile, "-DRPC_HDR", EXTEND, ".i3");
		reinitialize();
		l_output(cmd.infile, "-DRPC_CLNT", EXTEND, "_c.m3");
		reinitialize();
		s_output(cmd.infile, "-DRPC_SVC", EXTEND, "_s.m3");
	}
	exit(0);
}

/*
 * add extension to filename 
 */
static char *
extendfile(file, ext)
	char *file;
	char *ext;
{
	char *res;
	char *p;

	res = alloc(strlen(file) + strlen(ext) + 1);
	if (res == NULL) {
		abort();
	}
	p = rindex(file, '.');
	if (p == NULL) {
		p = file + strlen(file);
	}
	(void) strcpy(res, file);
	(void) strcpy(res + (p - file), ext);
	return (res);
}

/*
 * Return the basename of a file.
 */
static
char *ServiceName(filename)
     char *filename;
{
    char *s;
    char *p;
    char *svcName = alloc(strlen(filename)+1);

    p = rindex(filename, '/');
    if (p != NULL)
	s = p + 1;
    else
	s = filename;
    strcpy(svcName, s);
    p = rindex(svcName, '.');
    if (p != NULL) {
	*p = '\0';
    }
    return (svcName);
}

/*
 * Open output file with given extension 
 */
static
open_output(infile, outfile)
	char *infile;
	char *outfile;
{
	if (outfile == NULL) {
		fout = stdout;
		EmitNotice();
		return;
	}
	if (infile != NULL && streq(outfile, infile)) {
		f_print(stderr, "%s: output would overwrite %s\n", cmdname,
			infile);
		crash();
	}
	fout = fopen(outfile, "w");
	if (fout == NULL) {
		f_print(stderr, "%s: unable to open ", cmdname);
		perror(outfile);
		crash();
	}
	record_open(outfile);
	EmitNotice();
}

static
EmitNotice()
{
	f_print(fout, "(*\n");
	f_print(fout,
		" * This file was automatically generated using m3rpcgen.\n");
	f_print(fout, " *)\n\n");
}
/*
 * FIndFileOnPath
 */
static
char *
FindFileOnPath (name, varname, defaultpath)
     char *name;		/* Filename to find */
     char *varname;		/* Name of env var that contains path */
     char *defaultpath;		/* Default path if no environment variable */
{
    char *prefix;
    char *getenv();
    char *copy;
    char buf[1000];
    char *base, *p;
    static noted = 0;

    if (*name == '.' || *name == '/') {
	return (strdup(name));
    }
    if ((prefix = getenv (varname)) == NULL) {
	prefix = defaultpath;
    } else if (debugflag && !noted) {
	noted = 1;
	printf("m3rpcgen: %s = %s\n", varname, prefix);
    }
    copy = strdup(prefix);
    base = copy;

    do {
	p = strchr (base, ':');  
	if (p != NULL)
	    *p = (char) 0;
	sprintf (buf, "%s/%s", base, name);
	if (debugflag) {
	    printf ("m3rpcgen: checking \"%s\"\n", buf);
	}
	if (access (buf, R_OK) == 0) {
	    p = strdup (buf);
	    free (copy);
	    return (p);
	}
	if (p != NULL && *(p+1) != (char) 0) {
	    base = p + 1;
	} else {
	    base = NULL;
	}
    } while (base != NULL);
    free (copy);
    return (NULL);
}



/*
 * Open input file with given define for C-preprocessor 
 */
static
open_input(infile, define)
	char *infile;
	char *define;
{
	int pd[2];

	if (infile == NULL) {
	    CurrentContext->infilename = "<stdin>";
	} else {
	    CurrentContext->infilename = FindFileOnPath(infile,
						PATH_ENV_VAR, PATH_DEFAULT);
	    if (CurrentContext->infilename != NULL) {
		infile = CurrentContext->infilename;
	    }
	}
	(void) pipe(pd);
	switch (fork()) {
	case 0:
		(void) close(1);
		(void) dup2(pd[1], 1);
		(void) close(pd[0]);
		if (define != NULL && define[0] != '\0')
		    execl(CPP, CPP, CPPFLAGS, define, infile, NULL);
		else
		    execl(CPP, CPP, CPPFLAGS, infile, NULL);
		perror("execv");
		exit(1);
	case -1:
		perror("fork");
		exit(1);
	}
	(void) close(pd[1]);
	CurrentContext->fin = fdopen(pd[0], "r");
	if (CurrentContext->fin == NULL) {
		f_print(stderr, "%s: ", cmdname);
		perror(CurrentContext->infilename);
		crash();
	}
}

/*
 * Compile into a marshalling routines output file
 */
static
c_output(infile, define, extend, outfile)
	char *infile;
	char *define;
	int extend;
	char *outfile;
{
        char *m3Outfile, *i3Outfile;
	char *outfilename;
	int outfileLen = strlen(outfile);
	definition *def;
	long tell;
	list *l;

	/*
	 * Emit the interface file.
	 */
	OutFileType = xdrInterface;

	i3Outfile = alloc(outfileLen + 4);
	strcpy(i3Outfile, outfile);
	strcat(i3Outfile, ".i3");
	open_input(infile, define);
	outfilename = extend ? extendfile(infile, i3Outfile) : i3Outfile;
	open_output(infile, outfilename);

	f_print(fout, "INTERFACE %s_x;\n\n", svcName);
	f_print(fout, "IMPORT XDR, Thread, %s;\n\n", svcName);

	tell = ftell(fout);
	while (def = get_definition()) {
		emitDefn(def);
	}

	if (!EmitAllFiles && extend && tell == ftell(fout)) {
	    (void) unlink(outfilename);
	    return;
	}

	f_print(fout, "END %s_x.\n", svcName);


	/*
	 * Emit the module file.
	 */
	OutFileType = xdrImpl;

	m3Outfile = alloc(outfileLen + 4);
	strcpy(m3Outfile, outfile);
	strcat(m3Outfile, ".m3");
	open_input(infile, define);
	outfilename = extend ? extendfile(infile, m3Outfile) : m3Outfile;
	open_output(infile, outfilename);

	f_print(fout, "MODULE %s_x;\n\n", svcName);
	f_print(fout, "IMPORT Ctypes;  <*NOWARN*>\n");
	f_print(fout, "IMPORT XDR, Thread, %s;\n", svcName);

	if (Imports != NULL) {
	    for (l = Imports; l != NULL; l = l->next)
		f_print(fout, "IMPORT %s;\n", l->val);
	}
	if (GetPutImports != NULL) {
	    for (l = GetPutImports; l != NULL; l = l->next)
		f_print(fout, "IMPORT %s;\n", l->val);
	}
	f_print(fout, "\n");

	tell = ftell(fout);
	while (def = get_definition()) {
		emitImpl(def);
	}

	if (!EmitAllFiles && extend && tell == ftell(fout)) {
	    (void) unlink(outfilename);
	    return;
	}

	f_print(fout, "\nBEGIN\n");
	f_print(fout, "END %s_x.\n", svcName);
}

/*
 * Compile into an M3 interface file for the service.
 */
static
h_output(infile, define, extend, outfile)
     char *infile;
     char *define;
     int extend;
     char *outfile;
{
    definition *def;
    char *outfilename;
    list *l;

    OutFileType = svcInterface;

    /* Get open connection to (C-preprocessed) input file
       and an open output file. */
    open_input(infile, define);
    outfilename =  extend ? extendfile(infile, outfile) : outfile;
    open_output(infile, outfilename);
    
    /* Emit preamble. */
    f_print(fout, "INTERFACE %s;\n\n", svcName);
    if (RecoveryFlag)
	f_print(fout, "IMPORT ExceptionArg;\n");
    if (ProgramsExist)
	f_print(fout, "IMPORT RPC, RPCSun, Thread;\n");
    f_print(fout, "IMPORT Ctypes;  <*NOWARN*>\n");
    
    if (Imports != NULL) {
	for (l = Imports; l != NULL; l = l->next)
	    f_print(fout, "IMPORT %s;\n", l->val);
    }

    if (RecoveryFlag && ProgramsExist) {
	f_print(fout, "TYPE RecoveryProc = \n");
	f_print(fout, "\tPROCEDURE (procNum: INTEGER;\n");
	f_print(fout, "\t\tnumTries: CARDINAL;\n");
	f_print(fout, "\t\targ: ExceptionArg.T): RPCSun.Client\n");
	f_print(fout, "\t\tRAISES {RPC.Failed};\n\n");
    }
    
    /* Emit definitions. */
    while (def = get_definition()) {
	print_datadef(def);
    }
    
    /* Print postamble. */
    f_print(fout, "\nEND %s.\n", svcName);
}

/*
 * Compile into an RPC service
 */
static
s_output(infile, define, extend, outfile)
	char *infile;
	char *define;
	int extend;
	char *outfile;
{
    definition *def;
    char *outfilename;
    version_list *vers;
    char progName[256];
    list *l;
    int foundprogram = FALSE;
    
    OutFileType = serverImpl;

    open_input(infile, define);
    outfilename = extend ? extendfile(infile, outfile) : outfile;
    open_output(infile, outfilename);
    
    f_print(fout, "MODULE %s_s EXPORTS %s;\n\n",
	    svcName, svcName);
    f_print(fout, "IMPORT RPC, RPCSun, RPCOS, Thread, XDR;\n");
    f_print(fout, "IMPORT Ctypes;  <*NOWARN*>\n");
    if (TypeDefsExist) f_print(fout, "IMPORT %s_x;  <*NOWARN*>\n", svcName);
    
    if (Imports != NULL) {
	for (l = Imports; l != NULL; l = l->next)
	    f_print(fout, "IMPORT %s;  <*NOWARN*>\n", l->val);
    }
    if (GetPutImports != NULL) {
	for (l = GetPutImports; l != NULL; l = l->next)
	    f_print(fout, "IMPORT %s;  <*NOWARN*>\n", l->val);
    }
    f_print(fout, "\n");

    f_print(fout, "<* FATAL RPCSun.Erred *>\n\n");
    
    while (def = get_definition()) {
	if (def->def_kind == DEF_PROGRAM) {
	    foundprogram = TRUE;
	    for (vers = def->def.pr.versions; vers != NULL; vers = vers->next) {
		sprintf(progName, "%s%s", def->def_name, vers->vers_num);
		write_server_prog(svcName, vers->vers_name, vers);
	    }
	}
    }
    
    if (!EmitAllFiles && extend && !foundprogram) {
	(void) unlink(outfilename);
	return;
    }

    f_print(fout, "BEGIN\n");
    f_print(fout, "END %s_s.\n", svcName);
}

static
l_output(infile, define, extend, outfile)
	char *infile;
	char *define;
	int extend;
	char *outfile;
{
	definition *def;
	char *outfilename;
	int foundprogram = FALSE;
	list *l;

	OutFileType = clientImpl;

	open_input(infile, define);
	outfilename = extend ? extendfile(infile, outfile) : outfile;
	open_output(infile, outfilename);

	f_print(fout, "MODULE %s_c EXPORTS %s;\n\n",
		svcName, svcName);
	f_print(fout, "IMPORT RPC, RPCSun, XDR, Thread;\n");
	f_print(fout, "IMPORT Ctypes;  <*NOWARN*>\n");
	if (TypeDefsExist) f_print(fout, "IMPORT %s_x;  <*NOWARN*>\n", svcName);

	if (Imports != NULL) {
	    for (l = Imports; l != NULL; l = l->next)
		f_print(fout, "IMPORT %s;  <*NOWARN*>\n", l->val);
	}
	if (GetPutImports != NULL) {
	    for (l = GetPutImports; l != NULL; l = l->next)
		f_print(fout, "IMPORT %s;  <*NOWARN*>\n", l->val);
	}
	f_print(fout, "\n");

	while (def = get_definition()) {
		foundprogram |= (def->def_kind == DEF_PROGRAM);
	}
	if (!EmitAllFiles && extend && !foundprogram) {
		(void) unlink(outfilename);
		return;
	}

	write_stubs(svcName);

	f_print(fout, "\nBEGIN\n");
	f_print(fout, "END %s_c.\n", svcName);
}

/*
 * Construct a list of IMPORT statements that must go into each output file.
 * Also construct a hash table of symbol definitions encountered.
 */
void MakeSymbolTables(name)
     char *name;
{
    definition *def;
    StringHashElement elem;
    case_list *c;
    decl_list *d;

    Imports = NULL;
    GetPutImports = NULL;
    ImportNames = StringHashCreate(128, NULL, NULL);
    if (ImportNames == NULL) {
	fprintf(stderr,
		"ERROR: couldn't create hash table for import names.\n");
	exit(-1);
    }

    SymbolNames = StringHashCreate(128, NULL, NULL);
    if (SymbolNames == NULL) {
	fprintf(stderr,
		"ERROR: couldn't create hash table for struct names.\n");
	exit(-1);
    }

    OutFileType = noFile;

    CurrentContext->printDirectives = FALSE;
    open_input(name, "-DRPC_HDR");
    /* Just run through the file so that all IMPORT directives get seen
       by the scanner. */
    TypeDefsExist = FALSE;
    ProgramsExist = FALSE;
    while (def = get_definition()) {
	elem = StringHashFind(SymbolNames, def->def_name, insert, NULL);
	elem->userData = (char *) def;
	switch (def->def_kind) {
	  case DEF_STRUCT:
	  case DEF_UNION:
	  case DEF_TYPEDEF:
	  case DEF_ENUM:
	    TypeDefsExist = TRUE;
	    break;
	  case DEF_PROGRAM:
	    ProgramsExist = TRUE;
	    break;
	  default:
	    break;
	}
    }
    reinitialize();
}

/*
 * Process IMPORT statements for imported .x files.
 */
void ProcessImport(name)
     char *name;
{
    Context *save = CurrentContext;
    Context import;
    char infile[256];
    definition *def;
    char buf[512];
    list *l;
    StringHashElement elem;
    int inserted;
    ImportModuleRec *m;
    int foundDefnsFlag = FALSE;
    int foundTypeDefsFlag = FALSE;

    if (OutFileType != noFile) return;

    sprintf(infile, "%s.x", name);
    CurrentContext = &import;
    reinitialize();
    CurrentContext->printDirectives = FALSE;
    open_input(infile, "");
    name = strdup(name);
    while (def = get_definition()) {
	elem = StringHashFind(ImportNames, def->def_name, insert, &inserted);
	elem->userData = (char *) malloc(sizeof(ImportModuleRec));
	m = (ImportModuleRec *) (elem->userData);
	m->moduleName = name;
	sprintf(buf, "%s.%s", name, StripPrefix(name, def->def_name));
	m->defnName = strdup(buf);
	elem = StringHashFind(SymbolNames, def->def_name, insert, NULL);
	elem->userData = (char *) def;
	switch (def->def_kind) {
	  case DEF_STRUCT:
	  case DEF_UNION:
	  case DEF_TYPEDEF:
	  case DEF_ENUM:
	    foundDefnsFlag = TRUE;
	    foundTypeDefsFlag = TRUE;
	    break;
	  case DEF_CONST:
	    foundDefnsFlag = TRUE;
	    break;
	  default:
	    break;
	}
    }
    if (foundDefnsFlag) {
	l = (list *) malloc(sizeof(list));
	l->val = name;
	l->next = Imports;
	Imports = l;
    }
    if (foundTypeDefsFlag) {
	l = (list *) malloc(sizeof(list));
	sprintf(buf, "%s_x", name);
	l->val = strdup(buf);
	l->next = GetPutImports;
	GetPutImports = l;
    }

    CurrentContext = save;
}

/*
 * Parse command line arguments 
 */
static
parseargs(argc, argv, cmd)
	int argc;
	char *argv[];
	struct commandline *cmd;

{
	int i;
	int j;
	char c;
	char flag[(1 << 8 * sizeof(char))];
	int nflags;

	cmdname = argv[0];
	cmd->infile = cmd->outfile = NULL;
	if (argc < 2) {
		return (0);
	}
	flag['c'] = 0;
	flag['h'] = 0;
	flag['l'] = 0;
	flag['s'] = 0;
	for (i = 1; i < argc; i++) {
		if (argv[i][0] != '-') {
			if (cmd->infile) {
				return (0);
			}
			cmd->infile = argv[i];
		} else {
			for (j = 1; argv[i][j] != 0; j++) {
				c = argv[i][j];
				switch (c) {
				case 's':
				case 'c':
				case 'h':
				case 'l':
					if (flag[c]) {
						return (0);
					}
					flag[c] = 1;
					break;
				case 'o':
					if (argv[i][j - 1] != '-' || 
					    argv[i][j + 1] != 0) {
						return (0);
					}
					if (++i == argc) {
						return (0);
					}
					if (cmd->outfile) {
					    return (0);
					}
					cmd->outfile = argv[i];
					goto nextarg;
				case 'Z':
					debugflag = 1;
					break;
			        case 'p':
					PFlag = TRUE;
					break;
			        case 'e':
					ExpandProcArgs = TRUE;
					break;
			        case 'f':
					EmitAllFiles = TRUE;
					break;
			        case 'r':
					RecoveryFlag = TRUE;
					break;
				default:
					return (0);
				}
			}
	nextarg:
			;
		}
	}
	cmd->cflag = flag['c'];
	cmd->hflag = flag['h'];
	cmd->sflag = flag['s'];
	cmd->lflag = flag['l'];
	nflags = cmd->cflag + cmd->hflag + cmd->sflag + cmd->lflag;
	if (nflags == 0) {
		if (cmd->outfile != NULL || cmd->infile == NULL) {
			return (0);
		}
	} else if (nflags > 1) {
		return (0);
	}
	return (1);
}
