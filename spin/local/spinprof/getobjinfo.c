/*
 * *****************************************************************
 * *                                                               *
 * *    Copyright (c) Digital Equipment Corporation, 1991, 1994    *
 * *                                                               *
 * *   All Rights Reserved.  Unpublished rights  reserved  under   *
 * *   the copyright laws of the United States.                    *
 * *                                                               *
 * *   The software contained on this media  is  proprietary  to   *
 * *   and  embodies  the  confidential  technology  of  Digital   *
 * *   Equipment Corporation.  Possession, use,  duplication  or   *
 * *   dissemination of the software and media is authorized only  *
 * *   pursuant to a valid written license from Digital Equipment  *
 * *   Corporation.                                                *
 * *                                                               *
 * *   RESTRICTED RIGHTS LEGEND   Use, duplication, or disclosure  *
 * *   by the U.S. Government is subject to restrictions  as  set  *
 * *   forth in Subparagraph (c)(1)(ii)  of  DFARS  252.227-7013,  *
 * *   or  in  FAR 52.227-19, as applicable.                       *
 * *                                                               *
 * *****************************************************************
 */
/*
 * HISTORY
 */
/*
 * @(#)$RCSfile: getobjinfo.c,v $ $Revision: 1.4 $ (DEC) $Date: 1997/05/23 18:35:10 $
 */
/*
 * (c) Copyright 1993, OPEN SOFTWARE FOUNDATION, INC.
 * ALL RIGHTS RESERVED
 */
/*
 * OSF/1 Release 1.2
 */

#include "prof_common.h"

extern	nl_catd	catd;	/* message catalog descriptor */
extern char *defaultEs[];
extern struct nl *defaultEnl[];

void
getobjinfo()
{
    /* SPIN: The real gprof code which reads COFF files has been removed.
     * We will read in our own simple files containing symbol names and
     * values. 
     */

    FILE *fd;
    struct stat stat_buf;
    long name_ind;
    long name_buffer_len;
    long nsyms;
    int i;
    int valcmp();
    char **dE;
    int index;

    if (stat( a_outname, &stat_buf ) != 0) {
	perror(a_outname);
	done();
    }
    if ((fd = fopen( a_outname, "r" )) == NULL) {
	fprintf(stderr,
		MSGSTR(OPENFILE, "%s:  Cannot open file %s.\n"),
		whoami, a_outname);
	done();
    }

    fread((void*) &nsyms, sizeof(long), 1, fd);
    nname = nsyms;
    fread((void*) &name_buffer_len, sizeof(long), 1, fd);

    /* Symbol names are kept in this table */
    strtab = (char*) malloc(name_buffer_len);
    if (strtab == 0) {
	fprintf(stderr,
		MSGSTR(NOROOM,
		       "%s: No room for %d bytes of string table\n"),
		whoami, name_buffer_len );
	return;
    }

    nl = (nltype*) calloc(nname, sizeof(nltype));
    if (nl == 0) {
	fprintf(stderr,
		MSGSTR(NOROOM,
		       "%s: No room for %d bytes of symbol table\n"),
		whoami, nname * sizeof(nltype) );
	return;
    }

    fread((void*) strtab, sizeof(char), name_buffer_len, fd);

    for (i=0; i<nname; i++) {
	fread((void*) &name_ind, sizeof(long), 1, fd);
	nl[i].name = &strtab[name_ind];
	fread((void*) &nl[i].value, sizeof(unsigned long), 1, fd);
    }

    fclose( fd );

    npe = &nl[nname];

    qsort(nl, nname, sizeof(nltype), valcmp);

    for (i=0; i<nname; i++) {
	/* Chop out the names of the profiling procedures themselves. */
	for (dE = &defaultEs[0], index = 0; *dE; dE++,index++) {
	    if (defaultEnl[index] != NULL) continue;
	    if (!strcmp(nl[i].name, *dE)) {
		defaultEnl[index] = &nl[i];
		break;
	    }
	}
    }
}

