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
 * 29-May-96  Charles Garrett (garrett) at the University of Washington
 *	Changed gmon.out file format to use a sparse representation
 *	instead of a dense one. The range of possible addresses in SPIN
 *	was too large to use the dense one.
 *
 */
#ifndef lint
static char	*sccsid = "@(#)$RCSfile: gprof.c,v $ $Revision: 1.4 $ (DEC) $Date: 1997/05/23 18:35:10 $";
#endif 
/*
 */
/*
 * (c) Copyright 1993, OPEN SOFTWARE FOUNDATION, INC.
 * ALL RIGHTS RESERVED
 */
/*
 * OSF/1 Release 1.2
 */
/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

/*
 * (c) Copyright 1990, 1994, OPEN SOFTWARE FOUNDATION, INC.
 * ALL RIGHTS RESERVED
 */

#include "gprof.h"
#include "prof_common.h"
#include "gmon.h"

char	*whoami = "gprof";

    /*
     *	things which get -E excluded by default.
     *  Pointers are saved to the nl structures when the
     *  symbol table is read.  These are used to exclude
     *  these procedures from total time calculations and
     *  listings.
     */
char	*defaultEs[] = { "_mcount", "_gprof_mcount", 0 };
struct nl *defaultEnl[] = { NULL, NULL, NULL };

nl_catd	catd;	/* message catalog descriptor */

main(argc, argv)
    int argc;
    char **argv;
{
    char	**sp;
    nltype	**timesortnlp;

    (void) setlocale(LC_ALL, "");
    catd = catopen(MF_GPROF, 0);

    --argc;
    argv++;
    debug = 0;
    bflag = TRUE;
    while ( *argv != 0 && **argv == '-' ) {
	(*argv)++;
	switch ( **argv ) {
	case 'a':
	    aflag = TRUE;
	    break;
	case 'b':
	    bflag = FALSE;
	    break;
	case 'c':
#ifdef	DO_STATIC_DEP
	    cflag = TRUE;
	    break;
#else
	    fprintf(stderr, MSGSTR(STATICDEP,
				   "static dependencies not supported\n"));
	    exit(1);
#endif /* DO_STATIC_DEP */
	case 'd':
	    dflag = TRUE;
	    (*argv)++;
	    debug |= atoi( *argv );
	    debug |= ANYDEBUG;
		printf("[main] debug = %d\n", debug);
		fprintf(stderr, MSGSTR(DIGNORED, "%s: -d ignored\n"), whoami);
	    break;
	case 'E':
	    ++argv;
	    addlist( Elist , *argv );
	    Eflag = TRUE;
	    addlist( elist , *argv );
	    eflag = TRUE;
	    break;
	case 'e':
	    addlist( elist , *++argv );
	    eflag = TRUE;
	    break;
	case 'F':
	    ++argv;
	    addlist( Flist , *argv );
	    Fflag = TRUE;
	    addlist( flist , *argv );
	    fflag = TRUE;
	    break;
	case 'f':
	    addlist( flist , *++argv );
	    fflag = TRUE;
	    break;
	case 'k':
	    addlist( kfromlist , *++argv );
	    addlist( ktolist , *++argv );
	    kflag = TRUE;
	    break;
	case 's':
	    sflag = TRUE;
	    break;
	case 'z':
	    zflag = TRUE;
	    break;
	}
	argv++;
    }
    if ( *argv != 0 ) {
	a_outname  = *argv;
	argv++;
    } else {
	a_outname  = A_OUTNAME;
    }
    if ( *argv != 0 ) {
	gmonname = *argv;
	argv++;
    } else {
	gmonname = GMONNAME;
    }
	/*
	 *	turn off default functions
	 */
    for ( sp = &defaultEs[0] ; *sp ; sp++ ) {
	Eflag = TRUE;
	addlist( Elist , *sp );
	eflag = TRUE;
	addlist( elist , *sp );
    }
	/*
	 *	get information about a.out file.
	 */
    getobjinfo();
	/*
	 *	get information about mon.out file(s).
	 */
    do	{
	getpfile( gmonname );
	if ( *argv != 0 ) {
	    gmonname = *argv;
	}
    } while ( *argv++ != 0 );
	/*
	 *	dump out a gmon.sum file if requested
	 */
    if ( sflag ) {
	dumpsum( GMONSUM );
    }
	/*
	 *	assemble the dynamic profile
	 */
    timesortnlp = doarcs();
	/*
	 *	print the dynamic profile
	 */
    printgprof( timesortnlp );	
	/*
	 *	print the flat profile
	 */
    printprof();	
	/*
	 *	print the index
	 */
    printindex();	
    done();
}


#define OVERHEAD_CYCLES 0

    /*
     *	information from a gmon.out file is just
     *	the arcs themselves.
     */
getpfile(filename)
    char *filename;
{
    FILE		*pfile;
    struct spin_arc	arc;
    long                rate;

    if((pfile = fopen(filename, "r")) == NULL) {
	perror(filename);
	done();
    }

    if (fread(&rate, sizeof(long), 1, pfile) != 1) {
	perror("Could not read clock rate");
	done();
    }
    hz = (double) rate;

	/*
	 *	the file consists of
	 *	a bunch of <from,self,count,cycles> tuples.
	 */
    while ( fread( &arc , sizeof arc , 1 , pfile ) == 1 ) {
	/* Adjust for profiling overhead. */
	arc.cycles -= arc.count * OVERHEAD_CYCLES;

	totime += arc.cycles;
	    if ( debug & SAMPLEDEBUG ) {
		printf( "[getpfile] frompc 0x%lx selfpc 0x%lx count %ld cyc %ld total %lf\n" ,
			arc.frompc , arc.selfpc , arc.count, arc.cycles, totime);
	    }
	    /*
	     *	add this arc
	     */
	if (arc.count != 0)		/* don't tally empty arcs */
		tally( &arc );
    }
    fclose(pfile);
}

tally( rawp )
    struct spin_arc	*rawp;
{
    nltype		*parentp;
    nltype		*childp;

    parentp = nllookup( rawp -> frompc );
    childp = nllookup( rawp -> selfpc );
    if (parentp == NULL || childp == NULL) {
	    if ( debug & TALLYDEBUG ) {
		    printf("[tally]bad raw arc ignored\n");
	    }
	    return;
    }

		
    if ( kflag
	 && onlist( kfromlist , parentp -> name )
	 && onlist( ktolist , childp -> name ) ) {
	return;
    }
    childp -> ncall += rawp -> count;
    childp -> time += rawp -> cycles;
	if ( debug & TALLYDEBUG ) {
	    printf( "[tally] arc from %s to %s traversed %d times\n" ,
		    parentp -> name , childp -> name , rawp -> count );
	}
    addarc( parentp , childp , rawp -> count, rawp -> cycles );
}

/*
 * dump out the gmon.sum file
 */
dumpsum( sumfile )
    char *sumfile;
{
    register nltype *nlp;
    register arctype *arcp;
    struct spin_arc arc;
    FILE *sfile;

    if ( ( sfile = fopen ( sumfile , "w" ) ) == NULL ) {
	perror( sumfile );
	done();
    }
    /*
     * dump the normalized raw arc information
     */
    for ( nlp = nl ; nlp < npe ; nlp++ ) {
	for ( arcp = nlp -> children ; arcp ; arcp = arcp -> arc_childlist ) {
	    arc.frompc = arcp -> arc_parentp -> value;
	    arc.selfpc = arcp -> arc_childp -> value;
	    arc.count = arcp -> arc_count;
	    if ( fwrite ( &arc , sizeof arc , 1 , sfile ) != 1 ) {
		perror( sumfile );
		done();
	    }
		if ( debug & SAMPLEDEBUG ) {
		    printf( "[dumpsum] frompc 0x%lx selfpc 0x%lx count %d\n" ,
			    arc.frompc , arc.selfpc , arc.count );
		}
	}
    }
    fclose( sfile );
}

valcmp(p1, p2)
    nltype *p1, *p2;
{
    if ( p1 -> value < p2 -> value ) {
	return LESSTHAN;
    }
    if ( p1 -> value > p2 -> value ) {
	return GREATERTHAN;
    }
    return EQUALTO;
}


unsigned long
min(a, b)
    unsigned long a,b;
{
    if (a<b)
	return(a);
    return(b);
}

unsigned long
max(a, b)
    unsigned long a,b;
{
    if (a>b)
	return(a);
    return(b);
}

    /*
     *	calculate scaled entry point addresses (to save time in asgnsamples),
     *	and possibly push the scaled entry points over the entry mask,
     *	if it turns out that the entry point is in one bucket and the code
     *	for a routine is in the next bucket.
     */
alignentries()
{
    register struct nl	*nlp;

    for (nlp = nl; nlp < npe; nlp++) {
	nlp -> svalue = nlp -> value;
    }
}

void	    
done()
{

    exit(0);
}
