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
 * @(#)$RCSfile: prof_common.h,v $ $Revision: 1.3 $ (DEC) $Date: 1997/05/23 18:35:07 $
 */
/*
 * (c) Copyright 1993, OPEN SOFTWARE FOUNDATION, INC.
 * ALL RIGHTS RESERVED
 */
/*
 * OSF/1 Release 1.2
 */
/* 
 * This header file exists to allow the profiling object format
 * dependent routines to share the same source while operating 
 * on different name structures.
 */

#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <locale.h>
#include <unistd.h>
#include <nl_types.h>

#ifndef _H_GPROF_MSG 
#define _H_GPROF_MSG 
#include <limits.h>
#include <nl_types.h>
#define MF_GPROF "gprof.cat"



/* The following was generated from gprof.msg. */


/* definitions for set MS_PROF */
#define MS_PROF 1

#define STATICDEP 1
#define DIGNORED 2
#define TIMETICKS 3
#define INCOMPAT 4
#define NOROOM 5
#define NOROOMBUF 6
#define UNEXPEOF 7
#define OUTMEMTOPO 8
#define OUTMEMSORT 9
#define TOPOBOTCH 10
#define CYCLE 11
#define OVERFLOW 12
#define HEADCYCLE 13
#define GLOMMED 14
#define OPENFILE 15
#define FORMAT 16
#define SEARCH 17
#define PARENTP 18
#define MEMSORT 19
#define NGRAN 20
#define NGRAN2 21
#define NOTIMEACC 22
#define NOTIMEPROP 23
#define SPONTANEOUS 24
#define CYCLE2 25
#define WHOLECYCLE 26
#define INDEX 27
#define READERR 28
#define FILEHEAD 29
#define NUMSYMS 30
#define NOOPTHDR 31
#define NOSYMS 32
#define NOSYMS2 33
#define NOROOMSYM 34
#define READSYM 35
#define FAILED 36
#define NOSYMS3 37
#define SYMBOL 38
#define MALLOCFAIL 39
#define AOUT1 40
#define AOUT2 41
#define AOUT3 42
#define AOUT4 43
#define AOUT5 44
#define AOUT6 45
#define AOUT7 46
#define AOUT8 47
#define UNEXPEOF2 48
#define NOPD 49
#define FLATBLURB 50
#define CALLGBLURB 51
#endif 

#define MSGSTR(Num,Str) catgets(catd,MS_PROF,Num,Str)


/*
 * who am i, for error messages.
 */
char	*whoami;

char	*a_outname;

typedef int	bool;


    /*
     *	a constructed arc,
     *	    with pointers to the namelist entry of the parent and the child,
     *	    a count of how many times this arc was traversed,
     *	    and pointers to the next parent of this child and
     *		the next child of this parent.
     */
struct arcstruct {
    struct nl		*arc_parentp;	/* pointer to parent's nl entry */
    struct nl		*arc_childp;	/* pointer to child's nl entry */
    long		arc_count;	/* how calls from parent to child */
    long		arc_time;	/* time inherited along arc */
    long		arc_childtime;	/* childtime inderited along arc */
    struct arcstruct	*arc_parentlist; /* parents-of-this-child list */
    struct arcstruct	*arc_childlist;	/* children-of-this-parent list */
};
typedef struct arcstruct	arctype;

    /*
     * The symbol table;
     * for each external in the specified file we gather
     * its address, the number of calls and compute its share of cpu time.
     */
struct nl {
    char		*name;		/* the name */
    unsigned long	value;		/* the pc entry point */
    unsigned long	svalue;		/* entry point aligned to histograms */
    long		time;		/* ticks in this routine */
    long		childtime;	/* cumulative ticks in children */
    long		ncall;		/* how many times called */
    long		selfcalls;	/* how many calls to self */
    double		propfraction;	/* what % of time propagates */
    long		propself;	/* how much self time propagates */
    long		propchild;	/* how much child time propagates */
    bool		printflag;	/* should this be printed? */
    int			index;		/* index in the graph list */
    int			toporder;	/* graph call chain top-sort order */
    int			cycleno;	/* internal number of cycle on */
    struct nl		*cyclehead;	/* pointer to head of cycle */
    struct nl		*cnext;		/* pointer to next member of cycle */
    arctype		*parents;	/* list of caller arcs */
    arctype		*children;	/* list of callee arcs */
};
typedef struct nl	nltype;

nltype	*nl;			/* the whole namelist */
nltype	*npe;			/* the virtual end of the namelist */
char	*strtab;		/* string table for some formats */
bool	aflag;	

int	nname;			/* the number of function names */
int	debug;
#define	AOUTDEBUG	64	/* inherited from gprof.h */
