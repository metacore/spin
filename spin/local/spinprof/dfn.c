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
#ifndef lint
static char	*sccsid = "@(#)$RCSfile: dfn.c,v $ $Revision: 1.3 $ (DEC) $Date: 1997/05/23 18:35:09 $";
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

/***
 *** "dfn.c	5.4 (Berkeley) 6/1/90";
 ***/

#include <stdio.h>
#include "gprof.h"
#include "prof_common.h"
#include "gmon.h"

extern	nl_catd	catd;	/* message catalog descriptor */

#define	DFN_DEPTH	100
struct dfnstruct {
    nltype	*nlentryp;
    int		cycletop;
};
typedef struct dfnstruct	dfntype;

dfntype	dfn_stack[ DFN_DEPTH ];
int	dfn_depth = 0;

int	dfn_counter = DFN_NAN;

    /*
     *	given this parent, depth first number its children.
     */
dfn( parentp )
    nltype	*parentp;
{
    arctype	*arcp;

#   ifdef DEBUG
	if ( debug & DFNDEBUG ) {
	    printf( "[dfn] dfn(" );
	    printname( parentp );
	    printf( ")\n" );
	}
#   endif /* DEBUG */
	/*
	 *	if we're already numbered, no need to look any furthur.
	 */
    if ( dfn_numbered( parentp ) ) {
	return;
    }
	/*
	 *	if we're already busy, must be a cycle
	 */
    if ( dfn_busy( parentp ) ) {
	dfn_findcycle( parentp );
	return;
    }
	/*
	 *	visit yourself before your children
	 */
    dfn_pre_visit( parentp );
	/*
	 *	visit children
	 */
    for ( arcp = parentp -> children ; arcp ; arcp = arcp -> arc_childlist ) {
	    dfn( arcp -> arc_childp );
    }
	/*
	 *	visit yourself after your children
	 */
    dfn_post_visit( parentp );
}

    /*
     *	push a parent onto the stack and mark it busy
     */
dfn_pre_visit( parentp )
    nltype	*parentp;
{

    dfn_depth += 1;
    if ( dfn_depth >= DFN_DEPTH ) {
	fprintf(stderr,	MSGSTR(OVERFLOW,"[dfn] out of my depth (dfn_stack overflow)\n"));
	exit( 1 );
    }
    dfn_stack[ dfn_depth ].nlentryp = parentp;
    dfn_stack[ dfn_depth ].cycletop = dfn_depth;
    parentp -> toporder = DFN_BUSY;
#   ifdef DEBUG
	if ( debug & DFNDEBUG ) {
	    printf( "[dfn_pre_visit]\t\t%d:" , dfn_depth );
	    printname( parentp );
	    printf( "\n" );
	}
#   endif /* DEBUG */
}

    /*
     *	are we already numbered?
     */
bool
dfn_numbered( childp )
    nltype	*childp;
{
    
    return ( childp -> toporder != DFN_NAN && childp -> toporder != DFN_BUSY );
}

    /*
     *	are we already busy?
     */
bool
dfn_busy( childp )
    nltype	*childp;
{

    if ( childp -> toporder == DFN_NAN ) {
	return FALSE;
    }
    return TRUE;
}

    /*
     *	MISSING: an explanation
     */
dfn_findcycle( childp )
    nltype	*childp;
{
    int		cycletop;
    nltype	*cycleheadp;
    nltype	*tailp;
    int		index;

    for ( cycletop = dfn_depth ; cycletop > 0 ; cycletop -= 1 ) {
	cycleheadp = dfn_stack[ cycletop ].nlentryp;
	if ( childp == cycleheadp ) {
	    break;
	}
	if ( childp -> cyclehead != childp &&
	    childp -> cyclehead == cycleheadp ) {
	    break;
	}
    }
    if ( cycletop <= 0 ) {
	fprintf(stderr,
		MSGSTR(HEADCYCLE,
		       "[dfn_findcycle] couldn't find head of cycle\n"));
	exit( 1 );
    }
#   ifdef DEBUG
	if ( debug & DFNDEBUG ) {
	    printf( "[dfn_findcycle] dfn_depth %d cycletop %d " ,
		    dfn_depth , cycletop  );
	    printname( cycleheadp );
	    printf( "\n" );
	}
#   endif /* DEBUG */
    if ( cycletop == dfn_depth ) {
	    /*
	     *	this is previous function, e.g. this calls itself
	     *	sort of boring
	     */
	dfn_self_cycle( childp );
    } else {
	    /*
	     *	glom intervening functions that aren't already
	     *	glommed into this cycle.
	     *	things have been glommed when their cyclehead field
	     *	points to the head of the cycle they are glommed into.
	     */
	for ( tailp = cycleheadp ; tailp -> cnext ; tailp = tailp -> cnext ) {
	    /* void: chase down to tail of things already glommed */
#	    ifdef DEBUG
		if ( debug & DFNDEBUG ) {
		    printf( "[dfn_findcycle] tail " );
		    printname( tailp );
		    printf( "\n" );
		}
#	    endif /* DEBUG */
	}
	    /*
	     *	if what we think is the top of the cycle
	     *	has a cyclehead field, then it's not really the
	     *	head of the cycle, which is really what we want
	     */	
	if ( cycleheadp -> cyclehead != cycleheadp ) {
	    cycleheadp = cycleheadp -> cyclehead;
#	    ifdef DEBUG
		if ( debug & DFNDEBUG ) {
		    printf( "[dfn_findcycle] new cyclehead " );
		    printname( cycleheadp );
		    printf( "\n" );
		}
#	    endif /* DEBUG */
	}
	for ( index = cycletop + 1 ; index <= dfn_depth ; index += 1 ) {
	    childp = dfn_stack[ index ].nlentryp;
	    if ( childp -> cyclehead == childp ) {
		    /*
		     *	not yet glommed anywhere, glom it
		     *	and fix any children it has glommed
		     */
		tailp -> cnext = childp;
		childp -> cyclehead = cycleheadp;
#		ifdef DEBUG
		    if ( debug & DFNDEBUG ) {
			printf( "[dfn_findcycle] glomming " );
			printname( childp );
			printf( " onto " );
			printname( cycleheadp );
			printf( "\n" );
		    }
#		endif /* DEBUG */
		for ( tailp = childp ; tailp->cnext ; tailp = tailp->cnext ) {
		    tailp -> cnext -> cyclehead = cycleheadp;
#		    ifdef DEBUG
			if ( debug & DFNDEBUG ) {
			    printf( "[dfn_findcycle] and its tail " );
			    printname( tailp -> cnext );
			    printf( " onto " );
			    printname( cycleheadp );
			    printf( "\n" );
			}
#		    endif /* DEBUG */
		}
	    } else if ( childp -> cyclehead != cycleheadp /* firewall */ ) {
		fprintf(stderr,
			MSGSTR(GLOMMED,
			       "[dfn_busy] glommed, but not to cyclehead\n"));
	    }
	}
    }
}

    /*
     *	deal with self-cycles
     *	for lint: ARGSUSED
     */
dfn_self_cycle( parentp )
    nltype	*parentp;
{
	/*
	 *	since we are taking out self-cycles elsewhere
	 *	no need for the special case, here.
	 */
#   ifdef DEBUG
	if ( debug & DFNDEBUG ) {
	    printf( "[dfn_self_cycle] " );
	    printname( parentp );
	    printf( "\n" );
	}
#   endif /* DEBUG */
}

    /*
     *	visit a node after all its children
     *	[MISSING: an explanation]
     *	and pop it off the stack
     */
dfn_post_visit( parentp )
    nltype	*parentp;
{
    nltype	*memberp;

#   ifdef DEBUG
	if ( debug & DFNDEBUG ) {
	    printf( "[dfn_post_visit]\t%d: " , dfn_depth );
	    printname( parentp );
	    printf( "\n" );
	}
#   endif /* DEBUG */
	/*
	 *	number functions and things in their cycles
	 *	unless the function is itself part of a cycle
	 */
    if ( parentp -> cyclehead == parentp ) {
	dfn_counter += 1;
	for ( memberp = parentp ; memberp ; memberp = memberp -> cnext ) {
	    memberp -> toporder = dfn_counter;
#	    ifdef DEBUG
		if ( debug & DFNDEBUG ) {
		    printf( "[dfn_post_visit]\t\tmember " );
		    printname( memberp );
		    printf( " -> toporder = %d\n" , dfn_counter );
		}
#	    endif /* DEBUG */
	}
    } else {
#	ifdef DEBUG
	    if ( debug & DFNDEBUG ) {
		printf( "[dfn_post_visit]\t\tis part of a cycle\n" );
	    }
#	endif /* DEBUG */
    }
    dfn_depth -= 1;
}
