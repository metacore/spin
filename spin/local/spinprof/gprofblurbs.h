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
 * @(#)$RCSfile: gprofblurbs.h,v $ $Revision: 1.3 $ (DEC) $Date: 1997/05/23 18:35:11 $
 */
/*
 * (c) Copyright 1993, OPEN SOFTWARE FOUNDATION, INC.
 * ALL RIGHTS RESERVED
 */
/*
 * OSF/1 Release 1.2
 */

/*
 * Created to hold the field descriptions that used to be in 
 * separate files.  Eventually these descriptions should migrate
 * to the I18N message file.
 */

static char	flat_blurb_buf[] =
"								\n\
								\n\
								\n\
flat profile:							\n\
								\n\
								\n\
 %%         the percentage of the total running time of the	\n\
time       program used by this function.			\n\
								\n\
cumulative a running sum of the number of seconds accounted	\n\
 seconds   for by this function and those listed above it.	\n\
								\n\
 self      the number of seconds accounted for by this		\n\
seconds    function alone.  This is the major sort for this	\n\
           listing.						\n\
								\n\
calls      the number of times this function was invoked, if	\n\
           this function is profiled, else blank.		\n\
							 	\n\
 self      the average number of microseconds spent in this	\n\
us/call    function per call, if this function is profiled,	\n\
	   else blank.						\n\
								\n\
 total     the average number of microseconds spent in this	\n\
us/call    function and its descendents per call, if this 	\n\
	   function is profiled, else blank.			\n\
								\n\
 self       the average number of cycles spent in this	        \n\
cycles/call function per call, if this function is profiled,	\n\
	    else blank.						\n\
								\n\
 total      the average number of cycles spent in this          \n\
cycles/call function and its descendents per call, if this 	\n\
	    function is profiled, else blank.			\n\
								\n\
name       the name of the function.  This is the minor sort	\n\
           for this listing. The index shows the location of	\n\
	   the function in the gprof listing. If the index is	\n\
	   in parenthesis it shows where it would appear in	\n\
	   the gprof listing if it were to be printed.		\n\
								\n\
";

static char	callg_blurb_buf[] =
"								\n\
								\n\
								\n\
call graph profile:						\n\
          The sum of self and descendents is the major sort	\n\
          for this listing.					\n\
								\n\
          function entries:					\n\
								\n\
index     the index of the function in the call graph		\n\
          listing, as an aid to locating it (see below).	\n\
								\n\
%%time     the percentage of the total time of the program	\n\
          accounted for by this function and its		\n\
          descendents.						\n\
								\n\
self      the number of seconds spent in this function		\n\
          itself.						\n\
								\n\
descendents							\n\
          the number of seconds spent in the descendents of	\n\
          this function on behalf of this function.		\n\
								\n\
called    the number of times this function is called (other	\n\
          than recursive calls).				\n\
								\n\
self      the number of times this function calls itself	\n\
          recursively.						\n\
								\n\
name      the name of the function, with an indication of	\n\
          its membership in a cycle, if any.			\n\
								\n\
index     the index of the function in the call graph		\n\
          listing, as an aid to locating it.			\n\
								\n\
								\n\
								\n\
          parent listings:					\n\
								\n\
self*     the number of seconds of this function's self time	\n\
          which is due to calls from this parent.		\n\
								\n\
descendents*							\n\
          the number of seconds of this function's		\n\
          descendent time which is due to calls from this	\n\
          parent.						\n\
								\n\
called**  the number of times this function is called by	\n\
          this parent.  This is the numerator of the		\n\
          fraction which divides up the function's time to	\n\
          its parents.						\n\
								\n\
total*    the number of times this function was called by	\n\
          all of its parents.  This is the denominator of	\n\
          the propagation fraction.				\n\
								\n\
parents   the name of this parent, with an indication of the	\n\
          parent's membership in a cycle, if any.		\n\
								\n\
index     the index of this parent in the call graph		\n\
          listing, as an aid in locating it.			\n\
								\n\
								\n\
								\n\
          children listings:					\n\
								\n\
self*     the number of seconds of this child's self time	\n\
          which is due to being called by this function.	\n\
								\n\
descendent*							\n\
          the number of seconds of this child's descendent's	\n\
          time which is due to being called by this		\n\
          function.						\n\
								\n\
called**  the number of times this child is called by this	\n\
          function.  This is the numerator of the		\n\
          propagation fraction for this child.			\n\
								\n\
total*    the number of times this child is called by all	\n\
          functions.  This is the denominator of the		\n\
          propagation fraction.					\n\
								\n\
children  the name of this child, and an indication of its	\n\
          membership in a cycle, if any.			\n\
								\n\
index     the index of this child in the call graph listing,	\n\
          as an aid to locating it.				\n\
								\n\
								\n\
								\n\
          * these fields are omitted for parents (or		\n\
          children) in the same cycle as the function.  If	\n\
          the function (or child) is a member of a cycle,	\n\
          the propagated times and propagation denominator	\n\
          represent the self time and descendent time of the	\n\
          cycle as a whole.					\n\
								\n\
          ** static-only parents and children are indicated	\n\
          by a call count of 0.					\n\
								\n\
								\n\
								\n\
          cycle listings:					\n\
          the cycle as a whole is listed with the same		\n\
          fields as a function entry.  Below it are listed	\n\
          the members of the cycle, and their contributions	\n\
          to the time and call counts of the cycle.		\n\
								\n\
";

static char	*callg_blurb_ptr = (char *)callg_blurb_buf;
static char	*flat_blurb_ptr = (char *)flat_blurb_buf;
