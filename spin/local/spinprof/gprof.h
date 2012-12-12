/*
 * *****************************************************************
 * *                                                               *
 * *    Copyright (c) Digital Equipment Corporation, 1991, 1995    *
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
/* @(#)$RCSfile: gprof.h,v $ $Revision: 1.1 $ (OSF) $Date: 1997/05/23 18:35:08 $ */
/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */
/***
 ***	@(#)gprof.h	5.9 (Berkeley) 6/1/90
 ***/

#ifndef _GPROF_H_
#define _GPROF_H_


#ifndef _SYS_TYPES_H
#include <sys/types.h>
#endif

    /*
     * The header on the gmon.out file.
     * gmon.out consists of one of these headers,
     * and then an array of ncnt samples
     * representing the discretized program counter values.
     */

struct phdr {
    char        *lpc;
    char        *hpc;
    int         ncnt;
#ifdef KERNEL
    /*
     * The kernel uses the rest of this structure in startup()
     * to keep track of allocated memory for profiling.
     */
    char	*pc_buf;	/* address of pc sample buckets */
    char	*bb_buf;	/* address of bb counters */
    int		bb_bytes;	/* size of invocation/bb counters in bytes */
    char	*tos_buf;	/* address of tos array */
    int		tos_bytes;	/* size of gprof tos array in bytes */
    char	*froms_buf;	/* address of froms array */
    int		froms_bytes;	/* size of gprof froms array in bytes */
    int		sample_hz;	/* frequency of sampling */
    int		proftype;	/* profile type, see below */
#endif /* KERNEL */
};

    /*
     *  histogram counters are unsigned shorts (according to the kernel).
     */
#define HISTCOUNTER     unsigned short

    /*
     *  fraction of text space to allocate for histogram counters
     *  here, 1/2
     */
#define HISTFRACTION    2

    /*
     *  Fraction of text space to allocate for from hash buckets.
     *  The value of HASHFRACTION is based on the minimum number of bytes
     *  of separation between two subroutine call points in the object code.
     *  Given MIN_SUBR_SEPARATION bytes of separation the value of
     *  HASHFRACTION is calculated as:
     *
     *          HASHFRACTION = MIN_SUBR_SEPARATION / (2 * sizeof(short) - 1);
     *
     *  For the VAX, the shortest two call sequence is:
     *
     *          calls   $0,(r0)
     *          calls   $0,(r0)
     *
     *  which is separated by only three bytes, thus HASHFRACTION is
     *  calculated as:
     *
     *          HASHFRACTION = 3 / (2 * 2 - 1) = 1
     *
     *  Note that the division above rounds down, thus if MIN_SUBR_FRACTION
     *  is less than three, this algorithm will not work!
     *
     *  On alpha, the minimum distance between two subroutine call instructions
     *  is 4 bytes.
     */
#define HASHFRACTION    1

    /*
     *  percent of text space to allocate for tostructs
     *  with a minimum.
     */
#define ARCDENSITY      2
#define MINARCS         50
#define MAXARCS		65534

struct tostruct {
    char                *selfpc;
    long                count;
    unsigned short      link;
};

    /*
     *  a raw arc,
     *      with pointers to the calling site and the called site
     *      and a count.
     */
struct rawarc {
    unsigned long       raw_frompc;
    unsigned long       raw_selfpc;
    long                raw_count;
};

    /*
     *  general rounding functions.
     */
#define ROUNDDOWN(x,y)  (((x)/(y))*(y))
#define ROUNDUP(x,y)    ((((x)+(y)-1)/(y))*(y))

#ifdef KERNEL
/*
 * profile types
 */
#define PC_SAMPLES	0x1	/* pc samples taken */
#define INV_COUNTS	0x2	/* procedure invocations */
#define BB_COUNTS	0x4	/* basic block counts */
#define GPROF_COUNTS	0x8	/* gprof call graph info */

#define BB_SCALE	2	/* half of textsize for bb counts */
#endif /* KERNEL */

#endif /* _GPROF_H_ */
