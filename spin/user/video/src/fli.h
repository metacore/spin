/* 
 * Mach Operating System
 * Copyright (c) 1991,1990,1989,1988,1987 Carnegie Mellon University
 * All Rights Reserved.
 * 
 * Permission to use, copy, modify and distribute this software and its
 * documentation is hereby granted, provided that both the copyright
 * notice and this permission notice appear in all copies of the
 * software, derivative works or modified versions, and any portions
 * thereof, and that both notices appear in supporting documentation.
 * 
 * CARNEGIE MELLON ALLOWS FREE USE OF THIS SOFTWARE IN ITS "AS IS"
 * CONDITION.  CARNEGIE MELLON DISCLAIMS ANY LIABILITY OF ANY KIND FOR
 * ANY DAMAGES WHATSOEVER RESULTING FROM THE USE OF THIS SOFTWARE.
 * 
 * Carnegie Mellon requests users of this software to return to
 * 
 *  Software Distribution Coordinator  or  Software.Distribution@CS.CMU.EDU
 *  School of Computer Science
 *  Carnegie Mellon University
 *  Pittsburgh PA 15213-3890
 * 
 * any improvements or extensions that they make and grant Carnegie Mellon
 * the rights to redistribute these changes. */
/*
 *	Created
 *	[92/02/01	savage]
 */

#ifndef _FLI_H
#define _FLI_H

#include <mach/boolean.h>
#include "video.h"

#if	__alpha
/* 
 * this stuff comes from a world where int == long,
 * so we need to convince alpha this is the case ...
 */
#define	long	int
#endif	__alpha

#define oldstylefli 0
struct fli_file_header
{
   long		size;		/* total size of file (in bytes) */
   unsigned short	magic;		/* header magic to check format */
   unsigned short	frames;		/* number of frames */
   unsigned short	width;		/* screen width (in pixels) */
   unsigned short	height;		/* screen height (in pixels) */
#if oldstylefli > 0
   unsigned short       flags;
   unsigned short       res1;
#else
   unsigned short	depth;		/* screen depth (in bits/pixel) */
   unsigned short	flags;		/* should be 0 */
#endif
   unsigned short	speed;		/* number of video ticks between frames */
   long 	next;		/* should be 0 */
   long		frit;		/* should be 0 */
   char 	expand[102];	/* should be 0 */
};

struct fli_frame_header
{
   long		  size;		/* size of frame (in bytes) */
   unsigned short magic;	/* type of frame (CLR, LC, BLK, BRUN, CPY) */
   unsigned short chunks;	/* number of "chunks" in frame */
   char		  expand[8];	/* should be 0 */
};

struct fli_chunk_header
{
   long			size;		/* size of chunk (in bytes) */
   unsigned short	type;	/* chunk type */

};

#define FLI_FILE_HEADER_SIZE	128
#define FLI_FRAME_HEADER_SIZE	16
#define FLI_CHUNK_HEADER_SIZE	6

#if	__alpha
/*
 * offsets of 'size' field in frame and chunk structs
 * see fli.c for why alpha needs these
 */
#define FLI_FRAME_HEADER_SIZE_OFFSET	0
#define FLI_CHUNK_HEADER_SIZE_OFFSET	0
#endif	__alpha

#define FLI_FILE_HEADER_MAGIC	0xaf11
#define FLI_FILE_HEADER_MAGIC_V26976 0xaf12
#define FLI_FRAME_HEADER_MAGIC	0xf1fa
#define FLI_FRAME_HEADER_MAGIC_V26976	0xf100


#define FLI_CHUNK_4              4
#define FLI_LC7                  7
#define FLI_CLR			11
#define FLI_LC			12
#define FLI_BLK			13
#define FLI_BRUN		15
#define FLI_CPY			16
#define FLI_MINI			18

#define FLI_MAX_COLORS  256

void fli_clr(video_t dpy,
	     unsigned short cbits,
	     char *chunk_buff,
	     boolean_t lsb);

void fli_lc(video_t dpy,
	    char *chunk_buff,
	    boolean_t lsb);

void fli_brun(video_t dpy,
	      unsigned short height,
	      unsigned short width,
	      char *chunk_buff,
	      boolean_t lsb);

void fli_display_frame(struct fli_frame_header *fh, video_t dpy,
		      unsigned short height, unsigned short width,
		      char *buff, boolean_t lsb);

void fli_play(char * buff, 
	      video_t dpy,
	      unsigned short height,
	      unsigned short width,
	      unsigned short frames,
	      unsigned short repeat,
	      unsigned short speed);

unsigned int fli_total_size(char *refany);

unsigned short fli_frames(char *refany);
unsigned short fli_width(char *refany);
unsigned short fli_height(char *refany);

boolean_t fli_magic(char *refany);
boolean_t fli_read_frame_header(char *refany, struct fli_frame_header *fh, unsigned short *offset, boolean_t *lsb);
unsigned int fli_size(char *refany, unsigned short offset, boolean_t lsb);

void fli_dump(char *refany);
#ifdef	__alpha
/* roll back grotesque hack from above */
#undef	long
#endif	__alpha

#endif _FLI_H


