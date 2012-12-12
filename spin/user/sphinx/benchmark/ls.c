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
#if !defined(lint) && !defined(_NOIDENT)
static char rcsid[] = "@(#)$RCSfile: ls.c,v $ $Revision: 1.3 $ (OSF) $Date: 1997/10/29 18:00:15 $";
#endif
/*
 * COMPONENT_NAME: (CMDFS) ls command - list files or directories
 *
 * FUNCTIONS: main, pdirectory, pem, pentry, pmode, ls_select, column,
 *            new_line, readdirs, gstat, makename, getname, cache_hit,
 *            add_cache, compar, pprintf, savestr
 *
 *
 * (C) COPYRIGHT International Business Machines Corp. 1985, 1989, 1991
 * All Rights Reserved
 *
 * US Government Users Restricted Rights - Use, duplication or
 * disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
 */
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/
/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/
/* static char sccsid[] = "ls.c	1.24"; */
/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 * char copyright[] =
 *  " Copyright (c) 1980 Regents of the University of California.\n\
 * All rights reserved.\n";
 * static char sccsid[] = "ls.c	5.9 (Berkeley) 10/22/87";
 *
 * 1.21.1.12  com/cmd/files/ls.c, cmdfiles, bos320, 9138320 9/6/91 15:00:39
 */

#include	<stdlib.h>
#include	<string.h>
#include	<time.h>
#include 	<ctype.h>
#include <sys/types.h>
#ifndef i386
#include	<wchar.h>
#endif
#include	<mbstr.h>
#include	<sys/types.h>
#include 	<sys/param.h>
#include	<sys/stat.h>
#include	<sys/errno.h>
#include	<stdio.h>
#include	<dirent.h>
#include	<pwd.h>
#include	<grp.h>
#include	<locale.h>
#include	<sys/ioctl.h>
#include	<unistd.h>

#include        <nl_types.h>
#include        <langinfo.h>
#include        "ls_msg.h"
#include <spy.h>
#define ISARG   0x8000 /* this bit equals 1 in lflags of structure lbuf 
                        *  if *namep is to be used;
                        */

#define	NAME_UID	0	/* pass to getname for user name */
#define NAME_GID	1	/*  "   "     "     "  group  "  */

#define MSGSTR(num,str) catgets(catd,MS_LS,num,str)  /*MSG*/

struct	lbuf	{
        union   {                        /* used for filename in a directory */
                char    lname[FILENAME_MAX+1];
		char	*namep;          /* for name in ls-command; */

	} ln;
	char	ltype;  	/* filetype */
	ino_t 	lnum;		/* inode number of file */
	mode_t	lflags; 	/* 0777 bits used as r,w,x permissions */
	short	lnl;    	/* number of links to file */
	uid_t	luid;   	/* local uid */
	gid_t	lgid;   	/* local gid */
	off_t	lsize;  	/* filesize or major/minor dev numbers */
	time_t	lmtime;
	char	*flinkto;	/* symbolic link value */
	mode_t	flinktype;	/* filetype (mode) of obj ref-d by symlink */
	ulong_t	lblkbytes;	/* number of bytes in allocated blocks */
};

struct dchain {
	char *dc_name;		/* path name */
	struct dchain *dc_next;	/* next directory in the chain */
};

struct dchain *dfirst;	/* start of the dir chain */
struct dchain *cdfirst;	/* start of the durrent dir chain */
struct dchain *dtemp;	/* temporary - used for linking */
char *curdir;		/* the current directory */

int	nfiles = 0;	/* number of flist entries in current use */
int	nargs = 0;	/* number of flist entries used for arguments */
int	maxfils = 0;	/* number of flist/lbuf entries allocated */
int	maxn = 0;	/* number of flist entries with lbufs assigned */
int	quantn = 1024;	/* allocation growth quantum */

struct	lbuf	*nxtlbf;	/* pointer to next lbuf to be assigned */
struct	lbuf	**flist;	/* pointer to list of lbuf pointers */
struct	lbuf	*gstat();
char		*savestr();

ulong_t		readdirs();
int     aflg, bflg, cflg, dflg, fflg, gflg, iflg, lflg, mflg;
int     nflg, oflg, pflg, qflg, sflg, tflg, uflg, xflg;
int	Aflg, Cflg, Fflg, Lflg, Rflg;
int	rflg = 1;   /* initialized to 1 for special use in compar() */
int	terminal;	/* indicates whether output device is a terminal */
int     flags;
int	err = 0;	/* Contains return code */

char	*dmark;	/* Used if -p option active. Contains "/" or NULL. */

int	statreq;    /* is > 0 if any of sflg, (n)lflg, tflg are on */

char	*dotp = ".";
char	*makename();
char	tbufu[16], tbufg[16];   /* assumed 15=max. length of user/group name*/

#define REPORT_BSIZE	UBSIZE
#define	BLOCK_BYTES(x)	( (ulong_t) x.st_blocks * (ulong_t) S_BLKSIZE )

time_t	year, now;

int	num_cols;
int	colwidth;
int	filewidth;
int	fixedwidth;
int	curcol;
static int	compar();
static void	pem();
static void pdirectory();
static void pprintf();
static void new_line();
static void pentry();
static void column();
static void pmode();
static int	getname();
static void ls_select();

static char *cache_hit();
static void add_cache();
/* The following are used to calculate maximmum field widths for printing. DAL001 */
static unsigned long max_st_ino = 99999;
static size_t max_st_size = 9999999;
static unsigned long max_lblkbytes = 9999;
static unsigned long max_nlinks = 9999;
static unsigned long max_major = 999;
static unsigned long max_minor = 999;
static int st_ino_width = 5;
static int st_size_width = 7;
static int st_lblkbytes_width = 4;
static int st_nlink_width = 4;
static int st_major_width = 3;
static int st_minor_width = 3;

/* The following are used to reset the value of the maximum field widths */
static unsigned long con_max_st_ino = 99999;
static size_t con_max_st_size = 9999999;
static unsigned long con_max_lblkbytes = 9999;
static unsigned long con_max_nlinks = 9999;
static unsigned long con_max_major = 999;
static unsigned long con_max_minor = 999;

int	mb_cur_max;

nl_catd catd;   /* message catalog descriptor */

static char rec_form_posix[] = "%b %e %H:%M";
static char old_form_posix[] = "%b %e  %Y";
static char *loc_rec_form;
static char *loc_old_form;
/* Function to take the integral log to base 10 of an integer for calculating
   the maximum necessary print field width.  DAL001
 */
static int decimal_width(x)
int x;
{
	int i;

	if(x == 0)
		return 1;
	for(i=0; x; i++)
		x /= 10;
	return i;
}

/* Function to convert all the captured maximum values into maximum necessary
   field widths for printing.  DAL001
 */
static void calc_widths()
{
	int i;

	st_ino_width = decimal_width(max_st_ino);
	st_size_width = decimal_width(max_st_size);
	st_lblkbytes_width = decimal_width(max_lblkbytes/REPORT_BSIZE);/* 003 */
	st_nlink_width = decimal_width(max_nlinks);
	st_major_width = decimal_width(max_major);
	st_minor_width = decimal_width(max_minor);
	if((i=st_minor_width+st_major_width+1) > st_size_width) {
		st_size_width = i;
	} else if(i < st_size_width) {
		i = st_size_width-1;
		if (st_major_width > i/2)
		  st_minor_width = i - st_major_width;
	        else if (st_minor_width > i/2 + i%2)
	          st_major_width = i - st_minor_width;
	        else {
		st_major_width = i/2;
		st_minor_width = i/2+i%2;
		}
	}
}


void
main(argc, argv)
int argc;
char *argv[];
{
	struct	winsize	win;		/* window size structure */
	extern int	optind;
	int	amino, opterr=0;
	int	c;
	register struct lbuf *ep;
	struct	lbuf	lb;
	int	i, width;
	spy_t spy = spy_create("ls", 0);

	/*doredirect(&argc, argv);*/
	spy_start(spy);

	(void) setlocale(LC_ALL,"");
	/*catd = catopen(MF_LS,NL_CAT_LOCALE);*/
	catd = 0;
	mb_cur_max = MB_CUR_MAX;

	loc_rec_form = nl_langinfo(_M_D_RECENT);
	if (loc_rec_form == NULL || !*loc_rec_form)
		loc_rec_form = rec_form_posix;

	loc_old_form = nl_langinfo(_M_D_OLD);
	if (loc_old_form == NULL || !*loc_old_form)
		loc_old_form = old_form_posix;

	lb.lmtime = time((time_t *) NULL);
	year = lb.lmtime - 6L*30L*24L*60L*60L; /* 6 months ago */
	now = lb.lmtime + 60;
	Aflg = !getuid();

	if (isatty(1)) {
		terminal = qflg = Cflg = 1;
	} 
	while ((c=getopt(argc, argv, "1ARadCxmnlogrtucpFbqisfL")) != EOF)
		switch(c) {
		case '1':
			Cflg = 0; 
			continue;
		case 'A':
			Aflg++; 
			continue;
		case 'R':
			Rflg++;
			statreq++;
			continue;
		case 'a':
			aflg++;
			continue;
		case 'd':
			dflg++;
			continue;
		case 'C':
			Cflg = 1;
			mflg = 0;
			lflg = 0;
			continue;
		case 'x':
			xflg = 1;
			Cflg = 1;
			mflg = 0;
			lflg = 0;
			continue;
		case 'm':
			Cflg = 0;
			mflg = 1;
			lflg = 0;
			continue;
		case 'n':
			nflg++;
			/* FALL through */
		case 'N':
		case 'l':
			lflg++;
			Cflg = mflg = xflg = 0;
			statreq++;
			continue;
		case 'o':
			oflg++;
			statreq++;
			continue;
		case 'g':
			gflg++;
			statreq++;
			continue;
		case 'r':
			rflg = -1;
			continue;
		case 't':
			tflg++;
			statreq++;
			continue;
		case 'u':
			uflg++;
			cflg = 0;
			continue;
		case 'c':
			cflg++;
			uflg = 0;
			continue;
		case 'p':
			pflg++;
			statreq++;
			continue;
		case 'F':
			Fflg++;
			statreq++;
			continue;
		case 'b':
			bflg = 1;
			qflg = 0;
			continue;
		case 'q':
			qflg = 1;
			bflg = 0;
			continue;
		case 'i':
			iflg++;
			statreq++;
			continue;
		case 's':
			sflg++;
			statreq++;
			continue;
		case 'f':
			fflg++;
			continue;
		case 'L':
			Lflg++;
			continue;
		case '?':
		default:
			opterr++;
			continue;
		}

	if(opterr) {
		fprintf(stderr, MSGSTR(USAGE,
			"usage: ls [ -1ACFLRabcdfgilmnopqrstux ]  [files]\n"));
		exit(2);
	}
	if (lflg && oflg && gflg)
		oflg = gflg = 0;	/* -lgo, -ngo => System 5 behavior */
	else if (lflg)
		oflg = gflg = 1;	/* -l, -n => System 5 behavior */
					/* -lg, -ng => BSD 4.3 behavior */
					/* -lo, -no similar to -lg */
	else if (oflg || gflg)
		lflg = 1;		/* -o, -g, -og => System 5 */
	if (fflg) {
		aflg++;
		lflg = 0;
		sflg = 0;
		tflg = 0;
		statreq = 0;
	}

	fixedwidth = 2;
	if (pflg || Fflg)
		fixedwidth++;
	if (iflg)
		fixedwidth += 6;
	if (sflg)
		fixedwidth += 5;

	if (lflg) {                             /* This is the way  */
		Cflg = mflg = 0;
	}

	if (Cflg || mflg) {
		char *clptr;
				/* assume 80 column output */
		num_cols = 80;
				/* check for user environment override */
		if ((clptr = getenv("COLUMNS")) != NULL)
			num_cols = atoi(clptr);
				/* otherwise check for tty window size */
		else if (terminal) {
			if ((ioctl(1, TIOCGWINSZ, &win) != -1) &&
					(win.ws_col != 0)) /* not undefined */
				num_cols = win.ws_col;
		}
				/* assume outrageous values are errors */
		if (num_cols < 20 || num_cols > 400)
			num_cols = 80;
	}

	/* allocate space for flist and the associated	*/
	/* data structures (lbufs)			*/
	maxfils = quantn;
	if ( (flist = (struct lbuf **)malloc(
			(size_t)(maxfils * sizeof(struct lbuf *)))) == NULL
		|| (nxtlbf = (struct lbuf *)malloc(
			(size_t)(quantn * sizeof(struct lbuf)))) == NULL) {
		fprintf(stderr, MSGSTR(NOMEMORY, "ls: out of memory\n"));
		exit(2);
	}
	if ((amino=(argc-optind))==0) { /* case when no names are given
					* in ls-command and current 
					* directory is to be used 
 					*/
		argv[optind] = dotp;
	}
	if (amino<1)
	  amino = 1;
	for (i=0; i < amino; i++) {
		if (Cflg || mflg) {
			width = mbswidth(argv[optind], strlen(argv[optind]));
			if (width > filewidth)
				filewidth = width;
		}

		if ((ep = 
			gstat((*argv[optind] ? argv[optind] : dotp), 1, NULL))
			==NULL)
		{
			err = 2;
			optind++;
			continue;
		}
		ep->ln.namep = (*argv[optind] ? argv[optind] : dotp);
		ep->lflags |= ISARG;
		optind++;
		nargs++;	/* count good arguments stored in flist */
	}
	colwidth = fixedwidth + filewidth;
	qsort((void *)flist, (size_t)nargs, (size_t)sizeof(struct lbuf *),(int(*)()) compar);
	for (i=0; i<nargs; i++)
		if (flist[i]->ltype=='d' && dflg==0 || fflg)
			break;
	calc_widths();
	pem(&flist[0],&flist[i], 0);
	for (; i<nargs; i++) {
		pdirectory(flist[i]->ln.namep, (amino>1), nargs);
		/* -R: print subdirectories found */
		while (dfirst || cdfirst) {
			/* Place direct subdirs on front in right order */
			while (cdfirst) {
				/* reverse cdfirst onto front of dfirst */
				dtemp = cdfirst;
				cdfirst = cdfirst -> dc_next;
				dtemp -> dc_next = dfirst;
				dfirst = dtemp;
			}
			/* take off first dir on dfirst & print it */
			dtemp = dfirst;
			dfirst = dfirst->dc_next;
			pdirectory (dtemp->dc_name, 1, nargs);
			free ((void *)dtemp->dc_name);
			free ((void *)dtemp);
		}
	}
	
	spy_stop(spy);
	spy_dump(spy);
	exit(err);
	/* NOTREACHED */
}

/*
 * pdirectory: print the directory name, labelling it if title is
 * nonzero, using lp as the place to start reading in the dir.
 */
static void
pdirectory (name, title, lp)
char *name;
int title;
int lp;
{
	register struct dchain *dp;
	register struct lbuf *ap;
	register char *pname;
	register int j;
	ulong_t	tKblkbytes;
	filewidth = 0;
	curdir = name;
	if (title) {
		putc('\n', stdout);
		pprintf(name, ":");
		new_line();
	}
	nfiles = lp;
/*
 * reset the values of the maximum values
 */
	  max_st_ino = con_max_st_ino;
	  max_st_size = con_max_st_size;
	  max_lblkbytes = con_max_lblkbytes;
	  max_nlinks = con_max_nlinks;
	  max_major = con_max_major;
	  max_minor = con_max_minor;


	tKblkbytes = readdirs(name);
	if (fflg==0)
		qsort((void *)&flist[lp],(size_t)(nfiles - lp), (size_t)sizeof(struct lbuf *),(int(*)())compar);
	if (Rflg) for (j = nfiles - 1; j >= lp; j--) {
		ap = flist[j];
		if (ap->ltype == 'd' &&
				strcmp(ap->ln.lname, ".") &&
				strcmp(ap->ln.lname, "..")) {
			dp = (struct dchain *)calloc((size_t)1,(size_t)sizeof(struct dchain));
			if (dp == NULL)
				fprintf(stderr, 
					MSGSTR(NOMEMORY,"ls: out of memory\n"));
			printf("curdir = %s\n", curdir);
			pname = makename(curdir, ap->ln.lname);
			dp->dc_name = (char *)calloc((size_t)1,(size_t)(strlen(pname)+1));
			if (dp->dc_name == NULL) {
				fprintf(stderr, 
					MSGSTR(NOMEMORY,"ls: out of memory\n"));
				free((void *)dp);
			}
			else {
				strcpy(dp->dc_name, pname);
				dp -> dc_next = dfirst;
				dfirst = dp;
			}
		}
	}
     /* 001GZ 06/30/93. If sflg - print TOTAL on separate line. QAR12375*/
	if ( tKblkbytes != (ulong_t)-1 ) {
	   if ( lflg)
		curcol += printf(MSGSTR(TOTAL, "total %lu"), tKblkbytes);
	   else if ( sflg ) {
		printf(MSGSTR(TOTAL, "total %lu"), tKblkbytes);
		putc('\n', stdout);
		curcol = 0;
	   }
	} /* end of 001GZ */
	calc_widths();

	/* 003
	 * -s option: increase colwidth when max file size >=100M.
	 */
	if (sflg && st_lblkbytes_width > (fixedwidth - 2))
	{
		colwidth += st_lblkbytes_width - fixedwidth + 2;
	}

	/* 001GZ. Deleted sflag from 3-d parameter.
	 * Total is printed on separate line */
	pem(&flist[lp],&flist[nfiles],lflg);
}

/*
 * pem: print 'em.  Print a list of files (e.g. a directory) bounded
 * by slp and lp.
 */
static void
pem(slp, lp, tot_flag)
	register struct lbuf **slp, **lp;
	int tot_flag;
{
	int ncols, nrows, row, col;
	register struct lbuf **ep;

	if (Cflg || mflg)
		ncols = num_cols / colwidth;
	else
		ncols = 1;

	if (ncols <= 1 || mflg || xflg || !Cflg) {
		for (ep = slp; ep < lp; ep++)
			pentry(*ep);
		new_line();
		return;
	}
	/* otherwise print -C columns */
	if (tot_flag)
		slp--;
	nrows = (lp - slp - 1) / ncols + 1;
	for (row = 0; row < nrows; row++) {
		col = (row == 0 && tot_flag);
		for (; col < ncols; col++) {
			ep = slp + (nrows * col) + row;
			if (ep < lp)
				pentry(*ep);
		}
		new_line();
	}
}

static void
pentry(ap)  /* print one output entry;
            *  if uid/gid is not found in the appropriate
            *  file (passwd/group), then print uid/gid instead of 
            *  user/group name;
            */
struct lbuf *ap;
{
	register struct lbuf *p;
	struct tm *timestr;
	char timebuf [NLTBMAX];
	static int datewid = 0, ldatewid, sdatewid;
	p = ap;
	column();
	datewid = 0;
	if (iflg)
		if (mflg && !lflg)
			curcol += printf("%u ", p->lnum);
		else
			curcol += printf("%*u ", st_ino_width, p->lnum);	/*DAL001*/
	if (sflg)
		if(mflg && !lflg)
			curcol += printf("%lu ", p->lblkbytes / REPORT_BSIZE);
		else
			curcol += printf("%*lu ", st_lblkbytes_width, p->lblkbytes / REPORT_BSIZE);	/*DAL001*/
	if (lflg) {
		putchar(p->ltype);
		curcol++;
		pmode(p->lflags);
		curcol += printf("%*d ", st_nlink_width, p->lnl);	/*DAL001*/
		if (oflg)
		  	if(!nflg && getname(p->luid, tbufu, NAME_UID)==0)
				curcol += printf("%-9.9s", tbufu);
			else
				curcol += printf("%-8u ", p->luid);
		if (gflg)
			if(!nflg && getname(p->lgid, tbufg, NAME_GID)==0)
				curcol += printf("%-9.9s", tbufg);
			else
				curcol += printf("%-8u ", p->lgid);
		if (p->ltype=='b' || p->ltype=='c')
			curcol += printf("%*d,%*d", st_major_width, major((int)p->lsize),
			    st_minor_width, minor((int)p->lsize));	/*DAL001*/
		else
			curcol += printf("%*lu", st_size_width, p->lsize);	/*DAL001*/
		timestr = localtime (&p->lmtime);
		/* determine width of date column if not known */
		if (!datewid) {
			sdatewid = strftime (timebuf, (size_t)NLTBMAX,
				 loc_rec_form, timestr);
			sdatewid = strlen(timebuf);
			ldatewid = strftime (timebuf, (size_t)NLTBMAX,
				 loc_old_form, timestr);
			ldatewid = strlen(timebuf);
			datewid = sdatewid > ldatewid ?
			    sdatewid : ldatewid;
		}
		if ((p->lmtime < year) || (p->lmtime > now)) {
			strftime (timebuf, (size_t)NLTBMAX, loc_old_form, timestr);
			curcol += printf(" %*.*s ", -datewid, ldatewid, timebuf);

		}
		else {
			strftime (timebuf, (size_t)NLTBMAX, loc_rec_form, timestr);
			curcol += printf(" %*.*s ", -datewid, sdatewid, timebuf);

		}
	}
	switch(p->ltype) {
	case 'd':	/* Directories */
			dmark = (pflg || Fflg) ? "/" : "";
			break;
	case 'l':	/* Symbolic links */
			dmark = (Fflg) ? "@" : "";
			/* dmark = (Fflg && !lflg) ? "@" : ""; */
			break;
	case 's':	/* Sockets */
			dmark = (Fflg) ? "=" : "";
			break;
        case 'p':       /* FIFO */      /*XZ001*/
                        dmark = (Fflg) ? "|" : "";
                        break;
	default:	/* Executable files */
	         dmark = (Fflg && (p->lflags & 0111)) ? "*" : "";
			break;
	}
	if (p->lflags & ISARG) {
		if (qflg || bflg)
			pprintf(p->ln.namep,dmark);
		else
			curcol += printf("%s%s",p->ln.namep,dmark);
	}
	else {
		if (qflg || bflg)
			pprintf(p->ln.lname,dmark);
		else
			curcol += printf("%s%s",p->ln.lname,dmark);
	}
	if (lflg && p->flinkto) {
		if (Fflg) {
			dmark = "";
			switch (p->flinktype & S_IFMT) {
			case S_IFDIR:
					dmark =  "/";
					break;
			case S_IFSOCK:
					dmark = "=";
					break;
			default:
					if (p->flinktype & 0111)
						dmark = "*";
					break;
			}
		}
		fputs(" -> ", stdout);
		curcol += 4;
		if (qflg || bflg)
			pprintf(p->flinkto, dmark);
		else
			curcol += printf("%s%s", p->flinkto, dmark);
		free((void *)p->flinkto);
        }
   
}

/* print various r,w,x permissions 
 */
static void
pmode(aflag)
mode_t aflag;
{
        /* these arrays are declared static to allow initializations */
	static int	m0[] = { 1, S_IREAD>>0, 'r', '-' };
	static int	m1[] = { 1, S_IWRITE>>0, 'w', '-' };
	static int	m2[] = 
		{ 3, S_ISUID|S_IEXEC, 's', S_IEXEC, 'x', S_ISUID, 'S', '-' };
	static int	m3[] = { 1, S_IREAD>>3, 'r', '-' };
	static int	m4[] = { 1, S_IWRITE>>3, 'w', '-' };
	static int	m5[] = { 3, S_ISGID|(S_IEXEC>>3),'s', 
					S_IEXEC>>3,'x', S_ISGID,'S', '-'};
	static int	m6[] = { 1, S_IREAD>>6, 'r', '-' };
	static int	m7[] = { 1, S_IWRITE>>6, 'w', '-' };
	static int	m8[] = { 3, S_ISVTX|(S_IEXEC>>6),'t', 
					S_IEXEC>>6,'x', S_ISVTX,'T', '-'};
        static int  *m[] = { m0, m1, m2, m3, m4, m5, m6, m7, m8};

	register int **mp;

	flags = aflag;
	for (mp = &m[0]; mp < &m[sizeof(m)/sizeof(m[0])];)
		ls_select(*mp++);
}

static void
ls_select(pairp)
register int *pairp;
{
	register int n;

	n = *pairp++;
	while (n-->0) {
		if((flags & *pairp) == *pairp) {
			pairp++;
			break;
		}else {
			pairp += 2;
		}
	}
	putchar(*pairp);
	curcol++;
}

/*
 * column: get to the beginning of the next column.
 */
static void
column()
{

	if (curcol == 0)
		return;
	if (mflg) {
		putc(',', stdout);
		curcol++;
		if (curcol + colwidth + 2 > num_cols) {
			putc('\n', stdout);
			curcol = 0;
			return;
		}
		putc(' ', stdout);
		curcol++;
		return;
	}
	if (Cflg == 0) {
		putc('\n', stdout);
		curcol = 0;
		return;
	}
	if ((curcol / colwidth + 2) * colwidth > num_cols) {
		putc('\n', stdout);
		curcol = 0;
		return;
	}
	do {
		putc(' ', stdout);
		curcol++;
	} while (curcol % colwidth);
}

static void
new_line()
{
	if (curcol) {
		putc('\n',stdout);
		curcol = 0;
	}
}

/* read each filename in directory dir and store its
 *  status in flist[nfiles] 
 *  use makename() to form pathname dir/filename;
 */
ulong_t
readdirs(dir)
char *dir;
{
	ulong_t	tblkbytes;
	struct dirent *dentry;
	register struct lbuf *ep;
	register int width;
	DIR	*dirf;

	if ((dirf = opendir(dir)) == NULL) {
		fflush(stdout);
		perror(dir);
		/* fprintf(stderr, MSGSTR(UNREADABLE,"%s unreadable\n"),dir); */
		err = 2;
		return( (ulong_t)-1 ); 
	}
        else {
          	tblkbytes = 0;
          	for(;;) {
			errno = 0;
          		if ( (dentry = readdir(dirf)) == NULL)
				if (errno == 0)
          				break;  /* end of directory */
				else {
					fflush(stdout);	/* error */
					perror(dir);
					err = 2;
					closedir(dirf);
					return((ulong_t) -1);
				}
			/* check for directory items '.', '..', 
                         *  and items without valid inode-number;
                         */
          		if (dentry->d_ino ==0
          			 || aflg==0 && dentry->d_name[0]=='.' 
          			 && ( Aflg == 0 || dentry->d_name[1]=='\0' || 
				      dentry->d_name[1]=='.' &&
          			      dentry->d_name[2]=='\0') 
			   )
          			continue;

			if (Cflg || mflg) {
				width = mbswidth(dentry->d_name, strlen(dentry->d_name));
				if (width > filewidth)
					filewidth = width;
			}
          		ep = gstat(makename(dir,dentry->d_name),0,&tblkbytes);
          		if (ep==NULL)
          			continue;
                        else {
			     if (!statreq)                      /*ptm 24874*/
				 ep->lnum = dentry->d_ino;
          		         strcpy(ep->ln.lname, dentry->d_name);
                        }
          	}
          	closedir(dirf);
		colwidth = fixedwidth + filewidth;
	}
	return (tblkbytes / REPORT_BSIZE);
}

/* get status of file and recomputes tblkbytes (number of blocks
 * times bytes per block = number of bytes in allocated blocks);
 * argfl = 1 if file is a name in ls-command and  = 0
 * for filename in a directory whose name is an
 * argument in the command;
 * stores a pointer in flist[nfiles] and
 * returns that pointer;
 * returns NULL if failed;
 */
struct lbuf *
gstat(file, argfl, tblkbytes)
char	*file;
int	argfl;
ulong_t	*tblkbytes;
{
	struct stat statb, statb1;
	register struct lbuf *rep;
	static int nomocore;
	int (*statf)() = Lflg ? (int(*)())stat : (int(*)())lstat;
	if (nomocore)
		return(NULL);
	else if (nfiles >= maxfils) { 
		/* all flist/lbuf pair assigned files 
		   time to get some more space */
		maxfils += quantn;
		if ( (flist=(struct lbuf **)realloc((void *)flist,
			(size_t)(maxfils * sizeof(struct lbuf *)))) == NULL
			|| (nxtlbf = (struct lbuf *)malloc(
			(size_t)(quantn * sizeof(struct lbuf)))) == NULL) {
			fprintf(stderr, MSGSTR(NOMEMORY,"ls: out of memory\n"));
			nomocore = 1;
			return(NULL);
		}
	}

/* nfiles is reset to nargs for each directory
 * that is given as an argument maxn is checked
 * to prevent the assignment of an lbuf to a flist entry
 * that already has one assigned.
 */
	if(nfiles >= maxn) {
		rep = nxtlbf++;
		flist[nfiles++] = rep;
		maxn = nfiles;
	}else {
		rep = flist[nfiles++];
	}
	rep->lflags = 0;
	if (argfl || statreq) {
		if ((*statf)(file, &statb) < 0)
			if (statf == lstat || lstat(file, &statb) < 0) {
			if (errno==EACCES)
			  fprintf(stderr,MSGSTR(NOPERM, "%s: No permission\n"),
				  file);
			else
			  fprintf(stderr, MSGSTR(NOTFOUND, "%s not found\n"),
				file);
			nfiles--;
			return(NULL);
		}

		rep->lnum = statb.st_ino;
		rep->lsize = statb.st_size;
		rep->lblkbytes = BLOCK_BYTES(statb);
		rep->flinkto = NULL;
		switch(statb.st_mode & S_IFMT) {

	            	case S_IFDIR:
				rep->ltype = 'd';
	            		break;

	            	case S_IFBLK:
				rep->ltype = 'b';
				rep->lsize = statb.st_rdev;
	            		break;

	            	case S_IFCHR:
				rep->ltype = 'c';
				rep->lsize = statb.st_rdev;
	            		break;

			case S_IFSOCK:
				rep->ltype = 's'; 
				rep->lsize = 0; 
				break;

			case S_IFLNK:
				rep->ltype = 'l';
                        	if (lflg) {
					char	buf[BUFSIZ];
					int	cc;
                                	cc = readlink(file, buf, BUFSIZ);
                                	if (cc >= 0) {
                                        	buf[cc] = 0;
                                        	rep->flinkto = savestr(buf);
                               	 	}
					if (stat(file, &statb1) >= 0)
						rep->flinktype = statb1.st_mode;
                        	} else if (argfl
				    && stat(file, &statb1) >= 0
				    && (statb1.st_mode & S_IFMT) == S_IFDIR) {
					statb = statb1;
					rep->ltype = 'd';
					rep->lsize = statb.st_size;
					rep->lblkbytes = BLOCK_BYTES(statb);
				}
				break;

	            	case S_IFIFO:
				rep->ltype = 'p';
                 		break;
                        default:
				rep->ltype = '-';
                 }
#ifdef DEBUG
	printf	( "after stat( ino, size, mode, uid, gid): %d %d %o %d %d\n",
		statb.st_ino, statb.st_size,
		statb.st_mode, statb.st_uid,
		statb.st_gid
 		);
#endif
		rep->lflags = statb.st_mode & ~S_IFMT;
                                   /* mask ISARG and other file-type bits */
		rep->luid = statb.st_uid;
		rep->lgid = statb.st_gid;
		rep->lnl = statb.st_nlink;
          	if(uflg)
			rep->lmtime = statb.st_atime;
          	else if (cflg)
			rep->lmtime = statb.st_ctime;
          	else
			rep->lmtime = statb.st_mtime;
		if (tblkbytes != NULL)
			*tblkbytes += rep->lblkbytes;
#ifdef DEBUG
	printf( "type, mode, inum, size, uid, gid): '%c' %o %d %d %d %d\n",
		rep->ltype, rep->lflags, rep->lnum, rep->lsize, rep->luid, rep->lgid
	);
#endif

	}
	if(statb.st_ino > max_st_ino)
		max_st_ino = statb.st_ino;
	if(statb.st_size > max_st_size)
		max_st_size = statb.st_size;
	if(rep->lblkbytes >  max_lblkbytes)
		max_lblkbytes = rep->lblkbytes;
	if(statb.st_nlink > max_nlinks)
		max_nlinks = statb.st_nlink;
	if(rep->ltype == 'c' || rep->ltype == 'b') {
		if(major(rep->lsize) > max_major)
			max_major = major(rep->lsize);
		if(minor(rep->lsize) > max_minor)
			max_minor = minor(rep->lsize);
	}
        return(rep);
}


/* returns pathname of the form dir/file;
 *  dir is a null-terminated string;
 */
char *
makename(dir, file) 
char *dir, *file;
{
	register char *dp, *fp;
	register int i;
	static	int dflen = 0;		/* length of malloc'd dfile */
	static	char *dfile = NULL;

	if ((i = strlen(dir) + strlen(file) + 2) > dflen) {
		if (dfile != NULL)
			free((void *)dfile);
		if ((dfile = malloc((size_t)i)) == NULL) {
			fprintf(stderr,MSGSTR(NOMEMORY,"ls: out of memory\n"));
			exit(1);
		}
		dflen = i;
	}
	dp = dfile;
	fp = dir;
	while (*fp)
		*dp++ = *fp++;
	if (dp > dfile && *(dp - 1) != '/')
		*dp++ = '/';
	fp = file;
	strcpy(dp,fp);
	return(dfile);
}

/* get name from passwd/group file for a given uid/gid
 * and store it in buf; Use a LRU cache system to cut down on
 * accesses to the passwd and group files.
 */
struct idcache {
	uid_t 	id;
	char	*name;
	struct idcache *forward;
	struct idcache *back;
};

struct idcache *uid_head, *gid_head;
int	uid_cachesize = 0, gid_cachesize = 0;
#define MAX_CACHE	255
static int
getname(uid, buf, type)
uid_t	uid;
int type;
char buf[];
{
	char *ptr, *cache_hit();
	struct passwd *pwd;
	struct group  *grp;

	switch (type) {
	case NAME_UID:	if ((ptr = cache_hit(&uid_head, uid)) == NULL) {
				if ((pwd = getpwuid((uid_t)uid)) == NULL) 
					return(1);
				ptr = pwd->pw_name;
				add_cache(&uid_head, uid, ptr, &uid_cachesize);
			}
			break;

	case NAME_GID:	if ((ptr = cache_hit(&gid_head, uid)) == NULL) {
				if ((grp = getgrgid((uid_t)uid)) == NULL) 
					return(1);
				ptr = grp->gr_name;
				add_cache(&gid_head, uid, ptr, &gid_cachesize);
			}
			break;
	default: 	return(1);
	}
	strcpy(buf, ptr);
	return(0);
}

static char *
cache_hit(head, id)
struct idcache **head;
unsigned id;
{
	register struct idcache *p;
	for (p = *head; p != NULL; p = p->forward) {
		if (p->id == id) {
			if (p != *head) {
				/* take it out of the list */
				p->back->forward = p->forward;
				p->forward->back = p->back;
				/* Add it at the head */
				p->forward = *head;
				p->back = (*head)->back;
				(*head)->back->forward = p;
				(*head)->back = p;
				*head = p;
			}
			return(p->name);
		}
		if (p == (*head)->back)
			break;
	}
	return(NULL);
}

static void
add_cache(head, id, name, size)
struct  idcache **head;
unsigned id;
char *name;
int	*size;
{
	register struct idcache *ptr;

	if (*size < MAX_CACHE) {
		if ((ptr = (struct idcache *)malloc((size_t)sizeof(struct idcache)))
								== NULL) {
			fprintf(stderr, MSGSTR(ADDCACHE, 
				"add_cache: no more memory\n"));
			exit(1);
		}
	}
	else {
		ptr = (*head)->back;
		free((void *)ptr->name); /* free him we're going to malloc a new one */
	}

	/* fill in the struct */
	ptr->name = savestr(name);
	ptr->id = id;
	if (*size < MAX_CACHE) {
		ptr->forward = ptr->back = NULL;
	}

	/* add to the list */
	if (*size < MAX_CACHE) {
		(*size)++; 
		if (*head == NULL) {
			*head = ptr;
			ptr->forward = *head;
			ptr->back = *head;
		}
		else {
			ptr->forward = *head;
			ptr->back = (*head)->back;
			(*head)->back->forward = ptr;
			(*head)->back = ptr;
			*head = ptr;
		}
	}
	else
		*head = ptr;
}

/* return >0 if item pointed by pp2 should appear first */
static int
compar(pp1, pp2)
struct lbuf **pp1;
struct lbuf **pp2;
{
	register struct lbuf *p1, *p2;

	p1 = *pp1;
	p2 = *pp2;
	if (dflg==0) {
/* compare two names in ls-command one of which is file
 *  and the other is a directory;
 *  this portion is not used for comparing files within
 *  a directory name of ls-command;
 */
		if (p1->lflags&ISARG && p1->ltype=='d')
		{
			if (!(p2->lflags&ISARG && p2->ltype=='d'))
				return(1);
                }
                else {
			if (p2->lflags&ISARG && p2->ltype=='d')
				return(-1);
		}
	}
	if (tflg) {
		if(p2->lmtime == p1->lmtime)
			return(0);
		else if(p2->lmtime > p1->lmtime)
			     return(rflg);
		else return(-rflg);
	}
        else
	return (rflg * strcoll(
	    p1->lflags&ISARG ? p1->ln.namep:p1->ln.lname,
	    p2->lflags&ISARG ? p2->ln.namep:p2->ln.lname));
}

static void
pprintf(s1,s2)
	char *s1, *s2;
{
	register uchar_t *s;
	register int   c;
	register int  cc;
        int z;
	wchar_t wct;
	int i;

        for (s = (uchar_t *)s1, i = 0; i < 2; i++, s = (uchar_t *)s2) {
                while((c = mbtowc(&wct, (char *)s, mb_cur_max))>0) {
                        if (! iswprint (wct)) {
                                for(z=0; z<c; z++) {
					char outc = *s;

                                        if (qflg)
                                                outc = '?';
                                        else if (bflg) {
                                                curcol += 3;
                                                putc ('\\', stdout);
                                                cc = '0' + (*s>>6 & 07);
                                                putc (cc, stdout);
                                                cc = '0' + (*s>>3 & 07);
                                                putc (cc, stdout);
                                                outc = '0' + (*s & 07);
                                                }       /* if (bflg)        */
                                        putc(outc, stdout);
                                        curcol++;
                                        s++;
                                        }
                                continue;
                                }  /* if (! iswprint() */

			/* printable mb char */
                        for(z=0; z<c; z++)              
                                putc(*s++, stdout);

			/* call wcwidth for mb case only */
			if (c == 1)	
			        curcol++;
			else
			        curcol+=wcwidth(wct);
                        }
                }
}

char *
savestr(str)
        char *str;
{
        char *cp = malloc((size_t)(strlen(str) + 1));

        if (cp == NULL) {
                fprintf(stderr, MSGSTR(NOMEMORY, "ls: out of memory\n"));
                exit(1);
        }
        (void) strcpy(cp, str);
        return (cp);
}
