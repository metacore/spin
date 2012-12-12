/*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 16-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Created from Alpha version.
 *
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/kernel.h>
#include <machine/spl.h>

#define LOGSIZE (8 * 1024)

char printlog[LOGSIZE];
int logend = 0, logstart = 0;

void plog(char *s)
{
    long i, spl;

    if(s == 0)
        return;
    spl = splhigh();
    for(i = 0; i < strlen(s); ++i) {
        printlog[logend] = s[i];
        logend = (logend + 1) % LOGSIZE;
        if(logend == logstart)
            logstart = (logstart + 1) % LOGSIZE;
    }
    splx(spl);
}

void plogi(long l)
{
    char buf[1024];
    long i, spl;

    spl = splhigh();
    sprintf(buf, "%d", l);
    plog(buf);
    splx(spl);
}

void plogx(long l)
{
    char buf[1024];
    long i, spl;

    spl = splhigh();
    sprintf(buf, "%lx", l);
    plog(buf);
    splx(spl);
}

void dumplog()
{
    int i;
    long spl;

    spl = splhigh();
    for(i = logstart; i != logend; i = (i + 1) % LOGSIZE) {
        printf("%c", printlog[i]);
    }
    splx(spl);
}

/* FIXME -- replace with code that writes to a frame buffer */
void drawRect(int funk)
{
}

