/*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 26-Mar-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added unlock_unlocked.
 *
 * 10-Dec-94  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Log functions for use in debugging.
 */
#define SPLEXTREME 7

#define LOGSIZE 8192

char printlog[LOGSIZE];
int logend = 0, logstart = 0;

void plog(char *s)
{
    long i, spl;

    if(s == 0)
        return;
    spl = SpinSwapIpl(SPLEXTREME);
    for(i = 0; i < strlen(s); ++i) {
        printlog[logend] = s[i];
        logend = (logend + 1) % LOGSIZE;
        if(logend == logstart)
            logstart = (logstart + 1) % LOGSIZE;
    }
    SpinSwapIpl(spl);
}

void plogi(long l)
{
    char buf[1024];
    long i, spl;

    spl = SpinSwapIpl(SPLEXTREME);
    sprintf(buf, "%d", l);
    plog(buf);
    SpinSwapIpl(spl);
}

void plogx(long l)
{
    char buf[1024];
    long i, spl;

    spl = SpinSwapIpl(SPLEXTREME);
    sprintf(buf, "%lx", l);
    plog(buf);
    SpinSwapIpl(spl);
}

void dumplog()
{
    int i;
    long spl;

    spl = SpinSwapIpl(SPLEXTREME);
    for(i = logstart; i != logend; i = (i + 1) % LOGSIZE) {
        printf("%c", printlog[i]);
    }
    SpinSwapIpl(spl);
}

void unlock_unlocked ()
{
  printf("ERROR >> attempt to unlock an unlocked lock\n");
  Debugger();
}
