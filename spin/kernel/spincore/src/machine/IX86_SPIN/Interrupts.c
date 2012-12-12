/*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 16-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Created.
 *
 *
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/kernel.h>
#include <machine/cpufunc.h>

enum InterruptClass {Low, SoftClock, IO, Clock, High};

unsigned SetInterruptMask(enum InterruptClass class)
{
    unsigned oldspl;

    switch(class)
	{
	  case Low:
	    oldspl = cpl;
	    spl0();
	    return oldspl;

	  case SoftClock:
	    return splsoftclock();

	  case IO:
	    return splhigh();
	  
	  case Clock:
	    return splclock();
	   
	  case High:
	    return splhigh();

	  default:
	    panic("Unknown interrupt class");
	}
}

void RestoreInterruptMask(unsigned mask)
{
    splx(mask);
}


unsigned RTSetInterruptMask(enum InterruptClass class)
{
    unsigned oldspl;

    switch(class)
	{
	  case Low:
	    oldspl = cpl;
	    spl0();
	    return oldspl;

	  case SoftClock:
	    return splsoftclock();

	  case IO:
	    return splhigh();
	  
	  case Clock:
	    return splclock();
	   
	  case High:
	    return splhigh();

	  default:
	    panic("Unknown interrupt class");
	}
}

void RTRestoreInterruptMask(unsigned mask)
{
    splx(mask);
}

int IsLow(unsigned level)
{
    if(level)
	return 0;
    else
	return 1;
}

int IsHigh(unsigned level)
{
    if(~level)
	return 0;
    else
	return 1;
}

