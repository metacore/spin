#include <limits.h>
#include "rpcc.h"

#define MACHINE_HZ 175 /* 175MHz Alpha */
#define REPETITION 200

main ()
{
    unsigned start, stop;
    unsigned cycles;
    int i;
    spy_t null_spy = spy_create("null", REPETITION);
    spy_t getpid_spy = spy_create("getpid", REPETITION);

    for (i = 0; i < REPETITION; i++) {
	spy_start(null_spy);
	spy_stop(null_spy);
    }

    for (i = 0; i < REPETITION; i++) {
	spy_start(getpid_spy);
	getpgrp();
	spy_stop(getpid_spy);
    }
    spy_dump_all();
}	   
