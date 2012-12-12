#ifndef __COMMONDEF_H_
#define __COMMONDEF_H_

#include <iostream.h>
// #include <CC/libc.h>
#include <string.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

// Common externs
extern void IncrementDerefCount();
// count total number of dereferences in the program
extern long NumDerefs;

// count the total number of mallocs done in the program
extern long NumMallocs;
extern long NumBytes;

// needed in Bench.cc
extern void Open_Database(char *);
extern void Setup_References();

// needed in GenDB.cc
extern void GenDBInitialize(char *);
extern void GenDBSetRoot();
extern void GenDBFinalize();  
extern void PrintHeapInfo(char* initstr);
extern void BenchInitialize();
extern void BenchFinalize();

#define MULTIPLE_SEGMENT 0


#endif __COMMONDEF_H_
