#ifndef __BENCH_H
#define __BENCH_H

#if defined(RVM)
#include "rvm_def.h"
#elif defined(SPIN)
#include "spin_def.h"
#elif defined(OSTORE)
#include "ostore_def.h"
#else
#error ???? you have to set database name.
#endif

#endif

