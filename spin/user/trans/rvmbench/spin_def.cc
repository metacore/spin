#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/mman.h>
#include <mach.h>
#include "getopt.h"
#include "work.h"
#include "bench.h"

struct root_struct *_root;
long _sid;
long _tid;
static char *data_device = "rds_data";
unsigned long write_time;

void rvmlib_internal_abort(char *errmsg) 
{
    fprintf(stderr, errmsg);
    abort();
}

static void 
help (char *argv0)
{
    fputs("RVMBENCH : a crappy TPC-A benchmark for RVM&SPIN.\n", stderr);
    fprintf(stderr, "%s [options]\n", argv0);
    fputs("\t[--ntrans NTRANS] [--naccounts NACCOUNTS]\n", stderr);
    fputs("  -h, --help : display this message.\n", stderr);
    fputs("  -p, --profile : run SPIN profiler.\n", stderr);
    fputs("  -R, --random : run randomized transactions(default).\n", stderr);
    fputs("  -L, --localized : run localized transactions.\n", stderr);
    fputs("  -b, --barrier N : wait for N procs before starting benchmark\n",
	  stderr);
    fputs("  -f, --file FILE : set the database file name.\n", stderr);
    fputs("  -n, --ntrans N : set the # of transactions(default 4000)\n",
	  stderr);
    fputs("  -a, --naccounts N : set the # of accounts(default 32768)\n", 
	  stderr);
    fputs("  -G, --pagelog : page grain logging instead of page diffing.\n", 
	  stderr);
}

struct option longopts[] =
  {
    { "help", 0, 0, 'h' },
    { "spy", 0, 0, 'y' },
    { "random", 0, 0, 'R' },
    { "localized", 0, 0, 'L' },
    { "file", 1, 0, 'f' },
    { "barrier", 1, 0, 'b' },
    { "ntrans", 1, 0, 'n' },
    { "naccounts", 1, 0, 'a' },
    { "type", 1, 0, 't' },
    { "profile", 0, 0, 'p' },
    { "silent", 0, 0, 's' },
    { "trace", 0, 0, 'x' },
    { "pagelog", 0, 0, 'G' },
    { 0, 0, 0, 0 }
  };

#ifdef osf
extern "C" bench_start(int argc, char **argv);
bench_start(int argc, char **argv)
#else
main(int argc, char **argv)
#endif
{
    int fd;
    int c;
    int silent = 0;
    int trace = 0;

    while ((c = getopt_long(argc, argv, 
		    "t:b:f:d:l:n:a:phRLsxyG", longopts, (int*)0)) != EOF) {
	switch (c) {
	  case 'R':
	    RANDOM = 1;
	    break;
	  case 'L':
	    RANDOM = 0;
	    break;
	  case 'f':
	    data_device = optarg;
	    break;
	  case 'b':
	    do_barrier = atoi(optarg);
	    break;
	  case 'n':
	    NTRANS = atoi(optarg);
	    break;
	  case 'a':
	    NACCOUNTS = atoi(optarg);
	    break;
	  case 'p':
	    do_profile = 1;
	    break;
	  case 's':
	    do_silent = 1;
	    break;
	  case 'x':
	    do_trace = 1;
	    break;
	  case 'G':
	    trans_mode = TRANSMODE_PAGEGRAINLOGGING;
	    break;
	  case 'h': /* fallthrough */
	  default:
	    help(argv[0]);
	    exit(1);
	}
    }
    bootstrap_trans();
    _sid = trans_open(data_device);
    trans_mmap((void*)0x142000000, 
	       256 * 1024*1024, PROT_READ|PROT_WRITE,
	       MAP_FILE|MAP_SHARED, _sid, 0);
    _root = (void*)0x142000000;
    run_trial();
    if (do_spy) { USyscall_System("spy off"); }
    if (do_spy) { USyscall_System("spy dump"); }
    printf("rvm benchmark finished.\n");
}
