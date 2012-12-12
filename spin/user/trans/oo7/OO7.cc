#include <iostream.h>
#include <unistd.h>

#include "macrodef.h"

#include "GenList.h"
#include "GenVHSet.h"
#include "GenVHBag.h"
#include "GenBBag.h"
#include "GenAVLIndex.h"

#include "OO7.h"
#include "BenchParams.h"
#include "GenParams.h"
#include "VarParams.h"

extern void GenDB(int, char **);
extern void Bench(int, char **);

#ifdef LOG
#include "Logger.h"
#endif  

#include "utils.h"

struct rusage startUsage, endUsage;
struct timeval startWallTime;
struct timeval tempWallTime;
struct timeval endWallTime;
struct timezone ignoreTimeZone;
struct timeval startWarmTime;

const int FALSe = 0;
const int TRUe = 1;

int	nextAtomicId = 1;
int	nextCompositeId = 1;
int	nextComplexAssemblyId = 1;
int	nextBaseAssemblyId = 1;
int	nextModuleId = 1;

int	debugMode = FALSe;
int	clusterLevel = 0;
int     printHeap = FALSe;

char    *types[NumTypes] = {
  "type000", "type001", "type002", "type003", "type004",
  "type005", "type006", "type007", "type008", "type009"
  };

int cmpint(int,int);
int cmpstr(char *, char *);

/************************************************************************/
// The following containers  are in PERSISTENT segments
/************************************************************************/

// Containers for the database and indexes.
// All variables are ***pointers***  to sets and indexes
// Design library = Set of Composite Parts

void *DesignLib;

// Main program. Looks at command line and decides whether to call
// GenDB or Bench.
// main() is defined in libutils.a

void usage()
{
    cout << "Usage: OO7 [-r LOG] [-gbG] [-d] [-f datafile] args..\n";
    cout << "if -g then args = configFile\n";
    cout << "if -b then args = configFile addressFile"
      << " repeatCount [op1] [op2] ... " << "<single/many>\n";
    exit(1);
}

char *dataFile = "rds_data";
char *use_raw_device;
int log_mode;
extern int	debugMode;

#ifdef osf
extern "C"
void
oo7_main(int argc, char **argv)
#else
void
main(int argc, char **argv)
#endif
{
    enum {Generate, DoBench} type = DoBench;
    int c;
    MAININIT;

    while ((c = getopt(argc, argv, "r:Ggbdf:")) != EOF) {
	switch (c) {
	  case 'G':
	    log_mode++;
	    break;
	  case 'r':
	    use_raw_device = optarg;
	    break;
	  case 'g':
	    type = Generate;
	    break;
	  case 'b':
	    type = DoBench;
	    break;
	  case 'd':
	    debugMode = 1;
	    break;
	  case 'f':
	    dataFile = optarg;
	    break;
	  default:
	    usage();
	    exit(1);
	}
    }
    if (type == Generate) {
	GenDB(argc-optind,argv+optind);
    } else {
	Bench(argc-optind,argv+optind);
    }
    // done
    exit(0);
}

