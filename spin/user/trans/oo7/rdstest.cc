/*  Vivek's rds tester
 * 1) create rds_log using rvmutl (500k)
 * 2) run rdstest s to get the break
 * 3) run rdsinit with rds_log and rds_data as args.
 * 4) make the size of rds_data about 20MB
 * 5) make the heap about 16 MB or so
 * 6) run rdstest g to generate the tree
 * 7) run rdstest r to run the traversal
 */

#define STRING_SIZE 80
#define TRUNCATE_VAL 30
#define MAX_LEVEL 5
#define MEMORY_POOL_SIZE 5*1024*1024

#define BEGIN_NEW(x, y, args...)  (persNew=1, new y(##args)); persNew = 0;

#include <iostream.h>
#include <fstream.h>
#include <unistd.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/resource.h>
#include <stdlib.h>
#include <errno.h>

#include "utils.h"
#include "rds_interface.h"

#define RDS_LOG_FILE "log_file"
char *RDS_DATA_FILE = "rds_data";

#ifdef spin
#define extern
#endif

#ifdef osf
#define extern static
#endif

#ifdef rvmunix
#define extern
#endif

extern int NumMallocs;

#include "VarParams.h"
#undef extern


class Foo
{
public:
  static int nodeCount;
  char tag[32];
  int node_id;
  Foo *l, *r;
  Foo(Foo *left, Foo *right, int level);
  Traverse(int level);
};

int Foo::nodeCount = 0;
static Foo *f, *l, *r;
static char *static_addr; /* start of RVM/RDS static data region. */
static rvm_tid_t *cur_tid;

static int _count = 0;
Foo::Foo(Foo *left, Foo *right, int level)
{
    _count++;
    if (_count % 10 == 0) {
	putchar('.');
	fflush(stdout);
    }
    SetRange(cur_tid, (char *)this, sizeof(*this));
    node_id = nodeCount++;
    sprintf(tag, "node %d, level %d", node_id, level);
    if(level < MAX_LEVEL) {
	l = BEGIN_NEW(0,Foo, 0,0,level+1);
	r = BEGIN_NEW(0,Foo, 0,0,level+1);
    } else {
	l = r = 0;
    }
}

Foo::Traverse(int level)
{
  if (node_id != nodeCount++) {
    cout << "at level " << level << " node_id " << node_id << " != " 
         << nodeCount << endl;
    printf("node tag: %s\n", tag);
  }
  if (level > MAX_LEVEL) {
    cout << "alert! level " << level << " reached" << endl;
  }
  if(l)
    l->Traverse(level + 1);
  if(r)
    r->Traverse(level + 1);
}

#define RVM_PAGE_SIZE 8192

static void
GenDBInitialize()
{ 
  int err;
  rvm_return_t retval;
  rvm_length_t cur_brk;
  unsigned long heapSize = 16*1024*1024;
  //long j, numPagesInSegment;

  // perform basic initialization stuff
  cur_brk = show_break(256*RVM_PAGE_SIZE);

  cur_tid = rvm_malloc_tid();
  BeginTransaction(cur_tid);

#ifdef OPAL_MALLOC  
  // allocate va space so that during traversal the heap can be put here.
  OpalLoadHeap(RDS_DATA_FILE, (char *)0, heapSize);
#else
  static_addr = LoadHeap(RDS_DATA_FILE);
#endif
}

static void
GenDBSetRoot()
{
  cout << "*********Root*********" << endl;
  cout << "Root = " << f << endl;
#ifdef OPAL_MALLOC
  OpalSetRoot((void *)f);
#else
  SetRange(cur_tid, static_addr, sizeof(void*));
  *(void**)static_addr = f;
#endif
}

static void
GenDBFinalize()
{
  rvm_return_t ret;

  // the tid used for doing persistent mallocs.
  Commit(cur_tid);
  rvm_free_tid(cur_tid);

  // flush the log buffer to disk
  FlushLog();
  // perform truncation of log -- this propagates updates back to data file
  TruncateLog();
  // terminate rvm 
  Terminate();
}

static void
Open_Database(char *arg)
{

}

static void
BenchInitialize()
{  
  rvm_return_t retval;
  unsigned long heapSize;
  char *addr;

  cur_tid = rvm_malloc_tid();
  BeginTransaction(cur_tid);

  static_addr = LoadHeap(RDS_DATA_FILE);
  f = *(Foo**)static_addr;
}

static void
BenchFinalize()
{
  // the tid used for doing persistent mallocs.
  Commit(cur_tid);
  rvm_free_tid(cur_tid);
  FlushLog();
  TruncateLog();
  Terminate();
}

void usage(char *argv0)
{
    fprintf(stderr, "%s [-f DATABASE_FILE] [gsr]\n", argv0);
    fprintf(stderr, "%s g : generate DB.\n", argv0);
    fprintf(stderr, "%s s : show break value.\n", argv0);
    fprintf(stderr, "%s r : traverse.\n", argv0);
    exit(1);
}
#ifdef osf
extern "C"
void
rds_test_main(int argc, char **argv)
#else
void
main(int argc, char **argv)
#endif
{
    int err;
    rvm_return_t retval;
    char *static_addr;
    char string[STRING_SIZE];
    int c;
    
    while ((c = getopt(argc, argv, "f:")) != EOF) {
	switch (c) {
	  case 'f':
	    RDS_DATA_FILE = optarg;
	    break;
	  default:
	    usage(argv[0]);
	}
    }
    if (argv[optind] == 0) {
	usage(argv[0]);
    }

    if (*argv[optind]=='g') {
	GenDBInitialize();
	// allocate an object on the persistent heap
	f = BEGIN_NEW(0,Foo,0,0,0);
	// publish the address of f
	cout << "*********address of f*********" << endl;
	cout << "f = " << f << endl;
	cout << "node count = " << Foo::nodeCount << endl;
	GenDBSetRoot();
	GenDBFinalize();
	Foo::nodeCount = 0;
	cout << "traversing\n";
	f->Traverse(0);
    } else if(*argv[optind]=='s') {
	// show the current program break 
	RVMInitialize();
	show_break(RVM_PAGE_SIZE*5);
    } else {
	Open_Database("rdstest.address");
	// read in f
	BenchInitialize();
	
	l = f->l;
	r = f->r;
	f->Traverse(0);
	cout << f->node_id << " " << l->node_id << " " << r->node_id << endl;
	BenchFinalize();
    }
#ifdef MAP_MEM
    extern int fault_count;
    cout << fault_count << " page_faults " << endl;
#endif
} 

