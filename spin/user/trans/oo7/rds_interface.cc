#include <iostream.h>
extern "C" {
#include "rvm.h"
#include "rds.h"
}
#include "VarParams.h"
#include "rvmdef.h"

rvm_tid_t *mallocTid;
long persNew;

char *
LoadHeap(char *fileName, char *load_addr = 0, int fileSize = 0)
{
  int err;
  rvm_return_t retval;
  char *static_addr;
  rvm_offset_t offset = RVM_MK_OFFSET(0,300*1024*1024);
#ifdef OPAL_MALLOC

  opal_load_heap(fileName, load_addr, fileSize);

#else // OPAL_MALLOC

#ifdef MAP_MEM
  rds_load_heap_simple(fileName, &static_addr, &err);
#else
  rds_load_heap(fileName, offset, &static_addr, &err);
#endif
  if(err != RVM_SUCCESS)
    {
      cout << "error loading heap. err = " << err <<endl;
      exit(1);
    }
#endif // OPAL_MALLOC
  //cout << "starting address of data segment = " << static_addr << endl;
  return static_addr;
}

#ifdef NO_SETRANGE
void
HeapSetRange()
{
  int err;
  mallocTid = (rvm_tid_t *) rvm_malloc_tid();
  BeginTransaction(mallocTid); // no_restore mode
  ns_huge_setrange(mallocTid, &err);
  if(err!=RVM_SUCCESS)
    {
      cout << "error in heap set range" << endl;
      exit(1);
    }
}
#endif // NO_SETRANGE

void 
Preallocate()
{
  int err;
  unsigned int numElements, numAtomicParts, numConnections;

  // preallocation is done in order of increasing block sizes
  // so as to prevent fragmentation

  numAtomicParts = TotalModules*NumCompPerModule*NumAtomicPerComp;
  numConnections = numAtomicParts*NumConnPerAtomic;
  numElements = 2*numAtomicParts + numAtomicParts*NumConnPerAtomic*2
    + 6*NumCompPerModule;
  

  // first preallocate the Elements (a struct with two pointers in it)
  rds_prealloc(2*sizeof(void *), numElements, mallocTid, &err);
  if(err!=RVM_SUCCESS)
    {
      cout << "error in preallocation of Elements" << endl;
      exit(1);
    }
  
  // next allocate the "to" GenBBag arrays
  rds_prealloc(NumConnPerAtomic*sizeof(void *), numAtomicParts,
	       mallocTid, &err);

  if(err!=RVM_SUCCESS)
    {
      cout << "error in preallocation of GenBBag arrays" << endl;
      exit(1);
    }


  // next allocate the Connections
  rds_prealloc(256, numConnections, mallocTid, &err);

  if(err!=RVM_SUCCESS)
    {
      cout << "error in preallocation of Connections" << endl;
      exit(1);
    }

  // next allocate the AtomicParts
  rds_prealloc(256, numAtomicParts, mallocTid, &err);
  if(err!=RVM_SUCCESS)
    {
      cout << "error in preallocation of AtomicParts" << endl;
      exit(1);
    }
/*
  // next allocate the "from" GenBBag arrays
  rds_prealloc(4*NumConnPerAtomic*sizeof(void *), numAtomicParts, 
  mallocTid, &err);
  if(err!=RVM_SUCCESS)
    {
      cout << "error in preallocation of GenBBag arrays" << endl;
      exit(1);
    }
*/
}

extern int NumMallocs;

void *
operator new(unsigned long size)
{
  void *ptr;
  int err;
  if(persNew) {
      ptr = (void *)rds_malloc(size, mallocTid, &err);
      if(err != RVM_SUCCESS) {
	  cout << "rds_malloc failed. err = " << err << endl;
	  exit(1);
      }
      NumMallocs++;
      persNew = 0;
      return ptr;
  }
  else return malloc(size);
}


void *DBAddr;

void
GetAddresses(char *addressFileName)
{
  char buf[100];
  char *c;
  FILE *addressFile;

  addressFile = fopen(addressFileName, "r");

  if(!addressFile)
    {
      cout << "unable to open address file " << addressFileName << "\n" ;
      exit(1);
    }
  fscanf(addressFile, "%s\n", &buf);
  //cout << "buffer is " << buf ;
  DBAddr = strtol(buf, &c, 10);
  if (*c != (char)0) {
    cerr << "Not a hex number.\n";
    exit(1);
  }
}
