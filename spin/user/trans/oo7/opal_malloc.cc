/////////////////////////////////////////////////////////////////
// 
// This file contains the interface to Opal malloc.
//
/////////////////////////////////////////////////////////////////

#ifdef OPAL_MALLOC

#include <fstream.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

#include "utils.h"

extern "C" {
#include "rvm.h"
}

Address DBAddr;
MemoryPool *DBPool;
MemoryPool *TempDBPool;
MemoryPool *__savedMP;
MemoryPool *__newMP;

long ThreadsInitialized;
long persNew;

/////////////////////////////////////////////////////////////////
// 
// Overloaded operator new. We use it to call MemoryPool malloc.
//
/////////////////////////////////////////////////////////////////
void*
operator new(ulong size)
{
  register MemoryPool* pool;
  void *ptr;
  if(persNew)
    {
      //  assert(!ThreadsInitialized || (Address)thisthread>0x10000);
      if (ThreadsInitialized && (pool=thisthread->GetMemoryPool())) 
	{
	  //printf("invoking pool->Malloc\n");
	  ptr = (void*)pool->Malloc(size);
	  persNew = 0;
	  return ptr;
	} 
    }
  else
    return malloc(size);
}

/////////////////////////////////////////////////////////////////
// 
// Initialize a segment as a MemoryPool.
//
/////////////////////////////////////////////////////////////////
MemoryPool * 
MyMemoryPoolInit(Address start, Address end) 
{ 
  MemoryPool *ds; 
  ds = (MemoryPool *)start; 
  ds->Initialize(start + 
      ((sizeof(*ds) + sizeof(Address)-1) & ~(sizeof(Address)-1)), end); 
  return(ds); 
} 

/////////////////////////////////////////////////////////////////
// 
// If the file doesn't exist, create one of the specified size.
// Treat the file as a heap and map it in. 
// If the file exists, just map it in.
//
/////////////////////////////////////////////////////////////////
void
OpalLoadHeap(char *fileName, char *load_addr, int fileSize)
{
  int creation = 0;
  struct stat buffer;
  rvm_return_t retval;
  rvm_region_t *region = rvm_malloc_region();

  // first see if the file exists
  if(stat(fileName, &buffer)==-1)
    {
      if(errno==ENOENT)
        {
          // the file does not exist
	  // create one of the given size
	  cout << "file " << fileName << " doesn't exist. Creating." << endl;
	  CreateFile(fileName, fileSize);
	  creation = 1;
        }
    }

  // treat the data file as a MemoryPool and map it in.
  region->data_dev = (char *)malloc(strlen(fileName)+1);
  strcpy(region->data_dev, fileName);
  region->dev_length = RVM_MK_OFFSET(0,GetFileLength(region->data_dev));
  region->offset = RVM_MK_OFFSET(0,0);
  region->length = GetFileLength(region->data_dev);
  region->vmaddr = (char *)load_addr;
#ifdef MAP_MEM
  region->mmap_cow = true;
#endif MAP_MEM

  if((retval = rvm_map(region, 0))!=RVM_SUCCESS)
    {
      cout << "rvm map failed: retval = " << retval << endl;
      exit(1);
    }
  if(creation)
    {
      DBPool = MyMemoryPoolInit((Address)region->vmaddr, 
				(Address)(region->vmaddr+region->length));
    }

  __savedMP = thisthread->GetMemoryPool();
  thisthread->SetMemoryPool(DBPool);
  (thisthread->GetMemoryPool())->PackedModeOn();

}

/////////////////////////////////////////////////////////////////
// 
// Return the starting address of the heap
//
/////////////////////////////////////////////////////////////////
Address
GetHeapStart()
{
  return (Address)DBPool;
}


/////////////////////////////////////////////////////////////////
// 
// Set the root of the MemoryPool.
//
/////////////////////////////////////////////////////////////////
void
OpalSetRoot(void *root)
{
  cout << "*********Setting Root*********" << endl;
  DBPool->SetRoot((ulong)root);
}

/////////////////////////////////////////////////////////////////
// 
// Write out the Root of the MemoryPool into a file.
//
/////////////////////////////////////////////////////////////////
void
OpalFinalize(char *addressFileName)
{

  FILE* addressFile = fopen(addressFileName, "w");
  if(!addressFile)
    {
      cout << "unable to open file " << addressFileName << " for writing\n";
      cout.flush();
      exit(1);
    }
  fprintf(addressFile, "%ld\n", (long)DBPool);
  fclose(addressFile);

  thisthread->SetMemoryPool(__savedMP);
}

/////////////////////////////////////////////////////////////////
// 
// Read in the address from the address file. This is the 
// starting address of the MemoryPool.
//
/////////////////////////////////////////////////////////////////
void
OpalGetMemoryPool(char *addressFileName)
{
  char buf[100];
  char *c;
  FILE *addressFile;
  long address;

  addressFile = fopen(addressFileName, "r");

  if(!addressFile)
    {
      cout << "unable to open address file " << addressFileName << "\n" ;
      exit(1);
    }
  fscanf(addressFile, "%s\n", &buf);
  address = strtol(buf, &c, 10);
  if (*c != (char)0) {
    cerr << "Not a hex number.\n";
    exit(1);
  }
  DBPool = (MemoryPool *)address;
}

/////////////////////////////////////////////////////////////////
// 
// return the root of the database.
//
/////////////////////////////////////////////////////////////////
void *
OpalGetRoot()
{
  return (void *) DBPool->GetRoot();
}

/////////////////////////////////////////////////////////////////
// 
// All programs start here because we need to wrap the main 
// procedure with the Start_OThreads macro.
//
/////////////////////////////////////////////////////////////////
main(int argc, char **argv)
{
  ThreadsInitialized = 1;
  Start_OThreads(argc,argv,mainproc);
}

#endif
