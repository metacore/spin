#ifndef _UTILS_H_
#define _UTILS_H_

#include <fstream.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/resource.h>

#define TRUNCATE_VAL 30   // truncate threshold


extern int errno;
extern long persNew;

#ifdef OPAL_MALLOC

#include "threads/OThreads.h"

#define SMALL_MEMORYPOOL_SIZE 16*1024*1024
#define MEDIUM_MEMORYPOOL_SIZE 96*1024*1024
#define LARGE_MEMORYPOOL_SIZE 154*1024*1024

extern void *operator new(ulong);
extern void OpalLoadHeap(char *, char *, int);
extern Address GetHeapStart();
extern void OpalSetRoot(void *);
extern void *OpalGetRoot();
extern void OpalFinalize(char *);
extern void OpalGetMemoryPool(char *);

#endif // OPAL_MALLOC

extern "C" {
#include "rvm.h"
}

extern void CreateFile(char *, int);
extern unsigned long GetFileLength(char *);

extern void RVMInitialize();
extern void SetOptions(char *logFile);

extern void BeginTransaction(rvm_tid_t *);
extern void Commit(rvm_tid_t *);
extern void SetRange(rvm_tid_t *, char *, rvm_length_t);

extern void FlushLog();
extern void TruncateLog();
extern void Terminate();
rvm_length_t show_break(rvm_length_t);

#endif _UTILS_H_

