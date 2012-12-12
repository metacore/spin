#include "utils.h"

void
CreateFile(char* name, int bytes)
{
  int f;
  
  if ((f=creat(name, 0644))==-1) {
    perror("creating seg file");
    exit(1);
  }
  if (lseek(f, bytes-1, SEEK_CUR)==-1) {
    perror("lseeking seg file");
    exit(1);
  }
  if (write(f,"",1)==-1) {
    perror("extending file");
    exit(1);
  }
  close(f);
}


unsigned long
GetFileLength(char *fileName)
{
  struct stat buffer;
  if(stat(fileName, &buffer)==-1)
    {
      cout << "GetFileLength: could not open file " << fileName << endl;
      cout << "make sure the data file " << fileName << " exists" << endl;
      exit(1);
    }
  return (unsigned long)buffer.st_size;
}
