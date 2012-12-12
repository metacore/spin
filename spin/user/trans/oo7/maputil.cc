#include <iostream.h>
#include <sys/types.h>
#include <sys/mman.h>

main(int argc, char **argv)
{
  int len;
  if(!argv[1])
    {
      cout << "usage: " << argv[0] << " length" << endl;
      exit(1);
    }
  len = atoi(argv[1]);
  if(madvise(0,len,0xbb)==-1)
    {
      cout << "call to madvise with length = " << len << " failed" << endl;
      exit(1);
    }
  else
    cout << "Bumped up map entries by " <<  len << endl;
}
