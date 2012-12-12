#include <iostream.h>

main()
{
  int i;
  char *p;

  while(1)
    {
      if(!(p = (char *) malloc(184)))
	{
	  cout << "number of mallocs = " << i << endl;
	  exit(1);
	}
      i++;
    }
}
