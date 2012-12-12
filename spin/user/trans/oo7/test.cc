#include <iostream.h>
#include "utils.h"
#include "rds_interface.h"
#define extern
#include "VarParams.h"
#undef extern

main()
{
  cout << "hello, world\n";
  LoadHeap(10);
  CreateFile(0, 0);
  BeginTransaction(0);
}
