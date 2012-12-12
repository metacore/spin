#include <iostream.h>
#include <spin_trans.h>
#include "utils.h"

/////////////////////////////////////////////////////////////////
// 
// show the program break  after adding "prog_brk_size" bytes
// to the current break. Also show the break limit.
//
/////////////////////////////////////////////////////////////////
/* print break point and limit */
rvm_length_t
show_break(rvm_length_t prog_brk_size)
{
  rvm_length_t    cur_brk;
  struct rlimit   rlp;

  /* get current break point */
  errno = 0;
  if ((cur_brk=(rvm_length_t)sbrk(0)) == -1)
    {
      printf("\n? Error getting current break point\n");
      printf("    errno = %d\n",errno);
      exit(1);
    }

  /* get system maximum */
  errno = 0;
  if (getrlimit(RLIMIT_DATA,&rlp) < 0)
    {
      printf("\n? Error getting data segment limit\n");
      printf("    errno = %d\n",errno);
      exit(1);
    }
  
  /* print the limits */
  cur_brk = RVM_ROUND_LENGTH_UP_TO_PAGE_SIZE(cur_brk+prog_brk_size);
  printf("\nCurrent break point:         0x%lx\n",cur_brk);
  printf("Maximum data segment length: 0x%x\n\n",rlp.rlim_max);
  return cur_brk;
}

