/*********************************************************************
 * ulock support for reader/writer locks used by Digital Unix 
 *********************************************************************/

#include "ULockInterface.h"


struct ULockInterface ULock ;

static int data=1; /* to help spin dyn linker */
