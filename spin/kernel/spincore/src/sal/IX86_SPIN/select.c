#include <sys/types.h>
#include <sys/select.h>
#include <spincore/IX86_SPIN/Select.h>

void selrecord (struct proc *selector, struct selinfo *sip) 
{
    if (*Select__Record) Select__Record(selector, sip);
}
void selwakeup (struct selinfo *sip) 
{
    if (*Select__Wakeup) Select__Wakeup(sip);
}
