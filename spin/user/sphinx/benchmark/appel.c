#include <limits.h>
#include <spy.h>

#ifndef UINT_MAX
/* dec unix glitch */
#define UINT_MAX 4294967295U
#endif
/* Brian:  We decided to abandon the idea of accurately measuring the
   memory-protect time separately from the fault time, since there are
   too many operating system tricks that can "fool" the measurement.
   However, the benchmark below is much more realistic, in that (almost)
   any "trick" will also yield a performance improvement for the algorithms
   described in our paper (and this is more than just a "trick!).
   Take a look, if you like, and see if you agree.

   Also, if you'd like to run it for us, we need the following machines:

   microVax3 with Mach  (2.5 or 3.0; we'd prefer 2.5, except that that version
                         may be "broken" and not run the benchmark correctly.)
   sun 3/140 with Mach  (ditto)
   dec 3100 with Mach   (ditto)
   sparc.stn.1+ with Mach (ditto)
   intel386 with Mach   (just kidding)
   
   So far, we have already done:

   microVax3 with ultrix2.3, sun3/140 with sunos4.0, dec3100/ultrix3.1
   sparcstn1+/sunos4.1, intel386 with NX/2

   By the way, use -DMACH when compiling for Mach, and use cc -O in general.

                                            Andrew Appel

*/

#include <sys/time.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/resource.h>
#include <sys/mman.h>

#define ALL_PRIVILEGES	  (PROT_READ|PROT_WRITE|PROT_EXEC)

#define PAGES 100
#define ADDS 1000000
#define ROUNDS 200

int mytask;
int pagesize;
int bogo1,bogo2;
char *mem, *please_protect, *please_unprotect;

spy_t spy_add, spy_prot1, spy_protN;
int perm[PAGES];

rand()
{
	static  long    randx = 1;
	return((randx = randx * 1103515245 + 12345) & 0x7fffffff);
}

int count=0;

void handler()
{
	count++;
	mprotect(please_unprotect, pagesize, ALL_PRIVILEGES);
	if (please_protect) mprotect(please_protect, pagesize, 0); 
}


main(argc,argv) int argc; char **argv;
{
	register int i, j, k;

	spy_add = spy_create("add", 1000);
	spy_prot1 = spy_create("prot1", 1000);
	spy_protN = spy_create("protN", 1000);
	signal(SIGSEGV,handler);
	signal(SIGBUS,handler); 

	pagesize= getpagesize();
	printf("pagesize = %d\n",pagesize);

	mem = (char *)(((long) malloc((PAGES+1) * pagesize) + pagesize - 1)
		       & ~(pagesize-1));
	printf("mem = %lx\n",mem);
	for (i=0;i<PAGES;i++) {
/*		printf("accessing addr %lx\n",&mem[i*pagesize]); */
		mem[i*pagesize]=20;
	}
	for(i=1;i<PAGES;i++) perm[i]=i;

	for(i=1;i<PAGES-1;i++)
	{
		j=rand()%(PAGES-i-1);
		k=perm[j+i]; perm[j+i]=perm[i]; perm[i]=k;
	}

	spy_start(spy_add);
	i=ADDS/20; j=0;
	do {
		j+=k;j+=k;j+=k;j+=k;j+=k;j+=k;j+=k;j+=k;j+=k;j+=k;
		j+=k;j+=k;j+=k;j+=k;j+=k;j+=k;j+=k;j+=k;
	}
	while (--i>0);
	spy_stop(spy_add);

	spy_start(spy_protN);
	for(i=0;i<ROUNDS;i++)
	{
		mprotect(mem,PAGES*pagesize,0);
		for(j=0;j<PAGES;j++)
		{
			please_unprotect= mem+perm[j]*pagesize;
			please_protect= 0;
			*please_unprotect=10;
		}
	}
	spy_stop(spy_protN);

	mprotect(mem,PAGES*pagesize,0);
	mprotect(mem+perm[PAGES-1]*pagesize, pagesize, ALL_PRIVILEGES); 
	please_protect= mem+perm[PAGES-1]*pagesize;

	spy_start(spy_prot1);

	for(i=0;i<ROUNDS;i++) {
		for(j=0;j<PAGES;j++)
		{
			please_unprotect= mem+perm[j]*pagesize;
			*please_unprotect=10;
			please_protect=please_unprotect;
		}
	}

	spy_stop(spy_prot1);

	if (count != 2*ROUNDS*PAGES)
		printf("Operating system bug, only %d traps instead of %d\n",
		       count, 2*ROUNDS*PAGES);

	spy_dump_all();
	exit(0);

}

