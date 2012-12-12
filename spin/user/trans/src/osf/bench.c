#include <stdio.h>
#include <sys/time.h>
#include <mach.h>
#include "bench.h"
int duration = 10;
int alarmrang;
char *_Rvm_Data_Device = "data.spin";

void RunTrial(FILE *fp, FILE *summaryfp, Locality l) {
    struct timeval tvb, tve;
    int transexecuted = 0;
    long begin_usec, end_usec;
    vm_statistics_data_t vmb, vma;

    // get and print rvm, cpu and disk stats 

    // Run the actual transactions 
    RunBench(&transexecuted, &tvb, &tve, &vmb, &vma, l);

    fprintf(fp, "%lu transactions executed\n", transexecuted);
    
    begin_usec = tvb.tv_sec * 1000000 + tvb.tv_usec;
    end_usec = tve.tv_sec * 1000000 + tve.tv_usec;

    fprintf(fp, "time = %f secs.\n",
	    (double)(end_usec-begin_usec) / 1000000);
    fflush(fp);
    fflush(summaryfp);
}

bench_start()
{
    initialize_server();
    InitForExpt(100, 200);
    RunTrial(stdout, stdout, Random);  
}
