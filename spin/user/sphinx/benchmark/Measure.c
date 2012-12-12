#include <stdio.h>
#include <stdlib.h>
#include "Measure.h"

unsigned long mhz = 133;
unsigned long overhead=0;
int overheadRuns = 100000;

int MeasureCompare(unsigned long *l1, unsigned long* l2)
{
        if (*l1 > *l2) return 1;
        if (*l1 < *l2) return -1;
        if (*l1 == *l2) return 0;
}


unsigned long MeasureIntSqrt(unsigned long number)
{
	unsigned long i;
	for (i=0;i<number;i++)
	{
		if ((i*i) > number)
			return i;
	}
}

Measure_t MeasureCreate(char *name, unsigned long size)
{
	Measure_t new_timer;
	new_timer = (Measure_t)malloc(sizeof(struct Measure));
	strcpy(new_timer->name,name);
	new_timer->begin = 0L;
	new_timer->end = 0L;
	new_timer->index = 0L;
	new_timer->max_size = size;
	new_timer->hist = (unsigned long *)malloc(sizeof(unsigned long)*size);
	return new_timer;
}

MeasureFree(Measure_t timer)
{
	free(timer->hist);
	free(timer);
}

MeasureStart(Measure_t timer)
{
	timer->begin = get_rpcc();
}

MeasureStop(Measure_t timer)
{
	timer->end = get_rpcc();

	timer->begin &= 0xffffffff;
	timer->end &= 0xffffffff;

	if (timer->index > timer->max_size)
	{
		printf("Error: too many iterations.  index = %d, max = %d\n",
		       timer->index,timer->max_size);
	}

	if (timer->end < timer->begin)
		timer->hist[timer->index] = (0xffffffff - timer->begin)
			+ timer->end;
	else
		timer->hist[timer->index] = timer->end - timer->begin;

	timer->index++;
}

MeasurePrintStats(Measure_t timer)
{
	unsigned long outliers=0;
	unsigned long iterations;
	unsigned long average,median,min,max,total,variance,stddev,y2;
	unsigned long *histogram;
	unsigned long i;

	iterations = timer->index;
	histogram = timer->hist;
	printf("For the experiment: %s\n",timer->name);
	printf("Iterations = %d, %d outliers were removed\n",
	       iterations,outliers);

	if (iterations > timer->max_size) {
		printf("Error: Too many iterations\n");
		return;
	}
	total = 0L;
	y2 = 0L;
	for (i=0;i<iterations;i++)
	{
		histogram[i] -= overhead;
	}
	min = histogram[0];
	max = histogram[0];

	for (i=0;i<iterations;i++)
	{
		total += histogram[i];
		y2 += (histogram[i]*histogram[i]);
		if (histogram[i] > max) 
			max = histogram[i];
		if (histogram[i] < min) 
			min = histogram[i];
	}

	average = total/iterations;
	qsort((char *)histogram,iterations,sizeof(unsigned long),
	      MeasureCompare);
	median = histogram[iterations/2];

	printf("average = %ld cycles %ld usecs\n",average,average/mhz);
	printf("median = %ld cycles %ld usecs\n",median,median/mhz);
	printf("min = %ld cycles %ld usecs\n",min,min/mhz);
	printf("max = %ld cycles %ld usecs\n",max,max/mhz);
	if (iterations > 1 ) {
		
		variance = (y2 - ((total * total)/iterations))/(iterations-1);
		stddev = MeasureIntSqrt(variance);
		printf("variance = %ld cycles %ld usecs\n",variance,variance/mhz); 
		printf("stddev = %ld cycles %ld usecs\n",stddev,stddev/mhz);
	}
/*	MeasurePrintStatsUnderPartition(timer,median);
	MeasurePrintStatsOverPartition(timer,median); */
}			

MeasurePrintStatsUnderPartition(Measure_t timer, long partition)
{
	unsigned long outliers=0;
	unsigned long iterations;
	unsigned long average,median,min,max,total,variance,stddev,y2;
	unsigned long *histogram,*newhistogram;
	unsigned long i,j;

	iterations = timer->index;
	histogram = (unsigned long *)malloc(sizeof(unsigned long)*iterations);
	j = 0;
	for (i=0;i<iterations;i++) {
		if (timer->hist[i] < partition)
			histogram[j++] = timer->hist[i];
	}
	iterations = j;
	printf("Under the partition: %d\n",partition);
	printf("For the experiment: %s\n",timer->name);
	printf("Iterations = %d, %d outliers were removed\n",
	       iterations,outliers);

	if (iterations > timer->max_size) {
		printf("Error: Too many iterations\n");
		return;
	}
	     
	total = 0L;
	y2 = 0L;
	min = histogram[0];
	max = histogram[0];

	for (i=0;i<iterations;i++)
	{
		total += histogram[i];
		y2 += (histogram[i]*histogram[i]);
		if (histogram[i] > max) 
			max = histogram[i];
		if (histogram[i] < min) 
			min = histogram[i];
/*		printf("%d,",histogram[i]);*/
	}

	average = total/iterations;
	qsort((char *)histogram,iterations,sizeof(unsigned long),
	      MeasureCompare);
	median = histogram[iterations/2];

	printf("average = %ld cycles %ld usecs\n",average,average/mhz);
	printf("median = %ld cycles %ld usecs\n",median,median/mhz);
	printf("min = %ld cycles %ld usecs\n",min,min/mhz);
	printf("max = %ld cycles %ld usecs\n",max,max/mhz);
	if (iterations > 1 ) {
		
		variance = (y2 - ((total * total)/iterations))/(iterations-1);
		stddev = MeasureIntSqrt(variance);
		printf("variance = %ld cycles %ld usecs\n",variance,variance/mhz); 
		printf("stddev = %ld cycles %ld usecs\n",stddev,stddev/mhz);
	}
}			


MeasurePrintStatsOverPartition(Measure_t timer, long partition)
{
	unsigned long outliers=0;
	unsigned long iterations;
	unsigned long average,median,min,max,total,variance,stddev,y2;
	unsigned long *histogram,*newhistogram;
	unsigned long i,j;

	iterations = timer->index;
	histogram = (unsigned long *)malloc(sizeof(unsigned long)*iterations);
	j = 0;
	for (i=0;i<iterations;i++) {
		if (timer->hist[i] > partition)
			histogram[j++] = timer->hist[i];
	}
	iterations = j;
	printf("Over the partition: %d\n",partition);
	printf("For the experiment: %s\n",timer->name);
	printf("Iterations = %d, %d outliers were removed\n",
	       iterations,outliers);

	if (iterations > timer->max_size) {
		printf("Error: Too many iterations\n");
		return;
	}
	     
	total = 0L;
	y2 = 0L;
	min = histogram[0];
	max = histogram[0];

	for (i=0;i<iterations;i++)
	{
		total += histogram[i];
		y2 += (histogram[i]*histogram[i]);
		if (histogram[i] > max) 
			max = histogram[i];
		if (histogram[i] < min) 
			min = histogram[i];
/*		printf("%d,",histogram[i]);*/
	}

	average = total/iterations;
	qsort((char *)histogram,iterations,sizeof(unsigned long),
	      MeasureCompare);
	median = histogram[iterations/2];

	printf("average = %ld cycles %ld usecs\n",average,average/mhz);
	printf("median = %ld cycles %ld usecs\n",median,median/mhz);
	printf("min = %ld cycles %ld usecs\n",min,min/mhz);
	printf("max = %ld cycles %ld usecs\n",max,max/mhz);
	if (iterations > 1 ) {
		
		variance = (y2 - ((total * total)/iterations))/(iterations-1);
		stddev = MeasureIntSqrt(variance);
		printf("variance = %ld cycles %ld usecs\n",variance,variance/mhz); 
		printf("stddev = %ld cycles %ld usecs\n",stddev,stddev/mhz);
	}
}			


MeasureInit()
{
	unsigned long i,total,size;
	Measure_t new_timer;

	size = overheadRuns;

	new_timer = MeasureCreate("Overhead",size);

	for (i=0;i<overheadRuns;i++){
		MeasureStart(new_timer);
		MeasureStop(new_timer);
	}
	
	total = 0L;
	for (i=0;i<overheadRuns;i++) {
		total += new_timer->hist[i];
	}

	overhead = (unsigned long)(total/new_timer->index);
	printf("overhead = %ld cycles %ld usecs\n",overhead,(overhead/mhz));
	MeasureFree(new_timer);
}

