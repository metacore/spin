struct Measure {
	char name[50];
	unsigned long begin;
	unsigned long end;
	unsigned long index;
	unsigned long max_size;
	unsigned long *hist;
};

typedef struct Measure *Measure_t;

Measure_t MeasureCreate(char *name, unsigned long size);
