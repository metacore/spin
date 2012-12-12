#include <stdio.h>
main()
{
    int i;
    FILE *fp = fopen("xxxx", "w");
    for (i=0; i < 100000; i++) {
	fprintf(fp, "%d:", i);
    }
    fclose(fp);
}
