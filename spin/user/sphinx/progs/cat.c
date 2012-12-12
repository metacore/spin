#include <stdio.h>
void cat(FILE *in)
{
    char buf[BUFSIZ];
    int n;
    printf("start\n");
    while ((n = fread(buf, 1, BUFSIZ, in)) > 0) {
	fwrite(buf, 1, n, stdout);
    }
    printf("end\n");
    if (n < 0) {
	perror("xxx");
    }
}

main(int argc, char **argv)
{
    if (argc == 1) {
	cat(stdin);
    } else {
	int i;
	FILE *fp;
	for (i = 1; i < argc; i++) {
	    fp = fopen(argv[i], "r");
	    if (!fp) {
		perror(argv[i]);
	    } else {
		cat(fp);
		close(fp);
	    }
	}
    }
}
