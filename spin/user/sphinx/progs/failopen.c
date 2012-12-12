#include <stdio.h>
extern int errno;


main(argc, argv)
    int  argc;
    char *argv[];
{
    FILE *foo;

    if ( argc < 2 ) {
	printf("usage: %s <filename>\n",argv[0]);
	exit(1);
    }
    
    foo = fopen(argv[1], "r");
    if ( foo == NULL ) {
	printf("couldn't open %s, errno %d\n",argv[1],errno);
	exit(0);
    }
    printf("open %s normally\n",argv[1]);
    fclose(foo);
}
