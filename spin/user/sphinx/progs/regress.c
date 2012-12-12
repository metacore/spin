#include <stdio.h>
main(int argc, char **argv)
{
    FILE *fp = fopen("/dev/regress", "w");
    fprintf(fp, "Hello, world\n");
    fprintf(fp, "argc is %d\n", argc);
}
