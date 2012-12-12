extern int _end;
int xy;

enum {SIZE=0x2000};
void *sbrk();

static char
random_char()
{
    char ret;
    static char c = '@';
    ret = c;
    c++;
    if (c >= 'z') c = '@';
    return ret;
}

main(int argc, char **argv)
{
    char *start = sbrk(0);
    int i, j;
    int repetition = 100;
    

    if (argc >= 2) {
	repetition = atoi(argv[1]);
    }
    if (start != (char*)(((long)start/SIZE+1)*SIZE)) {
	/* align to the page boundary */
	sbrk((char*)(((long)start/SIZE+1)*SIZE) - start);
	start = sbrk(0);
    }
    printf("brk start %d repetition\n_end = %lx, sbrk ptr=%lx\n",
	   repetition, &_end, start);

	
    for (j = 0;  j < repetition; j++) {
	char *x;
	write(1, ".", 1);
	
	/* Allocate the pages */
	for (i = 0; i < 50; i++) {
	    char *s;
	    int i;
	    s = x = sbrk(SIZE);
	    
	    /* See if the region is all 0. */
	    /* XXX this doesn't hold on OSF */
	    for (i = 0; i < SIZE; i++) {
		if (*s != 0) {
		    printf("sbrk'ed memory not 0 at %lx", s);
		    exit(1);
		}
		*s = random_char();
		s++;
	    }

	}

	/* Deallocate the pages */
	for (i = 0; i < 50; i++) {
	    x = sbrk(-SIZE);
	}


	if (x != start + SIZE) {
	    printf("???? x(%lx) != start(%lx)\n", x, start);
	    exit(1);
	}
    }

    printf("\nok.\n");
    exit(0);
}
