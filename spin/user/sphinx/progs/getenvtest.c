main(int argc, char **argv) 
{
    char *getenv();
    if (argc <= 1) {
	printf("%s [vars...].\n");
	return 0;
    } else {
	int i;
	printf("Hello. I'm a getenv process.\n");
	for (i = i; i < argc; i++) {
	    printf("%s = %s\n", argv[i], getenv(argv[i]));
	}
    }
}

