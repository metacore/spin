main(int argc, char **argv) {
    int i;
    if (argc > 50) argc = 50;
    printf("xx %.10s\n", "print");
    printf("yy %10s\n", "print");
    printf("getpid = %d, argv = %lx\n", getpid(), argv);
    for (i = 0; i < argc; i++) {
	printf("arg%d : %lx %s\n", i, &argv[i], argv[i]);
    }
    for (i = 0; i < 30; i++) {
	printf("%d : Hello, World\n", i);
    }
}
