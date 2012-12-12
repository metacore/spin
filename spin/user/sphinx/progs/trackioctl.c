main(argc, argv)
    int argc;
    char *argv[];
{
    if ( argc < 2 ) {
	printf("usage: %s [on|off]\n",argv[0]);
	exit(1);
    }
    if (!strcmp(argv[1],"on") ) {
	ioctl(0,1,-1L);
	exit(0);
    }
    if (!strcmp(argv[1],"off") ) {
	ioctl(0,0,-1L);
	exit(0);
    }
    printf("usage: %s [on|off]\n",argv[0]);
    exit(1);
}
