main(argc, argv)
    int argc;
    char *argv[];
{
    double myx, osfx;
    int exp, preci;
    char *fmtinbuf = "%lQ";
    char *fmtoutbuf = "%.Qlf, %lE";
    char buffer[50];
    extern double _atod();
    extern double atof();
    long *lp;
    int i;
    
    if(argc != 0) {
      printf("arguments: ");
      for(i = 0; i < argc; i++) {
	printf("%d: %s; ", i, argv[i]);
      }
      printf("\n");
    } else {
      printf("no arguments\n");
    }      

    if ( argc < 3 ) {
	printf("usage: %s floating-value in-format out-precision\n",argv[0]);
	exit(1);
    }
    fmtinbuf[2] = argv[2][0];
    fmtoutbuf[2] = argv[3][0];
    printf("input is %s, format is %s\n",argv[1],fmtinbuf);
    sscanf(argv[1],fmtinbuf,&osfx);
    printf("result (%s) is: ",fmtoutbuf);
    lp = (long *)&osfx;
    printf("%lx\n",*lp);
    sprintf(buffer,fmtoutbuf,osfx,osfx);
    printf("from sprintf: %s\n",buffer);
    
/*    printf(fmtoutbuf,osfx,osfx); */
    exit(0);
}
