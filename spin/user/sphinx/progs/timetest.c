main()
{
    char buf[100];
    long x = time(0);
    strftime(buf, 100, "%c", localtime(&x));
    printf("%s\n", buf);
}
