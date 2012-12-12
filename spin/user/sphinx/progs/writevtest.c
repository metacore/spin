struct iovec {
        char*   iov_base;
        int     iov_len;
};

main()
{
  static char teststring[] = "This is a test. XXXX";
  static struct iovec iov[5] = { { "Base0 ", 6 }, { "Base1 ", 6 },
			    { "Base2 ", 6 }, { "Base3 ", 6 },
			    { "Base4 ", 6 } };

  printf("\nWriting first 15 chars of %s\n", teststring);
  write(1, teststring, 15);
  printf("\nwritev-ing 5 iov's\n");
  writev(1, iov, 5);
}


