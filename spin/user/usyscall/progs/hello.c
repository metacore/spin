main()
{
    char *p = "hello world\n";
    while (*p) {
	USyscall_Putc(*p);
	p++;
    }
}

__main() {}
