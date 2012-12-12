main()
{
    int self = USyscall_StrandSelf();
    int space = USyscall_SpaceSelf();
    char *p = "hello world\n";
    while (*p) {
	USyscall_Putc(*p);
	p++;
    }
    USyscall_Putx(self);
    USyscall_Putc('\n');
    USyscall_Putx(space);
    USyscall_Putc('\n');    
}

__main() {}
