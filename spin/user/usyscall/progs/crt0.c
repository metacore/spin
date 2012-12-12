
extern void USyscall_SpaceDestroy();
extern int USyscall_SpaceSelf();

char	**environ;
extern int main();


__start( sp )
	register char **sp;
{
    register unsigned argc;
    register char	  ** argv;

    argc = *(unsigned int *)sp;
    argv = ++sp;
    environ = argv + (argc + 1);

    main(argc, argv, environ);
    USyscall_SpaceDestroy(USyscall_SpaceSelf());
}
