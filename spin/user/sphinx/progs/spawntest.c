main()
{
    extern char **environ;
    char *argv[] = {
	"/spin/yasushi/getenvtest",
	"ipaddr", 
	0
    };
    
    printf("Hello, world. I'm spawning off getenv thread.\n");
    spawnve("/spin/yasushi/getenvtest", argv, environ);
}

int spawnve(char *path, char **argv, char **environ)
{
    int pid;
    if (pid = fork()) {
	return pid;
    } else {
	if (execve(path, argv, environ)) {
	    perror("execve");
	}
	exit(1);
    }
}
