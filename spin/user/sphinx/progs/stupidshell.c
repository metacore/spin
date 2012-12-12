#include <errno.h>
#include <string.h>
#include <stdio.h>
extern char **environ;
int parse_command_line(char *buf, int *argc, char **argv) 
{
    char *path;
    char *s;
    path = strtok(buf, "\n\r \t");
    if (path ==  0) {
	fprintf(stderr, "you have to input pathname.\n");
	return 0;
    }
    argv[0] = path;
    *argc= 1;

    while (s = strtok(0, "\n\r \t")) {
	argv[*argc] = s;
	(*argc)++;
    }
    argv[*argc] = 0;
    return 1;
}



int spawnve(char *path, char **argv, char **environ)
{
    int pid;
    if (pid = fork()) {
	return pid;
    } else {
	if (execve(path, argv, environ) <= 0) {
	    perror("execve");
	}
	exit(1);
    }
}

int spawnvp(char *path, char **argv)
{
    char *PATH = "/spin/egs";
    char buf[1024];
    char *s;
    
    if (*path == '/') 
	return spawnve(path, argv, environ);

    while (*PATH) {
	s = buf;
	while (*PATH && *PATH != ':' && s < buf + sizeof buf-1) {
	    *s = *PATH;
	    s++;
	    PATH++;
	}
	if (*PATH) PATH++; /* skip ':' */
	*s++ = '/';
	strcpy(s, path);
	if (access(buf, 0) == 0) {
	    return spawnve(buf, argv, environ);
	}
    }
    errno = ENOENT;
    return -1;
}

main()
{
    char buf[128];
    char *argv[100];
    int argc;

    for (;;) {
	printf("shell!>"); fflush(stdout);
	if (fgets(buf, sizeof buf, stdin) == 0) {
	    printf("error : %d\n", errno);
	    exit(0);
	}

	if (!parse_command_line(buf, &argc, argv)) {
	    ;
	} else {
	    if (!strcmp(argv[0], "exit")) {
		exit(0);
	    } else {
		int status;
		spawnvp(argv[0], argv);
		wait(&status);
	    }
	}
    }
}



