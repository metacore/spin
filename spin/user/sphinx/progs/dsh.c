/* 

   D(umb)-shell for the SPIN Dlib.


   Thu May 23 23:09:29 1996

   Yasushi Saito

*/

#include <assert.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <signal.h>
#include <sys/wait.h>

static int mypid;

#define MAX_JOBS 99
typedef enum {STOPPED, FG, BG} state_t;
typedef struct job {
    state_t state;
    int pgid;
    char *cmdline;
} *job_t;
struct job jobs[MAX_JOBS];

void job_add(int pgid, char *cmdline)
{
    int i;
    for (i = 0; i < MAX_JOBS; i++) {
	if (jobs[i].pgid == 0) {
	    jobs[i].pgid = pgid;
	    jobs[i].cmdline = strdup(cmdline);
	    return;
	}
    }
}
job_t job_find(int pgid)
{
    int i;
    for (i = 0; i < MAX_JOBS; i++) {
	if (jobs[i].pgid == pgid) {
	    return &jobs[i];
	}
    }
    abort();
}
void job_delete(int pgid)
{
    int i;
    for (i = 0; i < MAX_JOBS; i++) {
	if (jobs[i].pgid == pgid) {
	    jobs[i].pgid = 0;
	    free(jobs[i].cmdline);
	    return;
	}
    }
    printf("no job %d\n", pgid);
    abort();
}



extern char **environ;
char *getenv();

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
	if (execve(path, argv, environ)) {
	    perror("execve");
	}
	exit(1);
    }
}


int spawnvp(char *path, char **argv)
{
    char *PATH = getenv("PATH");
    char buf[1024];
    char *s;

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

void prompt()
{
    printf("dsh>");
    fflush(stdout);
}

int fg_pid;

void setfg(int pid)
{
    void (*old)() = signal(SIGTTOU, SIG_IGN);
    tcsetpgrp(0, pid);
    signal(SIGTTOU, old);
    fg_pid = pid;
}

void sigchld()
{
    int status;
    int pid;
    job_t job;
    write(1, "chld\n", 5);
    for (;;) {
	pid = waitpid(-1, &status, WNOHANG|WUNTRACED);
	printf("pid = %d\n", pid);
	if (pid <= 0) break;

	job = job_find(pid);
	
	if (WIFEXITED(status)) {
	    printf("%d exited\n", pid);
	    job_delete(pid);
	}
	if (WIFSIGNALED(status)) {
	    printf("%d signaled\n", pid);
	    job_delete(pid);
	}
	if (WIFSTOPPED(status)) {
	    printf("%d stopped\n", pid);
	    job->state = STOPPED;
	}
	if (WIFCONTINUED(status)) {
	    printf("%d cont'ed\n", pid);
	    job->state = FG;
	}
	
	if (fg_pid == pid) {
	    printf("restoreing tty.\n");
	    setfg(mypid);
	}
    }
}

char *state_str(state_t state)
{
    switch (state) {
      case STOPPED : return "Stopped";
      case FG : return "FG";
      case BG : return "BG";
    }
}
void cmd_jobs()
{
    int i;
    for (i = 0; i < MAX_JOBS; i++) {
	if (jobs[i].pgid) {
	    printf("[%d] %s    %s", i+1, state_str(jobs[i].state),
		   jobs[i].cmdline);
	}
    }
}
void cmd_fg(int argc, char **argv)
{
    int job = atoi(argv[1]+1) - 1;
    if (!jobs[job].pgid) {
	printf("fg : no such job\n");
	return;
    }

    setfg(jobs[job].pgid);
    kill(jobs[job].pgid, SIGCONT);
    pause();
}


main()
{
    int argc;
    char *argv[256];
    char buf[256];
    char cmdline[256];

    mypid = getpid();
    printf("Hello world. I'm a D(umb) shell(pid=%d).\n", mypid);
    setfg(mypid);
    
    printf("shell commands are : exit, fg, and jobs.\n");
    signal(SIGCHLD, sigchld);

    for (;;) {
	prompt();
	if (fgets(buf, sizeof buf, stdin) == 0) {
	    printf("eof\n");
	    break;
	}
	strcpy(cmdline, buf);

	if (!parse_command_line(buf, &argc, argv)) {
	    continue;
	}
	
	if (!strcmp(argv[0], "exit")) {
	    exit(0);
	} else if (!strcmp(argv[0], "jobs")) {
	    cmd_jobs(argc, argv);
	} else if (!strcmp(argv[0], "fg")) {
	    cmd_fg(argc, argv);
	} else {
	    int pid;
	    int status;
	    pid = spawnvp(argv[0], argv);
	    if (pid <= 0) {
		perror(argv[0]);
		errno = 0;
	    } else {
		setpgid(pid, pid);
		printf("child=%d\n", pid);
		tcsetpgrp(0, pid);
		fg_pid = pid;
		job_add(pid, cmdline);
		
		/*tcsetpgrp(1, pid);*/
		pause();
	    }
	}
    }
}
