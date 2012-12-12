#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
char *_stdin_file;
char *_stdin_open_mode;
char *_stdout_file;
char *_stdout_open_mode;
char *_stderr_file;
char *_stderr_open_mode;

void 
doredirect (int *argc, char **argv)
{
    int i;
    int di;
    for (i = 1; i < *argc; i++) {
	if (!argv[i]) continue;

	if (!strcmp(argv[i], ">") && i < *argc-1) {
	    _stdout_file = argv[i+1];
	    _stdout_open_mode = "w";
	    argv[i] = argv[i+1] = 0;
	} else if (!strcmp(argv[i], "2>") && i < *argc-1) {
	    _stderr_file = argv[i+1];
	    _stderr_open_mode = "w";
	    argv[i] = argv[i+1] = 0;
	} else if (!strcmp(argv[i], "<") && i < *argc-1) {
	    _stdin_file = argv[i+1];
	    _stdin_open_mode = "r";
	    argv[i] = argv[i+1] = 0;
	}
    }
    /* squash null entries */
    di = 1;
    for (i = 1; i < *argc; i) {
	if (argv[i]) {
	    argv[di++] = argv[i++];
	} else {
	    i++;
	}
    }
    argv[di] = 0;
    *argc = di;


    if (_stdin_file) {
	fclose(stdin);
	*(stdin) = *fopen(_stdin_file, _stdin_open_mode);
	if (!stdin) {
	    perror(_stdin_file);
	    exit(1);
	}
    }
    if (_stdout_file) {
	fclose(stdout);
	*(stdout) = *fopen(_stdout_file, _stdout_open_mode);
	if (!stdout) {
	    perror(_stdout_file);
	    exit(1);
	}
    }
    if (_stderr_file) {
	fclose(stderr);
	*(stderr) = *fopen(_stderr_file, _stderr_open_mode);
	if (!stderr) {
	    perror(_stderr_file);
	    exit(1);
	}
    }
    
}
