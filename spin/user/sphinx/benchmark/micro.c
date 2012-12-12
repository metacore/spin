#include "rpcc.h"
#include <sys/types.h>
#include <sys/stat.h>
#define ITR 200

main()
{
    spy_t null_spy, getpid_spy, stat_spy, write_spy;
    struct stat ss;
    int i;
    int fd;
    char c;

    null_spy = spy_create("null", ITR);
    getpid_spy = spy_create("getpid", ITR);
    stat_spy = spy_create("stat", ITR);
    write_spy = spy_create("write", ITR);
    USyscall_System("spy reset");

    for (i = 0; i < ITR; i++) {
	spy_start(null_spy);
	spy_stop(null_spy);
    }
    for (i = 0; i < ITR; i++) {
	spy_start(getpid_spy);
	getpid();
	spy_stop(getpid_spy);
    }
    for (i = 0; i < ITR; i++) {
	spy_start(stat_spy);
	stat("/", &ss);
	spy_stop(stat_spy);
    }
    fd = open("/dev/null", 3);
    for (i = 0; i < ITR; i++) {
	spy_start(write_spy);
	write(fd, &c, 1);
	spy_stop(write_spy);
    }
    USyscall_System("spy dump");
    spy_dump_all();
}
