#include <sys/utsname.h>
main()
{
    struct utsname u;
    uname(&u);
    printf("sys: %s, node: %s, rel: %s, ver: %s, mach: %s.\n",
	   u.sysname, u.nodename, u.release, u.version, u.machine);
}
