/**************************************************************************
NETBOOT -  BOOTP/TFTP Bootstrap Program

Author: Martin Renters
  Date: Dec/93

**************************************************************************/
#include "netboot.h"
#include <netinet/in.h>
#include <net/if.h>
#include <sys/mbuf.h>
#include <sal/salnet.h>
#include <salnet/bootp.h>

extern void (*fputchar)(int c);
extern int (*fgetchar)(void);
extern int (*fiskey)(void);

extern char kernel[];
extern ipaddr_t server_ip;

char *index(char*,char);

void cmd_ip(), cmd_server(), cmd_kernel(), cmd_help();
void cmd_set(), cmd_unset(), cmd_boot(), cmd_init(), cmd_bootp();

struct bootcmds_t {
	char *name;
	void (*func)();
	char *help;
} bootcmds[] = {
	{"?",		cmd_help,	"                 this list"},
	{"help",	cmd_help,	"              this list"},
	{"init",	cmd_init,	"              restart netboot"},
	{"exit",	cmd_init,	"              restart netboot"},
	{"halt",	cmd_init,	"              restart netboot"},
	{"bootp",	cmd_bootp,	"              retry bootp broadcast"},
	{"ip",		cmd_ip,		"<addr>          set my IP addr"},
	{"server",	cmd_server,	"<addr>      set nfs server IP addr"},
	{"kernel",	cmd_kernel,	"<file>      set boot filename"},
	{"set",         cmd_set,        "<option>       set boot flag"},
	{"show",        cmd_set,        "               show boot flag"},
	{"unset",       cmd_unset,      "<option>     clear boot flag"},
	{"b",	cmd_boot,	"                 alias for boot"},
	{"boot",	cmd_boot,	"[ [-fi] filename ]"},
	{NULL,		NULL,		NULL}
};

void
cmd_boot(char *cmd)
{
	if (strncmp(cmd,"-fi ",4)==0) {
		cmd += 4;
		}
	if (*cmd) {
		if (*cmd=='"') {
			++cmd;
			index(cmd,'"')[0] = '\0';
			}
		strcpy(kernel,cmd);
		}
	load();
}

void
cmd_bootp()
{ /* bootp setup */
	int localip;
	char servername[BP_SNAME_LEN];
	int err;

	err = salnet_bootp(&localip, &server_ip, servername, kernel);
	if (err) {
		salnet_perror("bootp error",err);
		printf("Use ip and server cmd to set addresses\n");
		}
	else	{
		salnet_setipaddr(localip);
		printf("bootp: %I.  Served by %s(%I), default kernel \"%s\"\n",
			ntohl(localip), servername, ntohl(server_ip), kernel);
		}
}

void
cmd_init()
{
	printf("Rebooting...\n");
	exit();
}

/**************************************************************************
CMD_HELP - Display help screen
**************************************************************************/
void
cmd_help()
{
	struct bootcmds_t *cmd = bootcmds;
	printf("\n");
	while (cmd->name) {
		printf("%s %s\n",cmd->name,cmd->help);
		cmd++;
	}
}

/**************************************************************************
CMD_IP - Set my IP address
**************************************************************************/
void
cmd_ip(p)
	char *p;
{
	int i;
	if (setip(p, &i))
		salnet_setipaddr(htonl(i));
	printf("ip: %I.  Served by %I, default kernel \"%s\"\n",
		ntohl(salnet_getipaddr()), ntohl(server_ip), kernel);
}


/**************************************************************************
CMD_SERVER - Set server's IP address
**************************************************************************/
void
cmd_server(p)
	char *p;
{
	int i;
	if (setip(p, &server_ip)) server_ip = htonl(server_ip);
	printf("server: Served by %I, default kernel \"%s\"\n",
		ntohl(server_ip), kernel);
}


/**************************************************************************
CMD_SET - Set boot flag
**************************************************************************/
extern int flags;

void
cmd_set(p)
	char *p;
{
    if(!strcmp(p, "")) {
	printf("show: %I.  Served by %I, default kernel \"%s\"\n",
		ntohl(salnet_getipaddr()), ntohl(server_ip), kernel);
	printf("cdrom:   %s\n", flags & RB_CDROM ? "SET" : "CLEAR");
	printf("askname: %s\n", flags & RB_ASKNAME ? "SET" : "CLEAR");
	printf("halt:    %s\n", flags & RB_HALT ? "SET" : "CLEAR");
	printf("config:  %s\n", flags & RB_CONFIG ? "SET" : "CLEAR");
	printf("debug:   %s\n", flags & RB_KDB ? "SET" : "CLEAR");
	printf("serial:  %s\n", flags & RB_SERIAL ? "SET" : "CLEAR");
	printf("single:  %s\n", flags & RB_SINGLE ? "SET" : "CLEAR");
	printf("verbose: %s\n", flags & RB_VERBOSE ? "SET" : "CLEAR");
	return;
    }

    if(!strcmp(p, "cdrom")) {
	flags |= RB_CDROM;
	return;
    }
    if(!strcmp(p, "askname")) {
	flags |= RB_ASKNAME;
	return;
    }
    if(!strcmp(p, "halt")) {
	flags |= RB_HALT;
	return;
    }
    if(!strcmp(p, "config")) {
	flags |= RB_CONFIG;
	return;
    }
    if(!strcmp(p, "debug")) {
	flags |= RB_KDB;
	return;
    }
    if(!strcmp(p, "serial")) {
	flags |= RB_SERIAL;
	return;
    }
    if(!strcmp(p, "default")) {
	flags |= RB_DFLTROOT;
	return;
    }
    if(!strcmp(p, "single")) {
	flags |= RB_SINGLE;
	return;
    }
    if(!strcmp(p, "verbose")) {
	flags |= RB_VERBOSE;
	return;
    }

    printf("Unknown option %s\n", p);
}

/**************************************************************************
CMD_UNSET - Clear boot flag
**************************************************************************/
void
cmd_unset(p)
	char *p;
{
    if(!strcmp(p, "cdrom")) {
	flags &= ~RB_CDROM;
	return;
    }
    if(!strcmp(p, "askname")) {
	flags &= ~RB_ASKNAME;
	return;
    }
    if(!strcmp(p, "halt")) {
	flags &= ~RB_HALT;
	return;
    }
    if(!strcmp(p, "config")) {
	flags &= ~RB_CONFIG;
	return;
    }
    if(!strcmp(p, "debug")) {
	flags &= ~RB_KDB;
	return;
    }
    if(!strcmp(p, "serial")) {
	flags &= ~RB_SERIAL;
	return;
    }
    if(!strcmp(p, "default")) {
	flags &= ~RB_DFLTROOT;
	return;
    }
    if(!strcmp(p, "single")) {
	flags &= ~RB_SINGLE;
	return;
    }
    if(!strcmp(p, "verbose")) {
	flags &= ~RB_VERBOSE;
	return;
    }

    printf("Unknown option %s\n", p);
}

/**************************************************************************
CMD_KERNEL - set kernel filename
**************************************************************************/
void
cmd_kernel(p)
	char *p;
{
	if (*p) sprintf(kernel ,"%s",p);
	printf("kernel: %I.  Served by %I, default kernel \"%s\"\n",
		ntohl(salnet_getipaddr()), ntohl(server_ip), kernel);
}

/**************************************************************************
EXECUTE - Decode command
**************************************************************************/
execute(buf)
	char *buf;
{
	char *p, *q;
	struct bootcmds_t *cmd = bootcmds;
	while (*buf == ' ' || *buf == '\t')
		buf++;
	if ((!(*buf)) || (*buf == '#')) 
		return(0);
	while(cmd->name) {
		p = buf;
		q = cmd->name;
		while (*q && (*q == *p)) p++,q++;
		if ((!(*q)) && ((*p == ' ') || (*p == '\t') || (!(*p)))) {
			if (!cmd->func) 
				return(1);
			while (*p == ' ')
				p++;
			(cmd->func)(p);
			return(0);
		} else
			cmd++;
	}
	printf("bad command - type 'help' for list\n");
	return(0);
}

/**************************************************************************
BOOTMENU - Present boot options
**************************************************************************/
void
bootmenu()
{
	char cmd[80];
	int ptr, c;
	unsigned long time;

	printf("\n");
	while (1) {
		ptr = 0;
		printf(">>> ");
		while (ptr < 80) {
			while(1) {
				time = currticks() + (TIMEOUT*4);
				while(time > currticks()){
					if (fiskey())
						goto handlechar;
			
				}
				eth_reset();
			}
		handlechar:
			c = fgetchar();
			if (c == '\r')
				break;
			else if (c == '\b') {
				if (ptr > 0) {
					ptr--;
					printf("\b \b");
				}
			} else {
				cmd[ptr++] = c;
				fputchar(c);
			}
		}
		cmd[ptr] = 0;
		printf("\n");
		if (execute(cmd)) break;
	}
	eth_reset();
}
