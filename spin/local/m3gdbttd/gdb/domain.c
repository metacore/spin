/*
 *
 * HISTORY
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Cosmetic changes to port to Linux.
 *
 * 21-Feb-96  Brian Bershad (bershad) at the University of Washington
 *	Check if target exists before sweeping.
 *
 */

#include "defs.h"
#include "bfd.h"		/* Binary File Description */
#include "symtab.h"
#include "symfile.h"
#include "objfiles.h"
#include "domain.h"
#include "gdb-stabs.h"
#include "target.h"
#include "gdbcmd.h"
#include "ttd.h"

#include <ctype.h>
#include <sys/types.h>
#include <signal.h>

#define MAX_STACKED_DOMAINS			16

typedef struct domain_info_data {
	char	*name;			  /* user's given name */
	struct target_ops exec_ops;	  /* primary target */
	bfd	*exec_bfd;

#ifdef NEED_TEXT_START_END
	CORE_ADDR text_start = 0;
	CORE_ADDR text_end   = 0;
#endif
	struct objfile *object_files;	  /* link list of all objfiles */
	struct objfile *current_objfile;  /* for symbol file being read in */
	struct objfile *symfile_objfile;  /* main symbol table loaded from */
	struct domain_info_data *next;
} *domain_t;


int skip_pre_open = 0;

domain_t domain_stack[MAX_STACKED_DOMAINS];
int	 domain_tos = 0;

domain_t all_domains = NULL;

domain_t default_domain = NULL;
char *default_domain_name = NULL; /* sometimes we can know the name
				   * without having enough information
				   * to know anything else
				   */

static void (*real_exec_close)(int) = NULL;
static void (*real_exec_open)(char*, int) = NULL;
static void (*real_exec_files_info)(struct target_ops *);
	
	
	

struct cmd_list_element *domain_cmd_list = NULL;

#ifndef linux
static void info_domain_command PARAMS ((char * tidstr, int from_tty));
static void domain_command PARAMS ((char * dstr, int from_tty));
static void domain_load_command PARAMS ((char * dstr, int from_tty));
static void domain_set_command PARAMS ((char * dstr, int from_tty));
static void domain_sweep_command PARAMS ((char * dstr, int from_tty));	
static void domain_push_command PARAMS ((char * dstr, int from_tty));	
static void domain_pop_command PARAMS ((char * dstr, int from_tty));
static void domain_stack_command PARAMS ((char * dstr, int from_tty));	
static domain_t find_all_domains PARAMS (());
static void domain_initialize PARAMS ((char *name));

static void	domain_snapshot PARAMS((domain_t d));
static void     domain_set_current PARAMS((domain_t d));
static domain_t domain_top_of_stack PARAMS(());
static domain_t domain_push PARAMS((domain_t d));
static domain_t domain_pop  PARAMS((domain_t d));
static domain_t domain_capture  PARAMS((char *name));
static char*	domain_exec_filename PARAMS((domain_t domain));
#endif

/*
 * exec_ops is used by the exec target. We hijack it here. Each domain
 * must have a primary exec image, to which we add or remove symbols.
 * We do not bother to tell exec that we are doing this.
 */

extern struct target_ops exec_ops;





extern bfd		*exec_bfd;
#ifdef NEED_TEXT_START_END
extern CORE_ADDR text_start;
extern CORE_ADDR text_end;
#endif


void
domain_exec_close(quitting)
	int quitting;
{
	domain_t d;
	
	printf("domain_exec_close returning\n");
	find_all_domains();
	d = domain_top_of_stack();
	if (d->exec_bfd)  {
		printf("Emptying domain %s at top of stack\n",
		       domain_exec_filename(d));
		(*real_exec_close)(quitting);
		domain_initialize(d, d->name, d->next);
	}
		
	/*domain_snapshot(d);*/

	/* Clear out the section and BFD
	 * information for exec's idea of what
	 * is currently active. We've still got our own copy inside d;
	 */
	exec_ops.to_sections = NULL;	
	exec_ops.to_sections_end = NULL;
	exec_bfd = NULL;       
	
}

void
domain_exec_open(args, from_tty) /*  */
	char *args;
	int from_tty;
{
	printf("domain_exec_open calling through to real\n");

	skip_pre_open = 1;		/* XX */
	(*real_exec_open)(args, from_tty);
	skip_pre_open = 0;
	domain_snapshot(domain_top_of_stack());
}

void
domain_files_info(struct target_ops *t)
{
	if (exec_bfd == 0)  {
		error("No files in top level domain\n");
	} else
		(*real_exec_files_info)(t);
}

		
static void
domain_snapshot(domain_t d)
{
	d->object_files    = object_files;
	d->current_objfile = current_objfile;
	d->symfile_objfile = symfile_objfile;
	d->exec_bfd	   = exec_bfd;
#ifdef NEED_TEXT_START_END	
	d->text_start      = text_start;
	d->text_end	   = text_end;
#endif

	d->exec_ops        = exec_ops;
}
	

static void
domain_hijack_exec_ops(domain_t d)
{
	if (!real_exec_close)  {
		real_exec_close = exec_ops.to_close;
		real_exec_open = exec_ops.to_open;
		real_exec_files_info = exec_ops.to_files_info;
	}
	exec_ops.to_close = domain_exec_close;
	exec_ops.to_open = domain_exec_open;
	exec_ops.to_files_info = domain_files_info;
}

static void
domain_set_current(domain_t d)
{
	object_files	= d->object_files;
	symfile_objfile = d->symfile_objfile;
	current_objfile = d->current_objfile;
	exec_ops	= d->exec_ops;
	exec_bfd	= d->exec_bfd;
	
#ifdef NEED_TEXT_START_END
	text_start	= d->text_start;
	text_end	= d->text_end;
#endif
	domain_hijack_exec_ops(d);

	/* any caches to clear? */
}


static void
domain_initialize(domain_t d, char *name)
{
	d->name = name;
	d->exec_ops = exec_ops;
	d->exec_bfd = NULL;
#ifdef NEED_TEXT_START	
	d->text_start = 0;
	d->text_end = 0;
#endif
	d->object_files = 0;
	d->current_objfile = 0;
	d->symfile_objfile = 0;

}


/*
 * Capture the current domain and give it the named name
 */
static domain_t
domain_capture(char *name)
{
	domain_t d = (domain_t)malloc(sizeof(*d));
	if (!d) {
		error("domain_capture no more core\n");
	}
	domain_initialize(d, (char*)strdup(name));
	domain_snapshot(d);
	d->next = all_domains;
	all_domains = d;	
	return d;
}

static char *
domain_exec_filename(domain_t d)
{
	if (d->exec_bfd->filename) {
		return d->exec_bfd->filename;
	}
	return "unknown";
}
		


/*
 * Capture the default domain and push it on the top of the stack
 */
static domain_t
domain_capture_default()
{

	if (default_domain) {
		error("domain_capture_default: default domain already exists\n");
	}
	default_domain = domain_capture("default");
	domain_push(default_domain);
	printf("Naming default domain 'default': %s\n",
	       domain_exec_filename(default_domain));
	return default_domain;
}

	
	

/*
 * Return a pointer to all known domains.
 * Checks if the current domain is in the domain list. if not, adds it.
 */

static domain_t
find_all_domains()
{
	domain_t d;

	if (!object_files) {
	        /* easy case */
		return all_domains;
	}
	
	for (d = all_domains; d; d = d->next)  {
		struct objfile *o;
		
		/*
		 * Scan the "current" domain and find the domain
		 * descriptor which describes it.
		 */
		for (o = object_files; o; o = o->next)  {
			if (o == d->object_files)  {
				/* we found the domain for "current"  */
				/* snapshot just to be safe */
				d->object_files = object_files;
				d->symfile_objfile = symfile_objfile;
				d->current_objfile = current_objfile;
				if (d != domain_top_of_stack()) {
					printf("current domain not at top of domain stack\n");
				}
				return all_domains;
			}
		}
	}

	
	/* If here, then we don't know anything about the
	 * relationship between the current domain (spec) and the
	 * rest of the system's idea of the current domain. So we
	 * make the domain at the top of the stack the current domain.
	 * if the stack is empty, then we create the default domain.
	 */
	d = domain_top_of_stack();
	if (!d)  {
		if (default_domain)  {
			error("already have a default domain\n");
		}
		d = domain_capture_default();
	} else {
		printf("initializing domain at top of stack\n");
		d = domain_top_of_stack();
		domain_snapshot(d);
	}
	domain_set_current(d);
	
	return all_domains;
}



static domain_t
domain_push(domain_t d)
{
	if (domain_tos > MAX_STACKED_DOMAINS)  {
		printf("domain_push: stack overflow. No push");
		return;
	}
	domain_stack[domain_tos++] = d;
	return d;
}

static domain_t
domain_top_of_stack()
{
	if (domain_tos == 0) return 0;
	else return domain_stack[domain_tos-1];
}

static domain_t
domain_pop()
{
	domain_t d;
	if (domain_tos == 0)  {
		printf("domain_pop: stack underflow");
		return 0;
	}
	d = domain_stack[--domain_tos];
	return d;
}


/*
 * Iterator function to iterate through all of the domains
 */


static int
domain_level_from_objfile(struct objfile *obj)
{
	domain_t d;
	struct objfile *o;
	int level = domain_tos - 1;
	
	if (level < 0) return -1;
	
	for (d = domain_stack[level]; level >= 0; level--) {
		for (o = d->object_files; o; o = o->next)  {
			if (o == obj)
				return level;
		}
	}
	return -1;
}
		
	

struct objfile*
get_next_object_file(struct objfile *obj)
{
	domain_t d;
	struct objfile *next;
	if (obj == 0)  {
		(void)find_all_domains();
		d = domain_top_of_stack();
		if (!d)
			return 0;
		next = d->object_files;
	} else {
		next = obj->next;
		if (!next) {
			int level = domain_level_from_objfile(obj);
			level--;
			if (level < 0) return 0;
			d = domain_stack[level];
			next = d->object_files;
		}
	}
	return next;
}


	
	

	

/*
 * Return the domain in which a given PC can be found.
 * works by walking all domains until we find one whose text
 * segment includes the specified pc.
 */

domain_t
pc_to_domain(CORE_ADDR pc)
{
	domain_t d;
	int found = /*FALSE*/ 0;
	int i = domain_tos - 1;
	domain_t dcur = domain_top_of_stack();

	while (i >= 0)  {
		d = domain_stack[i];
		domain_set_current(d);
		if (find_pc_section(pc))
			break;
	}
	domain_set_current(dcur);
	if (i>=0)
		return domain_stack[i];
	else
		return 0;
}



static domain_t
find_named_domain(char *name)
{
	domain_t d;
	for (d =  find_all_domains(); d; d = d->next)  {
		if (strcmp(d->name, name) == 0)
			break;
	}
	return d;
}


static void
domain_pc_range(CORE_ADDR *lpc, CORE_ADDR *hpc)
{
	struct obj_section *s;
	struct objfile *objfile;
	CORE_ADDR low = -1;
	CORE_ADDR high = 0;
  
	ALL_OBJFILES (objfile)  
		for (s = objfile->sections; s < objfile->sections_end; ++s)  {
			if (s->addr < low)  {
				low = s->addr;
			}
			if (s->endaddr > high) {
				high = s->endaddr;
			}
		}
	*lpc = low;
	*hpc = high;
}

static void
domain_print(domain_t d)
{		
	CORE_ADDR lowpc;
	CORE_ADDR highpc;
	domain_t dcur = domain_top_of_stack();

	printf("%s", (dcur == d) ? "*" : " ");

	printf("Domain %s (", d->name);
	
	domain_set_current(d);
	
	if (d->symfile_objfile) {
		domain_pc_range(&lowpc,  &highpc);		
		printf("objfile-name = %s: 0x%lx-0x%lx)\n",
		       domain_exec_filename(d),
		       lowpc, highpc);
	} else {
		printf("EMPTY)\n");
	}
	domain_set_current(dcur);
		 
}


static void
info_domain_command(char *arg, int from_tty)
{
	domain_t d;
	
	for (d = find_all_domains(); d; d = d->next) {
		domain_print(d);
	}

}

static void
domain_command(char *dst, int from_tty)
{
	domain_t d;

	if (!dst) {
		error("missing domain name");
	} else if (strcmp(dst,"info") == 0) {
		info_domain_command(0,0); /* syntactic sugar */
	} else {
		d = find_named_domain(dst);
	
		if (!d) {
			error("unknown domain");
		}
		(void)domain_pop();
		domain_push(d);
		domain_set_current(d);
		domain_stack_command(0,0);
	}
}


static void
domain_sweep_command (args, from_tty)
     char *args;
     int from_tty;
{
	static char sweepname[] = "Sweep       ";
	static int sweepcnt = 1;
	char *cnt = sweepname+5;
	domain_t d=0;

	if (!ttd_is_connected()) {
	    error("No ttd target.  Must attach first.");
	    return;
	}

#ifdef notdef    
	/* make a new domain for the sweep and push it */
	do { /* look for unused domain name */
		sprintf(cnt,"%d",sweepcnt++);
		d =  find_named_domain(sweepname);
		}
	while(d);
	d = domain_capture(sweepname);
	domain_initialize(d, d->name);
	domain_push(d);
	domain_set_current(d);
	domain_stack_command(0, 0);
#endif
	printf("Sweeping\n");
	if (!ttd_get_all_domain_info()) {
	    error("Could not fetch domain information from target");
	}
	else printf("done\n");
}


static void
domain_push_command(char *dst, int from_tty)
{
	domain_t d1;
	domain_t d2;

	
	if (!dst) {
		(void)find_all_domains();
		d2 = domain_pop();
		if (!d2) {
			error("No domain!\n");
		}
		d1 = domain_pop();
		if (!d1)  {
			domain_push(d2);
			error("domain push: no other domains");
		}
		domain_push(d2);
	} else {
		d1 = find_named_domain(dst);
		if (!d1) {
			error("unknown domain");
		}
	}
	domain_push(d1);
	domain_set_current(d1);
	domain_stack_command(0, 0);
}

static void
domain_pop_command(char *dst, int from_tty)
{
	domain_t d;

	(void)find_all_domains();
	(void)domain_pop();
	d = domain_top_of_stack();
	if (!d) {
		error("domain_pop domain empty. Resetting to default\n");
		/* XX */
		d = default_domain;
		domain_push(d);
	}
	domain_set_current(d);
	domain_stack_command(0,0);
}

static void
domain_stack_command(char *dst, int from_tty)
{
	int i;	


	(void)find_all_domains();
	for (i = 0; i < domain_tos; i++)  {
		domain_print(domain_stack[i]);
	}
}

static void
domain_create_command(char *dst, int from_tty)
{
	domain_t d;

	if (!dst) {
		error("Please specify a domain name.  Use the \"info domain\" command to see the names of currently known domains\n");
	}
	d =  find_named_domain(dst);
	if (d)  {
		error("That domain already exists.    Use the \"info domain\" command to see the names of currently known domains\n");
	}
	d = domain_capture(dst);
	domain_initialize(d, d->name);
}
		
	
		


void
_initialize_domain()
{
	extern struct cmd_list_element *cmdlist;

	add_info ("domain", info_domain_command,
		  "Names of currently known domains.");

	add_prefix_cmd ("domain", class_run, domain_command,
			"Use this command to switch between domains.\n\
The new domain name must be currently known.", &domain_cmd_list, "domain ", 1,
			&cmdlist);

	add_cmd("sweep", class_run, domain_sweep_command,
		"Read the object files for all domains into gdb.\n",
		&domain_cmd_list);

	add_cmd("push", class_files, domain_push_command,
		"Pushes the argument domain onto the domain stack.\n\
The topmost domain is always the current one.",
		&domain_cmd_list);

	add_cmd("pop", class_files, domain_pop_command,
		"Pops the top argument from the domain stack.  \n\
If the top domain is the initial \"default\" domain, then pop is a no-op\n",
		&domain_cmd_list);

	add_cmd("stack", class_files, domain_stack_command,
		"Shows the contents of the domain stack\n",
		&domain_cmd_list);

	add_cmd("create", class_files, domain_create_command,
		"Creates a new named domain which you can push before reading a new symbol table\n",
		&domain_cmd_list);
		

}
