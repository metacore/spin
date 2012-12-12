#include <limits.h>
#include <stdio.h>
#include <string.h>
#include "usyscall.h"
#define NIL_REF LONG_MAX

void *malloc();

static struct domain {
    char *name;
    struct domain *link;
} *linked_domains;

static int 
domain_linked_p (char *domain)
{
    struct domain *d;
    for (d = linked_domains; d; d = d->link) {
	if (!strcmp(d->name, domain))
	  return 1;
    }
    return 0;
}

static int
addto_linked_domain (char *domain) {
    struct domain *n = (struct domain*)malloc(sizeof(*n));
    n->name = strdup(domain);
    n->link = linked_domains;
    linked_domains = n;
}

void 
__usyscall_bootstrap (char *domain, char *prog_name)
{
    unsigned long reply;
    int fd;

    if (domain_linked_p(domain)) return;
    fprintf(stderr, "%s: try linking %s.", prog_name, domain);
    fd = USyscall_DomainLookup(domain);

    if (fd == NIL_REF)  {
        fprintf(stderr, "\n%s: domain \"%s\" not found.\n", prog_name, domain);
	exit(1);
    } 
    
    USyscall_Close(fd);
    
    if (USyscall_Rendezvous(domain, NIL_REF, &reply) != 0) {
	fprintf(stderr, "\n%s: Could not rendezvous.\n", prog_name);
	exit(1);
    }
    addto_linked_domain(domain);
    fprintf(stderr, "..done.\n");
}

void 
__usyscall_get_spy_info (char *name, usyscall_spy_info *info)
{
    USyscall_GetSpyInfo(name, info, info->samples, info->nsamples);
}
