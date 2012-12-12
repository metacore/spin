/*
 * Domain header file
 * 
 * HISTORY
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Cosmetic changes to port to Linux.
 *
 */

#ifndef __DOMAIN_H_


#define __DOMAIN_H_

struct domain_info_data;

#ifndef linux
struct domain_info_data * pc_to_domain PARAMS ((struct domain_info_data*));

struct objfile* get_next_object_file PARAMS((struct objfile*));
#endif

extern int skip_pre_open;
#endif


























