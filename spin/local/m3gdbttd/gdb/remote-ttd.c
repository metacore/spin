/* 
 * Mach Operating System
 * Copyright (c) 1993, 1992 Carnegie Mellon University
 * All Rights Reserved.
 * 
 * Permission to use, copy, modify and distribute this software and its
 * documentation is hereby granted, provided that both the copyright
 * notice and this permission notice appear in all copies of the
 * software, derivative works or modified versions, and any portions
 * thereof, and that both notices appear in supporting documentation.
 * 
 * CARNEGIE MELLON ALLOWS FREE USE OF THIS SOFTWARE IN ITS "AS IS"
 * CONDITION.  CARNEGIE MELLON DISCLAIMS ANY LIABILITY OF ANY KIND FOR
 * ANY DAMAGES WHATSOEVER RESULTING FROM THE USE OF THIS SOFTWARE.
 * 
 * Carnegie Mellon requests users of this software to return to
 * 
 *  Software Distribution Coordinator  or  Software.Distribution@CS.CMU.EDU
 *  School of Computer Science
 *  Carnegie Mellon University
 *  Pittsburgh PA 15213-3890
 * 
 * any improvements or extensions that they make and grant Carnegie Mellon
 * the rights to redistribute these changes.
 */
/***********************************************************
Copyright 1992 by Digital Equipment Corporation, Maynard, Massachusetts,

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, provided 
that the above copyright notice appear in all copies and that both that 
copyright notice and this permission notice appear in supporting 
documentation, and that the name of Digital not be used in advertising 
or publicity pertaining to distribution of the software without specific, 
written prior permission.  Digital makes no representations about the 
suitability of this software for any purpose.  It is provided "as is"
without express or implied warranty.

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

/*
 * The TTD target code for gdb-4.7 and later.
 * Memory-access and commands for remote TTD processes.
 *
 * HISTORY
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Cosmetic changes to port to Linux.
 *
 * 07-Apr-96  Brian Bershad (bershad) at the University of Washington
 *	Added flag to record when sweeping domains.
 *
 * s16-becker
 * 	set ttd_mid when doing "target ttd HALT" to avoid printing garbage.
 *	made ttd_host allocated, rather than set to args[0] to avoid printing
 *		garbage.
 *	add check for lower case halt in "target ttd halt"
 *	Moved the disconnect call for "target ttd halt" after the
 *		push_target call so gdb wouldn't seg fault the second time you
 *		do that command.
 *	Removed the connect queries.
 *	Print the langauage on connect.
 *	Removed the disconnect queries.
 *	Added "detach nocont" to replace the continue? query on disconnect
 *	Changed DISCONNECT_LIVE and HALT to _CONT, _NOCONT and _HALT
 *		for detach nocont
 *	Changed "textaddr" to loadaddr in domain_info since it is
 *		sometimes the data address
 *
 * 21-Feb-96  Brian Bershad (bershad) at the University of Washington
 *	Changed get_all_domain_info to ttd_get_all_domain_info.
 *
 *
 * s13-becker-jan25  Added get_all_domain_info for domain sweep command
 *
 * $Log: remote-ttd.c,v $
 * Revision 1.9  1997/07/22 16:51:02  mef
 * *** empty log message ***
 *
 * Revision 1.8  1997/05/23 18:38:31  garrett
 * profiling changes
 *
 * Revision 1.7  1996/08/15 04:33:46  fgray
 * x86 merge.
 *
 * Revision 1.6  1996/05/03  14:39:55  bershad
 * *** empty log message ***
 *
 * Revision 1.5  1996/04/08  05:27:52  bershad
 * *** empty log message ***
 *
 * Revision 1.4  1996/03/18  03:55:13  becker
 * fixed sweep for lots of domains and long domain names
 *
 * Revision 1.3  1996/02/28  18:06:08  becker
 * gdb fixes merged
 *
 * Revision 1.2.36.1  1996/02/28  01:35:54  becker
 * gdb general cleanup
 *
 * Revision 1.2  1996/01/28  02:58:17  savage
 * s13-becker-jan25 spin-13
 *
 * Revision 1.1.132.1  1996/01/26  00:45:31  becker
 * merged sweep code into s13
 *
 * Revision 1.1.122.1  1996/01/25  23:38:52  becker
 * Added GET_NEXT_DOMAIN rpc for domain sweep command
 *
 * Revision 1.1  1995/07/06  01:06:31  bershad
 * New File
 *
 * Revision 1.1  1995/07/06  00:23:40  bershad
 * *** empty log message ***
 *
 * Revision 2.1.2.1  93/08/06  12:17:43  mrt
 * 	First checkin.
 * 	[93/08/06  12:09:12  grm]
 * 
 *
 */

/*
 * Author: Gerald Malan <grm@cs.cmu.edu>
 *	   School of Computer Science
 *	   Carnegie Mellon University
 *	   Pittsburgh, PA, USA
 *
 * This work is based on previous versions by:
 *
 * Author: David Redell
 *         Michael Sclafani
 *	   DEC SRC
 *         Palo Alto CA.
 */

#include <stdio.h>
#include "defs.h"
#include "frame.h"
#include "inferior.h"
#include "wait.h"
#include "target.h"
#include "gdbcore.h"
#include "command.h"
/*#include "gdb-cache.h"*/
#include <signal.h>
#include "gdbcmd.h"

#include <string.h>
#include <errno.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/time.h>

#include "ttd.h"

extern int errno;

extern int remove_breakpoints ();
extern struct value *call_function_by_hand ();
extern struct target_ops ttd_ops;	/* Forward declaration */

static char usage[] = "target_hostname [target_pid | \"ux\" | \"mk\"]";
static int TTD_trap_to_signal[] = TTD_TRAP_MAPPING;

/* Saved name of target host and pid for "info files".  Both malloc'd.  */

static char *ttd_host;
static char *ttd_mid;

struct memory_cache * ttd_cache = NULL;

/*
 * The teledebugging session's global variables:
 */
static ttd_conn		connection;
static ttd_server	curr_server;
static ttd_target	curr_target;
static ttd_thread	curr_thread;
static ttd_trap_info	curr_trap_info;
static ttd_thread_info  curr_thread_info;
static ttd_machine_state curr_state;

/* Are we debugging the kernel? */
boolean_t ttd_mk_debug = FALSE;

/* The gdb ttd debugging flag */
boolean_t ttd_debug_enabled = FALSE;

/* Prototypes */

static void
ttd_files_info PARAMS ((struct target_ops *));

static void
disconnect PARAMS ((ttd_disconnect_mode));


/* Check the value returned by a TTD operation and display an error if
   necessary.  */

void test_rc(int rc, char *msg)
{
  if (rc < TTD_SuccessRC) 
    {
      if (rc == TTD_UnixErrorRC)
	perror_with_name (msg);
      else
	error ("%s: %s", msg, TTD_resultMsg[-rc]);
    }
}

/* Sleep for the specified number of milliseconds 
 * (assumed to be less than 1000).
 * If select () is interrupted, returns immediately;
 * takes an error exit if select () fails for some other reason.
 */

static void sleep_ms(long ms)
{
  struct timeval select_timeout;
  int status;
	
  select_timeout.tv_sec = 0;
  select_timeout.tv_usec = ms * 1000;
	
  status = select (0, (fd_set *) 0, (fd_set *) 0, (fd_set *) 0, 
		   &select_timeout);
	
  if (status < 0 && errno != EINTR)
    perror_with_name ("select");
}


/* Wait until the remote target stops, then return. */

static void net_wait(ttd_target_info *info)
{
  int stop_rc;
  int probe_rc;
  
  while (1) {
    /* If CTRL-C is hit during this loop, stop the target.  */
    
    stop_rc = TTD_SuccessRC;
    if (quit_flag) 
      {
	quit_flag = 0;
	stop_rc = ttd_stop_target(connection, curr_target);
	if ((stop_rc != TTD_SuccessRC) &&
	    (stop_rc != TTD_TargetStoppedRC))
	  warning ("StopTarget: %s", TTD_resultMsg[-stop_rc]);
      }
	
    probe_rc = ttd_probe_target(connection, curr_target, info);
    switch (probe_rc) {
      case TTD_SuccessRC:
        if (info->is_stopped)
	  return;
	break;

      case TTD_InvalidTargetRC:
	if ((curr_server == USER_TTD)
	    && query ("target has terminated: disconnect? ")) 
	  {
	    target_mourn_inferior();
	    error ("Disconnected from target.");
	  }
	warning ("ProbeTarget: %s", TTD_resultMsg[-probe_rc]);
	break;

      case TTD_ServerNotAvailableRC:
	if (curr_server == KERNEL_TTD)
	  probe_rc = TTD_SuccessRC;
	else
	  warning ("ProbeTarget: %s", TTD_resultMsg[-probe_rc]);
	break;

      case TTD_NoReplyRC:
	probe_rc = TTD_SuccessRC;
	break;

      default:
	warning ("ProbeTarget: %s", TTD_resultMsg[-probe_rc]);
	break;
    }
		
    /* If a StopTarget or ProbeTarget call has failed,
       allow the user to break the connection with the target.
       We can't simply error () out of this loop, since the 
       data structures representing the state of the target
       are in an inconsistent state.  */
		
    if ((stop_rc < TTD_SuccessRC) || (probe_rc < TTD_SuccessRC)) 
      {
	if (query("Can't %s.  Disconnect from target system? ",
		  (stop_rc < TTD_SuccessRC) 
		  ? "stop remote target"
		  : "get status of remote target")) 
	  {
	    target_mourn_inferior();
	    error ("Use the \"target\" command to reconnect.");
	  }
      }
    sleep_ms (200);	/* Don't kill the network too badly */
  }
}

static int get_current_thread_info(void)
{
  int rc;

  curr_thread = inferior_pid;	/* revalidation happens here */
  rc = ttd_get_thread_info(connection, curr_target, curr_thread, 
			   &curr_thread_info, &curr_trap_info,
			   THREAD_STATE_KERNEL,
			   &curr_state);
  if (rc == TTD_SuccessRC) 
    {
      TTD_FETCH_STATE_TO_REGISTERS(&curr_state);
      registers_fetched();
    }
  return rc;
}

static void set_current_thread(ttd_target_info info)
{
  int rc;
  
  if (info.trapped_thread != TTD_NO_THREAD) 
    {
      inferior_pid = curr_thread = info.trapped_thread;
      if (get_current_thread_info() == TTD_SuccessRC)
	return;
    }
  if (curr_thread != TTD_NO_THREAD) 
    {
      inferior_pid = curr_thread;
      if (get_current_thread_info() == TTD_SuccessRC)
	return;
    }

  /* Set current thread to the first thread */
  for (curr_thread = TTD_NO_THREAD;;) 
    {
      rc = ttd_get_next_thread(connection, curr_target,
			       curr_thread, &curr_thread);
      test_rc (rc, "GetNextThread");
      if (curr_thread == TTD_NO_THREAD)
	error ("no valid thread");
      inferior_pid = curr_thread;
      rc = get_current_thread_info();
      if (rc == TTD_SuccessRC)
	return;
    }
}

/* Wait for control to return from target to debugger.
   When this function actually returns it means the target
   should be left stopped and GDB should read more commands.  */

static int ttd_wait(int pid, struct target_waitstatus *status)
{
  int rc;
  WAITTYPE w;
  ttd_target_info target_info;
  
  net_wait(&target_info);
  set_current_thread(target_info);

  status->kind = TARGET_WAITKIND_STOPPED;
  status->value.sig = SIGTRAP;
  return target_info.trapped_thread;
}

/* Advance a string pointer across whitespace and return a pointer
   to the first non-white character.  */

static char *skip_white_space(char *p)
{
  while (*p == ' ' || *p == '\t')
    p++;
  return p;
}

/* Search for the first unquoted whitespace character in a string.
   Returns a pointer to the character, or to the null terminator
   if no whitespace is found.  */

static char *find_white_space(char *p)
{
  register int c;
  
  while ((c = *p) != ' ' && c != '\t' && c) 
    {
      if (c == '\'' || c == '"') 
	{
	  while (*++p != c && *p) 
	    {
	      if (*p == '\\')
		p++;
	    }
	  if (!*p)
	    break;
	}
      p++;
    }
  return p;
}

/* Fill ARGSTRUCT in argc/argv form with the arguments from the
   argument string ARGSTRING.  */

static void parse_args(char *arg_string, int *argc, char ***argv)
{
  register int arg_count = 0;	/* number of arguments */
  register int arg_index = 0;
  register char *p0;
  
  /* first count how many arguments there are */
  
  p0 = arg_string;
  if (!p0)  {
	  *argc = 0;
	  *argv = 0;
	  return;
  }
  while (*p0 != '\0') 
    {
      if (*(p0 = skip_white_space (p0)) == '\0')
	break;
      p0 = find_white_space (p0);
      arg_count++;
    }
  
  *argc = arg_count;
  *argv = (char **) xmalloc ((arg_count + 1) * sizeof (char *));
  
  /* now copy argument strings into arg_struct.  */
  
  while (*(arg_string = skip_white_space (arg_string))) 
    {
      p0 = find_white_space (arg_string);
      (*argv)[arg_index++] = savestring (arg_string,
					 p0 - arg_string);
      arg_string = p0;
    }
  
  (*argv)[arg_count] = NULL;
}

/* Target command for TTD target systems.  */

void ttd_open(char *args, int from_tty)
{
  int argc;
  char **argv;
  int rc;
  int pid;
  ttd_machine_type machine;
  ttd_target_info info;
  ttd_address bp, next_bp;
  ttd_flavor flavor;
  ttd_saved_inst saved_inst;
  int i;
  boolean_t remove_bp;
  boolean_t need_halt = FALSE;
  struct symtabs_and_lines sals;
  struct symtab_and_line sal;
  
  if (args == NULL)
    error_no_arg (usage);
  
  /*
   * By default, we're using this to debug a Mach kernel.
   * This may change someday, and when it does, we can change
   * the default, until then....
   */
  pid = TTD_KERNEL_MID;
  ttd_mk_debug = TRUE;
  ttd_mid = strsave("SPIN");
	
  parse_args (args, &argc, &argv);
  switch (argc) 
    {
      case 2:
        /*
	 * Free the "MK"
	 */
        free(ttd_mid);

	if (!strcmp (argv[1], "UX") || !strcmp (argv[1], "ux")) 
	  {
	    pid = TTD_UX_MID;
	    ttd_mid = strsave (argv[0]);
	  }
	else if (!strcmp (argv[1], "MK") || !strcmp (argv[1], "mk"))
	  {
	    pid = TTD_KERNEL_MID;
	    ttd_mid = strsave (argv[1]);
	    ttd_mk_debug = TRUE;
	  }
	else if (!strcmp(argv[1], "halt") || !strcmp(argv[1], "HALT")) {
		printf("Will try to HALT\n");
		need_halt = TRUE;
		ttd_mid = strsave("HALT");
	} else 	  {
	    pid = atoi (argv[1]);
	    if (pid == 0)
	      error ("invalid target pid.  Expected %s", usage);
	    ttd_mid = malloc (4 + strlen(argv[1]) + 1);
	    strcpy (ttd_mid, "pid ");
	    strcat (ttd_mid, argv[1]);
	  }
	

      case 1:
	ttd_host = strsave(argv[0]);
	break;

      default:    
	error (usage);
    }

#if	DEC_CODE	
  if (ttd_host == NULL)
    ttd_host = strsave ("localhost");
#endif	/* DEC_CODE */
	
  target_preopen (from_tty);
  
  printf ("Attaching to remote machine across net...\n");
  fflush (stdout);
  
  /* Allow the user to kill the connect attempt by typing ^C.
     Wait until the call to target_has_fp () completes before
     disallowing an immediate quit, since even if net_connect ()
     is successful, the remote debug server might be hung.  */
  
  immediate_quit++;
  
  curr_server = (pid == TTD_KERNEL_MID) ? KERNEL_TTD : USER_TTD;
  rc = ttd_bind(ttd_host, 0, curr_server, &connection);
  test_rc (rc, "Bind");
  
  rc = ttd_probe_server(connection, &machine);
  test_rc (rc, "ProbeServer");
  
  printf_filtered ("Attached to %s, ", ttd_host);
  switch (machine) 
    {
      case TTD_AT386:
        printf_filtered ("an AT386\n");     break;
      case TTD_MIPS:
	printf_filtered ("a MIPS\n");        break;
      case TTD_ALPHA:
	printf_filtered ("an ALPHA\n");        break;
      default:
	printf_filtered ("an unrecognized machine type(%d)\n",
			 machine);
    }
  
  immediate_quit--;
  rc = ttd_connect_to_target(connection, pid, TRUE /*FALSE*/, &curr_target, &info);
  if ((rc == TTD_TargetNotAvailableRC) 
      && query ("already in use: force connection? "))
    rc = ttd_connect_to_target(connection, pid, TRUE,
			       &curr_target, &info);
  test_rc (rc, "ConnectToTarget");
  
  printf_filtered ("Connected to %s\n", ttd_mid);
  inferior_pid = pid;
  
  /*
   * Push our target onto the target stack.
     This must happend before we try to HALT or we wind up popping a
     next existent target and gdb seg faults.  DB
   */
  push_target (&ttd_ops);
  

  if (need_halt)  {
	  curr_target = TTD_KERNEL_MID;
	  disconnect(DISCONNECT_HALT);
	  printf("Target terminated (I think....)\n");
	  return;
  }
  
  
  while (!info.is_stopped) 
    {
	  rc = ttd_stop_target (connection, curr_target);
	  test_rc (rc, "StopTarget");
	  rc = ttd_probe_target (connection, curr_target, &info);
	  test_rc (rc, "ProbeTarget");
    }
  /*
   * Get the state of the current thread.
   */
  curr_thread = TTD_NO_THREAD;
  set_current_thread(info);
  
  rc = ttd_get_next_breakpoint(connection, curr_target,
			       0, &bp, &flavor, &saved_inst);
  test_rc( rc, "GetNextBreakpoint" );
  
  if (bp != 0)
    remove_bp = query ("target has existing breakpoints.  Remove them? ");
  
  /*
   * get all the breakpoints and insert them into our
   * breakpoint list.
   */
  while(bp != 0) 
    {
      if (remove_bp) 
	{
	  rc = ttd_clear_breakpoint(connection, curr_target, bp );
	  
	  test_rc( rc, "ClearBreakpoint" );
	}
      else
	{
#if	0
	  /* Find symbols and lineno for bp */
	  sal = find_pc_line(bp, 0);

#endif	0			
	}

      rc = ttd_get_next_breakpoint(connection, curr_target, bp,
				   &next_bp, &flavor, &saved_inst);
      test_rc( rc, "GetNextBreakpoint" );

      bp = next_bp;
    }
	
  if (info.debug_reason.length != 0)
    {
      printf_filtered ("\"");
      for (i = 0; i < info.debug_reason.length; i++)
	printf_filtered ("%c", info.debug_reason.chars[i]);
      printf_filtered ("\"\n");
    }
	

  {
  extern struct language_defn *expected_language;
  expected_language=0;
  printf_filtered("\n");
  language_info (1);
  printf_filtered("\n");
  }
  start_remote ();
}

/* terminates the debugging session.  */

static void disconnect(mode)
	ttd_disconnect_mode mode;
{
  int rc;

  if (mode == DISCONNECT_CONT) {
      rc = remove_breakpoints ();
      test_rc (rc, "remove_breakpoints");
      printf_filtered("breakpoints removed, ");
      rc = ttd_restart_target (connection, curr_target);
      test_rc (rc, "RestartTarget");
      printf_filtered("target continued.\n");
    }
  rc = ttd_disconnect_from_target(connection, curr_target, mode);
  printf_filtered("dettached from %s\n", ttd_host);
  
  if (rc < TTD_SuccessRC) 
    {
      printf_filtered("Couldn't disconnect, but deleting ttd target.\n");
    }
  
#if	DEC_CODE
  test_rc (rc, "DisconnectFromTarget");
#endif	/* DEC_CODE */
  
  inferior_pid = 0;
  curr_target = 0;
  curr_thread = 0;
  pop_target ();
}

/* detach_command --
   takes a program previously attached to and detaches it.  */

void ttd_detach(char *args, int from_tty)
{
  int rc;
  int argc;
  char **argv;
  ttd_disconnect_mode mode = DISCONNECT_CONT;

  parse_args(args, &argc, &argv);

  if (argc == 1)  {
	  if (!strcmp(argv[0], "halt") || !strcmp(argv[0], "HALT"))
		  mode = DISCONNECT_HALT;
	  if (!strcmp(argv[0], "nocont") )
		  mode = DISCONNECT_NOCONT;
  }
  
  disconnect(mode);
}

/* kill_command -- takes a running task and wipes it out.  */

static void ttd_kill(void)
{
  /* TTD doesn't support killing the target, but this is the only
     procedure called when gdb quits.  */
  disconnect(DISCONNECT_CONT);
}

/* Clean up before losing control.  (Called when a target is popped.)  */

void ttd_close(int quitting)
{
  inferior_pid = 0;
  curr_target = 0;
  curr_thread = 0;
  
  if (connection != NULL)
    ttd_free(connection);		/* The net connection */
  connection = NULL;
  
  if (ttd_host != NULL)
    free(ttd_host);		/* The hostname */
  ttd_host = NULL;
  
  if (ttd_mid != NULL)
    free(ttd_mid);		/* The target pid */
  ttd_mid = NULL;
}

/* Read a register or registers from the remote system.  */

void ttd_read_register(int regno)
{
  int rc;

  curr_thread = inferior_pid;
  rc = ttd_get_thread_info(connection, curr_target,
			   curr_thread, &curr_thread_info,
			   &curr_trap_info,
			   THREAD_STATE_KERNEL, &curr_state);
  test_rc(rc, "GetThreadInfo");
  
  TTD_FETCH_STATE_TO_REGISTERS(&curr_state);
  registers_fetched();
}

/*
 * Fetch over the addresses of the thread's user space
 * registers. We do nothing with the extra information coming
 * back
 */
void
ttd_get_user_register_addresses(CORE_ADDR *regaddrs)

{
  int rc;
  ttd_machine_state *ttd_state = (ttd_machine_state*)regaddrs;
  ttd_trap_info trap_info;
  ttd_thread_info thread_info;
  

  curr_thread = inferior_pid;
  rc = ttd_get_thread_info(connection, curr_target,
			   curr_thread, &thread_info,
			   &trap_info,
			   THREAD_STATE_USER, ttd_state);
  test_rc(rc, "get_user_register_addresses GetThreadInfo");
}
  
	

/* Prepare to store registers.  Since we will store all of them,
   read out their current values now.  */

void ttd_prepare_to_store(void)
{
  ttd_read_register(-1);
}


/* Store our register values back into the target.
   If REGNO is -1, do this for all registers.
   Otherwise, REGNO specifies which register (so we can save time).  */

void ttd_write_register(int regno)
{
	int rc;
	ttd_machine_state state;

	curr_thread = inferior_pid;
	rc = ttd_get_thread_info(connection, curr_target, curr_thread,
				 &curr_thread_info,
				 &curr_trap_info,
				 THREAD_STATE_KERNEL,
				 &curr_state);
	test_rc (rc, "GetThreadInfo in ttd_write_register");
	if (rc < TTD_SuccessRC) return;
 
	TTD_STORE_REGISTERS_TO_STATE(&curr_state);
	rc = ttd_set_thread_info(connection, curr_target,
				 curr_thread,curr_trap_info,
				 curr_state);
	test_rc (rc, "SetThreadInfo");
}

/* Copy LEN bytes between target's memory starting at MEMADDR and debugger
   memory starting at MYADDR.  WRITE is true if writing to the target.
   Result is the number of bytes written or read (zero if error).  The
   protocol allows us to return a negative count, indicating that we can't
   handle the current address but can handle one N bytes further, but
   the TTD protocol doesn't currently provide that information.  */

int ttd_xfer_memory(CORE_ADDR memaddr,
		    char *myaddr,
		    int len,
		    int write,
		    struct target_ops *target)		/* ignored */
{
  int rc;
  int read_len;
  char * cache_ptr;

  len = min (len, TTD_MAX_BLOCK_SIZE);
  if (len > TTD_MAX_BLOCK_SIZE) 
    {
      printf("ttd_xfer_memory: len > TTD_MAX_BLOCK_SIZE, FIX!!!\n");
    }
  
  if (write) 
    {
      rc = ttd_write_into_target(connection, curr_target,
				 memaddr, myaddr, 0, len);
      if (rc < TTD_SuccessRC) 
	{
	  printf("gdb: write error rc = %d\n",rc);

	  if (ttd_cache->enabled)
	    {
	      gdb_cache_invalidate(ttd_cache, memaddr, len);
	    }
	  else
	    {
	      ttd_cache->valid = FALSE;
	    }
	  return 0;
	}

      if (ttd_cache->enabled)
	{
	  if (ttd_cache->valid == FALSE)
	    gdb_cache_clean(ttd_cache);

	  gdb_cache_write(ttd_cache, memaddr, myaddr, len);
	}

    }
  else	/* A read! */
    {
      if (ttd_cache->enabled)
	{
	  if (ttd_cache->valid == FALSE)
	    gdb_cache_clean(ttd_cache);
	      
	  if (gdb_cache_read(ttd_cache, memaddr, myaddr, len,
			     &cache_ptr, &read_len))
	    {
	      if (ttd_debug_enabled)
		printf("xfer: cache hit!\n");

	      return len;
	    }
	  else
	    {
	      if (ttd_debug_enabled)
		printf("xfer: cache miss.\n");
	    }

	  rc = ttd_read_from_target(connection, curr_target,
				    memaddr, cache_ptr, 0, read_len);

	  if (rc < TTD_SuccessRC)
	    {
	      printf("gdb: cache fill error rc = %d\n",rc);

	      if (ttd_cache->enabled)
		{
		  gdb_cache_invalidate(ttd_cache, memaddr, read_len);
		}
	      else
		{
		  ttd_cache->valid = FALSE;
		}
	      return 0;
	    }

	  gdb_cache_supply(ttd_cache, cache_ptr, myaddr, len);
	}
      else
	{
	  if (ttd_debug_enabled)
	    printf("xfer: read.\n");

	  rc = ttd_read_from_target(connection, curr_target,
				    memaddr, myaddr, 0, len);
	  if (rc < TTD_SuccessRC) 
	    {
	      printf("gdb: read error rc = %d\n",rc);
	      return 0;
	    }
	}
    }

  return len;
}

static void ttd_files_info(ignored)
     struct target_ops * ignored;
{
  printf_filtered("\tAttached to %s on host `%s'.\n", ttd_mid, ttd_host);
}

void ttd_mourn_inferior(void)
{
  pop_target();		/* Pop back to no-child state */
  generic_mourn_inferior();
}


/*
 * Return 0 if target pid is still there. Could run.
 * -1 if not
 */
int
ttd_notice_signals(int pid)
{
  int rc;
  ttd_machine_state state;
  ttd_trap_info trap_info;
	
  rc = ttd_get_thread_info(connection, curr_target, pid,
			   NULL, &trap_info,
			   THREAD_STATE_KERNEL, &state);
  if (ttd_debug_enabled)
	  printf("ttd_notice_signals: get thread info for %d returns %d\n",
		 pid, rc);
  if (rc == TTD_SuccessRC)
	  return 0;
  else return -1;
}

	
void ttd_resume(int pid, int step, int siggnal)
{
  int rc;
  
  if (siggnal != 0 && siggnal != stop_signal)
    error ("Cannot send signals to TTD processes");
  
  if (step) 
    {
      rc = ttd_single_step_thread (connection, curr_target, curr_thread);
      test_rc (rc, "SingleStepThread");
    }
  else
    {
      rc = ttd_restart_target (connection, curr_target);
      test_rc (rc, "RestartTarget");
    }

  ttd_cache->valid = FALSE;
}

/* returns 0 if successful, errno otherwise */

int ttd_insert_breakpoint(CORE_ADDR addr, char *save)
{
  return ttd_set_breakpoint (connection, curr_target,
			     curr_thread,
			     addr, 8, (void *) save);
}

/* returns 0 if successful, errno otherwise */

int ttd_remove_breakpoint(CORE_ADDR addr, char *save)
{
  return ttd_clear_breakpoint (connection, curr_target, curr_thread, addr);
}

/* Returns 0 if invalid frame, one otherwise */
int ttd_valid_frame(CORE_ADDR addr, FRAME next_frame)
{
  /* XXX This only works for the i386!!! XXX */
  if ((addr < next_frame->frame + 8) ||
      (next_frame->frame < 0xc0000000))
    return 0;
  else
    return 1;
}


char *
ttd_pid_to_str(int pid)
{
	static char buf[80];
	ttd_machine_state state;
	ttd_trap_info trap_info;	
	ttd_thread_info thread_info;
	int rc;

	rc = ttd_get_thread_info(connection, curr_target, pid,
				 &thread_info, &trap_info,
				 THREAD_STATE_KERNEL, &state);
	if (rc == TTD_SuccessRC)  {
		sprintf(buf,"(0x%lx 0x%lx) [%s]",
			thread_info.thread_address,
			thread_info.task_address,
			thread_info.thread_state_msg);
		return buf;
	} else {
		return "--??--";
	}
}

#ifdef notdef

/* Change the current thread to ARG.  */

void set_thread_command (char *arg)
{
  int rc;
  int thread;
  ttd_thread original;
  ttd_machine_state state;
  
  if (!arg) 
    {
      /* threadstat (); */
      error ("display of threads not yet implemented");
      return;
    }
  
  original = curr_thread;
  curr_thread = parse_and_eval_address (arg);
  rc = get_current_thread_info ();
  if (rc < TTD_SuccessRC) 
    {
      curr_thread = original;
      test_rc( get_current_thread_info(), "GetThreadInfo" );
      test_rc( rc, "GetThreadInfo" );
    }
  
  stop_pc = read_pc ();
  flush_cached_frames ();
  set_current_frame (create_new_frame (read_register (FP_REGNUM),
				       read_pc ()));
  select_frame (get_current_frame (), 0);
  print_stack_frame (selected_frame, selected_frame_level, -1);
}
#endif notdef


static int
get_all_task_info()
{
	ttd_thread thread;
	ttd_machine_state state;
	ttd_trap_info trap_info;
	int rc;
	extern void info_threads_command();

	thread = TTD_NO_THREAD;

	for (;;)  {
		rc = ttd_get_next_thread(connection, curr_target, thread, &thread);
		if (rc != TTD_SuccessRC)
			break;
		if (ttd_debug_enabled)
			printf("ttd_get_next_thread = %d\n", thread);	    
		if (thread == TTD_NO_THREAD) /* end of list */
			break;
		if (!in_thread_list(thread))  {
			rc = ttd_get_thread_info(connection, curr_target, thread, NULL,
						 &trap_info,
						 THREAD_STATE_KERNEL,
						 &state);
			if (rc != TTD_SuccessRC)  
				break;
			add_thread(thread);
		}
	}
}
	
static void
thread_sweep_command (args, from_tty)
     char *args;
     int from_tty;
{
  if (!ttd_is_connected()) {
       error("No ttd target.  Must attach first.");
       return;
  }    
  get_all_task_info();
  info_threads_command(args, from_tty);
}


/*
 *  XX Hack to be able to parse interface recs during sweeping
 */

static int ttd_domain_sweeping = 0;

int
ttd_in_get_all_domain_info()
{
    return ttd_domain_sweeping;
}



/*
 * Query the target for all active domains.  Target is assumed to be connected.
 */

int 
ttd_get_all_domain_info()	
{
	ttd_domain_info info;
	int rc;
	char *name = NULL;
	CORE_ADDR text_addr;
	int count = 0;


	for (;;)  {
		rc = ttd_get_next_domain(connection, curr_target, &info);
			if (rc != TTD_SuccessRC)
			break;
		if (!info.name.length) /* end of list */
			break;
     	        count++;
		if (info.loadaddr) { /* some domains say they start at 0 */
			if (ttd_debug_enabled)
				printf_filtered ("%#18lx %s\n", info.loadaddr,
					info.name.chars);

			ttd_domain_sweeping = 1;
			symbol_file_add(info.name.chars,0,info.loadaddr,0,0,0);
			ttd_domain_sweeping = 0;
		} else {
			printf_filtered ("%s has no text address!\n", 
				info.name.chars);
     	        }
	}
	return count;
}
	


struct type *
ttd_get_target_type(char *name)
{
	return lookup_typename(name, 0, 1);  /* no fail on error */
}


int
ttd_is_connected()
{
    return connection != 0;
}

    
	
void
ttd_set_internal_ktask(natural_t value)
{
	static struct type *task_type = 0;
	struct type* tt = task_type;
	

	if (!task_type)  {
		tt = ttd_get_target_type("Translation.T");
		if (tt)
			task_type = tt;
		else
			tt = builtin_type_unsigned_int;
	}
	set_internalvar(lookup_internalvar("Translation.T"),
			value_from_longest(tt, (LONGEST)value));
}

void
ttd_set_internal_kthread(natural_t value)
{
	static struct type *thread_type = 0;
	struct type *tt = thread_type;

	if (!thread_type)  {
		tt = ttd_get_target_type("Strand.T");		
		if (tt)
			thread_type = tt;
		else
			tt = builtin_type_unsigned_int;
	}	
	set_internalvar(lookup_internalvar("kstrand"),
			value_from_longest(tt, (LONGEST)value));
}

void
ttd_set_internal_thread(ttd_thread thread)
{
	set_internalvar(lookup_internalvar("thread"),
			value_from_longest(
				 builtin_type_unsigned_int,
					   (LONGEST)thread));
}



/* Target ops structure for accessing processes over the net via TTD. */

struct target_ops ttd_ops = {
	"ttd",			/* to_shortname */
	"Remote network target via Topaz TeleDebug UDP protocol",	/* to_longname */
	"TeleDebug a process using the TTD protocol.\n\
Specify the process ID and hostname.",	/* to_doc */
		ttd_open,		/* to_open */
		ttd_close,		/* to_close */
		NULL,			/* to_attach */
		ttd_detach,		/* to_detach */
		ttd_resume,		/* to_resume */
		ttd_wait,		/* to_wait */
		ttd_read_register,	/* to_fetch_registers */
		ttd_write_register,	/* to_store_registers */
		ttd_prepare_to_store,	/* to_prepare_to_store */
		ttd_xfer_memory,	/* to_xfer_memory */
		ttd_files_info,		/* to_file_info */
		ttd_insert_breakpoint,	/* to_insert_breakpoint */
		ttd_remove_breakpoint,	/* to_remove_breakpoint */
		NULL,			/* to_terminal_init */
		NULL,			/* to_terminal_inferior */
		NULL,			/* to_terminal_ours_for_output */
		NULL,			/* to_terminal_ours */
		NULL,			/* to_terminal_info */
		ttd_kill,		/* to_kill */
		NULL,			/* to_load */
		NULL,			/* to_lookup_symbol */
		NULL,			/* to_create_inferior */
		ttd_mourn_inferior, 	/* to_mourn_inferior */
		NULL,			/* to_can_run */
		ttd_notice_signals,	/* to_notice_signals */
		ttd_pid_to_str,		/* to_pid_to_str    */
#if	0
		ttd_valid_frame,	/* to_valid_frame */
#endif	0
		process_stratum,	/* to_stratum */
		NULL,			/* next */
		1,			/* to_has_all_memory */
		1,			/* to_has_memory */
		1,			/* to_has_stack */
		1,			/* to_has_registers */
		1,			/* to_has_execution */
		NULL,			/* sections */
		NULL,			/* sections_end */
		OPS_MAGIC,		/* to_magic */
	};

void
_initialize_ttd()		/* start in column 0 */
{
	extern struct cmd_list_element *thread_cmd_list;
	extern int ttd_retry_count, ttd_retry_timeout;

	if (!thread_cmd_list)
		_initialize_thread(); /* ugh dependency */
	

	add_cmd ("sweep", class_run, thread_sweep_command,
		 "Sweep the target's threads collecting all thread information.",
		 &thread_cmd_list);
  
	add_show_from_set
		(add_set_cmd ("ttd-debug", class_support, var_boolean,
			      (char *)&ttd_debug_enabled,
			      "Set the TTD debugging toggle.", &setlist),
		 &showlist);

	add_show_from_set
		(add_set_cmd ("ttd-retry-count", class_support, var_zinteger,
			      (char *)&ttd_retry_count,
			      "Set the TTD rpc retry count.", &setlist),
		 &showlist);

	add_show_from_set
		(add_set_cmd ("ttd-retry-timeout", class_support, var_zinteger,
			      (char *)&ttd_retry_timeout,
			      "Set the TTD rpc retry timeout.", &setlist),
		 &showlist);

	add_target (&ttd_ops);

	ttd_cache = gdb_cache_create("ttd", 1024);
}
