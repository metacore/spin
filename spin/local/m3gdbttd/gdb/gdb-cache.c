/* 
 * Mach Operating System
 * Copyright (c) 1993 Carnegie Mellon University
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
/*
 * Cache code for gdb targets.
 *
 * HISTORY
 * 28-Jun-96  Brian Bershad (bershad) at the University of Washington
 *	Fixed wrong printf format in gdb_cache_write.
 */


/*
 * $Log: gdb-cache.c,v $
 * Revision 1.2  1996/06/29 00:11:41  whsieh
 * *** empty log message ***
 *
 * Revision 1.1  1995/07/06  01:05:02  bershad
 * New File
 *
 * Revision 1.1  1995/07/06  00:21:56  bershad
 * *** empty log message ***
 *
 * Revision 2.1.2.1  93/08/06  23:37:55  mrt
 * 	Created.
 * 	[93/08/06  12:34:01  grm]
 * 
 */

/*
 * Author: Gerald Malan <grm@cs.cmu.edu>
 *	   School of Computer Science
 *	   Carnegie Mellon University
 *	   Pittsburgh, PA, USA
 *
 */

/*
 * A very simpleminded cache for people to help speed up their gdb
 * targets.  Initially written in a ad hoc way to speed up ttd.  Feel
 * free to add a hash table front end, etc.  Originally used to speed up
 * the Mach Teledebugger which does remote debugging over the ethernet,
 * so the cache is cleaned out frequently, so a faster search method than
 * the linear search wasn't really needed.  This code would also help the
 * remote serial line targets too.
 *
 * The seven external routines are used as follows:
 *
 * gdb_cache_create:  This function returns a pointer to a newly malloc'd
 *                    cache structure.  It initializes the cache parameters
 *		      to the default values.
 *
 * gdb_cache_destroy: This procedure takes a pointer to a cache and
 *		      frees all of its memory objects and then frees
 *		      the memory allocated for the cache structure itself.
 *
 * gdb_cache_clean:   Frees all of the memory objects in the parameter cache.
 *
 * gdb_cache_read:    This function is used to access the cache contents.
 *		      It works by finding the memory objects with either the
 *		      whole request, or parts of it.  When it finds all of
 *		      the request, it copies the whole request from the cache
 *		      into the gdb_addr buffer.  When it finds parts of it, it
 *		      copies what it can from the cache, consolidates the
 *		      cache memory objects around the request into a single
 *		      memory object, and then sets the cache_ptr and read_len
 *		      so that the a read of read_len bytes into cache_ptr will
 *		      fill in the holes in the newly consolidated memory
 *		      object.  This should be done in the calling routine by
 *		      doing a read_from_target using cache_ptr and read_len
 * 		      instead of myaddr and length.  Once you have filled in
 * 		      the memory object via a real read, you must then supply
 *		      the data to the myaddr buffer.  Basically you can do a
 *		      bcopy, but I supplied a macro called gdb_cache_supply.
 *
 *		      I realize that these semantics are disgusting, but I
 *		      didn't want to rewrite the code and then test every
 *		      possible cache construct just to make it more elegant.
 *		      It works, and it's not too yucky.
 *
 *		      The IN parameters are:
 *			 + cache:	 a cache to access
 *			 + target_start: the CORE_ADDR you are trying to find
 *			 + gdb_addr:	 the internal gdb buffer into which
 *					 you should place the data
 *			 + length:	 the amount of data in the request
 *
 *		      The OUT parameters are:
 *			 + cache_ptr:	 the address in the cache, that you
 *					 use with gdb_cache_supply
 *			 + read_len:	 the amount of data needed from the
 *					 real CORE_ADDR to fill the
 *					 memory object.
 *
 * gdb_cache_write:   Works like the read routine, but you don't need to
 *		      supply anything afterwards.
 *
 * gdb_cache_supply:  Explained in the cache_read description above.
 *
 * gdb_cache_invalidate:  Used to invalidate a part of the cache starting
 *			  at the parameter start until start plus the length
 *			  parameter.
 *
 * I suggest you centralize the use of the cache code.  The following
 * skeleton should be enough:
 *
 * int
 * target_xfer_memory(CORE_ADDR memaddr,
 *		      char * myaddr,
 *		      int len,
 *		      int write)
 * {
 *   if (write)
 *     {
 *	 ret = write_into_target(memaddr, myaddr, len);
 *	 if (ret == ERROR)
 *	   {
 *	     if (cache->enabled)
 *	       gdb_cache_invalidate(cache, memaddr, len);
 *	     else
 *	       cache->valid = FALSE;
 * 	     return 0;
 *	   }
 *       if (cache->enabled)
 *         {
 *           if (cache->valid == FALSE)
 *             gdb_cache_clean(cache);
 *           gdb_cache_write(cache, memaddr, myaddr, len);
 *         }
 *     }
 *   else ;;; A Read!
 *     {
 *       if (cache->enabled)
 *         {
 *           if (cache->valid == FALSE)
 *             gdb_cache_clean(cache);
 *           if (gdb_cache_read(cache, memaddr,myaddr, len,
 *                              &cache_ptr, &read_len))
 *           ;;; Cache hit! 
 *             return len;
 *           else
 *           ;;; Cache Miss! cache_ptr and read_len are set up for us.
 *           ret = read_from_target(memaddr, cache_ptr, read_len);
 *           if (ret == ERROR)
 *             {
 *               gdb_cache_invalidate(memaddr, read_len);
 *	         return 0;
 *             }
 *           gdb_cache_supply(cache, cache_ptr, myaddr, len);
 *         }
 *       else
 *         {
 *           ret = read_from_target(memaddr, myaddr, len);
 *           if (ret == ERROR)
 *             return 0;
 *         }
 *      }
 *   return len;
 * }
 *
 * For another example of its use see the ttd_xfer_memory procedure in the
 * file remote-ttd.c.
 *
 * Happy trails (and faster debugging!).... -grm
 */ 

#include "defs.h"
#include "gdbcmd.h"
#include "gdb-cache.h"


/* Prototypes for local functions */

static void parse_args PARAMS ((char *, int *, char ***));

/* Size of cache's smallest memory objects */
int default_cache_granularity = 256;

/* List of caches in system: */
struct cache_list_entry  * cache_list_head = NULL;
struct cache_list_entry  * cache_list_tail = NULL;

/* Create a cache and initialize its parameters. */
struct memory_cache *
gdb_cache_create(char * name, int max_granularity)
{
  struct memory_cache * cache;
  struct cache_list_entry * cache_entry;

  if (strlen(name) < 1)
    {
      printf("Invalid cachename length.  Must have a name for creation.\n");
      return INVALID_CACHE;
    }

  if (max_granularity < 1)
    {
      printf("Invalid cache max granularity.  Must have maximum of positive value.\n");
      return INVALID_CACHE;
    }

  cache = (struct memory_cache *) xmalloc (sizeof(struct memory_cache));
  cache->valid = FALSE;
  cache->enabled = TRUE;
  cache->max_granularity = max_granularity;
  cache->granularity = (DEFAULT_GRANULARITY > max_granularity ?
			max_granularity : DEFAULT_GRANULARITY);
  cache->head = NULL;
  cache->tail = NULL;
  cache->name = name;

  cache_entry = (cache_list_entry_t) xmalloc(sizeof(struct cache_list_entry));
  cache_entry->cache = cache;
  cache_entry->prev = cache_list_tail;
  cache_entry->next = NULL;

  if (cache_list_head == NULL)
    cache_list_head = cache_entry;
  else
    cache_list_tail->next = cache_entry;

  cache_list_tail = cache_entry;

  return (cache);
}

/* Get rid of a cache */
void
gdb_cache_destroy(struct memory_cache * cache)
{
  struct cache_list_entry * ptr;

  for(ptr = cache_list_head; ptr; ptr = ptr->next)
    {
      if (ptr->cache == cache)
	{
	  if (ptr->prev)
	    ptr->prev->next = ptr->next;
	  if (ptr->next)
	    ptr->next->prev = ptr->prev;
	  free (ptr);
	  break;
	}
    }

  gdb_cache_clean(cache);
  free (cache);
}

/* Clean out the cache, and throw away all the old memory objects.
   Reinitialize the valid bit to be TRUE. */
void
gdb_cache_clean(struct memory_cache * cache)
{
  struct memory_object * next;
  struct memory_object * tmp;

  next = cache->head;
  while(next != NULL)
    {
      tmp = next->next;
      free (next->bytes);
      free (next);
      next = tmp;
    }

  cache->head = cache->tail = NULL;

  cache->valid = TRUE;
}

/*  Write bytes into cache.  If the write is big, keep 'em, else ignore 'em */
void
gdb_cache_write(struct memory_cache * cache,
		CORE_ADDR target_start,
		vm_address_t gdb_addr,
		vm_size_t length)
{
  struct memory_object * begin = NULL;
  struct memory_object * obj = NULL;
  struct memory_object * in_end = NULL;
  struct memory_object * beyond_end = NULL;
  struct memory_object * tmp;
  struct memory_object * tmp2;
  vm_address_t  new;
  vm_address_t  ptr;
  vm_offset_t diff_end;
  vm_offset_t diff_start;
  vm_address_t dest;
  vm_address_t src;
  vm_size_t size_of_new_bytes;
  CORE_ADDR target_end;

  if (length < 1)
    {
      printf("cache_write: invalid %s cache write length < 1!\n", cache->name);
      return;
    }

  target_end = target_start + length - 1;

  size_of_new_bytes = 0;

  tmp = cache->head;

  while (tmp != NULL)
    {
      if (begin == NULL)
	{
	  if (tmp->start > target_start)
	    {
	      /* Don't put dinky little writes into the cache! */
	      if (length < cache->granularity)
		begin = INVALID_MEMORY_OBJECT;
	      else
		{
		  obj = (struct memory_object *)
		    xmalloc(sizeof(struct memory_object));

		  /* Leave ->end, ->len ->next and ->bytes empty for now... */
		  obj->prev = tmp->prev;
		  obj->start = target_start;

		  if (obj->prev)
		    obj->prev->next = obj;
		  else
		    cache->head = obj;

		  begin = obj;
		}
		 
	      if (target_end < tmp->start)
		{
		  beyond_end = tmp;
		  break;
		}

	      if (target_end <= tmp->end)
		{
		  in_end = tmp;
		  break;
		}

	      tmp = tmp->next;
	      continue;
	    }
	  else if (tmp->end < target_start)
	    {
	      /* We aren't in this object if the end is before our start */
	      tmp = tmp->next;
	      continue;
	    }
	  else /* Aha!  Our start is in this object */
	    {
	      begin = tmp;

	      /* If target is in this single object, just bcopy it into
		 the cache! */
	      if (target_end <= tmp->end)
		{
		  diff_end = (vm_offset_t) target_start - (vm_offset_t) tmp->start;
		  ptr = (vm_address_t)((vm_offset_t) tmp->bytes + diff_end);
		  bcopy (gdb_addr, ptr, length);
		  return;
		}

	      tmp = tmp->next;
	      continue;
	    }
	}
      else	/* We've found the beginning, but not the end. */
	{
	  if (target_end < tmp->start)
	    {
	      beyond_end = tmp;
	      break;
	    }

	  if (target_end > tmp->end)
	    {
	      /* We know that this object will be subsumed by the
		 impending xfer, so we may as well, just destroy it,
		 we get it's contents back for free. */

	      if (cache->tail == tmp)
		{
		  if (begin)
		    if (begin != INVALID_MEMORY_OBJECT)
		      cache->tail = begin;
		    else
		      {
			obj = (struct memory_object *)
			  xmalloc(sizeof(struct memory_object));
			begin = obj;
			
			cache->tail = obj;
			obj->next = NULL;
			obj->prev = tmp->prev;
			obj->start = target_start;
			if (obj->prev)
			  obj->prev->next = obj;
			else
			  cache->head = obj;
		      }
		  else
		    cache->tail = obj;

		  /* Sanity Check */
		  if (cache->tail == NULL)
		    printf("Insanity checks: rcache_write, cache->tail NULL\n");
		}

	      tmp2 = tmp->next;

	      free (tmp->bytes);
	      free (tmp);

	      tmp = tmp2;

	      continue;
	    }

	  /* We know that the end of the read is in this object,
	     so we keep whatever's after the end and add it to the
	     size */

	  in_end = tmp;
	  break;
	}

      /* Should never get here! */
      printf("write_cache: shouldn't get here!\n");
    }

  /* Just ignore it, it it's too small and doesn't hit any cache objects */
  if (begin == INVALID_MEMORY_OBJECT)
    if (in_end == NULL)
      return;
    else
      {
	obj = (struct memory_object *)xmalloc(sizeof(struct memory_object));

	begin = obj;
	obj->prev = in_end->prev;
	obj->next = in_end->next;
	obj->start = target_start;
	if (obj->prev)
	  obj->prev->next = obj;
	else
	  cache->head = obj;

	if (obj->next)
	  obj->next->prev = obj;
	else
	  cache->tail = obj;
      }

  if (!cache->head)
    {
      if (length < cache->granularity)
	return;
      cache->head = (struct memory_object *)
	xmalloc(sizeof(struct memory_object));
      cache->tail = cache->head;
      cache->head->next = NULL;
      cache->head->prev = NULL;
      cache->head->len = length;
      cache->head->start = target_start;
      cache->head->end = target_end;
      cache->head->bytes = (vm_address_t)xmalloc(length);
      bcopy(gdb_addr, cache->head->bytes, length);
      return;
    }

  if (!begin)
    {
      if (length < cache->granularity)
	return;
      obj = (struct memory_object *)xmalloc(sizeof(struct memory_object));
      obj->next = NULL;
      obj->prev = cache->tail;
      cache->tail->next = obj;
      cache->tail = obj;

      obj->len = length;
      obj->start = target_start;
      obj->end = target_end;
      obj->bytes = (vm_address_t)xmalloc(length);
      bcopy(gdb_addr, obj->bytes, length);
      return;
  }

  if (obj)
    {
      if (beyond_end != NULL)
	{
	  size_of_new_bytes = length;
	  obj->next = beyond_end;
	}
      else if (in_end != NULL)
	{
	  diff_end = in_end->end - target_end;
	  size_of_new_bytes = length + diff_end;
	  obj->next = in_end->next;
	}
      else
	{
	  size_of_new_bytes = length;
	  obj->next = NULL;
	}

      if (obj->next)
	obj->next->prev = obj;
      else
	cache->tail = obj;

      obj->len = size_of_new_bytes;
      obj->end = obj->start + size_of_new_bytes -1;

      new = (vm_address_t)xmalloc (size_of_new_bytes);
      obj->bytes = new;

      if (in_end)
	{
	  if (diff_end > 0)
	    {
	      dest = (vm_address_t)((vm_offset_t)new +
				       size_of_new_bytes - diff_end);
	      src = (vm_address_t)((vm_offset_t)in_end->bytes + 
				      in_end->len - diff_end);
	      bcopy (src, dest, diff_end);
	    }

	  free (in_end->bytes);
	  free (in_end);
	}

      bcopy(gdb_addr, obj->bytes, length);
    }
  else	/* The beginning is in an old object */
    {
      obj = (struct memory_object *)xmalloc(sizeof(struct memory_object));

      diff_start = target_start - begin->start;

      if (beyond_end != NULL)
	{
	  diff_end = 0;
	  size_of_new_bytes = target_end - begin->start + 1;
	  obj->end = target_end;
	  obj->next = beyond_end;
	}
      else if (in_end != NULL)
	{
	  diff_end = in_end->end - target_end;
	  size_of_new_bytes = in_end->end - begin->start + 1;
	  obj->end = in_end->end;
	  obj->next = in_end->next;
	}
      else
	{
	  diff_end = 0;
	  size_of_new_bytes = target_end - begin->start + 1;
	  obj->end = target_end;
	  obj->next = NULL;
	}

      if (obj->next)
	obj->next->prev = obj;
      else
	cache->tail = obj;

      if (begin->prev)
	{
	  obj->prev = begin->prev;
	  obj->prev->next = obj;
	}
      else
	{
	  cache->head = obj;
	  obj->prev = NULL;
	}

      obj->len = size_of_new_bytes;
      obj->start = begin->start;

      new = (vm_address_t)xmalloc (size_of_new_bytes);
      obj->bytes = new;

      if (diff_start > 0)
	{
	  dest = new;
	  src = begin->bytes;
	  bcopy (src, dest, diff_start);
	}

      /* There MUST be a begin or else we'd be in the THEN clause! */
      free (begin->bytes);
      free (begin);

      if (in_end)
	{
	  if (diff_end > 0)
	    {
	      dest = (vm_address_t)((vm_offset_t)new + 
				       size_of_new_bytes - diff_end);
	      src = (vm_address_t)((vm_offset_t)in_end->bytes +
				      in_end->len - diff_end);
	      bcopy (src, dest, diff_end);
	    }

	  free (in_end->bytes);
	  free (in_end);
	}

      bcopy(gdb_addr,  obj->bytes + diff_start, length);
    }
}

/* Read bytes out of the cache if they are there, if not, allocate
   a space for them to go, rearranging if necessary, and return
   the place to write them. */
int
gdb_cache_read(struct memory_cache * cache,
	       CORE_ADDR target_start,
	       vm_address_t gdb_addr,
	       vm_size_t length,
	       char ** cache_ptr,
	       int * read_len)
{
  struct memory_object * begin = NULL;
  struct memory_object * obj = NULL;
  struct memory_object * in_end = NULL;
  struct memory_object * beyond_end = NULL;
  struct memory_object * tmp;
  struct memory_object * tmp2;
  vm_address_t new;
  vm_address_t ptr;
  vm_offset_t diff_end;
  vm_offset_t diff_start;
  vm_address_t dest;
  vm_address_t src;
  vm_size_t size_of_new_bytes;
  CORE_ADDR max_end;
  CORE_ADDR target_end;

  if (length < 1)
    {
      printf("cache_read: invalid %s cache read length < 1!\n", cache->name);
      return FALSE;
    }

  /* We are guaranteed that target_end >= target_start, since length >= 1 */
  target_end = target_start + length - 1;

  /* Max_end > target_start since length >= 1 */
  max_end = ((length > cache->granularity) ?
	     target_end :
	     target_start + cache->granularity - 1);

  size_of_new_bytes = 0;

  tmp = cache->head;

  while (tmp != NULL)
    {
      /* Have we found the mem object with the start address yet? */
      if (begin == NULL)
	{
	  if (tmp->start > target_start)
	    {
	      /* This is the case where either the cache is empty and
		 this is the first read to fill the cache, or the target
		 address is lower than any cache entry. */
		 
	      obj = (struct memory_object *)
		      xmalloc(sizeof(struct memory_object));

	      /* Leave ->end, ->len ->next and ->bytes empty for now... */
	      obj->prev = tmp->prev;
	      obj->start = target_start;

	      if (obj->prev)
		obj->prev->next = obj;
	      else
		cache->head = obj;

	      begin = obj;

	      if (max_end < tmp->start)
		{
		  beyond_end = tmp;
		  break;
		}

	      if (max_end <= tmp->end)
		{
		  in_end = tmp;
		  break;
		}

	      tmp = tmp->next;
	      continue;
	    }
	  else if (tmp->end < target_start)
	    {
	      /* We aren't in this object if the end is before our start */
	      tmp = tmp->next;
	      continue;
	    }
	  else /* Aha!  Our start is in this object */
	    {
	      begin = tmp;

	      /* Check to see if we can just pull the read out of the
		 cache? */
	      if (target_end <= tmp->end)
		{
		  diff_end = (vm_offset_t) target_start - (vm_offset_t) tmp->start;
		  ptr = (vm_address_t)((vm_offset_t) tmp->bytes + diff_end);
		  bcopy (ptr, gdb_addr, length);
		  return TRUE;
		}

	      tmp = tmp->next;
	      continue;
	    }
	}
      else	/* We've found the beginning, but not the end. */
	{
	  /* We know we can break out of this loop, if the tmp object's
	     start begins at a memory location greater than our max
	     read location. */
	  if (max_end < tmp->start)
	    {
	      beyond_end = tmp;
	      break;
	    }

	  if (max_end > tmp->end)
	    {
	      /* We know that this object will be subsumed by the
		 impending xfer, so we may as well, just destroy it,
		 we get it's contents back for free. */

	      if (cache->tail == tmp)
		{
		  if (begin)
		    cache->tail = begin;
		  else
		    cache->tail = obj;

		  /* Sanity Check */
		  if (cache->tail == NULL)
		    printf("Insanity checks: rcache_read, cache->tail NULL\n");
		}

	      tmp2 = tmp->next;

	      free (tmp->bytes);
	      free (tmp);

	      tmp = tmp2;

	      continue;
	    }

	  /* We know that the end of the read is in this object,
	     so we keep whatever's after the end and add it to the
	     size */

	  in_end = tmp;
	  break;
	}

      /* Should never get here! */
      printf("read_cache: shouldn't get here!\n");

      tmp = tmp->next;
    }

  if (!cache->head)
    {
      cache->head = (struct memory_object *)
	xmalloc(sizeof(struct memory_object));
      cache->tail = cache->head;
      cache->head->next = NULL;
      cache->head->prev = NULL;
      cache->head->len = max_end - target_start + 1;
      cache->head->start = target_start;
      cache->head->end = max_end;
      cache->head->bytes = (vm_address_t)xmalloc(max_end - target_start + 1);
      *cache_ptr = (char*)cache->head->bytes;
      *read_len = cache->head->len;
      return FALSE;
    }

  if (!begin)
    {
      obj = (struct memory_object *)xmalloc(sizeof(struct memory_object));
      obj->next = NULL;
      obj->prev = cache->tail;
      cache->tail->next = obj;
      cache->tail = obj;

      obj->len = max_end - target_start + 1;
      obj->start = target_start;
      obj->end = max_end;
      obj->bytes = (vm_address_t)xmalloc(max_end - target_start + 1);
      *cache_ptr = (char*)obj->bytes;
      *read_len = obj->len;
      return FALSE; 
    }

  if (obj)
    {
      if (beyond_end != NULL)
	{
	  size_of_new_bytes = max_end - target_start + 1;
	  obj->next = beyond_end;
	}
      else if (in_end != NULL)
	{
	  diff_end = in_end->end - max_end;
	  size_of_new_bytes = max_end - target_start + 1 + diff_end;
	  obj->next = in_end->next;
	}
      else
	{
	  size_of_new_bytes = max_end - target_start + 1;
	  obj->next = NULL;
	}

      if (obj->next)
	obj->next->prev = obj;
      else
	cache->tail = obj;

      obj->len = size_of_new_bytes;
      obj->end = obj->start + size_of_new_bytes -1;

      new = (vm_address_t)xmalloc (size_of_new_bytes);
      obj->bytes = new;

      if (in_end)
	{
	  if (diff_end > 0)
	    {
	      dest = (vm_address_t)((vm_offset_t)new +
				       size_of_new_bytes - diff_end);
	      src = (vm_address_t)((vm_offset_t)in_end->bytes + 
				      in_end->len - diff_end);
	      bcopy (src, dest, diff_end);
	    }

	  free (in_end->bytes);
	  free (in_end);
	}

      *cache_ptr = (char*)obj->bytes;
      *read_len = max_end - target_start + 1;
    }
  else
    {
      obj = (struct memory_object *)xmalloc(sizeof(struct memory_object));

      diff_start = target_start - begin->start;

      if (beyond_end != NULL)
	{
	  diff_end = 0;
	  size_of_new_bytes = max_end - begin->start + 1;
	  obj->end = max_end;
	  obj->next = beyond_end;
	}
      else if (in_end != NULL)
	{
	  diff_end = in_end->end - max_end;
	  size_of_new_bytes = in_end->end - begin->start + 1;
	  obj->end = in_end->end;
	  obj->next = in_end->next;
	}
      else
	{
	  diff_end = 0;
	  size_of_new_bytes = max_end - begin->start + 1;
	  obj->end = max_end;
	  obj->next = NULL;
	}

      if (obj->next)
	obj->next->prev = obj;
      else
	cache->tail = obj;

      if (begin->prev)
	{
	  obj->prev = begin->prev;
	  obj->prev->next = obj;
	}
      else
	{
	  cache->head = obj;
	  obj->prev = NULL;
	}

      obj->len = size_of_new_bytes;
      obj->start = begin->start;

      new = (vm_address_t)xmalloc (size_of_new_bytes);
      obj->bytes = new;

      if (diff_start > 0)
	{
	  dest = new;
	  src = begin->bytes;
	  bcopy (src, dest, diff_start);
	}

      /* There MUST be a begin or else we'd be in the if clause! */
      free (begin->bytes);
      free (begin);

      if (in_end && diff_end > 0)
	{
	  if (diff_end > 0)
	    {
	      dest = (vm_address_t)((vm_offset_t)new + 
				       size_of_new_bytes - diff_end);
	      src = (vm_address_t)((vm_offset_t)in_end->bytes +
				      in_end->len - diff_end);
	      bcopy (src, dest, diff_end);
	    }

	  free (in_end->bytes);
	  free (in_end);
	}

      *cache_ptr = (char*)(obj->bytes + diff_start);
      *read_len = max_end - target_start + 1;
    }

  return FALSE;
}

/* Invalidate the memory objects starting at start and ending with
   start + length. */
void
gdb_cache_invalidate(struct memory_cache * cache,
		     CORE_ADDR start,
		     vm_size_t length)
{
	struct memory_object * obj;
	struct memory_object * obj_start;
	struct memory_object * obj_end;

	for (obj = cache->head; obj; obj = obj->next)
	{
		if ((obj->start <= start) && (obj->end >= start))
		{
			obj_start = obj->prev;
			break;
		}
	}

	if (!obj)
		return;

	while(obj)
	{
		obj_end = obj->next;

		free (obj->bytes);
		free (obj);

		if ((!obj_end) || (obj_end->start < start + length))
			break;

		obj = obj_end;
	}

	if (obj_start)
		obj_start->next = obj_end;
	else
		cache->head = obj_start;
	
	if (obj_end)
		obj_end->prev = obj_start;
	else
		cache->tail = obj_start;
}

struct cache_list_entry *
find_entry(char * name)
{
  struct cache_list_entry * ptr;

  for(ptr = cache_list_head; ptr; ptr = ptr->next)
    {
      if (strcmp(ptr->cache->name, name) == 0)
	break;
    }

  return (ptr);
}

/* Throw away a cache's contents. */

void
clean_cache_command(args, from_tty)
     char * args;
     int from_tty;
{
  int argc;
  char **argv;
  struct cache_list_entry * ptr;

  parse_args (args, &argc, &argv);

  if (argc != 1)
    {
      printf("Usage: cache-flush <cachename>\n");
      return;
    }

  ptr = find_entry(argv[0]);

  if (!ptr)
    {
      printf("Invalid cache name '%s'.\n", argv[0]);
      return;
    }

  gdb_cache_clean(ptr->cache);
}

/* Primarily used for debugging the cache code . */

void
walk_cache(args, from_tty)
     char * args;
     int from_tty;
{
  int argc;
  char **argv;
  struct memory_object * tmp;
  struct cache_list_entry * ptr;

  parse_args (args, &argc, &argv);

  if (argc != 1)
    {
      printf("Usage: cache-walk <cachename>\n");
      return;
    }

  ptr = find_entry(argv[0]);

  if (!ptr)
    {
      printf("Invalid cache name '%s'.\n", argv[0]);
      return;
    }

  if (!ptr->cache->head)
    {
      printf("%s cache empty.\n", ptr->cache->name);
      return;
    }

  for (tmp = ptr->cache->head; tmp; tmp = tmp->next)
    {
      printf("OBJ(0x%lx) prev = 0x%lx, next = 0x%lx.   Start = 0x%lx, End = 0x%lx.\n",tmp, tmp->prev, tmp->next, tmp->start, tmp->end);
    }
}

/* Enable/disable a cache. */

void
toggle_cache(args, from_tty)
     char * args;
     int from_tty;
{
  int argc;
  char **argv;
  struct cache_list_entry * ptr;

  parse_args (args, &argc, &argv);

  if (argc != 1)
    {
      printf("Usage: cache-toggle <cachename>\n");
      return;
    }

  ptr = find_entry(argv[0]);

  if (!ptr)
    {
      printf("invalid cache name '%s'.\n", argv[0]);
      return;
    }

  if (ptr->cache->enabled)
    {
      ptr->cache->enabled = FALSE;
    }
  else
    {
      ptr->cache->enabled = TRUE;
      gdb_cache_clean(ptr->cache);
    }

  printf("%s cache %s.\n", argv[0],
	 (ptr->cache->enabled ? "enabled" : "disabled"));
}

/* Set the default granularity for new memory objects. */

void
set_cache_gran(args, from_tty)
     char * args;
     int from_tty;
{
  int argc;
  char **argv;
  struct cache_list_entry * ptr;
  int gran;

  parse_args (args, &argc, &argv);

  if (argc != 2)
    {
      printf("usage: cache-gran <cachename> <granularity>\n");
      return;
    }

  ptr = find_entry(argv[0]);

  if (!ptr)
    {
      printf("Invalid cache name '%s'.\n", argv[0]);
      return;
    }

  gran = atoi(argv[1]);

  if ((gran < 1) || (gran > ptr->cache->max_granularity))
    {
      printf("Invalid granularity of %d.  Maximum is %d.\n", gran,
	     ptr->cache->max_granularity);
      return;
    }

  ptr->cache->granularity = gran;

  printf("%s cache default granularity set to %d.\n", gran);
}

/* Enumerate all gdb caches. */

void
list_caches(args, from_tty)
  char * args;
  int from_tty;
{
  struct cache_list_entry * ptr;

  if (!cache_list_head)
    {
      printf("No caches in use.\n");
      return;
    }

  printf("Caches currently in use by gdb:\n");

  for(ptr = cache_list_head; ptr; ptr = ptr->next)
      printf("Cachename '%s'\n", ptr->cache->name);
}

/* Print the status of a cache. */

void
cache_status(args, from_tty)
     char * args;
     int from_tty;
{
  int argc;
  char **argv;
  struct memory_object * tmp;
  struct cache_list_entry * ptr;

  parse_args (args, &argc, &argv);

  if (argc != 1)
    {
      printf("Usage: cache-status <cachename>\n");
      return;
    }

  ptr = find_entry(argv[0]);

  if (!ptr)
    {
      printf("Invalid cache name '%s'.\n", argv[0]);
      return;
    }

  printf("Status of '%s' cache:\n", ptr->cache->name);
  printf("cache is %s.\n", (ptr->cache->enabled ? "Enabled" : "Disabled"));
  printf("cache is %s.\n", (ptr->cache->name ? "Valid" : "Invalid"));
  printf("cache is %s.\n", (ptr->cache->head ? "Nonempty" : "Empty"));
  printf("cache maximum granularity is %d.\n", ptr->cache->max_granularity);
  printf("cache granularity is %d.\n", ptr->cache->granularity);
}

/* It's silly to have to replicate this everywhere!  Why doesn't
   someone put it in the eval.c or parse.c file? */

/* Advance a string pointer across whitespace and return a pointer
   to the first non-white character.  */

static char *
skip_white_space (p)
     register char *p;
{
  while (*p == ' ' || *p == '\t')
    p++;
  return p;
}
    
/* Search for the first unquoted whitespace character in a string.
   Returns a pointer to the character, or to the null terminator
   if no whitespace is found.  */

static char *
find_white_space (p)
     register char *p;
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
	register int arg_count = 0; /* number of arguments */
	register int arg_index = 0;
	register char *p0;
  
	/* first count how many arguments there are */
  
	if (p0 = arg_string)  {
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
	} else {
		*argc = arg_count = 0;
	}
  
	(*argv)[arg_count] = NULL;
}

void 
_initialize_remote_cache()
{


  add_cmd ("cache-walk", no_class, walk_cache,
	   "Display remote cache's memory object chain.",
	   &cmdlist);

  add_cmd ("cache-clean", no_class, clean_cache_command,
	   "Clean out the remote cache and throw away old memory objects.",
	   &cmdlist);
  

  add_cmd ("cache-toggle", no_class, toggle_cache,
	   "Toggle enable/disable of cache. Usage: cache-toggle <cachename>",
	   &cmdlist);

  add_cmd ("cache-gran", no_class, set_cache_gran,
	   "Set the default granularity for a cache.  Usage cache-gran <cachename> <granularity>",
	   &cmdlist);

  add_cmd ("cache-list", no_class, list_caches,
	   "Print the names of the current caches.",
	   &cmdlist);

  add_cmd ("cache-status", no_class, cache_status,
	   "Print the status of a cache.",
	   &cmdlist);
}
