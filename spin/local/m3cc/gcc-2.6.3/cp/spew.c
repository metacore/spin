/* Type Analyzer for GNU C++.
   Copyright (C) 1987, 1989, 1992, 1993 Free Software Foundation, Inc.
   Hacked... nay, bludgeoned... by Mark Eichin (eichin@cygnus.com)

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* This file is the type analyzer for GNU C++.  To debug it, define SPEW_DEBUG
   when compiling parse.c and spew.c.  */

#include "config.h"
#include <stdio.h>
#include "input.h"
#include "tree.h"
#include "lex.h"
#include "parse.h"
#include "cp-tree.h"
#include "flags.h"
#include "obstack.h"

/* This takes a token stream that hasn't decided much about types and
   tries to figure out as much as it can, with excessive lookahead and
   backtracking. */

/* fifo of tokens recognized and available to parser. */
struct token  {
  /* The values for YYCHAR will fit in a short.  */
  short		yychar;
  short		end_of_file;
  YYSTYPE	yylval;
};

static int do_aggr ();

/* From lex.c: */
/* the declaration found for the last IDENTIFIER token read in.
   yylex must look this up to detect typedefs, which get token type TYPENAME,
   so it is left around in case the identifier is not a typedef but is
   used in a context which makes it a reference to a variable.  */
extern tree lastiddecl;		/* let our brains leak out here too */
extern int	yychar;		/*  the lookahead symbol		*/
extern YYSTYPE	yylval;		/*  the semantic value of the		*/
				/*  lookahead symbol			*/
extern int end_of_file;

struct obstack token_obstack;
int first_token;
  
#ifdef SPEW_DEBUG
int spew_debug = 0;
static unsigned int yylex_ctr = 0;
static int debug_yychar ();
#endif

/* Initialize token_obstack. Called once, from init_lex.  */
void
init_spew ()
{
  gcc_obstack_init(&token_obstack);
}

#ifdef SPEW_DEBUG
/* Use functions for debugging...  */

/* Return the number of tokens available on the fifo. */
static int
num_tokens ()
{
  return (obstack_object_size(&token_obstack)/sizeof(struct token))
    - first_token;
}

/* Fetch the token N down the line from the head of the fifo. */
static struct token*
nth_token (n)
     int n;
{
  /* could just have this do slurp_ implicitly, but this way is easier
   * to debug... */
  my_friendly_assert (n < num_tokens(), 298);
  return ((struct token*)obstack_base(&token_obstack))+n+first_token;
}

/* Add a token to the token fifo. */
static void
add_token (t)
     struct token* t;
{
  obstack_grow(&token_obstack,t,sizeof (struct token));
}

/* Consume the next token out of the fifo.  */
static void
consume_token()
{
  if (num_tokens() == 1)
    {
      obstack_free(&token_obstack, obstack_base (&token_obstack));
      first_token = 0;
    }
  else
    first_token++;
}

#else
/* ...otherwise use macros.  */

#define num_tokens() \
  ((obstack_object_size(&token_obstack)/sizeof(struct token)) - first_token)

#define nth_token(N) \
  (((struct token*)obstack_base(&token_obstack))+(N)+first_token)

#define add_token(T) obstack_grow(&token_obstack, (T), sizeof (struct token))

#define consume_token() \
  (num_tokens() == 1							\
   ? (obstack_free (&token_obstack, obstack_base (&token_obstack)),	\
      (first_token = 0))						\
   : first_token++)
#endif

/* Pull in enough tokens from real_yylex that the queue is N long beyond
   the current token.  */

static void
scan_tokens (n)
     int n;
{
  int i;
  struct token *tmp;

  /* We cannot read past certain tokens, so make sure we don't.  */
  i = num_tokens ();
  if (i > n)
    return;
  while (i-- > 0)
    {
      tmp = nth_token (i);
      /* Never read past these characters: they might separate
	 the current input stream from one we save away later.  */
      if (tmp->yychar == '{' || tmp->yychar == ':' || tmp->yychar == ';')
	goto pad_tokens;
    }

  while (num_tokens() <= n)
    {
      obstack_blank(&token_obstack,sizeof (struct token));
      tmp = ((struct token *)obstack_next_free (&token_obstack))-1;
      tmp->yychar = real_yylex();
      tmp->end_of_file = end_of_file;
      tmp->yylval = yylval;
      end_of_file = 0;
      if (tmp->yychar == '{'
	  || tmp->yychar == ':'
	  || tmp->yychar == ';')
	{
	pad_tokens:
	  while (num_tokens () <= n)
	    {
	      obstack_blank(&token_obstack,sizeof (struct token));
	      tmp = ((struct token *)obstack_next_free (&token_obstack))-1;
	      tmp->yychar = EMPTY;
	      tmp->end_of_file = 0;
	    }
	}
    }
}

/* Create room for N tokens at the front of the fifo.  This is used
   to insert new tokens into the stream ahead of the current token.  */

static void
shift_tokens (n)
     int n;
{
  if (first_token >= n)
    first_token -= n;
  else
    {
      int old_token_count = num_tokens ();
      char *tmp;

      obstack_blank (&token_obstack, (n-first_token) * sizeof (struct token));
      if (old_token_count)
	{
	  tmp = (char *)alloca ((num_tokens () + (n-first_token))
				* sizeof (struct token));
	  /* This move does not rely on the system being able to handle
	     overlapping moves.  */
	  bcopy ((char *) nth_token (0), tmp,
		 old_token_count * sizeof (struct token));
	  bcopy (tmp, (char *) nth_token (n),
		 old_token_count * sizeof (struct token));
	}
      first_token = 0;
    }
}

static int
probe_obstack (h, obj, nlevels)
     struct obstack *h;
     tree obj;
     unsigned int nlevels;
{
  register struct _obstack_chunk*  lp;	/* below addr of any objects in this chunk */
  register struct _obstack_chunk*  plp;	/* point to previous chunk if any */

  lp = (h)->chunk;
  /* We use >= rather than > since the object cannot be exactly at
     the beginning of the chunk but might be an empty object exactly
     at the end of an adjacent chunk. */
  for (; nlevels != 0 && lp != 0 && ((tree)lp >= obj || (tree)lp->limit < obj);
       nlevels -= 1)
    {
      plp = lp->prev;
      lp = plp;      
    }
  return nlevels != 0 && lp != 0;
}

/* from lex.c: */
/* Value is 1 (or 2) if we should try to make the next identifier look like
   a typename (when it may be a local variable or a class variable).
   Value is 0 if we treat this name in a default fashion. */
extern int looking_for_typename;
int looking_for_template;

extern struct obstack *current_obstack, *saveable_obstack;
tree got_scope;

int
yylex()
{
  struct token tmp_token;
  tree trrr;

 retry:
#ifdef SPEW_DEBUG
  if (spew_debug)
  {
    yylex_ctr ++;
    fprintf(stderr, "\t\t## %d ##",yylex_ctr);
  }
#endif

  /* if we've got tokens, send them */
  if (num_tokens())
    {
      tmp_token= *nth_token(0);

      /* TMP_TOKEN.YYLVAL.TTYPE may have been allocated on the wrong obstack.
	 If we don't find it in CURRENT_OBSTACK's current or immediately
	 previous chunk, assume it was and copy it to the current obstack.  */
      if ((tmp_token.yychar == CONSTANT
	   || tmp_token.yychar == STRING)
	  && ! TREE_PERMANENT (tmp_token.yylval.ttype)
	  && ! probe_obstack (current_obstack, tmp_token.yylval.ttype, 2)
	  && ! probe_obstack (saveable_obstack, tmp_token.yylval.ttype, 2))
	tmp_token.yylval.ttype = copy_node (tmp_token.yylval.ttype);
    }
  else
    {
      /* if not, grab the next one and think about it */
      tmp_token.yychar = real_yylex ();
      tmp_token.yylval = yylval;
      tmp_token.end_of_file = end_of_file;
      add_token(&tmp_token);
    }

  /* many tokens just need to be returned. At first glance, all we
   * have to do is send them back up, but some of them are needed to
   * figure out local context. */
  switch(tmp_token.yychar)
    {
    case EMPTY:
      /* This is a lexical no-op.  */
      consume_token ();
#ifdef SPEW_DEBUG    
      if (spew_debug)
	debug_yychar (tmp_token.yychar);
#endif
      goto retry;

    case IDENTIFIER:
      scan_tokens (1);
      if (nth_token (1)->yychar == SCOPE)
	/* Don't interfere with the setting from an 'aggr' prefix.  */
	looking_for_typename++;
      else if (nth_token (1)->yychar == '<')
	looking_for_template = 1;

      trrr = lookup_name (tmp_token.yylval.ttype, -2);

      if (trrr)
	{
	  tmp_token.yychar = identifier_type (trrr);
	  switch (tmp_token.yychar)
	    {
	    case TYPENAME:
	      lastiddecl = identifier_typedecl_value (tmp_token.yylval.ttype);
	      if (lastiddecl != trrr)
		{
		  lastiddecl = trrr;
		  if (got_scope)
		    tmp_token.yylval.ttype = DECL_NESTED_TYPENAME (trrr);
		}
	      break;
	    case IDENTIFIER:
	      lastiddecl = trrr;
	      break;
	    case PTYPENAME:
	      lastiddecl = NULL_TREE;
	      break;
	    default:
	      my_friendly_abort (101);
	    }
	}
      else
	lastiddecl = trrr;
      got_scope = NULL_TREE;
      /* and fall through to... */
    case TYPENAME:
    case PTYPENAME:
      consume_token ();
      if (looking_for_typename > 0)
	looking_for_typename--;
      looking_for_template = 0;
      break;

    case SCSPEC:
      /* do_aggr needs to check if the previous token was RID_FRIEND,
	 so just increment first_token instead of calling consume_token. */
      first_token++;
      break;
    case TYPESPEC:
      consume_token ();
      break;

    case AGGR:
      *nth_token(0) = tmp_token;
      do_aggr ();
      /* fall through to output... */
    case ENUM:
      /* Set this again, in case we are rescanning.  */
      looking_for_typename = 1;
      /* fall through... */
    default:
      consume_token();
    }

  yylval = tmp_token.yylval;
  yychar = tmp_token.yychar;
  end_of_file = tmp_token.end_of_file;
#ifdef SPEW_DEBUG    
  if (spew_debug)
    debug_yychar(yychar);
#endif
  return yychar;
}

/* token[0] == AGGR (struct/union/enum)
 * Thus, token[1] is either a TYPENAME or a TYPENAME_DEFN.
 * If token[2] == '{' or ':' then it's TYPENAME_DEFN.
 * It's also a definition if it's a forward declaration (as in 'struct Foo;')
 * which we can tell lf token[2] == ';' *and* token[-1] != FRIEND.
 */
static int
do_aggr ()
{
  int yc1, yc2;
  
  scan_tokens (2);
  yc1 = nth_token (1)->yychar;
  if (yc1 != TYPENAME && yc1 != IDENTIFIER && yc1 != PTYPENAME)
    return 0;
  yc2 = nth_token (2)->yychar;
  if (yc2 == ';')
    {
      /* It's a forward declaration iff we were not preceded by 'friend'. */
      if (first_token > 0 && nth_token (-1)->yychar == SCSPEC
	  && nth_token (-1)->yylval.ttype == ridpointers[(int) RID_FRIEND])
	return 0;
    }
  else if (yc2 != '{' && yc2 != ':')
    return 0;

  switch (yc1)
    {
    case TYPENAME:
      nth_token (1)->yychar = TYPENAME_DEFN;
      break;
    case PTYPENAME:
      nth_token (1)->yychar = PTYPENAME_DEFN;
      break;
    case IDENTIFIER:
      nth_token (1)->yychar = IDENTIFIER_DEFN;
      break;
    default:
      my_friendly_abort (102);
    }
  return 0;
}  
  
#ifdef SPEW_DEBUG    
/* debug_yychar takes a yychar (token number) value and prints its name. */
static int
debug_yychar (yy)
     int yy;
{
  /* In parse.y: */
  extern char *debug_yytranslate ();
  
  int i;
  
  if(yy<256) {
    fprintf (stderr, "<%d: %c >\n", yy, yy);
    return 0;
  }
  fprintf (stderr, "<%d:%s>\n", yy, debug_yytranslate (yy));
  return 1;
}

#endif
