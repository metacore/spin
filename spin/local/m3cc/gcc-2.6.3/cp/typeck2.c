/* Report error messages, build initializers, and perform
   some front-end optimizations for C++ compiler.
   Copyright (C) '87, '88, '89, '92, 1993, 1994 Free Software Foundation, Inc.
   Hacked by Michael Tiemann (tiemann@cygnus.com)

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


/* This file is part of the C++ front end.
   It contains routines to build C++ expressions given their operands,
   including computing the types of the result, C and C++ specific error
   checks, and some optimization.

   There are also routines to build RETURN_STMT nodes and CASE_STMT nodes,
   and to process initializations in declarations (since they work
   like a strange sort of assignment).  */

#include "config.h"
#include <stdio.h>
#include "tree.h"
#include "cp-tree.h"
#include "flags.h"

static tree process_init_constructor ();
extern void pedwarn (), error ();

extern int errorcount;
extern int sorrycount;

/* Print an error message stemming from an attempt to use
   BASETYPE as a base class for TYPE.  */
tree
error_not_base_type (basetype, type)
     tree basetype, type;
{
  if (TREE_CODE (basetype) == FUNCTION_DECL)
    basetype = DECL_CLASS_CONTEXT (basetype);
  cp_error ("type `%T' is not a base type for type `%T'", basetype, type);
  return error_mark_node;
}

tree
binfo_or_else (parent_or_type, type)
     tree parent_or_type, type;
{
  tree binfo;
  if (TYPE_MAIN_VARIANT (parent_or_type) == TYPE_MAIN_VARIANT (type))
    return TYPE_BINFO (parent_or_type);
  if ((binfo = get_binfo (parent_or_type, TYPE_MAIN_VARIANT (type), 0)))
    {
      if (binfo == error_mark_node)
	return NULL_TREE;
      return binfo;
    }
  error_not_base_type (parent_or_type, type);
  return NULL_TREE;
}

/* Print an error message stemming from an invalid use of an
   aggregate type.

   TYPE is the type or binfo which draws the error.
   MSG is the message to print.
   ARG is an optional argument which may provide more information.  */
void
error_with_aggr_type (type, msg, arg)
     tree type;
     char *msg;
     HOST_WIDE_INT arg;
{
  tree name;

  if (TREE_CODE (type) == TREE_VEC)
    type = BINFO_TYPE (type);

  name = TYPE_NAME (type);
  if (TREE_CODE (name) == TYPE_DECL)
    name = DECL_NAME (name);
  error (msg, IDENTIFIER_POINTER (name), arg);
}

/* According to ARM $7.1.6, "A `const' object may be initialized, but its
   value may not be changed thereafter.  Thus, we emit hard errors for these,
   rather than just pedwarns.  If `SOFT' is 1, then we just pedwarn.  (For
   example, conversions to references.)  */
void
readonly_error (arg, string, soft)
     tree arg;
     char *string;
     int soft;
{
  char *fmt;
  void (*fn)();

  if (soft)
    fn = pedwarn;
  else
    fn = error;

  if (TREE_CODE (arg) == COMPONENT_REF)
    {
      if (TYPE_READONLY (TREE_TYPE (TREE_OPERAND (arg, 0))))
        fmt = "%s of member `%s' in read-only structure";
      else
        fmt = "%s of read-only member `%s'";
      (*fn) (fmt, string, lang_printable_name (TREE_OPERAND (arg, 1)));
    }
  else if (TREE_CODE (arg) == VAR_DECL)
    {
      if (DECL_LANG_SPECIFIC (arg)
	  && DECL_IN_AGGR_P (arg)
	  && !TREE_STATIC (arg))
	fmt = "%s of constant field `%s'";
      else
	fmt = "%s of read-only variable `%s'";
      (*fn) (fmt, string, lang_printable_name (arg));
    }
  else if (TREE_CODE (arg) == PARM_DECL)
    (*fn) ("%s of read-only parameter `%s'", string,
	   lang_printable_name (arg));
  else if (TREE_CODE (arg) == INDIRECT_REF
           && TREE_CODE (TREE_TYPE (TREE_OPERAND (arg, 0))) == REFERENCE_TYPE
           && (TREE_CODE (TREE_OPERAND (arg, 0)) == VAR_DECL
               || TREE_CODE (TREE_OPERAND (arg, 0)) == PARM_DECL))
    (*fn) ("%s of read-only reference `%s'",
	   string, lang_printable_name (TREE_OPERAND (arg, 0)));
  else if (TREE_CODE (arg) == RESULT_DECL)
    (*fn) ("%s of read-only named return value `%s'",
	   string, lang_printable_name (arg));
  else	       
    (*fn) ("%s of read-only location", string);
}

/* Print an error message for invalid use of a type which declares
   virtual functions which are not inheritable.  */
void
abstract_virtuals_error (decl, type)
     tree decl;
     tree type;
{
  tree u = CLASSTYPE_ABSTRACT_VIRTUALS (type);

  if (decl)
    {
      if (TREE_CODE (decl) == RESULT_DECL)
	return;

      if (TREE_CODE (decl) == VAR_DECL)
	cp_error ("cannot declare variable `%D' to be of type `%T'",
		    decl, type);
      else if (TREE_CODE (decl) == PARM_DECL)
	cp_error ("cannot declare parameter `%D' to be of type `%T'",
		    decl, type);
      else if (TREE_CODE (decl) == FIELD_DECL)
	cp_error ("cannot declare field `%D' to be of type `%T'",
		    decl, type);
      else if (TREE_CODE (decl) == FUNCTION_DECL
	       && TREE_CODE (TREE_TYPE (decl)) == METHOD_TYPE)
	cp_error ("invalid return type for method `%#D'", decl);
      else if (TREE_CODE (decl) == FUNCTION_DECL)
	cp_error ("invalid return type for function `%#D'", decl);
    }
  else cp_error ("cannot allocate an object of type `%T'", type);
  /* Only go through this once.  */
  if (TREE_PURPOSE (u) == NULL_TREE)
    {
      error ("  since the following virtual functions are abstract:");
      TREE_PURPOSE (u) = error_mark_node;
      while (u)
	{
	  cp_error ("\t%#D", TREE_VALUE (u));
	  u = TREE_CHAIN (u);
	}
    }
  else cp_error ("  since type `%T' has abstract virtual functions", type);
}

/* Print an error message for invalid use of a signature type.
   Signatures are treated similar to abstract classes here, they
   cannot be instantiated.  */
void
signature_error (decl, type)
     tree decl;
     tree type;
{
  if (decl)
    {
      if (TREE_CODE (decl) == RESULT_DECL)
	return;

      if (TREE_CODE (decl) == VAR_DECL)
	cp_error ("cannot declare variable `%D' to be of signature type `%T'",
		  decl, type);
      else if (TREE_CODE (decl) == PARM_DECL)
	cp_error ("cannot declare parameter `%D' to be of signature type `%T'",
		  decl, type);
      else if (TREE_CODE (decl) == FIELD_DECL)
	cp_error ("cannot declare field `%D' to be of signature type `%T'",
		  decl, type);
      else if (TREE_CODE (decl) == FUNCTION_DECL
	       && TREE_CODE (TREE_TYPE (decl)) == METHOD_TYPE)
	cp_error ("invalid return type for method `%#D'", decl);
      else if (TREE_CODE (decl) == FUNCTION_DECL)
	cp_error ("invalid return type for function `%#D'", decl);
    }
  else
    cp_error ("cannot allocate an object of signature type `%T'", type);
}

/* Print an error message for invalid use of an incomplete type.
   VALUE is the expression that was used (or 0 if that isn't known)
   and TYPE is the type that was invalid.  */

void
incomplete_type_error (value, type)
     tree value;
     tree type;
{
  char *errmsg;

  /* Avoid duplicate error message.  */
  if (TREE_CODE (type) == ERROR_MARK)
    return;

  if (value != 0 && (TREE_CODE (value) == VAR_DECL
		     || TREE_CODE (value) == PARM_DECL))
    error ("`%s' has an incomplete type",
	   IDENTIFIER_POINTER (DECL_NAME (value)));
  else
    {
    retry:
      /* We must print an error message.  Be clever about what it says.  */

      switch (TREE_CODE (type))
	{
	case RECORD_TYPE:
	  errmsg = "invalid use of undefined type `struct %s'";
	  break;

	case UNION_TYPE:
	  errmsg = "invalid use of undefined type `union %s'";
	  break;

	case ENUMERAL_TYPE:
	  errmsg = "invalid use of undefined type `enum %s'";
	  break;

	case VOID_TYPE:
	  error ("invalid use of void expression");
	  return;

	case ARRAY_TYPE:
	  if (TYPE_DOMAIN (type))
	    {
	      type = TREE_TYPE (type);
	      goto retry;
	    }
	  error ("invalid use of array with unspecified bounds");
	  return;

	case OFFSET_TYPE:
	  error ("invalid use of member type (did you forget the `&' ?)");
	  return;

	default:
	  my_friendly_abort (108);
	}

      error_with_aggr_type (type, errmsg);
    }
}

/* Like error(), but don't call report_error_function().  */
static void
ack (s, v, v2)
     char *s;
     HOST_WIDE_INT v;
     HOST_WIDE_INT v2;
{
  extern char * progname;
  
  if (input_filename)
    fprintf (stderr, "%s:%d: ", input_filename, lineno);
  else
    fprintf (stderr, "%s: ", progname);

  fprintf (stderr, s, v, v2);
  fprintf (stderr, "\n");
}
  
/* There are times when the compiler can get very confused, confused
   to the point of giving up by aborting, simply because of previous
   input errors.  It is much better to have the user go back and
   correct those errors first, and see if it makes us happier, than it
   is to abort on him.  This is because when one has a 10,000 line
   program, and the compiler comes back with ``core dump'', the user
   is left not knowing even where to begin to fix things and no place
   to even try and work around things.

   The parameter is to uniquely identify the problem to the user, so
   that they can say, I am having problem 59, and know that fix 7 will
   probably solve their problem.  Or, we can document what problem
   59 is, so they can understand how to work around it, should they
   ever run into it.

   Note, there will be no more calls in the C++ front end to abort,
   because the C++ front end is so unreliable still.  The C front end
   can get away with calling abort, because for most of the calls to
   abort on most machines, it, I suspect, can be proven that it is
   impossible to ever call abort.  The same is not yet true for C++,
   one day, maybe it will be.

   We used to tell people to "fix the above error[s] and try recompiling
   the program" via a call to fatal, but that message tended to look
   silly.  So instead, we just do the equivalent of a call to fatal in the
   same situation (call exit).  */

/* First used: 0 (reserved), Last used: 360.  Free: 261.  */

static int abortcount = 0;

void
my_friendly_abort (i)
     int i;
{
  /* if the previous error came through here, i.e. report_error_function
     ended up calling us again, don't just exit; we want a diagnostic of
     some kind.  */
  if (abortcount == 1)
    current_function_decl = NULL_TREE;
  else if (errorcount > 0 || sorrycount > 0)
    {
      if (abortcount > 1)
	{
	  if (i == 0)
	    ack ("Internal compiler error.");
	  else
	    ack ("Internal compiler error %d.", i);
	  ack ("Please submit a full bug report to `bug-g++@prep.ai.mit.edu'.");
	}
      else
	error ("confused by earlier errors, bailing out");
      
      exit (34);
    }
  ++abortcount;

  if (i == 0)
    error ("Internal compiler error.");
  else
    error ("Internal compiler error %d.", i);

  fatal ("Please submit a full bug report to `bug-g++@prep.ai.mit.edu'.");
}

void
my_friendly_assert (cond, where)
     int cond, where;
{
  if (cond == 0)
    my_friendly_abort (where);
}

/* Return nonzero if VALUE is a valid constant-valued expression
   for use in initializing a static variable; one that can be an
   element of a "constant" initializer.

   Return 1 if the value is absolute; return 2 if it is relocatable.
   We assume that VALUE has been folded as much as possible;
   therefore, we do not need to check for such things as
   arithmetic-combinations of integers.  */

static int
initializer_constant_valid_p (value)
     tree value;
{
  switch (TREE_CODE (value))
    {
    case CONSTRUCTOR:
      return TREE_STATIC (value);

    case INTEGER_CST:
    case REAL_CST:
    case STRING_CST:
      return 1;

    case ADDR_EXPR:
      return 2;

    case CONVERT_EXPR:
    case NOP_EXPR:
      /* Allow conversions between types of the same kind.  */
      if (TREE_CODE (TREE_TYPE (value))
	  == TREE_CODE (TREE_TYPE (TREE_OPERAND (value, 0))))
	return initializer_constant_valid_p (TREE_OPERAND (value, 0));
      /* Allow (int) &foo provided int is as wide as a pointer.  */
      if (TREE_CODE (TREE_TYPE (value)) == INTEGER_TYPE
	  && TREE_CODE (TREE_TYPE (TREE_OPERAND (value, 0))) == POINTER_TYPE
	  && ! tree_int_cst_lt (TYPE_SIZE (TREE_TYPE (value)),
				TYPE_SIZE (TREE_TYPE (TREE_OPERAND (value, 0)))))
	return initializer_constant_valid_p (TREE_OPERAND (value, 0));
      return 0;

    case PLUS_EXPR:
      {
	int valid0 = initializer_constant_valid_p (TREE_OPERAND (value, 0));
	int valid1 = initializer_constant_valid_p (TREE_OPERAND (value, 1));
	if (valid0 == 1 && valid1 == 2)
	  return 2;
	if (valid0 == 2 && valid1 == 1)
	  return 2;
	return 0;
      }

    case MINUS_EXPR:
      {
	int valid0 = initializer_constant_valid_p (TREE_OPERAND (value, 0));
	int valid1 = initializer_constant_valid_p (TREE_OPERAND (value, 1));
	if (valid0 == 2 && valid1 == 1)
	  return 2;
	return 0;
      }

    default:
      return 0;
    }
}

/* Perform appropriate conversions on the initial value of a variable,
   store it in the declaration DECL,
   and print any error messages that are appropriate.
   If the init is invalid, store an ERROR_MARK.

   C++: Note that INIT might be a TREE_LIST, which would mean that it is
   a base class initializer for some aggregate type, hopefully compatible
   with DECL.  If INIT is a single element, and DECL is an aggregate
   type, we silently convert INIT into a TREE_LIST, allowing a constructor
   to be called.

   If INIT is a TREE_LIST and there is no constructor, turn INIT
   into a CONSTRUCTOR and use standard initialization techniques.
   Perhaps a warning should be generated?

   Returns value of initializer if initialization could not be
   performed for static variable.  In that case, caller must do
   the storing.  */

tree
store_init_value (decl, init)
     tree decl, init;
{
  register tree value, type;

  /* If variable's type was invalidly declared, just ignore it.  */

  type = TREE_TYPE (decl);
  if (TREE_CODE (type) == ERROR_MARK)
    return NULL_TREE;

  /* Take care of C++ business up here.  */
  type = TYPE_MAIN_VARIANT (type);

  /* implicitly tests if IS_AGGR_TYPE.  */
  if (TYPE_NEEDS_CONSTRUCTING (type) && TREE_CODE (init) != CONSTRUCTOR)
    my_friendly_abort (109);
  else if (IS_AGGR_TYPE (type))
    {
      /* Although we are not allowed to declare variables of signature
	 type, we complain about a possible constructor call in such a
	 declaration as well.  */
      if (TREE_CODE (init) == TREE_LIST
	  && IS_SIGNATURE (type))
	{
	  cp_error ("constructor syntax cannot be used with signature type `%T'",
		    type);
	  init = error_mark_node;
	}
      else if (TREE_CODE (init) == TREE_LIST)
	{
	  cp_error ("constructor syntax used, but no constructor declared for type `%T'", type);
	  init = build_nt (CONSTRUCTOR, NULL_TREE, nreverse (init));
	}
#if 0
      if (TREE_CODE (init) == CONSTRUCTOR)
	{
	  tree field;
	  tree funcs;
	  int func;

	  /* Check that we're really an aggregate as ARM 8.4.1 defines it.  */
	  if (CLASSTYPE_N_BASECLASSES (type))
	    cp_error_at ("initializer list construction illegal for derived class object `%D'", decl);
	  if (CLASSTYPE_VTBL_PTR (type))
	    cp_error_at ("initializer list construction illegal for polymorphic class object `%D'", decl);
	  if (TYPE_NEEDS_CONSTRUCTING (type))
	    {
	      cp_error_at ("initializer list construction illegal for `%D'", decl);
	      error ("due to the presence of a constructor");
	    }
	  for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	    if (TREE_PRIVATE (field) || TREE_PROTECTED (field))
	      {
		cp_error_at ("initializer list construction illegal for `%D'", decl);
		cp_error_at ("due to non-public access of member `%D'", field);
	      }
	  funcs = TYPE_METHODS (type);
	  if (funcs)
	    for (func = 0; func < TREE_VEC_LENGTH (funcs); func++)
	      {
		field = TREE_VEC_ELT (funcs, func);
		if (field && (TREE_PRIVATE (field) || TREE_PROTECTED (field)))
		  {
		    cp_error_at ("initializer list construction illegal for `%D'", decl);
		    cp_error_at ("due to non-public access of member `%D'", field);
		  }
	      }
	}
#endif
    }
  else if (TREE_CODE (init) == TREE_LIST
	   && TREE_TYPE (init) != unknown_type_node)
    {
      if (TREE_CODE (decl) == RESULT_DECL)
	{
	  if (TREE_CHAIN (init))
	    {
	      warning ("comma expression used to initialize return value");
	      init = build_compound_expr (init);
	    }
	  else
	    init = TREE_VALUE (init);
	}
      else if (TREE_TYPE (init) != 0
	       && TREE_CODE (TREE_TYPE (init)) == OFFSET_TYPE)
	{
	  /* Use the type of our variable to instantiate
	     the type of our initializer.  */
	  init = instantiate_type (type, init, 1);
	}
      else if (TREE_CODE (init) == TREE_LIST
	       && TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE)
	{
	  error ("cannot initialize arrays using this syntax");
	  return NULL_TREE;
	}
      else
	{
	  /* We get here with code like `int a (2);' */
	     
	  if (TREE_CHAIN (init) != NULL_TREE)
	    {
	      pedwarn ("initializer list being treated as compound expression");
	      init = build_compound_expr (init);
	    }
	  else
	    init = TREE_VALUE (init);
	}
    }

  /* End of special C++ code.  */

  /* Digest the specified initializer into an expression.  */

  value = digest_init (type, init, (tree *) 0);

  /* Store the expression if valid; else report error.  */

  if (TREE_CODE (value) == ERROR_MARK)
    ;
  else if (TREE_STATIC (decl)
	   && (! TREE_CONSTANT (value)
	       || ! initializer_constant_valid_p (value)
#if 0
	       /* A STATIC PUBLIC int variable doesn't have to be
		  run time inited when doing pic.  (mrs) */
	       /* Since ctors and dtors are the only things that can
		  reference vtables, and they are always written down
		  the the vtable definition, we can leave the
		  vtables in initialized data space.
		  However, other initialized data cannot be initialized
		  this way.  Instead a global file-level initializer
		  must do the job.  */
	       || (flag_pic && !DECL_VIRTUAL_P (decl) && TREE_PUBLIC (decl))
#endif
	       ))

    return value;
#if 0 /* No, that's C.  jason 9/19/94 */
  else
    {
      if (pedantic && TREE_CODE (value) == CONSTRUCTOR
	  /* Don't complain about non-constant initializers of
	     signature tables and signature pointers/references.  */
	  && ! (TYPE_LANG_SPECIFIC (type)
		&& (IS_SIGNATURE (type)
		    || IS_SIGNATURE_POINTER (type)
		    || IS_SIGNATURE_REFERENCE (type))))
	{
	  if (! TREE_CONSTANT (value) || ! TREE_STATIC (value))
	    pedwarn ("ANSI C++ forbids non-constant aggregate initializer expressions");
	}
    }
#endif
  DECL_INITIAL (decl) = value;
  return NULL_TREE;
}

/* Digest the parser output INIT as an initializer for type TYPE.
   Return a C expression of type TYPE to represent the initial value.

   If TAIL is nonzero, it points to a variable holding a list of elements
   of which INIT is the first.  We update the list stored there by
   removing from the head all the elements that we use.
   Normally this is only one; we use more than one element only if
   TYPE is an aggregate and INIT is not a constructor.  */

tree
digest_init (type, init, tail)
     tree type, init, *tail;
{
  enum tree_code code = TREE_CODE (type);
  tree element = NULL_TREE;
  tree old_tail_contents;
  /* Nonzero if INIT is a braced grouping, which comes in as a CONSTRUCTOR
     tree node which has no TREE_TYPE.  */
  int raw_constructor;

  /* By default, assume we use one element from a list.
     We correct this later in the sole case where it is not true.  */

  if (tail)
    {
      old_tail_contents = *tail;
      *tail = TREE_CHAIN (*tail);
    }

  if (init == error_mark_node || (TREE_CODE (init) == TREE_LIST
				  && TREE_VALUE (init) == error_mark_node))
    return error_mark_node;

  /* Strip NON_LVALUE_EXPRs since we aren't using as an lvalue.  */
  if (TREE_CODE (init) == NON_LVALUE_EXPR)
    init = TREE_OPERAND (init, 0);

  if (init && TREE_TYPE (init) && TYPE_PTRMEMFUNC_P (type))
    init = default_conversion (init);

  if (init && TYPE_PTRMEMFUNC_P (type)
      && ((TREE_CODE (init) == ADDR_EXPR
	   && ((TREE_CODE (TREE_TYPE (init)) == POINTER_TYPE
		&& TREE_CODE (TREE_TYPE (TREE_TYPE (init))) == METHOD_TYPE)
	       || TREE_CODE (TREE_OPERAND (init, 0)) == TREE_LIST))
	  || TREE_CODE (init) == TREE_LIST
	  || integer_zerop (init)
	  || (TREE_TYPE (init) && TYPE_PTRMEMFUNC_P (TREE_TYPE (init)))))
    {
      return build_ptrmemfunc (TYPE_PTRMEMFUNC_FN_TYPE (type), init, 0);
    }

  raw_constructor = TREE_CODE (init) == CONSTRUCTOR && TREE_TYPE (init) == 0;

  if (init && raw_constructor
      && CONSTRUCTOR_ELTS (init) != 0
      && TREE_CHAIN (CONSTRUCTOR_ELTS (init)) == 0)
    {
      element = TREE_VALUE (CONSTRUCTOR_ELTS (init));
      /* Strip NON_LVALUE_EXPRs since we aren't using as an lvalue.  */
      if (element && TREE_CODE (element) == NON_LVALUE_EXPR)
	element = TREE_OPERAND (element, 0);
      if (element == error_mark_node)
	return element;
    }

  /* Any type can be initialized from an expression of the same type,
     optionally with braces.  */

  if (init && TREE_TYPE (init)
      && (TYPE_MAIN_VARIANT (TREE_TYPE (init)) == type
	  || (code == ARRAY_TYPE && comptypes (TREE_TYPE (init), type, 1))))
    {
      if (pedantic && code == ARRAY_TYPE
	  && TREE_CODE (init) != STRING_CST)
	pedwarn ("ANSI C++ forbids initializing array from array expression");
      if (TREE_CODE (init) == CONST_DECL)
	init = DECL_INITIAL (init);
      else if (TREE_READONLY_DECL_P (init))
	init = decl_constant_value (init);
      return init;
    }

  if (element && (TREE_TYPE (element) == type
		  || (code == ARRAY_TYPE && TREE_TYPE (element)
		      && comptypes (TREE_TYPE (element), type, 1))))
    {
      if (pedantic && code == ARRAY_TYPE)
	pedwarn ("ANSI C++ forbids initializing array from array expression");
      if (pedantic && (code == RECORD_TYPE || code == UNION_TYPE))
	pedwarn ("ANSI C++ forbids single nonscalar initializer with braces");
      if (TREE_CODE (element) == CONST_DECL)
	element = DECL_INITIAL (element);
      else if (TREE_READONLY_DECL_P (element))
	element = decl_constant_value (element);
      return element;
    }

  /* Initialization of an array of chars from a string constant
     optionally enclosed in braces.  */

  if (code == ARRAY_TYPE)
    {
      tree typ1 = TYPE_MAIN_VARIANT (TREE_TYPE (type));
      if ((typ1 == char_type_node
	   || typ1 == signed_char_type_node
	   || typ1 == unsigned_char_type_node
	   || typ1 == unsigned_wchar_type_node
	   || typ1 == signed_wchar_type_node)
	  && ((init && TREE_CODE (init) == STRING_CST)
	      || (element && TREE_CODE (element) == STRING_CST)))
	{
	  tree string = element ? element : init;

	  if ((TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (string)))
	       != char_type_node)
	      && TYPE_PRECISION (typ1) == BITS_PER_UNIT)
	    {
	      error ("char-array initialized from wide string");
	      return error_mark_node;
	    }
	  if ((TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (string)))
	       == char_type_node)
	      && TYPE_PRECISION (typ1) != BITS_PER_UNIT)
	    {
	      error ("int-array initialized from non-wide string");
	      return error_mark_node;
	    }

	  if (pedantic
	      && typ1 != char_type_node
	      && typ1 != signed_char_type_node
	      && typ1 != unsigned_char_type_node)
	    pedwarn ("ANSI C++ forbids string initializer except for `char' elements");
	  TREE_TYPE (string) = type;
	  if (TYPE_DOMAIN (type) != 0
	      && TREE_CONSTANT (TYPE_SIZE (type)))
	    {
	      register int size
		= TREE_INT_CST_LOW (TYPE_SIZE (type));
	      size = (size + BITS_PER_UNIT - 1) / BITS_PER_UNIT;
	      /* In C it is ok to subtract 1 from the length of the string
		 because it's ok to ignore the terminating null char that is
		 counted in the length of the constant, but in C++ this would
		 be invalid.  */
	      if (size < TREE_STRING_LENGTH (string))
		pedwarn ("initializer-string for array of chars is too long");
	    }
	  return string;
	}
    }

  /* Handle scalar types, including conversions,
     and signature pointers and references.  */

  if (code == INTEGER_TYPE || code == REAL_TYPE || code == POINTER_TYPE
      || code == ENUMERAL_TYPE || code == REFERENCE_TYPE
      || code == BOOLEAN_TYPE
      || (code == RECORD_TYPE && ! raw_constructor
	  && (IS_SIGNATURE_POINTER (type) || IS_SIGNATURE_REFERENCE (type))))
    {
      if (raw_constructor)
	{
	  if (element == 0)
	    {
	      error ("initializer for scalar variable requires one element");
	      return error_mark_node;
	    }
	  init = element;
	}

      return convert_for_initialization (0, type, init, LOOKUP_NORMAL,
					 "initialization", NULL_TREE, 0);
    }

  /* Come here only for records and arrays (and unions with constructors).  */

  if (TYPE_SIZE (type) && ! TREE_CONSTANT (TYPE_SIZE (type)))
    {
      cp_error ("variable-sized object of type `%T' may not be initialized",
		type);
      return error_mark_node;
    }

  if (code == ARRAY_TYPE || code == RECORD_TYPE || code == UNION_TYPE)
    {
      if (raw_constructor)
	return process_init_constructor (type, init, (tree *)0);
      else if (TYPE_NEEDS_CONSTRUCTING (type))
	{
	  /* This can only be reached when caller is initializing
	     ARRAY_TYPE.  In that case, we don't want to convert
	     INIT to TYPE.  We will let `expand_vec_init' do it.  */
	  return init;
	}
      else if (tail != 0)
	{
	  *tail = old_tail_contents;
	  return process_init_constructor (type, 0, tail);
	}
      else if (flag_traditional)
	/* Traditionally one can say `char x[100] = 0;'.  */
	return process_init_constructor (type,
					 build_nt (CONSTRUCTOR, 0,
						   tree_cons (0, init, 0)),
					 0);
      if (code != ARRAY_TYPE)
	return convert_for_initialization (0, type, init, LOOKUP_NORMAL,
					   "initialization", NULL_TREE, 0);
    }

  error ("invalid initializer");
  return error_mark_node;
}

/* Process a constructor for a variable of type TYPE.
   The constructor elements may be specified either with INIT or with ELTS,
   only one of which should be non-null.

   If INIT is specified, it is a CONSTRUCTOR node which is specifically
   and solely for initializing this datum.

   If ELTS is specified, it is the address of a variable containing
   a list of expressions.  We take as many elements as we need
   from the head of the list and update the list.

   In the resulting constructor, TREE_CONSTANT is set if all elts are
   constant, and TREE_STATIC is set if, in addition, all elts are simple enough
   constants that the assembler and linker can compute them.  */

static tree
process_init_constructor (type, init, elts)
     tree type, init, *elts;
{
  register tree tail;
  /* List of the elements of the result constructor,
     in reverse order.  */
  register tree members = NULL;
  tree result;
  int allconstant = 1;
  int allsimple = 1;
  int erroneous = 0;

  /* Make TAIL be the list of elements to use for the initialization,
     no matter how the data was given to us.  */

  if (elts)
    {
      if (warn_missing_braces)
	warning ("aggregate has a partly bracketed initializer");
      tail = *elts;
    }
  else
    tail = CONSTRUCTOR_ELTS (init);

  /* Gobble as many elements as needed, and make a constructor or initial value
     for each element of this aggregate.  Chain them together in result.
     If there are too few, use 0 for each scalar ultimate component.  */

  if (TREE_CODE (type) == ARRAY_TYPE)
    {
      tree domain = TYPE_DOMAIN (type);
      register long len;
      register int i;

      if (domain)
	len = (TREE_INT_CST_LOW (TYPE_MAX_VALUE (domain))
	       - TREE_INT_CST_LOW (TYPE_MIN_VALUE (domain))
	       + 1);
      else
	len = -1;  /* Take as many as there are */

      for (i = 0; (len < 0 || i < len) && tail != 0; i++)
	{
	  register tree next1;

	  if (TREE_VALUE (tail) != 0)
	    {
	      tree tail1 = tail;
	      next1 = digest_init (TYPE_MAIN_VARIANT (TREE_TYPE (type)),
				   TREE_VALUE (tail), &tail1);
	      my_friendly_assert (tail1 == 0
				  || TREE_CODE (tail1) == TREE_LIST, 319);
	      if (tail == tail1 && len < 0)
		{
		  error ("non-empty initializer for array of empty elements");
		  /* Just ignore what we were supposed to use.  */
		  tail1 = NULL_TREE;
		}
	      tail = tail1;
	    }
	  else
	    {
	      next1 = error_mark_node;
	      tail = TREE_CHAIN (tail);
	    }

	  if (next1 == error_mark_node)
	    erroneous = 1;
	  else if (!TREE_CONSTANT (next1))
	    allconstant = 0;
	  else if (! initializer_constant_valid_p (next1))
	    allsimple = 0;
	  members = tree_cons (NULL_TREE, next1, members);
	}
    }
  if (TREE_CODE (type) == RECORD_TYPE)
    {
      register tree field;

      if (tail)
	{
	  if (TYPE_USES_VIRTUAL_BASECLASSES (type))
	    {
	      sorry ("initializer list for object of class with virtual baseclasses");
	      return error_mark_node;
	    }

	  if (TYPE_BINFO_BASETYPES (type))
	    {
	      sorry ("initializer list for object of class with baseclasses");
	      return error_mark_node;
	    }

	  if (TYPE_VIRTUAL_P (type))
	    {
	      sorry ("initializer list for object using virtual functions");
	      return error_mark_node;
	    }
	}

      for (field = TYPE_FIELDS (type); field && tail;
	   field = TREE_CHAIN (field))
	{
	  register tree next1;

	  if (! DECL_NAME (field))
	    {
	      members = tree_cons (field, integer_zero_node, members);
	      continue;
	    }

	  if (TREE_CODE (field) != FIELD_DECL)
	    continue;

	  if (TREE_VALUE (tail) != 0)
	    {
	      tree tail1 = tail;

	      next1 = digest_init (TREE_TYPE (field),
				   TREE_VALUE (tail), &tail1);
	      my_friendly_assert (tail1 == 0
				  || TREE_CODE (tail1) == TREE_LIST, 320);
	      tail = tail1;
	    }
	  else
	    {
	      next1 = error_mark_node;
	      tail = TREE_CHAIN (tail);
	    }

	  if (next1 == error_mark_node)
	    erroneous = 1;
	  else if (!TREE_CONSTANT (next1))
	    allconstant = 0;
	  else if (! initializer_constant_valid_p (next1))
	    allsimple = 0;
	  members = tree_cons (field, next1, members);
	}
      for (; field; field = TREE_CHAIN (field))
	{
	  if (TREE_CODE (field) != FIELD_DECL)
	    continue;

	  /* Does this field have a default initialization?  */
	  if (DECL_INITIAL (field))
	    {
	      register tree next1 = DECL_INITIAL (field);
	      if (TREE_CODE (next1) == ERROR_MARK)
		erroneous = 1;
	      else if (!TREE_CONSTANT (next1))
		allconstant = 0;
	      else if (! initializer_constant_valid_p (next1))
		allsimple = 0;
	      members = tree_cons (field, next1, members);
	    }
	  else if (TREE_READONLY (field))
	    error ("uninitialized const member `%s'",
		   IDENTIFIER_POINTER (DECL_NAME (field)));
	  else if (TYPE_LANG_SPECIFIC (TREE_TYPE (field))
		   && CLASSTYPE_READONLY_FIELDS_NEED_INIT (TREE_TYPE (field)))
	    error ("member `%s' with uninitialized const fields",
		   IDENTIFIER_POINTER (DECL_NAME (field)));
	  else if (TREE_CODE (TREE_TYPE (field)) == REFERENCE_TYPE)
	    error ("member `%s' is uninitialized reference",
		   IDENTIFIER_POINTER (DECL_NAME (field)));
	}
    }

  if (TREE_CODE (type) == UNION_TYPE)
    {
      register tree field = TYPE_FIELDS (type);
      register tree next1;

      /* Find the first named field.  ANSI decided in September 1990
	 that only named fields count here.  */
      while (field && DECL_NAME (field) == 0)
	field = TREE_CHAIN (field);

      /* If this element specifies a field, initialize via that field.  */
      if (TREE_PURPOSE (tail) != NULL_TREE)
	{
	  int win = 0;

	  if (TREE_CODE (TREE_PURPOSE (tail)) == FIELD_DECL)
	    /* Handle the case of a call by build_c_cast.  */
	    field = TREE_PURPOSE (tail), win = 1;
	  else if (TREE_CODE (TREE_PURPOSE (tail)) != IDENTIFIER_NODE)
	    error ("index value instead of field name in union initializer");
	  else
	    {
	      tree temp;
	      for (temp = TYPE_FIELDS (type);
		   temp;
		   temp = TREE_CHAIN (temp))
		if (DECL_NAME (temp) == TREE_PURPOSE (tail))
		  break;
	      if (temp)
		field = temp, win = 1;
	      else
		error ("no field `%s' in union being initialized",
		       IDENTIFIER_POINTER (TREE_PURPOSE (tail)));
	    }
	  if (!win)
	    TREE_VALUE (tail) = error_mark_node;
	}
      else if (field == 0)
	{
	  cp_error ("union `%T' with no named members cannot be initialized",
		    type);
	  TREE_VALUE (tail) = error_mark_node;
	}

      if (TREE_VALUE (tail) != 0)
	{
	  tree tail1 = tail;

	  next1 = digest_init (TREE_TYPE (field),
			       TREE_VALUE (tail), &tail1);
	  if (tail1 != 0 && TREE_CODE (tail1) != TREE_LIST)
	    my_friendly_abort (357);
	  tail = tail1;
	}
      else
	{
	  next1 = error_mark_node;
	  tail = TREE_CHAIN (tail);
	}

      if (next1 == error_mark_node)
	erroneous = 1;
      else if (!TREE_CONSTANT (next1))
	allconstant = 0;
      else if (initializer_constant_valid_p (next1) == 0)
	allsimple = 0;
      members = tree_cons (field, next1, members);
    }

  /* If arguments were specified as a list, just remove the ones we used.  */
  if (elts)
    *elts = tail;
  /* If arguments were specified as a constructor,
     complain unless we used all the elements of the constructor.  */
  else if (tail)
    pedwarn ("excess elements in aggregate initializer");

  if (erroneous)
    return error_mark_node;

  result = build (CONSTRUCTOR, type, NULL_TREE, nreverse (members));
  if (init)
    TREE_HAS_CONSTRUCTOR (result) = TREE_HAS_CONSTRUCTOR (init);
  if (allconstant) TREE_CONSTANT (result) = 1;
  if (allconstant && allsimple) TREE_STATIC (result) = 1;
  return result;
}

/* Given a structure or union value DATUM, construct and return
   the structure or union component which results from narrowing
   that value by the types specified in TYPES.  For example, given the
   hierarchy

   class L { int ii; };
   class A : L { ... };
   class B : L { ... };
   class C : A, B { ... };

   and the declaration

   C x;

   then the expression

   x::C::A::L::ii refers to the ii member of the L part of
   of A part of the C object named by X.  In this case,
   DATUM would be x, and TYPES would be a SCOPE_REF consisting of

	SCOPE_REF
		SCOPE_REF
			C	A
		L

   The last entry in the SCOPE_REF is always an IDENTIFIER_NODE.

*/

tree
build_scoped_ref (datum, types)
     tree datum;
     tree types;
{
  tree ref;
  tree type = TREE_TYPE (datum);

  if (datum == error_mark_node)
    return error_mark_node;

  if (TREE_CODE (type) == REFERENCE_TYPE)
    type = TREE_TYPE (type);

  type = TYPE_MAIN_VARIANT (type);

  if (TREE_CODE (types) == SCOPE_REF)
    {
      /* We have some work to do.  */
      struct type_chain
	{ tree type; struct type_chain *next; }
      *chain = NULL, *head = NULL, scratch;
      ref = build_unary_op (ADDR_EXPR, datum, 0);
      while (TREE_CODE (types) == SCOPE_REF)
	{
	  tree t = TREE_OPERAND (types, 1);
	  if (is_aggr_typedef (t, 1))
	    {
	      head = (struct type_chain *)alloca (sizeof (struct type_chain));
	      head->type = IDENTIFIER_TYPE_VALUE (t);
	      head->next = chain;
	      chain = head;
	      types = TREE_OPERAND (types, 0);
	    }
	  else return error_mark_node;
	}
      if (! is_aggr_typedef (types, 1))
	return error_mark_node;

      head = &scratch;
      head->type = IDENTIFIER_TYPE_VALUE (types);
      head->next = chain;
      chain = head;
      while (chain)
	{
	  tree binfo = chain->type;
	  type = TREE_TYPE (TREE_TYPE (ref));
	  if (binfo != TYPE_BINFO (type))
	    {
	      binfo = get_binfo (binfo, type, 1);
	      if (binfo == error_mark_node)
		return error_mark_node;
	      if (binfo == 0)
		return error_not_base_type (chain->type, type);
	      ref = convert_pointer_to (binfo, ref);
	    }
	  chain = chain->next;
	}
      return build_indirect_ref (ref, "(compiler error in build_scoped_ref)");
    }

  /* This is an easy conversion.  */
  if (is_aggr_typedef (types, 1))
    {
      tree binfo = TYPE_BINFO (IDENTIFIER_TYPE_VALUE (types));
      if (binfo != TYPE_BINFO (type))
	{
	  binfo = get_binfo (binfo, type, 1);
	  if (binfo == error_mark_node)
	    return error_mark_node;
	  if (binfo == 0)
	    return error_not_base_type (IDENTIFIER_TYPE_VALUE (types), type);
	}

      switch (TREE_CODE (datum))
	{
	case NOP_EXPR:
	case CONVERT_EXPR:
	case FLOAT_EXPR:
	case FIX_TRUNC_EXPR:
	case FIX_FLOOR_EXPR:
	case FIX_ROUND_EXPR:
	case FIX_CEIL_EXPR:
	  ref = convert_pointer_to (binfo,
				    build_unary_op (ADDR_EXPR, TREE_OPERAND (datum, 0), 0));
	  break;
	default:
	  ref = convert_pointer_to (binfo,
				    build_unary_op (ADDR_EXPR, datum, 0));
	}
      return build_indirect_ref (ref, "(compiler error in build_scoped_ref)");
    }
  return error_mark_node;
}

/* Build a reference to an object specified by the C++ `->' operator.
   Usually this just involves dereferencing the object, but if the
   `->' operator is overloaded, then such overloads must be
   performed until an object which does not have the `->' operator
   overloaded is found.  An error is reported when circular pointer
   delegation is detected.  */
tree
build_x_arrow (datum)
     tree datum;
{
  tree types_memoized = NULL_TREE;
  register tree rval = datum;
  tree type = TREE_TYPE (rval);
  tree last_rval;

  if (type == error_mark_node)
    return error_mark_node;

  if (TREE_CODE (rval) == OFFSET_REF)
    {
      rval = resolve_offset_ref (datum);
      type = TREE_TYPE (rval);
    }

  if (TREE_CODE (type) == REFERENCE_TYPE)
    {
      rval = convert_from_reference (rval);
      type = TREE_TYPE (rval);
    }

  if (IS_AGGR_TYPE (type) && TYPE_OVERLOADS_ARROW (type))
    {
      while ((rval = build_opfncall (COMPONENT_REF, LOOKUP_NORMAL, rval, NULL_TREE, NULL_TREE)))
	{
	  if (rval == error_mark_node)
	    return error_mark_node;

	  if (value_member (TREE_TYPE (rval), types_memoized))
	    {
	      error ("circular pointer delegation detected");
	      return error_mark_node;
	    }
	  else
	    {
	      types_memoized = tree_cons (NULL_TREE, TREE_TYPE (rval),
					  types_memoized);
	    }
	  last_rval = rval;
	}     
      if (TREE_CODE (TREE_TYPE (last_rval)) == REFERENCE_TYPE)
	last_rval = convert_from_reference (last_rval);
    }
  else
    last_rval = default_conversion (rval);

  /* Signature pointers are not dereferenced.  */
  if (TYPE_LANG_SPECIFIC (TREE_TYPE (last_rval))
      && IS_SIGNATURE_POINTER (TREE_TYPE (last_rval)))
    return last_rval;

  if (TREE_CODE (TREE_TYPE (last_rval)) == POINTER_TYPE)
    return build_indirect_ref (last_rval, NULL_PTR);

  if (types_memoized)
    error ("result of `operator->()' yields non-pointer result");
  else
    error ("base operand of `->' is not a pointer");
  return error_mark_node;
}

/* Make an expression to refer to the COMPONENT field of
   structure or union value DATUM.  COMPONENT is an arbitrary
   expression.  DATUM has not already been checked out to be of
   aggregate type.

   For C++, COMPONENT may be a TREE_LIST.  This happens when we must
   return an object of member type to a method of the current class,
   but there is not yet enough typing information to know which one.
   As a special case, if there is only one method by that name,
   it is returned.  Otherwise we return an expression which other
   routines will have to know how to deal with later.  */
tree
build_m_component_ref (datum, component)
     tree datum, component;
{
  tree type;
  tree objtype = TREE_TYPE (datum);
  tree rettype;

  if (TYPE_PTRMEMFUNC_P (TREE_TYPE (component)))
    {
      type = TREE_TYPE (TYPE_PTRMEMFUNC_FN_TYPE (TREE_TYPE (component)));
      rettype = type;
    }
  else
    {
      component = build_indirect_ref (component, NULL_PTR);
      type = TREE_TYPE (component);
      rettype = TREE_TYPE (TREE_TYPE (component));
    }

  if (datum == error_mark_node || component == error_mark_node)
    return error_mark_node;

  if (TREE_CODE (type) != OFFSET_TYPE && TREE_CODE (type) != METHOD_TYPE)
    {
      cp_error ("`%E' cannot be used as a member pointer, since it is of type `%T'", component, type);
      return error_mark_node;
    }

  if (TREE_CODE (objtype) == REFERENCE_TYPE)
    objtype = TREE_TYPE (objtype);
  objtype = TYPE_MAIN_VARIANT (objtype);

  if (! IS_AGGR_TYPE (objtype))
    {
      cp_error ("cannot apply member pointer `%E' to `%E'", component, datum);
      cp_error ("which is of non-aggregate type `%T'", objtype);
      return error_mark_node;
    }
  
  if (! comptypes (TYPE_METHOD_BASETYPE (type), objtype, 0))
    {
      cp_error ("member type `%T::' incompatible with object type `%T'",
		TYPE_METHOD_BASETYPE (type), objtype);
      return error_mark_node;
    }

  return build (OFFSET_REF, rettype, datum, component);
}

/* Return a tree node for the expression TYPENAME '(' PARMS ')'.

   Because we cannot tell whether this construct is really a call to a
   constructor or a request for a type conversion, we try both, and
   report any ambiguities we find.  */
tree
build_functional_cast (exp, parms)
     tree exp;
     tree parms;
{
  /* This is either a call to a constructor,
     or a C cast in C++'s `functional' notation.  */
  tree type, name = NULL_TREE;
  tree expr_as_ctor = NULL_TREE;

  if (exp == error_mark_node || parms == error_mark_node)
    return error_mark_node;

  if (TREE_CODE (exp) == IDENTIFIER_NODE)
    {
      name = exp;

      if (IDENTIFIER_HAS_TYPE_VALUE (exp))
	/* Either an enum or an aggregate type.  */
	type = IDENTIFIER_TYPE_VALUE (exp);
      else
	{
	  type = lookup_name (exp, 1);
	  if (!type || TREE_CODE (type) != TYPE_DECL)
	    {
	      cp_error ("`%T' fails to be a typedef or built-in type", name);
	      return error_mark_node;
	    }
	  type = TREE_TYPE (type);
	}
    }
  else
    type = exp;

  if (IS_SIGNATURE (type))
    {
      error ("signature type not allowed in cast or constructor expression");
      return error_mark_node;
    }

  /* Prepare to evaluate as a call to a constructor.  If this expression
     is actually used, for example,
	 
     return X (arg1, arg2, ...);
	 
     then the slot being initialized will be filled in.  */

  if (name == NULL_TREE)
    {
      name = TYPE_NAME (type);
      if (TREE_CODE (name) == TYPE_DECL)
	name = DECL_NAME (name);
    }

  if (! IS_AGGR_TYPE (type))
    {
      /* this must build a C cast */
      if (parms == NULL_TREE)
	return build1 (NOP_EXPR, type, integer_zero_node);
      else if (TREE_CHAIN (parms) != NULL_TREE)
	{
	  pedwarn ("initializer list being treated as compound expression");
	  parms = build_compound_expr (parms);
	}
      return build_c_cast (type, parms);
    }

  if (TYPE_SIZE (type) == NULL_TREE)
    {
      cp_error ("type `%T' is not yet defined", type);
      return error_mark_node;
    }

  if (parms && TREE_CHAIN (parms) == NULL_TREE)
    return build_c_cast (type, parms);

  expr_as_ctor = build_method_call (NULL_TREE, name, parms,
				    NULL_TREE, LOOKUP_NORMAL);

  if (expr_as_ctor == error_mark_node)
    return error_mark_node;

  if (current_function_decl)
    return build_cplus_new (type, expr_as_ctor, 1);

  {
    register tree parm = TREE_OPERAND (expr_as_ctor, 1);

    /* Initializers for static variables and parameters have
       to handle doing the initialization and cleanup themselves.  */
    my_friendly_assert (TREE_CODE (expr_as_ctor) == CALL_EXPR, 322);
#if 0
    /* The following assertion fails in cases where we are initializing
       a static member variable of a particular instance of a template
       class with a call to a constructor of the given instance, as in:
       
       TMPL<int> object = TMPL<int>();
       
       Curiously, the assertion does not fail if we do the same thing
       for a static member of a non-template class, as in:
       
       T object = T();
       
       I can't see why we should care here whether or not the initializer
       expression involves a call to `new', so for the time being, it
       seems best to just avoid doing this assertion.  */
    my_friendly_assert (TREE_CALLS_NEW (TREE_VALUE (parm)), 323);
#endif
    TREE_VALUE (parm) = NULL_TREE;
    expr_as_ctor = build_indirect_ref (expr_as_ctor, NULL_PTR);
    TREE_HAS_CONSTRUCTOR (expr_as_ctor) = 1;
  }
  return expr_as_ctor;
}

/* Return the character string for the name that encodes the
   enumeral value VALUE in the domain TYPE.  */
char *
enum_name_string (value, type)
     tree value;
     tree type;
{
  register tree values = TYPE_VALUES (type);
  register HOST_WIDE_INT intval = TREE_INT_CST_LOW (value);

  my_friendly_assert (TREE_CODE (type) == ENUMERAL_TYPE, 324);
  while (values
	 && TREE_INT_CST_LOW (TREE_VALUE (values)) != intval)
    values = TREE_CHAIN (values);
  if (values == NULL_TREE)
    {
      char *buf = (char *)oballoc (16 + TYPE_NAME_LENGTH (type));

      /* Value must have been cast.  */
      sprintf (buf, "(enum %s)%d",
	       TYPE_NAME_STRING (type), intval);
      return buf;
    }
  return IDENTIFIER_POINTER (TREE_PURPOSE (values));
}

#if 0
/* Print out a language-specific error message for
   (Pascal) case or (C) switch statements.
   CODE tells what sort of message to print. 
   TYPE is the type of the switch index expression.
   NEW is the new value that we were trying to add.
   OLD is the old value that stopped us from adding it.  */
void
report_case_error (code, type, new_value, old_value)
     int code;
     tree type;
     tree new_value, old_value;
{
  if (code == 1)
    {
      if (new_value)
	error ("case label not within a switch statement");
      else
	error ("default label not within a switch statement");
    }
  else if (code == 2)
    {
      if (new_value == 0)
	{
	  error ("multiple default labels in one switch");
	  return;
	}
      if (TREE_CODE (new_value) == RANGE_EXPR)
	if (TREE_CODE (old_value) == RANGE_EXPR)
	  {
	    char *buf = (char *)alloca (4 * (8 + TYPE_NAME_LENGTH (type)));
	    if (TREE_CODE (type) == ENUMERAL_TYPE)
	      sprintf (buf, "overlapping ranges [%s..%s], [%s..%s] in case expression",
		       enum_name_string (TREE_OPERAND (new_value, 0), type),
		       enum_name_string (TREE_OPERAND (new_value, 1), type),
		       enum_name_string (TREE_OPERAND (old_value, 0), type),
		       enum_name_string (TREE_OPERAND (old_value, 1), type));
	    else
	      sprintf (buf, "overlapping ranges [%d..%d], [%d..%d] in case expression",
		       TREE_INT_CST_LOW (TREE_OPERAND (new_value, 0)),
		       TREE_INT_CST_LOW (TREE_OPERAND (new_value, 1)),
		       TREE_INT_CST_LOW (TREE_OPERAND (old_value, 0)),
		       TREE_INT_CST_LOW (TREE_OPERAND (old_value, 1)));
	    error (buf);
	  }
	else
	  {
	    char *buf = (char *)alloca (4 * (8 + TYPE_NAME_LENGTH (type)));
	    if (TREE_CODE (type) == ENUMERAL_TYPE)
	      sprintf (buf, "range [%s..%s] includes element `%s' in case expression",
		       enum_name_string (TREE_OPERAND (new_value, 0), type),
		       enum_name_string (TREE_OPERAND (new_value, 1), type),
		       enum_name_string (old_value, type));
	    else
	      sprintf (buf, "range [%d..%d] includes (%d) in case expression",
		       TREE_INT_CST_LOW (TREE_OPERAND (new_value, 0)),
		       TREE_INT_CST_LOW (TREE_OPERAND (new_value, 1)),
		       TREE_INT_CST_LOW (old_value));
	    error (buf);
	  }
      else if (TREE_CODE (old_value) == RANGE_EXPR)
	{
	  char *buf = (char *)alloca (4 * (8 + TYPE_NAME_LENGTH (type)));
	  if (TREE_CODE (type) == ENUMERAL_TYPE)
	    sprintf (buf, "range [%s..%s] includes element `%s' in case expression",
		     enum_name_string (TREE_OPERAND (old_value, 0), type),
		     enum_name_string (TREE_OPERAND (old_value, 1), type),
		     enum_name_string (new_value, type));
	  else
	    sprintf (buf, "range [%d..%d] includes (%d) in case expression",
		     TREE_INT_CST_LOW (TREE_OPERAND (old_value, 0)),
		     TREE_INT_CST_LOW (TREE_OPERAND (old_value, 1)),
		     TREE_INT_CST_LOW (new_value));
	  error (buf);
	}
      else
	{
	  if (TREE_CODE (type) == ENUMERAL_TYPE)
	    error ("duplicate label `%s' in switch statement",
		   enum_name_string (new_value, type));
	  else
	    error ("duplicate label (%d) in switch statement",
		   TREE_INT_CST_LOW (new_value));
	}
    }
  else if (code == 3)
    {
      if (TREE_CODE (type) == ENUMERAL_TYPE)
	warning ("case value out of range for enum %s",
		 TYPE_NAME_STRING (type));
      else
	warning ("case value out of range");
    }
  else if (code == 4)
    {
      if (TREE_CODE (type) == ENUMERAL_TYPE)
	error ("range values `%s' and `%s' reversed",
	       enum_name_string (new_value, type),
	       enum_name_string (old_value, type));
      else
	error ("range values reversed");
    }
}
#endif
