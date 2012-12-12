/* Language-level data type conversion for GNU C++.
   Copyright (C) 1987, 1988, 1992, 1993 Free Software Foundation, Inc.
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


/* This file contains the functions for converting C expressions
   to different data types.  The only entry point is `convert'.
   Every language front end must have a `convert' function
   but what kind of conversions it does will depend on the language.  */

#include "config.h"
#include "tree.h"
#include "flags.h"
#include "cp-tree.h"
#include "class.h"
#include "convert.h"

#undef NULL
#define NULL (char *)0

/* Change of width--truncation and extension of integers or reals--
   is represented with NOP_EXPR.  Proper functioning of many things
   assumes that no other conversions can be NOP_EXPRs.

   Conversion between integer and pointer is represented with CONVERT_EXPR.
   Converting integer to real uses FLOAT_EXPR
   and real to integer uses FIX_TRUNC_EXPR.

   Here is a list of all the functions that assume that widening and
   narrowing is always done with a NOP_EXPR:
     In convert.c, convert_to_integer.
     In c-typeck.c, build_binary_op_nodefault (boolean ops),
        and truthvalue_conversion.
     In expr.c: expand_expr, for operands of a MULT_EXPR.
     In fold-const.c: fold.
     In tree.c: get_narrower and get_unwidened.

   C++: in multiple-inheritance, converting between pointers may involve
   adjusting them by a delta stored within the class definition.  */

/* Subroutines of `convert'.  */

/* Build a thunk.  What it is, is an entry point that when called will
   adjust the this pointer (the first argument) by offset, and then
   goto the real address of the function given by REAL_ADDR that we
   would like called.  What we return is the address of the thunk.  */
static tree
build_thunk (offset, real_addr)
     tree offset, real_addr;
{
  if (TREE_CODE (real_addr) != ADDR_EXPR
      || TREE_CODE (TREE_OPERAND (real_addr, 0)) != FUNCTION_DECL)
    {
      sorry ("MI pointer to member conversion too complex");
      return error_mark_node;
    }
  sorry ("MI pointer to member conversion too complex");
  return error_mark_node;
}

/* Convert a `pointer to member' (POINTER_TYPE to METHOD_TYPE) into
   another `pointer to method'.  This may involved the creation of
   a thunk to handle the this offset calculation.  */
static tree
convert_fn_ptr (type, expr)
     tree type, expr;
{
  tree binfo = get_binfo (TYPE_METHOD_BASETYPE (TREE_TYPE (TREE_TYPE (expr))),
			  TYPE_METHOD_BASETYPE (TREE_TYPE (type)),
			  1);
  if (binfo == error_mark_node)
    {
      error ("  in pointer to member conversion");
      return error_mark_node;
    }
  if (binfo == NULL_TREE)
    {
      /* ARM 4.8 restriction. */
      error ("invalid pointer to member conversion");
      return error_mark_node;
    }
  if (BINFO_OFFSET_ZEROP (binfo))
    return build1 (NOP_EXPR, type, expr);
  return build1 (NOP_EXPR, type, build_thunk (BINFO_OFFSET (binfo), expr));
}

/* if converting pointer to pointer
     if dealing with classes, check for derived->base or vice versa
     else if dealing with method pointers, delegate
     else convert blindly
   else if converting class, pass off to build_type_conversion
   else try C-style pointer conversion  */
static tree
cp_convert_to_pointer (type, expr)
     tree type, expr;
{
  register tree intype = TREE_TYPE (expr);
  register enum tree_code form = TREE_CODE (intype);
  
  if (form == POINTER_TYPE)
    {
      intype = TYPE_MAIN_VARIANT (intype);

      if (TYPE_MAIN_VARIANT (type) != intype
	  && TREE_CODE (TREE_TYPE (type)) == RECORD_TYPE
	  && TREE_CODE (TREE_TYPE (intype)) == RECORD_TYPE)
	{
	  enum tree_code code = PLUS_EXPR;
	  tree binfo = get_binfo (TREE_TYPE (type), TREE_TYPE (intype), 1);
	  if (binfo == error_mark_node)
	    return error_mark_node;
	  if (binfo == NULL_TREE)
	    {
	      binfo = get_binfo (TREE_TYPE (intype), TREE_TYPE (type), 1);
	      if (binfo == error_mark_node)
		return error_mark_node;
	      code = MINUS_EXPR;
	    }
	  if (binfo)
	    {
	      if (TYPE_USES_VIRTUAL_BASECLASSES (TREE_TYPE (type))
		  || TYPE_USES_VIRTUAL_BASECLASSES (TREE_TYPE (intype))
		  || ! BINFO_OFFSET_ZEROP (binfo))
		{
		  /* Need to get the path we took.  */
		  tree path;

		  if (code == PLUS_EXPR)
		    get_base_distance (TREE_TYPE (type), TREE_TYPE (intype), 0, &path);
		  else
		    get_base_distance (TREE_TYPE (intype), TREE_TYPE (type), 0, &path);
		  return build_vbase_path (code, type, expr, path, 0);
		}
	    }
	}
      if (TYPE_MAIN_VARIANT (type) != intype
	  && TREE_CODE (TREE_TYPE (type)) == METHOD_TYPE
	  && TREE_CODE (type) == POINTER_TYPE
	  && TREE_CODE (TREE_TYPE (intype)) == METHOD_TYPE)
	return convert_fn_ptr (type, expr);

      return build1 (NOP_EXPR, type, expr);
    }

  my_friendly_assert (form != OFFSET_TYPE, 186);

  if (TYPE_LANG_SPECIFIC (intype)
      && (IS_SIGNATURE_POINTER (intype) || IS_SIGNATURE_REFERENCE (intype)))
    return convert_to_pointer (type, build_optr_ref (expr));

  if (IS_AGGR_TYPE (intype))
    {
      tree rval;
      rval = build_type_conversion (CONVERT_EXPR, type, expr, 1);
      if (rval)
	{
	  if (rval == error_mark_node)
	    cp_error ("conversion of `%E' from `%T' to `%T' is ambiguous",
		      expr, intype, type);
	  return rval;
	}
    }

  if (integer_zerop (expr))
    {
      if (type == TREE_TYPE (null_pointer_node))
	return null_pointer_node;
      expr = build_int_2 (0, 0);
      TREE_TYPE (expr) = type;
      return expr;
    }

  if (INTEGRAL_CODE_P (form))
    {
      if (type_precision (intype) == POINTER_SIZE)
	return build1 (CONVERT_EXPR, type, expr);
      expr = convert (type_for_size (POINTER_SIZE, 0), expr);
      /* Modes may be different but sizes should be the same.  */
      if (GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (expr)))
	  != GET_MODE_SIZE (TYPE_MODE (type)))
	/* There is supposed to be some integral type
	   that is the same width as a pointer.  */
	abort ();
      return convert_to_pointer (type, expr);
    }

  cp_error ("cannot convert `%E' from type `%T' to type `%T'",
	    expr, intype, type);
  return error_mark_node;
}

/* Like convert, except permit conversions to take place which
   are not normally allowed due to access restrictions
   (such as conversion from sub-type to private super-type).  */
static tree
convert_to_pointer_force (type, expr)
     tree type, expr;
{
  register tree intype = TREE_TYPE (expr);
  register enum tree_code form = TREE_CODE (intype);
  
  if (integer_zerop (expr))
    {
      if (type == TREE_TYPE (null_pointer_node))
	return null_pointer_node;
      expr = build_int_2 (0, 0);
      TREE_TYPE (expr) = type;
      return expr;
    }

  /* Convert signature pointer/reference to `void *' first.  */
  if (form == RECORD_TYPE
      && (IS_SIGNATURE_POINTER (intype) || IS_SIGNATURE_REFERENCE (intype)))
    {
      expr = build_optr_ref (expr);
      intype = TREE_TYPE (expr);
      form = TREE_CODE (intype);
    }

  if (form == POINTER_TYPE)
    {
      intype = TYPE_MAIN_VARIANT (intype);

      if (TYPE_MAIN_VARIANT (type) != intype
	  && TREE_CODE (TREE_TYPE (type)) == RECORD_TYPE
	  && TREE_CODE (TREE_TYPE (intype)) == RECORD_TYPE)
	{
	  enum tree_code code = PLUS_EXPR;
	  tree path;
	  int distance = get_base_distance (TREE_TYPE (type),
					    TREE_TYPE (intype), 0, &path);
	  if (distance == -2)
	    {
	    ambig:
	      cp_error ("type `%T' is ambiguous baseclass of `%s'", TREE_TYPE (type),
				    TYPE_NAME_STRING (TREE_TYPE (intype)));
	      return error_mark_node;
	    }
	  if (distance == -1)
	    {
	      distance = get_base_distance (TREE_TYPE (intype),
					    TREE_TYPE (type), 0, &path);
	      if (distance == -2)
		goto ambig;
	      if (distance < 0)
		/* Doesn't need any special help from us.  */
		return build1 (NOP_EXPR, type, expr);

	      code = MINUS_EXPR;
	    }
	  return build_vbase_path (code, type, expr, path, 0);
	}
      return build1 (NOP_EXPR, type, expr);
    }

  return cp_convert_to_pointer (type, expr);
}

/* We are passing something to a function which requires a reference.
   The type we are interested in is in TYPE. The initial
   value we have to begin with is in ARG.

   FLAGS controls how we manage access checking.
   CHECKCONST controls if we report error messages on const subversion.  */
static tree
build_up_reference (type, arg, flags, checkconst)
     tree type, arg;
     int flags, checkconst;
{
  tree rval, targ;
  int literal_flag = 0;
  tree argtype = TREE_TYPE (arg);
  tree target_type = TREE_TYPE (type);
  tree binfo = NULL_TREE;

  my_friendly_assert (TREE_CODE (type) == REFERENCE_TYPE, 187);
  if ((flags & LOOKUP_PROTECT)
      && TYPE_MAIN_VARIANT (argtype) != TYPE_MAIN_VARIANT (target_type)
      && IS_AGGR_TYPE (argtype)
      && IS_AGGR_TYPE (target_type))
    {
      binfo = get_binfo (target_type, argtype, 1);
      if (binfo == error_mark_node)
	return error_mark_node;
      if (binfo == NULL_TREE)
	return error_not_base_type (target_type, argtype);
    }

  /* Pass along const and volatile down into the type. */
  if (TYPE_READONLY (type) || TYPE_VOLATILE (type))
    target_type = cp_build_type_variant (target_type, TYPE_READONLY (type),
					TYPE_VOLATILE (type));
  targ = arg;
  if (TREE_CODE (targ) == SAVE_EXPR)
    targ = TREE_OPERAND (targ, 0);

  switch (TREE_CODE (targ))
    {
    case INDIRECT_REF:
      /* This is a call to a constructor which did not know what it was
	 initializing until now: it needs to initialize a temporary.  */
      if (TREE_HAS_CONSTRUCTOR (targ))
	{
	  tree temp = build_cplus_new (argtype, TREE_OPERAND (targ, 0), 1);
	  TREE_HAS_CONSTRUCTOR (targ) = 0;
	  return build_up_reference (type, temp, flags, 1);
	}
      /* Let &* cancel out to simplify resulting code.
         Also, throw away intervening NOP_EXPRs.  */
      arg = TREE_OPERAND (targ, 0);
      if (TREE_CODE (arg) == NOP_EXPR || TREE_CODE (arg) == NON_LVALUE_EXPR
	  || (TREE_CODE (arg) == CONVERT_EXPR && TREE_REFERENCE_EXPR (arg)))
	arg = TREE_OPERAND (arg, 0);

      /* in doing a &*, we have to get rid of the const'ness on the pointer
	 value.  Haven't thought about volatile here.  Pointers come to mind
	 here.  */
      if (TREE_READONLY (arg))
	{
	  arg = copy_node (arg);
	  TREE_READONLY (arg) = 0;
	}

      rval = build1 (CONVERT_EXPR, type, arg);
      TREE_REFERENCE_EXPR (rval) = 1;

      /* propagate the const flag on something like:

	 class Base {
	 public:
	   int foo;
	 };

      class Derived : public Base {
      public:
	int bar;
      };

      void func(Base&);

      void func2(const Derived& d) {
	func(d);
      }

        on the d parameter.  The below could have been avoided, if the flags
        were down in the tree, not sure why they are not.  (mrs) */
      /* The below code may have to be propagated to other parts of this
	 switch.  */
      if (TREE_READONLY (targ) && !TREE_READONLY (arg)
	  && (TREE_CODE (arg) == PARM_DECL || TREE_CODE (arg) == VAR_DECL)
	  && TREE_CODE (TREE_TYPE (arg)) == REFERENCE_TYPE
	  && (TYPE_READONLY (target_type) && checkconst))
	{
	  arg = copy_node (arg);
	  TREE_READONLY (arg) = TREE_READONLY (targ);
	}
      literal_flag = TREE_CONSTANT (arg);

      goto done;

      /* Get this out of a register if we happened to be in one by accident.
	 Also, build up references to non-lvalues it we must.  */
      /* For &x[y], return (&) x+y */
    case ARRAY_REF:
      if (mark_addressable (TREE_OPERAND (targ, 0)) == 0)
	return error_mark_node;
      rval = build_binary_op (PLUS_EXPR, TREE_OPERAND (targ, 0),
			      TREE_OPERAND (targ, 1), 1);
      TREE_TYPE (rval) = type;
      if (TREE_CONSTANT (TREE_OPERAND (targ, 1))
	  && staticp (TREE_OPERAND (targ, 0)))
	TREE_CONSTANT (rval) = 1;
      goto done;

    case SCOPE_REF:
      /* Could be a reference to a static member.  */
      {
	tree field = TREE_OPERAND (targ, 1);
	if (TREE_STATIC (field))
	  {
	    rval = build1 (ADDR_EXPR, type, field);
	    literal_flag = 1;
	    goto done;
	  }
      }

      /* We should have farmed out member pointers above.  */
      my_friendly_abort (188);

    case COMPONENT_REF:
      rval = build_component_addr (targ, build_pointer_type (argtype),
				   "attempt to make a reference to bit-field structure member `%s'");
      TREE_TYPE (rval) = type;
      literal_flag = staticp (TREE_OPERAND (targ, 0));

      goto done;

      /* Anything not already handled and not a true memory reference
	 needs to have a reference built up.  Do so silently for
	 things like integers and return values from function,
	 but complain if we need a reference to something declared
	 as `register'.  */

    case RESULT_DECL:
      if (staticp (targ))
	literal_flag = 1;
      TREE_ADDRESSABLE (targ) = 1;
      put_var_into_stack (targ);
      break;

    case PARM_DECL:
#if 0
      if (targ == current_class_decl)
	{
	  error ("address of `this' not available");
/* #if 0 */	  
	  /* This code makes the following core dump the compiler on a sun4,
	     if the code below is used.

	     class e_decl;
	     class a_decl;
	     typedef a_decl* a_ref;

	     class a_s {
	     public:
	       a_s();
	       void* append(a_ref& item);
	     };
	     class a_decl {
	     public:
	       a_decl (e_decl *parent);
	       a_s  generic_s;
	       a_s  decls;
	       e_decl* parent;
	     };

	     class e_decl {
	     public:
	       e_decl();
	       a_s implementations;
	     };

	     void foobar(void *);

	     a_decl::a_decl(e_decl *parent) {
	       parent->implementations.append(this);
	     }
	   */

	  TREE_ADDRESSABLE (targ) = 1; /* so compiler doesn't die later */
	  put_var_into_stack (targ);
	  break;
/* #else */
	  return error_mark_node;
/* #endif */	  
	}
#endif
      /* Fall through.  */
    case VAR_DECL:
    case CONST_DECL:
      if (DECL_REGISTER (targ) && !TREE_ADDRESSABLE (targ)
	  && !DECL_ARTIFICIAL (targ))
	cp_warning ("address needed to build reference for `%D', which is declared `register'",
		    targ);
      else if (staticp (targ))
	literal_flag = 1;

      TREE_ADDRESSABLE (targ) = 1;
      put_var_into_stack (targ);
      break;

    case COMPOUND_EXPR:
      {
	tree real_reference = build_up_reference (type, TREE_OPERAND (targ, 1),
						  LOOKUP_PROTECT, checkconst);
	rval = build (COMPOUND_EXPR, type, TREE_OPERAND (targ, 0), real_reference);
	TREE_CONSTANT (rval) = staticp (TREE_OPERAND (targ, 1));
	return rval;
      }

    case PREINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case MODIFY_EXPR:
    case INIT_EXPR:
      {
	tree real_reference = build_up_reference (type, TREE_OPERAND (targ, 0),
						  LOOKUP_PROTECT, checkconst);
	rval = build (COMPOUND_EXPR, type, arg, real_reference);
	TREE_CONSTANT (rval) = staticp (TREE_OPERAND (targ, 0));
	return rval;
      }

    case COND_EXPR:
      return build (COND_EXPR, type,
		    TREE_OPERAND (targ, 0),
		    build_up_reference (type, TREE_OPERAND (targ, 1),
					LOOKUP_PROTECT, checkconst),
		    build_up_reference (type, TREE_OPERAND (targ, 2),
					LOOKUP_PROTECT, checkconst));

    case WITH_CLEANUP_EXPR:
      return build (WITH_CLEANUP_EXPR, type,
		    build_up_reference (type, TREE_OPERAND (targ, 0),
					LOOKUP_PROTECT, checkconst),
		    0, TREE_OPERAND (targ, 2));

    case BIND_EXPR:
      arg = TREE_OPERAND (targ, 1);
      if (arg == NULL_TREE)
	{
	  compiler_error ("({ ... }) expression not expanded when needed for reference");
	  return error_mark_node;
	}
      rval = build1 (ADDR_EXPR, type, arg);
      TREE_REFERENCE_EXPR (rval) = 1;
      return rval;

    default:
      break;
    }

  if (TREE_ADDRESSABLE (targ) == 0)
    {
      tree temp;

      if (TREE_CODE (targ) == CALL_EXPR && IS_AGGR_TYPE (argtype))
	{
	  temp = build_cplus_new (argtype, targ, 1);
	  if (TREE_CODE (temp) == WITH_CLEANUP_EXPR)
	    rval = build (WITH_CLEANUP_EXPR, type,
			  build1 (ADDR_EXPR, type, TREE_OPERAND (temp, 0)),
			  0, TREE_OPERAND (temp, 2));
	  else
	    rval = build1 (ADDR_EXPR, type, temp);
	  goto done;
	}
      else
	{
	  temp = get_temp_name (argtype, 0);
	  if (global_bindings_p ())
	    {
	      /* Give this new temp some rtl and initialize it.  */
	      DECL_INITIAL (temp) = targ;
	      TREE_STATIC (temp) = 1;
	      finish_decl (temp, targ, NULL_TREE, 0);
	      /* Do this after declaring it static.  */
	      rval = build_unary_op (ADDR_EXPR, temp, 0);
	      TREE_TYPE (rval) = type;
	      literal_flag = TREE_CONSTANT (rval);
	      goto done;
	    }
	  else
	    {
	      rval = build_unary_op (ADDR_EXPR, temp, 0);
	      if (binfo && !BINFO_OFFSET_ZEROP (binfo))
		rval = convert_pointer_to (target_type, rval);
	      else
		TREE_TYPE (rval) = type;

	      temp = build (MODIFY_EXPR, argtype, temp, arg);
	      TREE_SIDE_EFFECTS (temp) = 1;
	      return build (COMPOUND_EXPR, type, temp, rval);
	    }
	}
    }
  else
    rval = build1 (ADDR_EXPR, type, arg);

 done:
  if (TYPE_USES_COMPLEX_INHERITANCE (argtype)
      || TYPE_USES_COMPLEX_INHERITANCE (target_type))
    {
      TREE_TYPE (rval) = build_pointer_type (argtype);
      if (flags & LOOKUP_PROTECT)
	rval = convert_pointer_to (target_type, rval);
      else
	rval
	  = convert_to_pointer_force (build_pointer_type (target_type), rval);
      TREE_TYPE (rval) = type;
    }
  TREE_CONSTANT (rval) = literal_flag;
  return rval;
}

/* For C++: Only need to do one-level references, but cannot
   get tripped up on signed/unsigned differences.

   DECL is either NULL_TREE or the _DECL node for a reference that is being
   initialized.  It can be error_mark_node if we don't know the _DECL but
   we know it's an initialization.  */

tree cp_convert PROTO((tree, tree, int, int));

tree
convert_to_reference (reftype, expr, convtype, flags, decl)
     tree reftype, expr;
     int convtype, flags;
     tree decl;
{
  register tree type = TYPE_MAIN_VARIANT (TREE_TYPE (reftype));
  register tree intype = TREE_TYPE (expr);
  register enum tree_code form = TREE_CODE (intype);
  tree rval = NULL_TREE;

  if (form == REFERENCE_TYPE)
    intype = TREE_TYPE (intype);
  intype = TYPE_MAIN_VARIANT (intype);

  if (((convtype & CONV_STATIC) && comptypes (type, intype, -1))
      || ((convtype & CONV_IMPLICIT) && comptypes (type, intype, 0)))
    {
      if (flags & LOOKUP_COMPLAIN)
	{
	  tree ttl = TREE_TYPE (reftype);
	  tree ttr;
	  
	  if (form == REFERENCE_TYPE)
	    ttr = TREE_TYPE (TREE_TYPE (expr));
	  else
	    {
	      int r = TREE_READONLY (expr);
	      int v = TREE_THIS_VOLATILE (expr);
	      ttr = cp_build_type_variant (TREE_TYPE (expr), r, v);
	    }

	  if (! lvalue_p (expr) &&
	      (decl == NULL_TREE || ! TYPE_READONLY (ttl)))
	    {
	      if (decl)
		/* Ensure semantics of [dcl.init.ref] */
		cp_pedwarn ("initialization of non-const `%T' from rvalue `%T'",
			    reftype, intype);
	      else
		cp_pedwarn ("conversion to `%T' from rvalue `%T'",
			    reftype, intype);
	    }
	  else if (! (convtype & CONV_CONST))
	    {
	      if (! TYPE_READONLY (ttl) && TYPE_READONLY (ttr))
		cp_pedwarn ("conversion from `%T' to `%T' discards const",
			    ttr, reftype);
	      else if (! TYPE_VOLATILE (ttl) && TYPE_VOLATILE (ttr))
		cp_pedwarn ("conversion from `%T' to `%T' discards volatile",
			    ttr, reftype);
	    }
	}

      if (form == REFERENCE_TYPE)
	{
	  tree type = TREE_TYPE (expr);
	  tree tmp = copy_node (expr);
	  TREE_TYPE (tmp) = build_pointer_type (TREE_TYPE (TREE_TYPE (expr)));
	  rval = cp_convert (build_pointer_type (TREE_TYPE (reftype)), tmp,
			     convtype, flags);
	  TREE_TYPE (tmp) = type;
	  TREE_TYPE (rval) = reftype;
	  return rval;
	}

      return build_up_reference (reftype, expr, flags,
				 ! (convtype & CONV_CONST));
    }

  if ((convtype & CONV_IMPLICIT)
      && IS_AGGR_TYPE (intype)
      && ! (flags & LOOKUP_NO_CONVERSION)
      && (rval = build_type_conversion (CONVERT_EXPR, reftype, expr, 1)))
    {
      if (rval == error_mark_node)
	cp_error ("conversion from `%T' to `%T' is ambiguous",
		  intype, reftype);
      return rval;
    }
  else if ((convtype & CONV_REINTERPRET) && lvalue_p (expr))
    {
      /* When casting an lvalue to a reference type, just convert into
	 a pointer to the new type and deference it.  This is allowed
	 by San Diego WP section 5.2.9 paragraph 12, though perhaps it
	 should be done directly (jason).  (int &)ri ---> *(int*)&ri */

      /* B* bp; A& ar = (A&)bp; is legal, but it's probably not what they
         meant.  */
      if (form == POINTER_TYPE
	  && (comptypes (TREE_TYPE (intype), type, -1)))
	cp_warning ("casting `%T' to `%T' does not dereference pointer",
		    intype, reftype);
	  
      rval = build_unary_op (ADDR_EXPR, expr, 0);
      if (rval != error_mark_node)
	rval = convert_force (build_pointer_type (TREE_TYPE (reftype)), rval);
      if (rval != error_mark_node)
	rval = build1 (NOP_EXPR, reftype, rval);
    }
  else if (decl)
    {
      tree rval_as_conversion = NULL_TREE;
      tree rval_as_ctor = NULL_TREE;
      
      if (IS_AGGR_TYPE (intype)
	  && (rval = build_type_conversion (CONVERT_EXPR, type, expr, 1)))
	{
	  if (rval == error_mark_node)
	    return rval;

	  rval_as_conversion = build_up_reference (reftype, rval, flags, 1);
	}
      
      /* Definitely need to go through a constructor here.  */
      if (TYPE_HAS_CONSTRUCTOR (type)
	  && ! CLASSTYPE_ABSTRACT_VIRTUALS (type)
	  && (rval = build_method_call
	      (NULL_TREE, constructor_name_full (type),
	       build_tree_list (NULL_TREE, expr), TYPE_BINFO (type),
	       LOOKUP_NO_CONVERSION|LOOKUP_SPECULATIVELY)))
	{
	  tree init;

	  if (global_bindings_p ())
	    {
	      extern tree static_aggregates;
	      tree t = get_temp_name (type, global_bindings_p ());
	      init = build_method_call (t, constructor_name_full (type),
					build_tree_list (NULL_TREE, expr),
					TYPE_BINFO (type),
					LOOKUP_NORMAL|LOOKUP_NO_CONVERSION);

	      if (init == error_mark_node)
		return error_mark_node;

	      make_decl_rtl (t, NULL_PTR, 1);
	      static_aggregates = perm_tree_cons (expr, t, static_aggregates);
	      rval = build_unary_op (ADDR_EXPR, t, 0);
	    }
	  else
	    {
	      init = build_method_call (NULL_TREE, constructor_name_full (type),
					build_tree_list (NULL_TREE, expr),
					TYPE_BINFO (type),
					LOOKUP_NORMAL|LOOKUP_NO_CONVERSION);

	      if (init == error_mark_node)
		return error_mark_node;

	      rval = build_cplus_new (type, init, 1);
	      rval = build_up_reference (reftype, rval, flags, 1);
	    }
	  rval_as_ctor = rval;
	}

      if (rval_as_ctor && rval_as_conversion)
	{
	  cp_error ("ambiguous conversion from `%T' to `%T'; both user-defined conversion and constructor apply",
		    intype, reftype);
	  return error_mark_node;
	}
      else if (rval_as_ctor)
	rval = rval_as_ctor;
      else if (rval_as_conversion)
	rval = rval_as_conversion;
      else if (! IS_AGGR_TYPE (type) && ! IS_AGGR_TYPE (intype))
	{
	  rval = convert (type, expr);
	  if (rval == error_mark_node)
	    return error_mark_node;
	  
	  rval = build_up_reference (reftype, rval, flags, 1);
	}

      if (rval && ! TYPE_READONLY (TREE_TYPE (reftype)))
	cp_pedwarn ("initializing non-const `%T' with `%T' will use a temporary",
		    reftype, intype);
    }

  if (rval)
    {
      /* If we found a way to convert earlier, then use it. */
      return rval;
    }

  my_friendly_assert (form != OFFSET_TYPE, 189);

  if (flags & LOOKUP_SPECULATIVELY)
    return NULL_TREE;

  else if (flags & LOOKUP_COMPLAIN)
    cp_error ("cannot convert type `%T' to type `%T'", intype, reftype);

  return error_mark_node;
}

/* We are using a reference VAL for its value. Bash that reference all the
   way down to its lowest form. */
tree
convert_from_reference (val)
     tree val;
{
  tree type = TREE_TYPE (val);

  if (TREE_CODE (type) == OFFSET_TYPE)
    type = TREE_TYPE (type);
 if (TREE_CODE (type) == REFERENCE_TYPE)
    {
      tree target_type = TREE_TYPE (type);
      tree nval;

      /* This can happen if we cast to a reference type.  */
      if (TREE_CODE (val) == ADDR_EXPR)
	{
	  nval = build1 (NOP_EXPR, build_pointer_type (target_type), val);
	  nval = build_indirect_ref (nval, NULL_PTR);
	  /* The below was missing, are other important flags missing too? */
	  TREE_SIDE_EFFECTS (nval) = TREE_SIDE_EFFECTS (val);
	  return nval;
	}

      nval = build1 (INDIRECT_REF, target_type, val);

      TREE_THIS_VOLATILE (nval) = TYPE_VOLATILE (target_type);
      TREE_SIDE_EFFECTS (nval) = TYPE_VOLATILE (target_type);
      TREE_READONLY (nval) = TYPE_READONLY (target_type);
      /* The below was missing, are other important flags missing too? */
      TREE_SIDE_EFFECTS (nval) |= TREE_SIDE_EFFECTS (val);
      return nval;
    }
  return val;
}

/* See if there is a constructor of type TYPE which will convert
   EXPR.  The reference manual seems to suggest (8.5.6) that we need
   not worry about finding constructors for base classes, then converting
   to the derived class.

   MSGP is a pointer to a message that would be an appropriate error
   string.  If MSGP is NULL, then we are not interested in reporting
   errors.  */
tree
convert_to_aggr (type, expr, msgp, protect)
     tree type, expr;
     char **msgp;
     int protect;
{
  tree basetype = type;
  tree name = TYPE_IDENTIFIER (basetype);
  tree function, fndecl, fntype, parmtypes, parmlist, result;
  tree method_name;
  enum access_type access;
  int can_be_private, can_be_protected;

  if (! TYPE_HAS_CONSTRUCTOR (basetype))
    {
      if (msgp)
	*msgp = "type `%s' does not have a constructor";
      return error_mark_node;
    }

  access = access_public;
  can_be_private = 0;
  can_be_protected = IDENTIFIER_CLASS_VALUE (name) || name == current_class_name;

  parmlist = build_tree_list (NULL_TREE, expr);
  parmtypes = tree_cons (NULL_TREE, TREE_TYPE (expr), void_list_node);

  if (TYPE_USES_VIRTUAL_BASECLASSES (basetype))
    {
      parmtypes = tree_cons (NULL_TREE, integer_type_node, parmtypes);
      parmlist = tree_cons (NULL_TREE, integer_one_node, parmlist);
    }

  /* The type of the first argument will be filled in inside the loop.  */
  parmlist = tree_cons (NULL_TREE, integer_zero_node, parmlist);
  parmtypes = tree_cons (NULL_TREE, TYPE_POINTER_TO (basetype), parmtypes);

  method_name = build_decl_overload (name, parmtypes, 1);

  /* constructors are up front.  */
  fndecl = TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (basetype), 0);
  if (TYPE_HAS_DESTRUCTOR (basetype))
    fndecl = DECL_CHAIN (fndecl);

  while (fndecl)
    {
      if (DECL_ASSEMBLER_NAME (fndecl) == method_name)
	{
	  function = fndecl;
	  if (protect)
	    {
	      if (TREE_PRIVATE (fndecl))
		{
		  can_be_private =
		    (basetype == current_class_type
		     || is_friend (basetype, current_function_decl)
		     || purpose_member (basetype, DECL_ACCESS (fndecl)));
		  if (! can_be_private)
		    goto found;
		}
	      else if (TREE_PROTECTED (fndecl))
		{
		  if (! can_be_protected)
		    goto found;
		}
	    }
	  goto found_and_ok;
	}
      fndecl = DECL_CHAIN (fndecl);
    }

  /* No exact conversion was found.  See if an approximate
     one will do.  */
  fndecl = TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (basetype), 0);
  if (TYPE_HAS_DESTRUCTOR (basetype))
    fndecl = DECL_CHAIN (fndecl);

  {
    int saw_private = 0;
    int saw_protected = 0;
    struct candidate *candidates =
      (struct candidate *) alloca ((decl_list_length (fndecl)+1) * sizeof (struct candidate));
    struct candidate *cp = candidates;

    while (fndecl)
      {
	function = fndecl;
	cp->h_len = 2;
	cp->harshness = (struct harshness_code *)
	  alloca (3 * sizeof (struct harshness_code));

	compute_conversion_costs (fndecl, parmlist, cp, 2);
	if ((cp->h.code & EVIL_CODE) == 0)
	  {
	    cp->u.field = fndecl;
	    if (protect)
	      {
		if (TREE_PRIVATE (fndecl))
		  access = access_private;
		else if (TREE_PROTECTED (fndecl))
		  access = access_protected;
		else
		  access = access_public;
	      }
	    else
	      access = access_public;

	    if (access == access_private
		? (basetype == current_class_type
		   || is_friend (basetype, cp->function)
		   || purpose_member (basetype, DECL_ACCESS (fndecl)))
		: access == access_protected
		? (can_be_protected
		   || purpose_member (basetype, DECL_ACCESS (fndecl)))
		: 1)
	      {
		if (cp->h.code <= TRIVIAL_CODE)
		  goto found_and_ok;
		cp++;
	      }
	    else
	      {
		if (access == access_private)
		  saw_private = 1;
		else
		  saw_protected = 1;
	      }
	  }
	fndecl = DECL_CHAIN (fndecl);
      }
    if (cp - candidates)
      {
	/* Rank from worst to best.  Then cp will point to best one.
	   Private fields have their bits flipped.  For unsigned
	   numbers, this should make them look very large.
	   If the best alternate has a (signed) negative value,
	   then all we ever saw were private members.  */
	if (cp - candidates > 1)
	  qsort (candidates,	/* char *base */
		 cp - candidates, /* int nel */
		 sizeof (struct candidate), /* int width */
		 rank_for_overload); /* int (*compar)() */

	--cp;
	if (cp->h.code & EVIL_CODE)
	  {
	    if (msgp)
	      *msgp = "ambiguous type conversion possible for `%s'";
	    return error_mark_node;
	  }

	function = cp->function;
	fndecl = cp->u.field;
	goto found_and_ok;
      }
    else if (msgp)
      {
	if (saw_private)
	  if (saw_protected)
	    *msgp = "only private and protected conversions apply";
	  else
	    *msgp = "only private conversions apply";
	else if (saw_protected)
	  *msgp = "only protected conversions apply";
	else
	  *msgp = "no appropriate conversion to type `%s'";
      }
    return error_mark_node;
  }
  /* NOTREACHED */

 found:
  if (access == access_private)
    if (! can_be_private)
      {
	if (msgp)
	  *msgp = TREE_PRIVATE (fndecl)
	    ? "conversion to type `%s' is private"
	    : "conversion to type `%s' is from private base class";
	return error_mark_node;
      }
  if (access == access_protected)
    if (! can_be_protected)
      {
	if (msgp)
	  *msgp = TREE_PRIVATE (fndecl)
	    ? "conversion to type `%s' is protected"
	    : "conversion to type `%s' is from protected base class";
	return error_mark_node;
      }
  function = fndecl;
 found_and_ok:

  /* It will convert, but we don't do anything about it yet.  */
  if (msgp == 0)
    return NULL_TREE;

  fntype = TREE_TYPE (function);
  if (DECL_INLINE (function) && TREE_CODE (function) == FUNCTION_DECL)
    function = build1 (ADDR_EXPR, build_pointer_type (fntype), function);
  else
    function = default_conversion (function);

  result = build_nt (CALL_EXPR, function,
		     convert_arguments (NULL_TREE, TYPE_ARG_TYPES (fntype),
					parmlist, NULL_TREE, LOOKUP_NORMAL),
		     NULL_TREE);
  TREE_TYPE (result) = TREE_TYPE (fntype);
  TREE_SIDE_EFFECTS (result) = 1;
  TREE_RAISES (result) = !! TYPE_RAISES_EXCEPTIONS (fntype);
  return result;
}

/* Call this when we know (for any reason) that expr is not, in fact,
   zero.  This routine is like convert_pointer_to, but it pays
   attention to which specific instance of what type we want to
   convert to.  This routine should eventually become
   convert_to_pointer after all references to convert_to_pointer
   are removed.  */
tree
convert_pointer_to_real (binfo, expr)
     tree binfo, expr;
{
  register tree intype = TREE_TYPE (expr);
  tree ptr_type;
  tree type, rval;

  if (TREE_CODE (binfo) == TREE_VEC)
    type = BINFO_TYPE (binfo);
  else if (IS_AGGR_TYPE (binfo))
    {
      type = binfo;
    }
  else
    {
      type = binfo;
      binfo = NULL_TREE;
    }

  ptr_type = build_pointer_type (type);
  if (ptr_type == TYPE_MAIN_VARIANT (intype))
    return expr;

  if (intype == error_mark_node)
    return error_mark_node;

  my_friendly_assert (!integer_zerop (expr), 191);

  if (TREE_CODE (type) == RECORD_TYPE
      && TREE_CODE (TREE_TYPE (intype)) == RECORD_TYPE
      && type != TYPE_MAIN_VARIANT (TREE_TYPE (intype)))
    {
      tree path;
      int distance
	= get_base_distance (binfo, TYPE_MAIN_VARIANT (TREE_TYPE (intype)),
			     0, &path);

      /* This function shouldn't be called with unqualified arguments
	 but if it is, give them an error message that they can read.  */
      if (distance < 0)
	{
	  cp_error ("cannot convert a pointer of type `%T' to a pointer of type `%T'",
		    TREE_TYPE (intype), type);

	  if (distance == -2)
	    cp_error ("because `%T' is an ambiguous base class", type);
	  return error_mark_node;
	}

      return build_vbase_path (PLUS_EXPR, ptr_type, expr, path, 1);
    }
  rval = build1 (NOP_EXPR, ptr_type,
		 TREE_CODE (expr) == NOP_EXPR ? TREE_OPERAND (expr, 0) : expr);
  TREE_CONSTANT (rval) = TREE_CONSTANT (expr);
  return rval;
}

/* Call this when we know (for any reason) that expr is
   not, in fact, zero.  This routine gets a type out of the first
   argument and uses it to search for the type to convert to.  If there
   is more than one instance of that type in the expr, the conversion is
   ambiguous.  This routine should eventually go away, and all
   callers should use convert_to_pointer_real.  */
tree
convert_pointer_to (binfo, expr)
     tree binfo, expr;
{
  tree type;

  if (TREE_CODE (binfo) == TREE_VEC)
    type = BINFO_TYPE (binfo);
  else if (IS_AGGR_TYPE (binfo))
      type = binfo;
  else
      type = binfo;
  return convert_pointer_to_real (type, expr);
}

/* Same as above, but don't abort if we get an "ambiguous" baseclass.
   There's only one virtual baseclass we are looking for, and once
   we find one such virtual baseclass, we have found them all.  */

tree
convert_pointer_to_vbase (binfo, expr)
     tree binfo;
     tree expr;
{
  tree intype = TREE_TYPE (TREE_TYPE (expr));
  tree binfos = TYPE_BINFO_BASETYPES (intype);
  int i;

  for (i = TREE_VEC_LENGTH (binfos)-1; i >= 0; i--)
    {
      tree basetype = BINFO_TYPE (TREE_VEC_ELT (binfos, i));
      if (BINFO_TYPE (binfo) == basetype)
	return convert_pointer_to (binfo, expr);
      if (binfo_member (BINFO_TYPE (binfo), CLASSTYPE_VBASECLASSES (basetype)))
	return convert_pointer_to_vbase (binfo, convert_pointer_to (basetype, expr));
    }
  my_friendly_abort (6);
  /* NOTREACHED */
  return NULL_TREE;
}

tree
cp_convert (type, expr, convtype, flags)
     tree type, expr;
     int convtype, flags;
{
  register tree e = expr;
  register enum tree_code code = TREE_CODE (type);

  if (type == TREE_TYPE (e)
      || TREE_CODE (e) == ERROR_MARK)
    return e;
  if (TREE_CODE (TREE_TYPE (e)) == ERROR_MARK)
    return error_mark_node;

  /* Trivial conversion: cv-qualifiers do not matter on rvalues.  */
  if (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (TREE_TYPE (e)))
    return fold (build1 (NOP_EXPR, type, e));
  
  if (code == VOID_TYPE && (convtype & CONV_STATIC))
    return build1 (CONVERT_EXPR, type, e);

#if 0
  /* This is incorrect.  A truncation can't be stripped this way.
     Extensions will be stripped by the use of get_unwidened.  */
  if (TREE_CODE (e) == NOP_EXPR)
    return convert (type, TREE_OPERAND (e, 0));
#endif

  /* Just convert to the type of the member.  */
  if (code == OFFSET_TYPE)
    {
      type = TREE_TYPE (type);
      code = TREE_CODE (type);
    }

  if (code == REFERENCE_TYPE)
    return fold (convert_to_reference (type, e, convtype, flags, NULL_TREE));
  else if (TREE_CODE (TREE_TYPE (e)) == REFERENCE_TYPE)
    e = convert_from_reference (e);

  if (TREE_CODE (e) == OFFSET_REF)
    e = resolve_offset_ref (e);

  if (TREE_READONLY_DECL_P (e))
    e = decl_constant_value (e);

  if (INTEGRAL_CODE_P (code))
    {
      tree intype = TREE_TYPE (e);
      enum tree_code form = TREE_CODE (intype);
      /* enum = enum, enum = int, enum = float are all errors. */
      if (flag_int_enum_equivalence == 0
	  && TREE_CODE (type) == ENUMERAL_TYPE
	  && ARITHMETIC_TYPE_P (intype))
	{
	  cp_pedwarn ("conversion from `%#T' to `%#T'", intype, type);

	  if (flag_pedantic_errors)
	    return error_mark_node;
	}
      if (IS_AGGR_TYPE (intype))
	{
	  tree rval;
	  rval = build_type_conversion (CONVERT_EXPR, type, e, 1);
	  if (rval)
	    return rval;
	  cp_error ("`%#T' used where a `%T' was expected", intype, type);
	  return error_mark_node;
	}
      if (code == BOOLEAN_TYPE)
	{
	  tree newe = truthvalue_conversion (e);
	  /* Avoid stupid (infinite) recursion from backend. */
	  if (TREE_CODE (newe) != NOP_EXPR || e != TREE_OPERAND (newe, 0))
	    e = newe;
	  if (TREE_TYPE (e) == bool_type_node)
	    return e;
	  else if (TREE_CODE (e) == INTEGER_CST)
	    {
	      if (e == integer_zero_node)
		e = false_node;
	      else
		e = true_node;
	    }
	  else
	    return build1 (NOP_EXPR, bool_type_node, e);
	}
      return fold (convert_to_integer (type, e));
    }
  if (code == POINTER_TYPE)
    return fold (cp_convert_to_pointer (type, e));
  if (code == REAL_TYPE)
    {
      if (IS_AGGR_TYPE (TREE_TYPE (e)))
	{
	  tree rval;
	  rval = build_type_conversion (CONVERT_EXPR, type, e, 1);
	  if (rval)
	    return rval;
	  else
	    cp_error ("`%#T' used where a floating point value was expected",
		      TREE_TYPE (e));
	}
      return fold (convert_to_real (type, e));
    }

  /* New C++ semantics:  since assignment is now based on
     memberwise copying,  if the rhs type is derived from the
     lhs type, then we may still do a conversion.  */
  if (IS_AGGR_TYPE_CODE (code))
    {
      tree dtype = TREE_TYPE (e);
      tree ctor = NULL_TREE;
      tree conversion = NULL_TREE;

      dtype = TYPE_MAIN_VARIANT (dtype);

      /* Conversion of object pointers or signature pointers/references
	 to signature pointers/references.  */

      if (TYPE_LANG_SPECIFIC (type)
	  && (IS_SIGNATURE_POINTER (type) || IS_SIGNATURE_REFERENCE (type)))
	{
	  tree constructor = build_signature_pointer_constructor (type, expr);
	  tree sig_ty = SIGNATURE_TYPE (type);
	  tree sig_ptr;

	  if (constructor == error_mark_node)
	    return error_mark_node;

	  sig_ptr = get_temp_name (type, 1);
	  DECL_INITIAL (sig_ptr) = constructor;
	  CLEAR_SIGNATURE (sig_ty);
	  finish_decl (sig_ptr, constructor, 0, 0);
	  SET_SIGNATURE (sig_ty);
	  TREE_READONLY (sig_ptr) = 1;

	  return sig_ptr;
	}

      /* Conversion between aggregate types.  New C++ semantics allow
	 objects of derived type to be cast to objects of base type.
	 Old semantics only allowed this between pointers.

	 There may be some ambiguity between using a constructor
	 vs. using a type conversion operator when both apply.  */

      if (IS_AGGR_TYPE (dtype) && ! DERIVED_FROM_P (type, dtype)
	  && TYPE_HAS_CONVERSION (dtype))
	conversion = build_type_conversion (CONVERT_EXPR, type, e, 1);

      if (conversion == error_mark_node)
	{
	  error ("ambiguous pointer conversion");
	  return conversion;
	}

      if (TYPE_HAS_CONSTRUCTOR (type))
	ctor = build_method_call (NULL_TREE, constructor_name_full (type),
				  build_tree_list (NULL_TREE, e),
				  TYPE_BINFO (type),
				  LOOKUP_NORMAL | LOOKUP_SPECULATIVELY
				  | (conversion ? LOOKUP_NO_CONVERSION : 0));

      if (ctor == error_mark_node)
	{
	  cp_error ("in conversion to type `%T'", type);
	  return error_mark_node;
	}
      
      if (conversion && ctor)
	{
	  error ("both constructor and type conversion operator apply");
	  return error_mark_node;
	}
      else if (conversion)
	return conversion;
      else if (ctor)
	{
	  if (current_function_decl)
	    /* We can't pass 1 to the with_cleanup_p arg here, because that
	       screws up passing classes by value.  */
	    ctor = build_cplus_new (type, ctor, 0);
	  else
	    {
	      register tree parm = TREE_OPERAND (ctor, 1);

	      /* Initializers for static variables and parameters
		 have to handle doing the initialization and
		 cleanup themselves.  */
	      my_friendly_assert (TREE_CODE (ctor) == CALL_EXPR, 322);
#if 0
	      /* The following assertion fails in cases where we
		 are initializing a static member variable of a
		 particular instance of a template class with a
		 call to a constructor of the given instance, as
		 in:
		 
		 TMPL<int> object = TMPL<int>();
		 
		 Curiously, the assertion does not fail if we do
		 the same thing for a static member of a
		 non-template class, as in:
		 
		 T object = T();
		 
		 I can't see why we should care here whether or not
		 the initializer expression involves a call to
		 `new', so for the time being, it seems best to
		 just avoid doing this assertion.  */
	      my_friendly_assert (TREE_CALLS_NEW (TREE_VALUE (parm)),
				  323);
#endif
	      TREE_VALUE (parm) = NULL_TREE;
	      ctor = build_indirect_ref (ctor, NULL_PTR);
	      TREE_HAS_CONSTRUCTOR (ctor) = 1;
	    }
	  return ctor;
	}
    }

  /* If TYPE or TREE_TYPE (E) is not on the permanent_obstack,
     then the it won't be hashed and hence compare as not equal,
     even when it is.  */
  if (code == ARRAY_TYPE
      && TREE_TYPE (TREE_TYPE (e)) == TREE_TYPE (type)
      && index_type_equal (TYPE_DOMAIN (TREE_TYPE (e)), TYPE_DOMAIN (type)))
    return e;

  cp_error ("conversion from `%T' to non-scalar type `%T' requested",
	    TREE_TYPE (expr), type);
  return error_mark_node;
}

/* Create an expression whose value is that of EXPR,
   converted to type TYPE.  The TREE_TYPE of the value
   is always TYPE.  This function implements all reasonable
   conversions; callers should filter out those that are
   not permitted by the language being compiled.  */

tree
convert (type, expr)
     tree type, expr;
{
  return cp_convert (type, expr, CONV_OLD_CONVERT, 0);
}

/* Like convert, except permit conversions to take place which
   are not normally allowed due to access restrictions
   (such as conversion from sub-type to private super-type).  */
tree
convert_force (type, expr)
     tree type;
     tree expr;
{
  register tree e = expr;
  register enum tree_code code = TREE_CODE (type);

  if (code == REFERENCE_TYPE)
    return fold (convert_to_reference (type, e, CONV_C_CAST, LOOKUP_COMPLAIN,
				       NULL_TREE));
  else if (TREE_CODE (TREE_TYPE (e)) == REFERENCE_TYPE)
    e = convert_from_reference (e);

  if (code == POINTER_TYPE)
    return fold (convert_to_pointer_force (type, e));

  /* From typeck.c convert_for_assignment */
  if (((TREE_CODE (TREE_TYPE (e)) == POINTER_TYPE && TREE_CODE (e) == ADDR_EXPR
	&& TREE_CODE (TREE_TYPE (e)) == POINTER_TYPE
	&& TREE_CODE (TREE_TYPE (TREE_TYPE (e))) == METHOD_TYPE)
       || integer_zerop (e)
       || TYPE_PTRMEMFUNC_P (TREE_TYPE (e)))
      && TYPE_PTRMEMFUNC_P (type))
    {
      /* compatible pointer to member functions. */
      return build_ptrmemfunc (TYPE_PTRMEMFUNC_FN_TYPE (type), e, 1);
    }
  {
    int old_equiv = flag_int_enum_equivalence;
    flag_int_enum_equivalence = 1;
    e = convert (type, e);
    flag_int_enum_equivalence = old_equiv;
  }
  return e;
}

/* Subroutine of build_type_conversion.  */
static tree
build_type_conversion_1 (xtype, basetype, expr, typename, for_sure)
     tree xtype, basetype;
     tree expr;
     tree typename;
     int for_sure;
{
  tree rval;
  int flags;

  if (for_sure == 0)
    flags = LOOKUP_PROTECT;
  else
    flags = LOOKUP_NORMAL;

  rval = build_method_call (expr, typename, NULL_TREE, NULL_TREE, flags);
  if (rval == error_mark_node)
    {
      if (for_sure == 0)
	return NULL_TREE;
      return error_mark_node;
    }
  if (TREE_CODE (TREE_TYPE (rval)) == REFERENCE_TYPE
      && TREE_CODE (xtype) != REFERENCE_TYPE)
    rval = default_conversion (rval);

  if (warn_cast_qual
      && TREE_TYPE (xtype)
      && (TREE_READONLY (TREE_TYPE (TREE_TYPE (rval)))
	  > TREE_READONLY (TREE_TYPE (xtype))))
    warning ("user-defined conversion casting away `const'");
  return convert (xtype, rval);
}

/* Convert an aggregate EXPR to type XTYPE.  If a conversion
   exists, return the attempted conversion.  This may
   return ERROR_MARK_NODE if the conversion is not
   allowed (references private members, etc).
   If no conversion exists, NULL_TREE is returned.

   If (FOR_SURE & 1) is non-zero, then we allow this type conversion
   to take place immediately.  Otherwise, we build a SAVE_EXPR
   which can be evaluated if the results are ever needed.

   If FOR_SURE >= 2, then we only look for exact conversions.

   TYPE may be a reference type, in which case we first look
   for something that will convert to a reference type.  If
   that fails, we will try to look for something of the
   reference's target type, and then return a reference to that.  */
tree
build_type_conversion (code, xtype, expr, for_sure)
     enum tree_code code;
     tree xtype, expr;
     int for_sure;
{
  /* C++: check to see if we can convert this aggregate type
     into the required scalar type.  */
  tree type, type_default;
  tree typename = build_typename_overload (xtype), *typenames;
  int n_variants = 0;
  tree basetype, save_basetype;
  tree rval;
  int exact_conversion = for_sure >= 2;
  for_sure &= 1;

  if (expr == error_mark_node)
    return error_mark_node;

  basetype = TREE_TYPE (expr);
  if (TREE_CODE (basetype) == REFERENCE_TYPE)
    basetype = TREE_TYPE (basetype);

  if (TYPE_PTRMEMFUNC_P (basetype) && TREE_CODE (xtype) == BOOLEAN_TYPE)
    {
      /* We convert a pointer to member function into a boolean,
	 by just checking the index value, for == 0, we want false, for
	 != 0, we want true.  */
      return convert (xtype, build_component_ref (expr, index_identifier, 0, 0));
    }

  basetype = TYPE_MAIN_VARIANT (basetype);
  if (! TYPE_LANG_SPECIFIC (basetype) || ! TYPE_HAS_CONVERSION (basetype))
    return NULL_TREE;

  if (TREE_CODE (xtype) == POINTER_TYPE
      || TREE_CODE (xtype) == REFERENCE_TYPE)
    {
      /* Prepare to match a variant of this type.  */
      type = TYPE_MAIN_VARIANT (TREE_TYPE (xtype));
      for (n_variants = 0; type; type = TYPE_NEXT_VARIANT (type))
	n_variants++;
      typenames = (tree *)alloca (n_variants * sizeof (tree));
      for (n_variants = 0, type = TYPE_MAIN_VARIANT (TREE_TYPE (xtype));
	   type; n_variants++, type = TYPE_NEXT_VARIANT (type))
	{
	  if (type == TREE_TYPE (xtype))
	    typenames[n_variants] = typename;
	  else if (TREE_CODE (xtype) == POINTER_TYPE)
	    typenames[n_variants] = build_typename_overload (build_pointer_type (type));
	  else
	    typenames[n_variants] = build_typename_overload (build_reference_type (type));
	}
    }

  save_basetype = basetype;
  type = xtype;

  while (TYPE_HAS_CONVERSION (basetype))
    {
      int i;
      if (lookup_fnfields (TYPE_BINFO (basetype), typename, 0))
	return build_type_conversion_1 (xtype, basetype, expr, typename, for_sure);
      for (i = 0; i < n_variants; i++)
	if (typenames[i] != typename
	    && lookup_fnfields (TYPE_BINFO (basetype), typenames[i], 0))
	  return build_type_conversion_1 (xtype, basetype, expr, typenames[i], for_sure);

      if (TYPE_BINFO_BASETYPES (basetype))
	basetype = TYPE_BINFO_BASETYPE (basetype, 0);
      else
	break;
    }

  if (TREE_CODE (type) == REFERENCE_TYPE)
    {
#if 0
      /* Only reference variable initializations can use a temporary; this
         must be handled elsewhere (like convert_to_reference and
         compute_conversion_costs).  */

      type = TYPE_MAIN_VARIANT (TREE_TYPE (type));
      typename = build_typename_overload (type);
      basetype = save_basetype;

      /* May need to build a temporary for this.  */
      while (TYPE_HAS_CONVERSION (basetype))
	{
	  if (lookup_fnfields (TYPE_BINFO (basetype), typename, 0))
	    {
	      int flags;

	      if (for_sure == 0)
		flags = LOOKUP_PROTECT;
	      else
		flags = LOOKUP_NORMAL;
	      rval = build_method_call (expr,
					constructor_name_full (typename),
					NULL_TREE, NULL_TREE, flags);
	      if (rval == error_mark_node)
		{
		  if (for_sure == 0)
		    return NULL_TREE;
		  return error_mark_node;
		}

	      return convert (xtype, rval);
	    }
	  if (TYPE_BINFO_BASETYPES (basetype))
	    basetype = TYPE_BINFO_BASETYPE (basetype, 0);
	  else
	    break;
	}
#endif
      /* No free conversions for reference types, right?.  */
      return NULL_TREE;
    }

  if (exact_conversion)
    return NULL_TREE;

  if (TREE_CODE (type) == BOOLEAN_TYPE)
    {
      tree as_int = build_type_conversion (code, long_long_unsigned_type_node, expr, 0);
      tree as_ptr = build_type_conversion (code, ptr_type_node, expr, 0);
      /* We are missing the conversion to pointer to member type. */
      /* We are missing the conversion to floating type. */
      if (as_int && as_ptr && for_sure)
	{
	  cp_error ("ambiguous conversion from `%T' to `bool', can convert to integral type or pointer", TREE_TYPE (expr));
	  return error_mark_node;
	}
      if (as_int)
	{
	  as_int = build_type_conversion (code, long_long_unsigned_type_node, expr, for_sure+exact_conversion*2);
	  return convert (xtype, as_int);
	}
      if (as_ptr)
	{
	  as_ptr = build_type_conversion (code, ptr_type_node, expr, for_sure+exact_conversion*2);
	  return convert (xtype, as_ptr);
	}
      return NULL_TREE;
    }

  /* No perfect match found, try default.  */
#if 0 /* This is wrong; there is no standard conversion from void* to
         anything.  -jason */
  if (code == CONVERT_EXPR && TREE_CODE (type) == POINTER_TYPE)
    type_default = ptr_type_node;
  else
#endif
  if (type == void_type_node)
    return NULL_TREE;
  else
    {
      tree tmp = default_conversion (build1 (NOP_EXPR, type, integer_zero_node));
      if (tmp == error_mark_node)
	return NULL_TREE;
      type_default = TREE_TYPE (tmp);
    }

  basetype = save_basetype;

  if (type_default != type)
    {
      type = type_default;
      typename = build_typename_overload (type);

      while (TYPE_HAS_CONVERSION (basetype))
	{
	  if (lookup_fnfields (TYPE_BINFO (basetype), typename, 0))
	    return build_type_conversion_1 (xtype, basetype, expr, typename, for_sure);
	  if (TYPE_BINFO_BASETYPES (basetype))
	    basetype = TYPE_BINFO_BASETYPE (basetype, 0);
	  else
	    break;
	}
    }

  if (TREE_CODE (type) == POINTER_TYPE && TYPE_READONLY (TREE_TYPE (type)))
    {
      /* Try converting to some other const pointer type and then using
         standard conversions. */

      while (TYPE_HAS_CONVERSION (basetype))
	{
	  if (CLASSTYPE_CONVERSION (basetype, constptr_conv) != 0)
	    {
	      if (CLASSTYPE_CONVERSION (basetype, constptr_conv) == error_mark_node)
		return error_mark_node;
	      typename = DECL_NAME (CLASSTYPE_CONVERSION (basetype, constptr_conv));
	      return build_type_conversion_1 (xtype, basetype, expr, typename, for_sure);
	    }
	  if (TYPE_BINFO_BASETYPES (basetype))
	    basetype = TYPE_BINFO_BASETYPE (basetype, 0);
	  else
	    break;
	}
    }
  if (TREE_CODE (type) == POINTER_TYPE)
    {
      /* Try converting to some other pointer type and then using standard
	 conversions.  */

      while (TYPE_HAS_CONVERSION (basetype))
	{
	  if (CLASSTYPE_CONVERSION (basetype, ptr_conv) != 0)
	    {
	      if (CLASSTYPE_CONVERSION (basetype, ptr_conv) == error_mark_node)
		return error_mark_node;
	      typename = DECL_NAME (CLASSTYPE_CONVERSION (basetype, ptr_conv));
	      return build_type_conversion_1 (xtype, basetype, expr, typename, for_sure);
	    }
	  if (TYPE_BINFO_BASETYPES (basetype))
	    basetype = TYPE_BINFO_BASETYPE (basetype, 0);
	  else
	    break;
	}
    }

  /* Use the longer or shorter conversion that is appropriate.  Have
     to check against 0 because the conversion may come from a baseclass.  */
  if (TREE_CODE (type) == INTEGER_TYPE
      && TYPE_HAS_INT_CONVERSION (basetype)
      && CLASSTYPE_CONVERSION (basetype, int_conv) != 0
      && CLASSTYPE_CONVERSION (basetype, int_conv) != error_mark_node)
    {
      typename = DECL_NAME (CLASSTYPE_CONVERSION (basetype, int_conv));
      return build_type_conversion_1 (xtype, basetype, expr, typename, for_sure);
    }

  if (TREE_CODE (type) == REAL_TYPE
      && TYPE_HAS_REAL_CONVERSION (basetype)
      && CLASSTYPE_CONVERSION (basetype, real_conv) != 0
      && CLASSTYPE_CONVERSION (basetype, real_conv) != error_mark_node)
    {
      /* Only accept using an operator double() if there isn't a conflicting
	 operator int().  */
      if (TYPE_HAS_INT_CONVERSION (basetype))
	{
	  if (for_sure)
	    {
	      cp_error ("two possible conversions for type `%T'", type);
	      return error_mark_node;
	    }
	  else
	    return NULL_TREE;
	}

      typename = DECL_NAME (CLASSTYPE_CONVERSION (basetype, real_conv));
      return build_type_conversion_1 (xtype, basetype, expr, typename, for_sure);
    }

  /* THESE ARE TOTAL KLUDGES.  */
  /* Default promotion yields no new alternatives, try
     conversions which are anti-default, such as

     double -> float or int -> unsigned or unsigned -> long

     */
  if (type_default == type
      && (INTEGRAL_TYPE_P (type) || TREE_CODE (type) == REAL_TYPE))
    {
      int not_again = 0;

      if (type == double_type_node)
	typename = build_typename_overload (float_type_node);
      else if (type == integer_type_node)
	typename = build_typename_overload (unsigned_type_node);
      else if (type == unsigned_type_node)
	typename = build_typename_overload (long_integer_type_node);

    again:
      basetype = save_basetype;
      while (TYPE_HAS_CONVERSION (basetype))
	{
	  if (lookup_fnfields (TYPE_BINFO (basetype), typename, 0))
	    return build_type_conversion_1 (xtype, basetype, expr, typename, for_sure);
	  if (TYPE_BINFO_BASETYPES (basetype))
	    basetype = TYPE_BINFO_BASETYPE (basetype, 0);
	  else
	    break;
	}
      if (! not_again)
	{
	  if (type == integer_type_node)
	    {
	      typename = build_typename_overload (long_integer_type_node);
	      not_again = 1;
	      goto again;
	    }
	  else
	    {
	      typename = build_typename_overload (integer_type_node);
	      not_again = 1;
	      goto again;
	    }
	}
    }

  /* Now, try C promotions...

     float -> int
     int -> float  */

    basetype = save_basetype;
    if (TREE_CODE (type) == REAL_TYPE)
      type = integer_type_node;
    else if (TREE_CODE (type) == INTEGER_TYPE)
      if (TYPE_HAS_REAL_CONVERSION (basetype))
	type = double_type_node;
      else
	return NULL_TREE;
    else
      return NULL_TREE;

    typename = build_typename_overload (type);
    while (TYPE_HAS_CONVERSION (basetype))
      {
	if (lookup_fnfields (TYPE_BINFO (basetype), typename, 0))
	  {
	    rval = build_type_conversion_1 (xtype, basetype, expr, typename, for_sure);
	    return rval;
	  }
	if (TYPE_BINFO_BASETYPES (basetype))
	  basetype = TYPE_BINFO_BASETYPE (basetype, 0);
	else
	  break;
      }

  return NULL_TREE;
}

/* Must convert two aggregate types to non-aggregate type.
   Attempts to find a non-ambiguous, "best" type conversion.

   Return 1 on success, 0 on failure.

   @@ What are the real semantics of this supposed to be??? */
int
build_default_binary_type_conversion (code, arg1, arg2)
     enum tree_code code;
     tree *arg1, *arg2;
{
  tree type1 = TREE_TYPE (*arg1);
  tree type2 = TREE_TYPE (*arg2);

  if (TREE_CODE (type1) == REFERENCE_TYPE
      || TREE_CODE (type1) == POINTER_TYPE)
    type1 = TREE_TYPE (type1);
  if (TREE_CODE (type2) == REFERENCE_TYPE
      || TREE_CODE (type2) == POINTER_TYPE)
    type2 = TREE_TYPE (type2);

  if (TREE_CODE (TYPE_NAME (type1)) != TYPE_DECL)
    {
      tree decl = typedecl_for_tag (type1);
      if (decl)
	error ("type conversion nonexistent for type `%s'",
	       IDENTIFIER_POINTER (DECL_NAME (decl)));
      else
	error ("type conversion nonexistent for non-C++ type");
      return 0;
    }
  if (TREE_CODE (TYPE_NAME (type2)) != TYPE_DECL)
    {
      tree decl = typedecl_for_tag (type2);
      if (decl)
	error ("type conversion nonexistent for type `%s'",
	       IDENTIFIER_POINTER (decl));
      else
	error ("type conversion nonexistent for non-C++ type");
      return 0;
    }

  if (!IS_AGGR_TYPE (type1) || !TYPE_HAS_CONVERSION (type1))
    {
      if (!IS_AGGR_TYPE (type2) || !TYPE_HAS_CONVERSION (type2))
	cp_error ("no conversion from `%T' and `%T' to types with default `%O' ",
		  type1, type2, code);
      else
	cp_error ("no conversion from `%T' to type with default `%O'",
		  type1, code);
      return 0;
    }
  else if (!IS_AGGR_TYPE (type2) || !TYPE_HAS_CONVERSION (type2))
    {
      cp_error ("no conversion from `%T' to type with default `%O'",
		type2, code);
      return 0;
    }

  if (code == TRUTH_ANDIF_EXPR
      || code == TRUTH_ORIF_EXPR)
    {
      *arg1 = convert (bool_type_node, *arg1);
      *arg2 = convert (bool_type_node, *arg2);
    }
  else if (TYPE_HAS_INT_CONVERSION (type1))
    {
      if (TYPE_HAS_REAL_CONVERSION (type1))
	cp_pedwarn ("ambiguous type conversion for type `%T', defaulting to int",
		    type1);
      *arg1 = build_type_conversion (code, integer_type_node, *arg1, 1);
      *arg2 = build_type_conversion (code, integer_type_node, *arg2, 1);
    }
  else if (TYPE_HAS_REAL_CONVERSION (type1))
    {
      *arg1 = build_type_conversion (code, double_type_node, *arg1, 1);
      *arg2 = build_type_conversion (code, double_type_node, *arg2, 1);
    }
  else
    {
      *arg1 = build_type_conversion (code, ptr_type_node, *arg1, 1);
      if (*arg1 == error_mark_node)
	error ("ambiguous pointer conversion");
      *arg2 = build_type_conversion (code, ptr_type_node, *arg2, 1);
      if (*arg1 != error_mark_node && *arg2 == error_mark_node)
	error ("ambiguous pointer conversion");
    }
  if (*arg1 == 0)
    {
      if (*arg2 == 0 && type1 != type2)
	cp_error ("default type conversion for types `%T' and `%T' failed",
		  type1, type2);
      else
	cp_error ("default type conversion for type `%T' failed", type1);
      return 0;
    }
  else if (*arg2 == 0)
    {
      cp_error ("default type conversion for type `%T' failed", type2);
      return 0;
    }
  return 1;
}

/* Must convert an aggregate type to non-aggregate type.
   Attempts to find a non-ambiguous, "best" type conversion.

   Return 1 on success, 0 on failure.

   The type of the argument is expected to be of aggregate type here.

   @@ What are the real semantics of this supposed to be??? */
int
build_default_unary_type_conversion (code, arg)
     enum tree_code code;
     tree *arg;
{
  tree type = TREE_TYPE (*arg);

  if (! TYPE_HAS_CONVERSION (type))
    {
      cp_error ("type conversion required for type `%T'", type);
      return 0;
    }

  if (code == TRUTH_NOT_EXPR)
    *arg = convert (bool_type_node, *arg);
  else if (TYPE_HAS_INT_CONVERSION (type))
    {
      if (TYPE_HAS_REAL_CONVERSION (type))
	cp_pedwarn ("ambiguous type conversion for type `%T', defaulting to int",
		    type);
      *arg = build_type_conversion (code, integer_type_node, *arg, 1);
    }
  else if (TYPE_HAS_REAL_CONVERSION (type))
    *arg = build_type_conversion (code, double_type_node, *arg, 1);
  else
    {
      *arg = build_type_conversion (code, ptr_type_node, *arg, 1);
      if (*arg == error_mark_node)
	error ("ambiguous pointer conversion");
    }
  if (*arg == NULL_TREE)
    {
      cp_error ("default type conversion for type `%T' failed", type);
      return 0;
    }
  return 1;
}

/* Implements integral promotion (4.1) and float->double promotion. */
tree
type_promotes_to (type)
     tree type;
{
  int constp = TYPE_READONLY (type);
  int volatilep = TYPE_VOLATILE (type);
  type = TYPE_MAIN_VARIANT (type);

  /* bool always promotes to int (not unsigned), even if it's the same
     size.  */
  if (type == bool_type_node)
    type = integer_type_node;

  /* Normally convert enums to int, but convert wide enums to something
     wider.  */
  else if (TREE_CODE (type) == ENUMERAL_TYPE
	   || type == wchar_type_node)
    {
      int precision = MAX (TYPE_PRECISION (type),
			   TYPE_PRECISION (integer_type_node));
      tree totype = type_for_size (precision, 0);
      if (TREE_UNSIGNED (type)
	  && ! int_fits_type_p (TYPE_MAX_VALUE (type), totype))
	type = type_for_size (precision, 1);
      else
	type = totype;
    }
  else if (C_PROMOTING_INTEGER_TYPE_P (type))
    {
      /* Traditionally, unsignedness is preserved in default promotions.
         Otherwise, retain unsignedness if really not getting bigger.  */
      if (TREE_UNSIGNED (type)
	  && (flag_traditional
	      || TYPE_PRECISION (type) == TYPE_PRECISION (integer_type_node)))
	type = unsigned_type_node;
      else
	type = integer_type_node;
    }
  else if (type == float_type_node)
    type = double_type_node;

  return cp_build_type_variant (type, constp, volatilep);
}
