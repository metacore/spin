/* Evaluate expressions for GDB.
   Copyright 1986, 1987, 1989, 1991, 1992, 1993, 1994
   Free Software Foundation, Inc.

This file is part of GDB.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "defs.h"
#include <string.h>
#include "symtab.h"
#include "gdbtypes.h"
#include "value.h"
#include "expression.h"
#include "target.h"
#include "frame.h"
#include "demangle.h"
#include "language.h"	/* For CAST_IS_CONVERSION */

#include "m3-lang.h"

/* Values of NOSIDE argument to eval_subexp.  */
enum noside
{ EVAL_NORMAL,
  EVAL_SKIP,			/* Only effect is to increment pos.  */
  EVAL_AVOID_SIDE_EFFECTS	/* Don't modify any variables or
				   call any functions.  The value
				   returned will have the correct
				   type, and will have an
				   approximately correct lvalue
				   type (inaccuracy: anything that is
				   listed as being in a register in
				   the function in which it was
				   declared will be lval_register).  */
};

/* special value attached to "type" values */
#define TYPE_MAGIC 1234567890


/* Prototypes for local functions. */

static value_ptr
evaluate_subexp_for_sizeof PARAMS ((struct expression *, int *));

static value_ptr
evaluate_subexp_with_coercion PARAMS ((struct expression *, int *,
				       enum noside));

static value_ptr
evaluate_subexp_for_address PARAMS ((struct expression *, int *,
				     enum noside));

static value_ptr
evaluate_subexp PARAMS ((struct type *, struct expression *, int *,
			 enum noside));


/* Parse the string EXP as a C expression, evaluate it,
   and return the result as a number.  */

CORE_ADDR
parse_and_eval_address (exp)
     char *exp;
{
  struct expression *expr = parse_expression (exp);
  register CORE_ADDR addr;
  register struct cleanup *old_chain = 
      make_cleanup (free_current_contents, &expr);

  addr = value_as_pointer (evaluate_expression (expr));
  do_cleanups (old_chain);
  return addr;
}

/* Like parse_and_eval_address but takes a pointer to a char * variable
   and advanced that variable across the characters parsed.  */

CORE_ADDR
parse_and_eval_address_1 (expptr)
     char **expptr;
{
  struct expression *expr = parse_exp_1 (expptr, (struct block *)0, 0);
  register CORE_ADDR addr;
  register struct cleanup *old_chain =
      make_cleanup (free_current_contents, &expr);

  addr = value_as_pointer (evaluate_expression (expr));
  do_cleanups (old_chain);
  return addr;
}

value_ptr
parse_and_eval (exp)
     char *exp;
{
  struct expression *expr = parse_expression (exp);
  register value_ptr val;
  register struct cleanup *old_chain
    = make_cleanup (free_current_contents, &expr);

  val = evaluate_expression (expr);
  do_cleanups (old_chain);
  return val;
}

/* Parse up to a comma (or to a closeparen)
   in the string EXPP as an expression, evaluate it, and return the value.
   EXPP is advanced to point to the comma.  */

value_ptr
parse_to_comma_and_eval (expp)
     char **expp;
{
  struct expression *expr = parse_exp_1 (expp, (struct block *) 0, 1);
  register value_ptr val;
  register struct cleanup *old_chain
    = make_cleanup (free_current_contents, &expr);

  val = evaluate_expression (expr);
  do_cleanups (old_chain);
  return val;
}

/* Evaluate an expression in internal prefix form
   such as is constructed by parse.y.

   See expression.h for info on the format of an expression.  */

value_ptr
evaluate_expression (exp)
     struct expression *exp;
{
  int pc = 0;
  return evaluate_subexp (NULL_TYPE, exp, &pc, EVAL_NORMAL);
}

/* Evaluate an expression, avoiding all memory references
   and getting a value whose type alone is correct.  */

value_ptr
evaluate_type (exp)
     struct expression *exp;
{
  int pc = 0;
  return evaluate_subexp (NULL_TYPE, exp, &pc, EVAL_AVOID_SIDE_EFFECTS);
}

static LONGEST
m3_div (a, b)
  LONGEST a, b;
{
  if (a == 0) { return 0; }
  if (a < 0) {
    return (b < 0)
             ? ((-a) / (-b))
	     : (- ((-a-1) / b) - 1);
  } else {
    return (b < 0)
             ? (- ((a - 1) / (-b)) - 1)
             : (a / b);
  }
} /* m3_div */

static LONGEST
m3_modi (a, b)
  LONGEST a, b;
{
  if (a == 0) { return 0; }
  if (a < 0) {
    return (b < 0)
             ? (- ((-a) % (-b)))
             : (b - 1 - ((-a-1) % b));
  } else {
    return (b < 0)
             ? (b + 1 + ((a - 1) % (-b)))
             : (a % b);
  }
} /* m3_modi */

static double
m3_modf (a, b)
  double a, b;
{
  double  z = a / b;
  LONGEST zi = (LONGEST) z;
  if ((z < 0.0) && ((double)zi != z)) { zi--; }
  return a - b * (double)zi;
} /* m3_modf */



static value_ptr
evaluate_subexp (expect_type, exp, pos, noside)
     struct type *expect_type;
     register struct expression *exp;
     register int *pos;
     enum noside noside;
{
  enum exp_opcode op;
  int tem, tem2, tem3;
  register int pc, pc2 = 0, oldpos;
  register value_ptr arg1 = NULL, arg2 = NULL, arg3;
  struct type *type;
  int nargs;
  value_ptr *argvec;
  int float_ok, int_ok;  

  pc = (*pos)++;
  op = exp->elts[pc].opcode;

  switch (op)
    {
    case OP_SCOPE:
      tem = longest_to_int (exp->elts[pc + 2].longconst);
      (*pos) += 4 + BYTES_TO_EXP_ELEM (tem + 1);
      arg1 = value_struct_elt_for_reference (exp->elts[pc + 1].type,
					     0,
					     exp->elts[pc + 1].type,
					     &exp->elts[pc + 3].string,
					     expect_type);
      if (arg1 == NULL)
	error ("There is no field named %s", &exp->elts[pc + 3].string);
      return arg1;

    case OP_LONG:
      (*pos) += 3;
      return value_from_longest (exp->elts[pc + 1].type,
				 exp->elts[pc + 2].longconst);

    case OP_DOUBLE:
      (*pos) += 3;
      return value_from_double (exp->elts[pc + 1].type,
				exp->elts[pc + 2].doubleconst);

    case OP_VAR_VALUE:
      (*pos) += 3;
      if (noside == EVAL_SKIP)
	goto nosideret;
#if bad_for_m3      
      if (noside == EVAL_AVOID_SIDE_EFFECTS)
	{
	  struct symbol * sym = exp->elts[pc + 2].symbol;
	  enum lval_type lv;

	  switch (SYMBOL_CLASS (sym))
	    {
	    case LOC_CONST:
	    case LOC_LABEL:
	    case LOC_CONST_BYTES:
	      lv = not_lval;
	      break;

	    case LOC_REGISTER:
	    case LOC_REGPARM:
	      lv = lval_register;
	      break;

	    default:
	      lv = lval_memory;
	      break;
	    }

	  return value_zero (SYMBOL_TYPE (sym), lv);
	}
      else
#endif	      
	return value_of_variable (exp->elts[pc + 2].symbol,
				  exp->elts[pc + 1].block);

    case OP_LAST:
      (*pos) += 2;
      return
	access_value_history (longest_to_int (exp->elts[pc + 1].longconst));

    case OP_REGISTER:
      (*pos) += 2;
	arg1 = value_of_register (longest_to_int (exp->elts[pc + 1].longconst));
      /* hack to convert registers to Modula-3 types... */
      if (VALUE_TYPE(arg1) == builtin_type_long) {
	VALUE_TYPE(arg1) = builtin_type_m3_integer;
      } else if (VALUE_TYPE(arg1) = builtin_type_double) {
	VALUE_TYPE(arg1) = builtin_type_m3_longreal;
      }      
      return arg1;

    case OP_BOOL:
      (*pos) += 2;
      return value_from_longest (builtin_type_chill_bool,
				 exp->elts[pc + 1].longconst);

    case OP_INTERNALVAR:
      (*pos) += 2;
      return value_of_internalvar (exp->elts[pc + 1].internalvar);

    case OP_M3_TEXT:
    case OP_STRING:
      tem = longest_to_int (exp->elts[pc + 1].longconst);
      (*pos) += 3 + BYTES_TO_EXP_ELEM (tem + 1);
      if (noside == EVAL_SKIP)
	goto nosideret;
      return value_string (&exp->elts[pc + 2].string, tem);

    case OP_BITSTRING:
      error ("support for OP_BITSTRING unimplemented");
      break;

    case OP_ARRAY:
      (*pos) += 3;
      tem2 = longest_to_int (exp->elts[pc + 1].longconst);
      tem3 = longest_to_int (exp->elts[pc + 2].longconst);
      nargs = tem3 - tem2 + 1;
      argvec = (value_ptr *) alloca (sizeof (value_ptr) * nargs);
      for (tem = 0; tem < nargs; tem++)
	{
	  /* Ensure that array expressions are coerced into pointer objects. */
	  argvec[tem] = evaluate_subexp_with_coercion (exp, pos, noside);
	}
      if (noside == EVAL_SKIP)
	goto nosideret;
      return (value_array (tem2, tem3, argvec));
      break;

    case TERNOP_COND:
      /* Skip third and second args to evaluate the first one.  */
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      if (value_logical_not (arg1))
	{
	  evaluate_subexp (NULL_TYPE, exp, pos, EVAL_SKIP);
	  return evaluate_subexp (NULL_TYPE, exp, pos, noside);
	}
      else
	{
	  arg2 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
	  evaluate_subexp (NULL_TYPE, exp, pos, EVAL_SKIP);
	  return arg2;
	}

    case OP_FUNCALL:
      (*pos) += 2;
      op = exp->elts[*pos].opcode;
      if (op == STRUCTOP_MEMBER || op == STRUCTOP_MPTR)
	{
	  int fnptr;

	  nargs = longest_to_int (exp->elts[pc + 1].longconst) + 1;
	  /* First, evaluate the structure into arg2 */
	  pc2 = (*pos)++;

	  if (noside == EVAL_SKIP)
	    goto nosideret;

	  if (op == STRUCTOP_MEMBER)
	    {
	      arg2 = evaluate_subexp_for_address (exp, pos, noside);
	    }
	  else
	    {
	      arg2 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
	    }

	  /* If the function is a virtual function, then the
	     aggregate value (providing the structure) plays
	     its part by providing the vtable.  Otherwise,
	     it is just along for the ride: call the function
	     directly.  */

	  arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);

	  fnptr = longest_to_int (value_as_long (arg1));

	  if (METHOD_PTR_IS_VIRTUAL(fnptr))
	    {
	      int fnoffset = METHOD_PTR_TO_VOFFSET(fnptr);
	      struct type *basetype;
	      struct type *domain_type =
		  TYPE_DOMAIN_TYPE (TYPE_TARGET_TYPE (VALUE_TYPE (arg1)));
	      int i, j;
	      basetype = TYPE_TARGET_TYPE (VALUE_TYPE (arg2));
	      if (domain_type != basetype)
		  arg2 = value_cast(lookup_pointer_type (domain_type), arg2);
	      basetype = TYPE_VPTR_BASETYPE (domain_type);
	      for (i = TYPE_NFN_FIELDS (basetype) - 1; i >= 0; i--)
		{
		  struct fn_field *f = TYPE_FN_FIELDLIST1 (basetype, i);
		  /* If one is virtual, then all are virtual.  */
		  if (TYPE_FN_FIELD_VIRTUAL_P (f, 0))
		    for (j = TYPE_FN_FIELDLIST_LENGTH (basetype, i) - 1; j >= 0; --j)
		      if (TYPE_FN_FIELD_VOFFSET (f, j) == fnoffset)
			{
			  value_ptr temp = value_ind (arg2);
			  arg1 = value_virtual_fn_field (&temp, f, j, domain_type, 0);
			  arg2 = value_addr (temp);
			  goto got_it;
			}
		}
	      if (i < 0)
		error ("virtual function at index %d not found", fnoffset);
	    }
	  else
	    {
	      VALUE_TYPE (arg1) = lookup_pointer_type (TYPE_TARGET_TYPE (VALUE_TYPE (arg1)));
	    }
	got_it:

	  /* Now, say which argument to start evaluating from */
	  tem = 2;
	}
      else if (op == STRUCTOP_STRUCT || op == STRUCTOP_PTR)
	{
	  /* Hair for method invocations */
	  int tem2;

	  nargs = longest_to_int (exp->elts[pc + 1].longconst) + 1;
	  /* First, evaluate the structure into arg2 */
	  pc2 = (*pos)++;
	  tem2 = longest_to_int (exp->elts[pc2 + 1].longconst);
	  *pos += 3 + BYTES_TO_EXP_ELEM (tem2 + 1);
	  if (noside == EVAL_SKIP)
	    goto nosideret;

	  if (op == STRUCTOP_STRUCT)
	    {
	      /* If v is a variable in a register, and the user types
		 v.method (), this will produce an error, because v has
		 no address.

		 A possible way around this would be to allocate a
		 copy of the variable on the stack, copy in the
		 contents, call the function, and copy out the
		 contents.  I.e. convert this from call by reference
		 to call by copy-return (or whatever it's called).
		 However, this does not work because it is not the
		 same: the method being called could stash a copy of
		 the address, and then future uses through that address
		 (after the method returns) would be expected to
		 use the variable itself, not some copy of it.  */
	      arg2 = evaluate_subexp_for_address (exp, pos, noside);
	    }
	  else
	    {
	      arg2 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
	    }
	  /* Now, say which argument to start evaluating from */
	  tem = 2;
	}
      else
	{
	  nargs = longest_to_int (exp->elts[pc + 1].longconst);
	  tem = 0;
	}
      /* Allocate arg vector, including space for the function to be
	 called in argvec[0] and a terminating NULL */
      argvec = (value_ptr *) alloca (sizeof (value_ptr) * (nargs + 2));
      for (; tem <= nargs; tem++)
	/* Ensure that array expressions are coerced into pointer objects. */
	argvec[tem] = evaluate_subexp_with_coercion (exp, pos, noside);

      /* signal end of arglist */
      argvec[tem] = 0;

      if (op == STRUCTOP_STRUCT || op == STRUCTOP_PTR)
	{
	  int static_memfuncp;
	  value_ptr temp = arg2;
	  char tstr[64];

	  argvec[1] = arg2;
	  argvec[0] = 0;
	  strcpy(tstr, &exp->elts[pc2+2].string);
          if (!argvec[0]) 
	    {
	      temp = arg2;
	      argvec[0] =
	      value_struct_elt (&temp, argvec+1, tstr,
			      &static_memfuncp,
			      op == STRUCTOP_STRUCT
			      ? "structure" : "structure pointer");
	    }
	  arg2 = value_from_longest (lookup_pointer_type(VALUE_TYPE (temp)),
			 VALUE_ADDRESS (temp)+VALUE_OFFSET (temp));
	  argvec[1] = arg2;

	  if (static_memfuncp)
	    {
	      argvec[1] = argvec[0];
	      nargs--;
	      argvec++;
	    }
	}
      else if (op == STRUCTOP_MEMBER || op == STRUCTOP_MPTR)
	{
	  argvec[1] = arg2;
	  argvec[0] = arg1;
	}

      if (noside == EVAL_SKIP)
	goto nosideret;
      if (noside == EVAL_AVOID_SIDE_EFFECTS)
	{
	  /* If the return type doesn't look like a function type, call an
	     error.  This can happen if somebody tries to turn a variable into
	     a function call. This is here because people often want to
	     call, eg, strcmp, which gdb doesn't know is a function.  If
	     gdb isn't asked for it's opinion (ie. through "whatis"),
	     it won't offer it. */

	  struct type *ftype =
	    TYPE_TARGET_TYPE (VALUE_TYPE (argvec[0]));

	  if (ftype)
	    return allocate_value (TYPE_TARGET_TYPE (VALUE_TYPE (argvec[0])));
	  else
	    error ("Expression of type other than \"Function returning ...\" used as function");
	}
      return call_function_by_hand (argvec[0], nargs, argvec + 1);

    case STRUCTOP_STRUCT:
      tem = longest_to_int (exp->elts[pc + 1].longconst);
      (*pos) += 3 + BYTES_TO_EXP_ELEM (tem + 1);
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      if (noside == EVAL_SKIP)
	goto nosideret;
      if (noside == EVAL_AVOID_SIDE_EFFECTS)
	return value_zero (lookup_struct_elt_type (VALUE_TYPE (arg1),
						   &exp->elts[pc + 2].string,
						   0),
			   lval_memory);
      else
	{
	  value_ptr temp = arg1;
	  return value_struct_elt (&temp, NULL, &exp->elts[pc + 2].string,
				   NULL, "structure");
	}

    case STRUCTOP_PTR:
      tem = longest_to_int (exp->elts[pc + 1].longconst);
      (*pos) += 3 + BYTES_TO_EXP_ELEM (tem + 1);
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      if (noside == EVAL_SKIP)
	goto nosideret;
      if (noside == EVAL_AVOID_SIDE_EFFECTS)
	return value_zero (lookup_struct_elt_type (VALUE_TYPE (arg1),
						   &exp->elts[pc + 2].string,
						   0),
			   lval_memory);
      else
	{
	  value_ptr temp = arg1;
	  return value_struct_elt (&temp, NULL, &exp->elts[pc + 2].string,
				   NULL, "structure pointer");
	}

    case STRUCTOP_MEMBER:
      arg1 = evaluate_subexp_for_address (exp, pos, noside);
      goto handle_pointer_to_member;
    case STRUCTOP_MPTR:
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
    handle_pointer_to_member:
      arg2 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      if (noside == EVAL_SKIP)
	goto nosideret;
      if (TYPE_CODE (VALUE_TYPE (arg2)) != TYPE_CODE_PTR)
	goto bad_pointer_to_member;
      type = TYPE_TARGET_TYPE (VALUE_TYPE (arg2));
      if (TYPE_CODE (type) == TYPE_CODE_METHOD)
	error ("not implemented: pointer-to-method in pointer-to-member construct");
      if (TYPE_CODE (type) != TYPE_CODE_MEMBER)
	goto bad_pointer_to_member;
      /* Now, convert these values to an address.  */
      arg1 = value_cast (lookup_pointer_type (TYPE_DOMAIN_TYPE (type)),
			 arg1);
      arg3 = value_from_longest (lookup_pointer_type (TYPE_TARGET_TYPE (type)),
				 value_as_long (arg1) + value_as_long (arg2));
      return value_ind (arg3);
    bad_pointer_to_member:
      error("non-pointer-to-member value used in pointer-to-member construct");

    case BINOP_CONCAT:
      arg1 = evaluate_subexp_with_coercion (exp, pos, noside);
      arg2 = evaluate_subexp_with_coercion (exp, pos, noside);
      if (noside == EVAL_SKIP)
	goto nosideret;
      if (binop_user_defined_p (op, arg1, arg2))
	return value_x_binop (arg1, arg2, op, OP_NULL);
      else
	return value_concat (arg1, arg2);

    case BINOP_ASSIGN:
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = evaluate_subexp (VALUE_TYPE (arg1), exp, pos, noside);
      if (noside == EVAL_SKIP || noside == EVAL_AVOID_SIDE_EFFECTS)
	return arg1;
      if (binop_user_defined_p (op, arg1, arg2))
	return value_x_binop (arg1, arg2, op, OP_NULL);
      else
	return value_assign (arg1, arg2);

    case BINOP_ASSIGN_MODIFY:
      (*pos) += 2;
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = evaluate_subexp (VALUE_TYPE (arg1), exp, pos, noside);
      if (noside == EVAL_SKIP || noside == EVAL_AVOID_SIDE_EFFECTS)
	return arg1;
      op = exp->elts[pc + 1].opcode;
      if (binop_user_defined_p (op, arg1, arg2))
	return value_x_binop (arg1, arg2, BINOP_ASSIGN_MODIFY, op);
      else if (op == BINOP_ADD)
	arg2 = value_add (arg1, arg2);
      else if (op == BINOP_SUB)
	arg2 = value_sub (arg1, arg2);
      else
	arg2 = value_binop (arg1, arg2, op);
      return value_assign (arg1, arg2);

    case BINOP_ADD:
      arg1 = evaluate_subexp_with_coercion (exp, pos, noside);
      arg2 = evaluate_subexp_with_coercion (exp, pos, noside);
      if (noside == EVAL_SKIP)
	goto nosideret;
      if (binop_user_defined_p (op, arg1, arg2))
	return value_x_binop (arg1, arg2, op, OP_NULL);
      else
	return value_add (arg1, arg2);

    case BINOP_SUB:
      arg1 = evaluate_subexp_with_coercion (exp, pos, noside);
      arg2 = evaluate_subexp_with_coercion (exp, pos, noside);
      if (noside == EVAL_SKIP)
	goto nosideret;
      if (binop_user_defined_p (op, arg1, arg2))
	return value_x_binop (arg1, arg2, op, OP_NULL);
      else
	return value_sub (arg1, arg2);

    case BINOP_MUL:
    case BINOP_DIV:
    case BINOP_REM:
    case BINOP_MOD:
    case BINOP_LSH:
    case BINOP_RSH:
    case BINOP_BITWISE_AND:
    case BINOP_BITWISE_IOR:
    case BINOP_BITWISE_XOR:
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      if (noside == EVAL_SKIP)
	goto nosideret;
      if (binop_user_defined_p (op, arg1, arg2))
	return value_x_binop (arg1, arg2, op, OP_NULL);
      else
	if (noside == EVAL_AVOID_SIDE_EFFECTS
	    && (op == BINOP_DIV || op == BINOP_REM || op == BINOP_MOD))
	  return value_zero (VALUE_TYPE (arg1), not_lval);
      else
	return value_binop (arg1, arg2, op);

    case BINOP_SUBSCRIPT:
      arg1 = evaluate_subexp_with_coercion (exp, pos, noside);
      arg2 = evaluate_subexp_with_coercion (exp, pos, noside);
      if (noside == EVAL_SKIP)
	goto nosideret;
      if (noside == EVAL_AVOID_SIDE_EFFECTS)
	{
	  /* If the user attempts to subscript something that has no target
	     type (like a plain int variable for example), then report this
	     as an error. */

	  type = TYPE_TARGET_TYPE (VALUE_TYPE (arg1));
	  if (type)
	    return value_zero (type, VALUE_LVAL (arg1));
	  else
	    error ("cannot subscript something of type `%s'",
		   TYPE_NAME (VALUE_TYPE (arg1)));
	}
			   
      if (binop_user_defined_p (op, arg1, arg2))
	return value_x_binop (arg1, arg2, op, OP_NULL);
      else
	return value_subscript (arg1, arg2);

    case BINOP_IN:
      arg1 = evaluate_subexp_with_coercion (exp, pos, noside);
      arg2 = evaluate_subexp_with_coercion (exp, pos, noside);
      if (noside == EVAL_SKIP)
	goto nosideret;
      return value_in (arg1, arg2);
      
    case MULTI_SUBSCRIPT:
      (*pos) += 2;
      nargs = longest_to_int (exp->elts[pc + 1].longconst);
      arg1 = evaluate_subexp_with_coercion (exp, pos, noside);
      while (nargs-- > 0)
	{
	  arg2 = evaluate_subexp_with_coercion (exp, pos, noside);
	  /* FIXME:  EVAL_SKIP handling may not be correct. */
	  if (noside == EVAL_SKIP)
	    {
	      if (nargs > 0)
		{
		  continue;
		}
	      else
		{
		  goto nosideret;
		}
	    }
	  /* FIXME:  EVAL_AVOID_SIDE_EFFECTS handling may not be correct. */
	  if (noside == EVAL_AVOID_SIDE_EFFECTS)
	    {
	      /* If the user attempts to subscript something that has no target
		 type (like a plain int variable for example), then report this
		 as an error. */
	      
	      type = TYPE_TARGET_TYPE (VALUE_TYPE (arg1));
	      if (type != NULL)
		{
		  arg1 = value_zero (type, VALUE_LVAL (arg1));
		  noside = EVAL_SKIP;
		  continue;
		}
	      else
		{
		  error ("cannot subscript something of type `%s'",
			 TYPE_NAME (VALUE_TYPE (arg1)));
		}
	    }
	  
	  if (binop_user_defined_p (op, arg1, arg2))
	    {
	      arg1 = value_x_binop (arg1, arg2, op, OP_NULL);
	    }
	  else
	    {
	      arg1 = value_subscript (arg1, arg2);
	    }
	}
      return (arg1);

    case BINOP_LOGICAL_AND:
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      if (noside == EVAL_SKIP)
	{
	  arg2 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
	  goto nosideret;
	}
      
      oldpos = *pos;
      arg2 = evaluate_subexp (NULL_TYPE, exp, pos, EVAL_AVOID_SIDE_EFFECTS);
      *pos = oldpos;
      
      if (binop_user_defined_p (op, arg1, arg2)) 
	{
	  arg2 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
	  return value_x_binop (arg1, arg2, op, OP_NULL);
	}
      else
	{
	  tem = value_logical_not (arg1);
	  arg2 = evaluate_subexp (NULL_TYPE, exp, pos,
				  (tem ? EVAL_SKIP : noside));
	  return value_from_longest (builtin_type_int,
				  (LONGEST) (!tem && !value_logical_not (arg2)));
	}

    case BINOP_LOGICAL_OR:
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      if (noside == EVAL_SKIP)
	{
	  arg2 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
	  goto nosideret;
	}
      
      oldpos = *pos;
      arg2 = evaluate_subexp (NULL_TYPE, exp, pos, EVAL_AVOID_SIDE_EFFECTS);
      *pos = oldpos;
      
      if (binop_user_defined_p (op, arg1, arg2)) 
	{
	  arg2 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
	  return value_x_binop (arg1, arg2, op, OP_NULL);
	}
      else
	{
	  tem = value_logical_not (arg1);
	  arg2 = evaluate_subexp (NULL_TYPE, exp, pos,
				  (!tem ? EVAL_SKIP : noside));
	  return value_from_longest (builtin_type_int,
				  (LONGEST) (!tem || !value_logical_not (arg2)));
	}

    case BINOP_EQUAL:
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = evaluate_subexp (VALUE_TYPE (arg1), exp, pos, noside);
      if (noside == EVAL_SKIP)
	goto nosideret;
      if (binop_user_defined_p (op, arg1, arg2))
	{
	  return value_x_binop (arg1, arg2, op, OP_NULL);
	}
      else
	{
	  tem = value_equal (arg1, arg2);
	  return value_from_longest (builtin_type_int, (LONGEST) tem);
	}

    case BINOP_NOTEQUAL:
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = evaluate_subexp (VALUE_TYPE (arg1), exp, pos, noside);
      if (noside == EVAL_SKIP)
	goto nosideret;
      if (binop_user_defined_p (op, arg1, arg2))
	{
	  return value_x_binop (arg1, arg2, op, OP_NULL);
	}
      else
	{
	  tem = value_equal (arg1, arg2);
	  return value_from_longest (builtin_type_int, (LONGEST) ! tem);
	}

    case BINOP_LESS:
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = evaluate_subexp (VALUE_TYPE (arg1), exp, pos, noside);
      if (noside == EVAL_SKIP)
	goto nosideret;
      if (binop_user_defined_p (op, arg1, arg2))
	{
	  return value_x_binop (arg1, arg2, op, OP_NULL);
	}
      else
	{
	  tem = value_less (arg1, arg2);
	  return value_from_longest (builtin_type_int, (LONGEST) tem);
	}

    case BINOP_GTR:
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = evaluate_subexp (VALUE_TYPE (arg1), exp, pos, noside);
      if (noside == EVAL_SKIP)
	goto nosideret;
      if (binop_user_defined_p (op, arg1, arg2))
	{
	  return value_x_binop (arg1, arg2, op, OP_NULL);
	}
      else
	{
	  tem = value_less (arg2, arg1);
	  return value_from_longest (builtin_type_int, (LONGEST) tem);
	}

    case BINOP_GEQ:
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = evaluate_subexp (VALUE_TYPE (arg1), exp, pos, noside);
      if (noside == EVAL_SKIP)
	goto nosideret;
      if (binop_user_defined_p (op, arg1, arg2))
	{
	  return value_x_binop (arg1, arg2, op, OP_NULL);
	}
      else
	{
	  tem = value_less (arg2, arg1) || value_equal (arg1, arg2);
	  return value_from_longest (builtin_type_int, (LONGEST) tem);
	}

    case BINOP_LEQ:
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = evaluate_subexp (VALUE_TYPE (arg1), exp, pos, noside);
      if (noside == EVAL_SKIP)
	goto nosideret;
      if (binop_user_defined_p (op, arg1, arg2))
	{
	  return value_x_binop (arg1, arg2, op, OP_NULL);
	}
      else 
	{
	  tem = value_less (arg1, arg2) || value_equal (arg1, arg2);
	  return value_from_longest (builtin_type_int, (LONGEST) tem);
	}

    case BINOP_REPEAT:
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      if (noside == EVAL_SKIP)
	goto nosideret;
      if (TYPE_CODE (VALUE_TYPE (arg2)) != TYPE_CODE_INT)
	error ("Non-integral right operand for \"@\" operator.");
      if (noside == EVAL_AVOID_SIDE_EFFECTS)
	return allocate_repeat_value (VALUE_TYPE (arg1),
				      longest_to_int (value_as_long (arg2)));
      else
	return value_repeat (arg1, longest_to_int (value_as_long (arg2)));

    case BINOP_COMMA:
      evaluate_subexp (NULL_TYPE, exp, pos, noside);
      return evaluate_subexp (NULL_TYPE, exp, pos, noside);

    case UNOP_NEG:
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      if (noside == EVAL_SKIP)
	goto nosideret;
      if (unop_user_defined_p (op, arg1))
	return value_x_unop (arg1, op);
      else
	return value_neg (arg1);

    case UNOP_COMPLEMENT:
      /* C++: check for and handle destructor names.  */
      op = exp->elts[*pos].opcode;

      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      if (noside == EVAL_SKIP)
	goto nosideret;
      if (unop_user_defined_p (UNOP_COMPLEMENT, arg1))
	return value_x_unop (arg1, UNOP_COMPLEMENT);
      else
	return value_complement (arg1);

    case UNOP_LOGICAL_NOT:
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      if (noside == EVAL_SKIP)
	goto nosideret;
      if (unop_user_defined_p (op, arg1))
	return value_x_unop (arg1, op);
      else
	return value_from_longest (builtin_type_int,
				   (LONGEST) value_logical_not (arg1));

    case UNOP_IND:
      if (expect_type && TYPE_CODE (expect_type) == TYPE_CODE_PTR)
        expect_type = TYPE_TARGET_TYPE (expect_type);
      arg1 = evaluate_subexp (expect_type, exp, pos, noside);
      if (noside == EVAL_SKIP)
	goto nosideret;
      if (noside == EVAL_AVOID_SIDE_EFFECTS)
	{
	  if (TYPE_CODE (VALUE_TYPE (arg1)) == TYPE_CODE_PTR
	      || TYPE_CODE (VALUE_TYPE (arg1)) == TYPE_CODE_REF
	      /* In C you can dereference an array to get the 1st elt.  */
	      || TYPE_CODE (VALUE_TYPE (arg1)) == TYPE_CODE_ARRAY
	      )
	    return value_zero (TYPE_TARGET_TYPE (VALUE_TYPE (arg1)),
			       lval_memory);
	  else if (TYPE_CODE (VALUE_TYPE (arg1)) == TYPE_CODE_INT)
	    /* GDB allows dereferencing an int.  */
	    return value_zero (builtin_type_int, lval_memory);
	  else
	    error ("Attempt to take contents of a non-pointer value.");
	}
      return value_ind (arg1);

    case UNOP_ADDR:
      /* C++: check for and handle pointer to members.  */
      
      op = exp->elts[*pos].opcode;

      if (noside == EVAL_SKIP)
	{
	  if (op == OP_SCOPE)
	    {
	      int temm = longest_to_int (exp->elts[pc+3].longconst);
	      (*pos) += 3 + BYTES_TO_EXP_ELEM (temm + 1);
	    }
	  else
	    evaluate_subexp (expect_type, exp, pos, EVAL_SKIP);
	  goto nosideret;
	}

      return evaluate_subexp_for_address (exp, pos, noside);

    case UNOP_SIZEOF:
      if (noside == EVAL_SKIP)
	{
	  evaluate_subexp (NULL_TYPE, exp, pos, EVAL_SKIP);
	  goto nosideret;
	}
      return evaluate_subexp_for_sizeof (exp, pos);

    case UNOP_CAST:
      (*pos) += 2;
      arg1 = evaluate_subexp (expect_type, exp, pos, noside);
      if (noside == EVAL_SKIP)
	goto nosideret;
      return value_cast (exp->elts[pc + 1].type, arg1);

    case UNOP_MEMVAL:
      (*pos) += 2;
      arg1 = evaluate_subexp (expect_type, exp, pos, noside);
      if (noside == EVAL_SKIP)
	goto nosideret;
      if (noside == EVAL_AVOID_SIDE_EFFECTS)
	return value_zero (exp->elts[pc + 1].type, lval_memory);
      else
	return value_at_lazy (exp->elts[pc + 1].type,
			      value_as_pointer (arg1));

    case UNOP_PREINCREMENT:
      arg1 = evaluate_subexp (expect_type, exp, pos, noside);
      if (noside == EVAL_SKIP || noside == EVAL_AVOID_SIDE_EFFECTS)
	return arg1;
      else if (unop_user_defined_p (op, arg1))
	{
	  return value_x_unop (arg1, op);
	}
      else
	{
	  arg2 = value_add (arg1, value_from_longest (builtin_type_char, 
						   (LONGEST) 1));
	  return value_assign (arg1, arg2);
	}

    case UNOP_PREDECREMENT:
      arg1 = evaluate_subexp (expect_type, exp, pos, noside);
      if (noside == EVAL_SKIP || noside == EVAL_AVOID_SIDE_EFFECTS)
	return arg1;
      else if (unop_user_defined_p (op, arg1))
	{
	  return value_x_unop (arg1, op);
	}
      else
	{
	  arg2 = value_sub (arg1, value_from_longest (builtin_type_char, 
						   (LONGEST) 1));
	  return value_assign (arg1, arg2);
	}

    case UNOP_POSTINCREMENT:
      arg1 = evaluate_subexp (expect_type, exp, pos, noside);
      if (noside == EVAL_SKIP || noside == EVAL_AVOID_SIDE_EFFECTS)
	return arg1;
      else if (unop_user_defined_p (op, arg1))
	{
	  return value_x_unop (arg1, op);
	}
      else
	{
	  arg2 = value_add (arg1, value_from_longest (builtin_type_char, 
						   (LONGEST) 1));
	  value_assign (arg1, arg2);
	  return arg1;
	}

    case UNOP_POSTDECREMENT:
      arg1 = evaluate_subexp (expect_type, exp, pos, noside);
      if (noside == EVAL_SKIP || noside == EVAL_AVOID_SIDE_EFFECTS)
	return arg1;
      else if (unop_user_defined_p (op, arg1))
	{
	  return value_x_unop (arg1, op);
	}
      else
	{
	  arg2 = value_sub (arg1, value_from_longest (builtin_type_char, 
						   (LONGEST) 1));
	  value_assign (arg1, arg2);
	  return arg1;
	}
	
    case OP_THIS:
      (*pos) += 1;
      return value_of_this (1);

    case OP_M3_LONG:
    case OP_M3_CHAR:
      (*pos) += 3;
      return value_from_longest (exp->elts[pc+1].type,
                                 exp->elts[pc + 2].longconst);

    case OP_M3_REEL:
    case OP_M3_LREEL:
    case OP_M3_XREEL: {
      (*pos) += 3;
      return value_from_double (exp->elts[pc + 1].type,
				exp->elts[pc + 2].doubleconst); }

    case STRUCTOP_M3_INTERFACE:
    case STRUCTOP_M3_MODULE:
    case STRUCTOP_M3_STRUCT: {
      struct type *t;
      value_ptr v;
      int offset;
      CORE_ADDR tc_addr;
      char *field_name;

      field_name = &exp->elts[pc + 2].string;
      tem = longest_to_int (exp->elts[pc + 1].longconst);
      (*pos) += 3 + BYTES_TO_EXP_ELEM (tem + 1);
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);

    deref:
      t = VALUE_TYPE (arg1);
      if (TYPE_CODE (t) == TYPE_CODE_M3_REFANY
          || TYPE_CODE (t) == TYPE_CODE_M3_OBJECT
          || TYPE_CODE (t) == TYPE_CODE_M3_ROOT) {
        if (value_as_pointer (arg1) == 0) {
          error ("Cannot dereference NIL"); }
	t = find_m3_heap_type (value_as_pointer (arg1)); }

      if (TYPE_CODE (t) == TYPE_CODE_M3_POINTER
	  || TYPE_CODE (t) == TYPE_CODE_M3_INDIRECT) {
	arg1 = value_at_lazy (TYPE_M3_TARGET (t), value_as_pointer (arg1)); 
        goto deref; }

      else if (TYPE_CODE (t) == TYPE_CODE_M3_OBJECT) {
	tc_addr = find_m3_heap_tc_addr (value_as_pointer (arg1));
	while (TYPE_CODE (t) == TYPE_CODE_M3_OBJECT) {
	  if (find_m3_obj_field (t, field_name, 0, &offset, &t)) {
	    arg1 = value_at_lazy (t, value_as_pointer (arg1));
	    offset += 8 * tc_address_to_dataOffset (tc_addr); 
	    goto found; }

	  if (find_m3_obj_method (t, field_name, 0, &offset, &t)) {
	    arg1 = value_at_lazy (t, tc_address_to_defaultMethods (tc_addr));
	    offset += 8 * tc_address_to_methodOffset (tc_addr);
	    goto found; }

	  tc_addr = tc_address_to_parent_tc_address (tc_addr); 
	  t = find_m3_type_from_tc (tc_addr); }
	t = 0;
        found:; }
	
      else if (TYPE_CODE (t) == TYPE_CODE_M3_RECORD) {
	if (! find_m3_rec_field (t, field_name, 0, &offset, &t)) {
	  t = 0; }}
      else {
	error ("cannot apply \".%s\" to a value that is not a record, a REF record nor an object", field_name);
	return 0; }

      if (!t) {
	error ("no such field: %s", field_name);
	return 0; }

      v = allocate_value (t);
      VALUE_LVAL (v) = 1;
      VALUE_ADDRESS (v) = VALUE_ADDRESS (arg1);

      if (offset % 8 != 0) {
	  /* We have a non-byte aligned value, we need to pull it
	     now, so that we can shift the bits.  Fortunately, we
	     know that the type is a scalar.  We need to pull the
	     word that contains this field, because the offset is 
	     expressed from the lsb of this word */
	LONGEST i;
	VALUE_LAZY (v) = 0; 
	target_read_memory (VALUE_ADDRESS (v) + VALUE_OFFSET (arg1)
			      + offset / TARGET_LONG_BIT, 
			    (char*) &i, TYPE_LENGTH (t));
	*(LONGEST *) VALUE_CONTENTS_RAW (v) = m3_unpack_ord ((char *) &i,
					           offset % TARGET_LONG_BIT, 
						   TYPE_M3_SIZE (t), 0); }
      else {
	VALUE_LAZY (v) = 1; }
      VALUE_OFFSET (v) = VALUE_OFFSET (arg1) + offset / 8;
      return (v); }

    case M3_FINAL_TYPE: {
      struct type *arg1_type;

      arg1 = evaluate_subexp (0, exp, pos, noside);
      arg1_type = VALUE_TYPE (arg1);

      while (TYPE_CODE (arg1_type) == TYPE_CODE_M3_INDIRECT) {
	arg1_type = TYPE_M3_TARGET (arg1_type);
	arg1 = value_at_lazy (arg1_type, m3_unpack_pointer2 (arg1)); }

      if ((TYPE_CODE (arg1_type) == TYPE_CODE_M3_REFANY
           || TYPE_CODE (arg1_type) == TYPE_CODE_M3_ROOT
           || TYPE_CODE (arg1_type) == TYPE_CODE_M3_OBJECT)
          && value_as_pointer (arg1) != 0) {
	arg1_type = find_m3_heap_type (value_as_pointer (arg1)); }


      VALUE_TYPE (arg1) = arg1_type;
      return (arg1); }

    case OP_M3_TYPE:
      (*pos) += 2;
      arg1 = allocate_value (exp->elts[pc+1].type);
      *(LONGEST *) VALUE_CONTENTS_RAW (arg1) = TYPE_MAGIC;
      return arg1;

    case UNOP_M3_DEREF: {
      struct type *res_type, *arg1_type;

      arg1 = evaluate_subexp (0, exp, pos, noside);
      arg1_type = VALUE_TYPE (arg1);

      while (TYPE_CODE (arg1_type) == TYPE_CODE_M3_INDIRECT) {
	arg1_type = TYPE_M3_TARGET (arg1_type);
	arg1 = value_at_lazy (arg1_type, m3_unpack_pointer2 (arg1)); }

      if (value_as_pointer (arg1) == 0) {
        error ("^ applied to NIL"); }

      if (TYPE_CODE (arg1_type) == TYPE_CODE_M3_REFANY) {
	arg1_type = find_m3_heap_type (value_as_pointer (arg1)); }

      if (TYPE_CODE (arg1_type) == TYPE_CODE_M3_POINTER) {
        res_type = TYPE_M3_TARGET (arg1_type); }

      else {
        error ("^ applied to a non-REF"); }

      return value_at_lazy (res_type, m3_unpack_pointer2 (arg1)); }

    case UNOP_M3_NEG: {
      arg1 = evaluate_subexp (0, exp, pos, noside);
      
      type = VALUE_TYPE (arg1);
      if (TYPE_CODE (type) == TYPE_CODE_FLT)
	return value_from_double (type, - m3_unpack_float2 (arg1));
      else if (TYPE_CODE (type) == TYPE_CODE_M3_INTEGER)
	return value_from_longest (type, - m3_unpack_int2 (arg1));
      else {
	error ("'-' must be applied to an integer or floating-point value");
	return arg1;
      }}
      

    case UNOP_M3_FIRST:
    case UNOP_M3_LAST: 
    case UNOP_M3_NUMBER: {
      value_ptr res, array;
      struct type *array_type, *index_type;
      LONGEST lowerbound, upperbound, val;

      array = evaluate_subexp (0, exp, pos, noside);
      array_type = VALUE_TYPE (array);

      while (TYPE_CODE (array_type) == TYPE_CODE_M3_POINTER
	     || TYPE_CODE (array_type) == TYPE_CODE_M3_INDIRECT) {
	array_type = TYPE_M3_TARGET (array_type);
	array = value_at_lazy (array_type, m3_unpack_pointer2 (array));
        if (array == 0) {
          error ("FIRST, LAST or NUMBER applied to NIL");  }}

      if (TYPE_CODE (array_type) == TYPE_CODE_M3_ARRAY) {
	index_type = TYPE_M3_ARRAY_INDEX (array_type);
	m3_ordinal_bounds (index_type, &lowerbound, &upperbound);
      } else if (TYPE_CODE (array_type) == TYPE_CODE_M3_OPEN_ARRAY) {
	lowerbound = 0;
	upperbound = *(long*) (VALUE_CONTENTS (array) + sizeof(long)) - 1;
      } else if (is_m3_ordinal_type (array_type)) {
	index_type = array_type;
	m3_ordinal_bounds (index_type, &lowerbound, &upperbound);
      } else {
	error ("FIRST, LAST, NUMBER can only be applied to arrays.");
      }

      res = allocate_value (builtin_type_m3_integer);
      VALUE_LAZY (res) = 0;
      switch (op) {
	case UNOP_M3_FIRST:   val = lowerbound;  break;
	case UNOP_M3_LAST:    val = upperbound;  break;
	case UNOP_M3_NUMBER:  val = upperbound - lowerbound + 1; 
                              index_type = builtin_type_m3_cardinal;  break;
      }
      res = allocate_value (index_type);
      *(LONGEST *)VALUE_CONTENTS_RAW (res) = val;
      VALUE_LAZY (res) = 0;
      return res;
    }

    case UNOP_M3_ABS: {
      struct type *arg1_type;
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg1_type = VALUE_TYPE (arg1);
      if (TYPE_CODE (arg1_type) == TYPE_CODE_M3_INTEGER) {
	LONGEST val = m3_unpack_int2 (arg1);
	if (val < 0) { val = -val; };
	return value_from_longest (arg1_type, val);
      } else if (TYPE_CODE (arg1_type) == TYPE_CODE_FLT) {
	double val = m3_unpack_float2 (arg1);
	if (val < 0.0) { val = -val; };
	return value_from_double (arg1_type, val);
      } else {
	error ("ABS requires an INTEGER, REAL, LONGREAL, or EXTENDED parameter");
	return arg1;
      }}

    case UNOP_M3_ADR: {
      value_ptr v = evaluate_subexp_for_address (exp, pos, noside); 
      TYPE_CODE (VALUE_TYPE (v)) = TYPE_CODE_M3_ADDRESS;
      TYPE_M3_SIZE (VALUE_TYPE (v)) = TARGET_PTR_BIT;
      return v; }

    case UNOP_M3_ADRSIZE: {
      LONGEST sz;
      struct type *arg1_type;
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg1_type = VALUE_TYPE (arg1);
      sz = TYPE_M3_SIZE (arg1_type) / HOST_CHAR_BIT;
      if (TYPE_CODE (arg1_type) == TYPE_CODE_M3_OPEN_ARRAY) {
	error ("ADRSIZE(open array) not implemented");
	sz = 1;
      }
      return m3_value_from_longest (builtin_type_m3_integer, sz); }

    case UNOP_M3_BITSIZE: {
      LONGEST sz;
      struct type *arg1_type;
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg1_type = VALUE_TYPE (arg1);
      sz = TYPE_M3_SIZE (arg1_type);
      if (TYPE_CODE (arg1_type) == TYPE_CODE_M3_OPEN_ARRAY) {
	error ("BITSIZE(open array) not implemented");
	sz = 8;
      }
      return m3_value_from_longest (builtin_type_m3_integer, sz); }

    case UNOP_M3_BYTESIZE: {
      LONGEST sz;
      struct type *arg1_type;
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg1_type = VALUE_TYPE (arg1);
      sz = TYPE_M3_SIZE (arg1_type) / 8;
      if (TYPE_CODE (arg1_type) == TYPE_CODE_M3_OPEN_ARRAY) {
	error ("BYTESIZE(open array) not implemented");
	sz = 1;
      }
      return m3_value_from_longest (builtin_type_m3_integer, sz);
    }

    case UNOP_M3_CEILING: {
      struct type *arg1_type;
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg1_type = VALUE_TYPE (arg1);

      if (TYPE_CODE (arg1_type) == TYPE_CODE_FLT) {
        double val;
        LONGEST intval;
        val  = m3_unpack_float2 (arg1);
        intval = (LONGEST) (val);
        if ((val > 0.0e0) && ((double)intval != val)) { intval++; }
        return m3_value_from_longest (builtin_type_m3_integer, intval);
      } else {
	error ("CEILING must be applied to a floating-point value");
	return arg1;
      }
    }

    case UNOP_M3_FLOOR: {
      struct type *arg1_type;
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg1_type = VALUE_TYPE (arg1);

      if (TYPE_CODE (arg1_type) == TYPE_CODE_FLT) {
        double val;
        LONGEST intval;
        val  = m3_unpack_float2 (arg1);
        intval = (LONGEST) (val);
        if ((val < 0.0e0) && ((double)intval != val)) { intval--; }
        return m3_value_from_longest (builtin_type_m3_integer, intval);
      } else {
	error ("FLOOR must be applied to a floating-point value");
	return arg1;
      }
    }

    case UNOP_M3_ROUND: {
      struct type *arg1_type;
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg1_type = VALUE_TYPE (arg1);

      if (TYPE_CODE (arg1_type) == TYPE_CODE_FLT) {
        double val;
        LONGEST intval;
        val  = m3_unpack_float2 (arg1);
        intval = (LONGEST) (val + 0.5);
        return m3_value_from_longest (builtin_type_m3_integer, intval);
      } else {
	error ("ROUND must be applied to a floating-point value");
	return arg1;
      }
    }

    case UNOP_M3_TRUNC: {
      struct type *arg1_type;
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg1_type = VALUE_TYPE (arg1);

      if (TYPE_CODE (arg1_type) == TYPE_CODE_FLT) {
        double val;
        LONGEST intval;
        val  = m3_unpack_float2 (arg1);
        intval = (LONGEST) (val);
        return m3_value_from_longest (builtin_type_m3_integer, intval);
      } else {
	error ("TRUNC must be applied to a floating-point value");
	return arg1;
      }
    }

    case UNOP_M3_ORD: {
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      if (is_m3_ordinal_type (VALUE_TYPE (arg1))) {
        LONGEST val;
        val  = m3_unpack_int2 (arg1);
        return m3_value_from_longest (builtin_type_m3_integer, val);
      } else {
	error ("value passed to ORD is not of an ordinal type");
        return arg1;
      }
    }

    case BINOP_M3_VAL: {
      struct type *arg1_type;
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg1_type = VALUE_TYPE (arg1);

      if (TYPE_CODE (arg1_type) != TYPE_CODE_M3_INTEGER) {
	error ("first argument of VAL must be an integer");
	return arg1;
      } else if ((*(LONGEST *) VALUE_CONTENTS_RAW (arg2) != TYPE_MAGIC) ||
		 (! is_m3_ordinal_type (VALUE_TYPE(arg2)))) {
	error ("second argument of VAL must be an ordinal type");
	return arg1;
      } else {
        LONGEST val, lower, upper;
        val  = m3_unpack_int2 (arg1);
	m3_ordinal_bounds (VALUE_TYPE(arg2), &lower, &upper);
	if ((val < lower) || (upper < val)) {
	  error ("value passed to VAL is out of range");
	  return arg1;
        } else {
          return m3_value_from_longest (VALUE_TYPE(arg2), val);
        }
      }
    }

    case BINOP_M3_FLOAT: {
      struct type *arg1_type;
      double val;

      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg1_type = VALUE_TYPE (arg1);

      if (TYPE_CODE (arg1_type) == TYPE_CODE_FLT) {
        val = m3_unpack_float2 (arg1);
      } else if (TYPE_CODE (arg1_type) == TYPE_CODE_M3_INTEGER) {
        val = (double) m3_unpack_int2 (arg1);
      } else {
	error ("first parameter of FLOAT must be an INTEGER, REAL, LONGREAL, or EXTENDED value");
	return arg1;
      }

      if ((*(LONGEST *) VALUE_CONTENTS_RAW (arg2) != TYPE_MAGIC)
	 || (TYPE_CODE (VALUE_TYPE(arg2)) != TYPE_CODE_FLT)) {
	error ("second parameter of FLOAT must be REAL, LONGREAL, or EXTENDED");
	return arg1;
      }

      return value_from_double (VALUE_TYPE(arg2), val);
    }

    case BINOP_M3_LOOPHOLE: {
      struct type *arg1_type, *arg2_type;
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg1_type = VALUE_TYPE (arg1);
      arg2_type = VALUE_TYPE (arg2);

      if (TYPE_CODE(arg1_type) == TYPE_CODE_M3_OPEN_ARRAY) {
	error ("LOOPHOLE of open array values is illegal");
        return arg1;
      } else if (*(LONGEST *) VALUE_CONTENTS_RAW (arg2) != TYPE_MAGIC) {
	error ("second parameter of LOOPHOLE must be a type");
	return arg1;
      } else if (TYPE_CODE (arg2_type) == TYPE_CODE_M3_OPEN_ARRAY) {
	error ("LOOPHOLE to an open array type is not (yet) supported");
        return arg1;
      } else if (TYPE_M3_SIZE (arg1_type) != TYPE_M3_SIZE (arg2_type)) {
	error ("size of value and type passed to LOOPHOLE don't agree");
	return arg1;
      }

      VALUE_TYPE (arg1) = arg2_type;
      return arg1;
    }

    case BINOP_M3_SUBSCRIPT: {
      long lowerbound, upperbound, index_val; 
      long offset;
      struct type *index_type, *elem_type, *array_type;
      value_ptr v, array, index;
      long elt_size;

      array = evaluate_subexp (0, exp, pos, noside);
      index = evaluate_subexp (0, exp, pos, noside);
      array_type = VALUE_TYPE (array);

      while (TYPE_CODE (array_type) == TYPE_CODE_M3_POINTER
	     || TYPE_CODE (array_type) == TYPE_CODE_M3_INDIRECT) {
	array_type = TYPE_M3_TARGET (array_type);
	array = value_at_lazy (array_type, m3_unpack_pointer2 (array)); }

      if (TYPE_CODE (array_type) == TYPE_CODE_M3_ARRAY) {
	index_type = TYPE_M3_ARRAY_INDEX (array_type);
	elem_type  = TYPE_M3_ARRAY_ELEM (array_type);
	elt_size   = TYPE_M3_SIZE (elem_type);
	m3_ordinal_bounds (index_type, &lowerbound, &upperbound); }
      else if (TYPE_CODE (array_type) == TYPE_CODE_M3_OPEN_ARRAY) {
	elem_type = TYPE_M3_OPEN_ARRAY_ELEM (array_type);
	lowerbound = 0;
	upperbound = *(long*) (VALUE_CONTENTS (array) + TARGET_PTR_BIT/HOST_CHAR_BIT) - 1;
        { struct type *e = elem_type;
	  long n = (TARGET_PTR_BIT + TARGET_LONG_BIT) / HOST_CHAR_BIT;
	  elt_size = 1;
	  while (TYPE_CODE (e) == TYPE_CODE_M3_OPEN_ARRAY) {
	    elt_size *= *(long*) (VALUE_CONTENTS (array) + n);
	    n += sizeof (long);
	    e = TYPE_M3_OPEN_ARRAY_ELEM (e); }
	  elt_size *= TYPE_M3_SIZE (e); }}
      else {
	error ("indexed expression is not an array"); }

      COERCE_REF (array);

      index_val = m3_unpack_int2 (index);
      if (lowerbound > index_val || index_val > upperbound) {
	error ("range fault on array access");
	return 0; }

      offset = elt_size * (index_val - lowerbound);
      if (offset % 8 != 0) {
	error ("Extracting a bitfield"); 
	return 0; }
      
      v = allocate_value (elem_type);

      if (TYPE_CODE (array_type) == TYPE_CODE_M3_OPEN_ARRAY) {

	if (TYPE_CODE (elem_type) == TYPE_CODE_M3_OPEN_ARRAY) {
	  /* recreate a dope vector for the next guy */
	  memcpy (VALUE_CONTENTS_RAW (v) + (TARGET_PTR_BIT / HOST_CHAR_BIT),
		  VALUE_CONTENTS_RAW (array)
		    + (TARGET_PTR_BIT + TARGET_LONG_BIT)/ HOST_CHAR_BIT, 
		  TYPE_LENGTH (elem_type) - TARGET_LONG_BIT / HOST_CHAR_BIT);
	  *(char **)VALUE_CONTENTS_RAW (v) = 
	    *(char **)VALUE_CONTENTS_RAW (array) + offset / 8; }

	else {
	  /* mark the thing as not read yet */
	  VALUE_LAZY (v) = 1;
	  VALUE_LVAL (v) = VALUE_LVAL (array);
	  VALUE_ADDRESS (v) = 
	    (*(long*)(VALUE_CONTENTS_RAW (array))) + offset / 8;
	  VALUE_OFFSET (v) = 0; }}

      else {

	if (VALUE_LAZY (array)) {
	  VALUE_LAZY (v) = 1; }
	else {
	  memcpy (VALUE_CONTENTS_RAW (v), 
		  VALUE_CONTENTS_RAW (array) + offset / 8,
		  TYPE_LENGTH (elem_type)); }
	VALUE_LVAL (v) = VALUE_LVAL (array);
	if (VALUE_LVAL (array) == lval_internalvar) {
	  VALUE_LVAL (v) = lval_internalvar_component; }
	VALUE_ADDRESS (v) = VALUE_ADDRESS (array);
	VALUE_OFFSET (v) = VALUE_OFFSET (array) + offset / 8;  }
      return v; 
      break; }
      

    case BINOP_M3_DIVIDE: {
      float_ok = 1;
      int_ok = 0;
      goto arith_binop; }

    case BINOP_M3_DIV: {
      float_ok = 0;
      int_ok = 1;
      goto arith_binop; }

    case BINOP_M3_MOD:
    case BINOP_M3_MULT: 
    case BINOP_M3_ADD:
    case BINOP_M3_MINUS: {
      float_ok = 1;
      int_ok = 1;
      goto arith_binop; }

    arith_binop: {
      value_ptr res;
      LONGEST ival1, ival2;
      double fval1, fval2;
      struct type *arg1_type, *arg2_type;

      arg1 = evaluate_subexp (0, exp, pos, noside);
      arg2 = evaluate_subexp (0, exp, pos, noside);

      arg1_type = VALUE_TYPE (arg1);
    restart:
      switch (TYPE_CODE (arg1_type)) 
	{
	case TYPE_CODE_M3_INDIRECT:
	  arg1_type = TYPE_M3_TARGET (arg1_type);
	  arg1 = value_at_lazy (arg1_type, m3_unpack_pointer2 (arg1));
	  goto restart; 
	case TYPE_CODE_M3_PACKED:
	  arg1_type = TYPE_M3_TARGET (arg1_type);
	  goto restart;
	case TYPE_CODE_M3_CARDINAL:
	case TYPE_CODE_M3_SUBRANGE:
	  arg1_type = builtin_type_m3_integer;
	  /* fall through */
	case TYPE_CODE_M3_INTEGER:
	  ival1 = m3_unpack_int2 (arg1);
	  break;
	case TYPE_CODE_FLT:
	  fval1 = m3_unpack_float2 (arg1);
	  arg1_type = builtin_type_double;
	  break;
	case TYPE_CODE_INT:
	  ival1 = value_as_long (arg1);
	  arg1_type = builtin_type_m3_integer;
	  break;
        case TYPE_CODE_M3_ADDRESS:
        case TYPE_CODE_M3_POINTER:
        case TYPE_CODE_M3_NULL:
        case TYPE_CODE_M3_UN_ROOT:
          ival1 = (LONGEST) m3_unpack_pointer2 (arg1);
	  arg1_type = builtin_type_m3_integer;
	  break;
	default:
	  arg1_type = builtin_type_m3_void;
	  break; }

      arg2_type = VALUE_TYPE (arg2);
    restart2:
      switch (TYPE_CODE (arg2_type)) 
	{
	case TYPE_CODE_M3_INDIRECT:
	  arg2_type = TYPE_M3_TARGET (arg2_type);
	  arg2 = value_at_lazy (arg2_type, m3_unpack_pointer2 (arg2));
	  goto restart2; 
	case TYPE_CODE_M3_PACKED:
	  arg2_type = TYPE_M3_TARGET (arg2_type);
	  goto restart2;
	case TYPE_CODE_M3_CARDINAL:
	case TYPE_CODE_M3_SUBRANGE:
	  arg2_type = builtin_type_m3_integer;
	  /* fall through */
	case TYPE_CODE_M3_INTEGER:
	  ival2 = m3_unpack_int2 (arg2);
	  break;
	case TYPE_CODE_FLT:
	  fval2 = m3_unpack_float2 (arg2);
	  arg2_type = builtin_type_double;
	  break;
	case TYPE_CODE_INT:
	  ival2 = value_as_long (arg2);
	  arg2_type = builtin_type_m3_integer;
	  break;
        case TYPE_CODE_M3_ADDRESS:
        case TYPE_CODE_M3_POINTER:
        case TYPE_CODE_M3_NULL:
        case TYPE_CODE_M3_UN_ROOT:
          ival2 = (LONGEST) m3_unpack_pointer2 (arg2);
	  arg2_type = builtin_type_m3_integer;
	  break;
	default:
	  arg2_type = builtin_type_m3_void;
	  break; }


      if (TYPE_CODE (arg1_type) != TYPE_CODE (arg2_type)
	  || TYPE_CODE (arg1_type) == TYPE_CODE_M3_VOID
	  || (TYPE_CODE (arg1_type) == TYPE_CODE_M3_INTEGER && !int_ok)
	  || (TYPE_CODE (arg1_type) == TYPE_CODE_FLT && !float_ok)) {
	error ("wrong arguments for binary operation");
      }

      if (TYPE_CODE (arg1_type) == TYPE_CODE_M3_INTEGER) {
	LONGEST res;
	switch (op) {
	  case BINOP_M3_MULT: 	res = ival1 * ival2;          break;
	  case BINOP_M3_ADD:	res = ival1 + ival2;          break;
	  case BINOP_M3_MINUS:  res = ival1 - ival2;          break;
	  case BINOP_M3_DIV:    res = m3_div (ival1, ival2);  break;
	  case BINOP_M3_MOD:    res = m3_modi (ival1, ival2); break;
	} /* switch */
	return value_from_longest (builtin_type_m3_integer, res);
      }

      if (TYPE_CODE (arg1_type) == TYPE_CODE_FLT) {
	double res;
	switch (op) {
	  case BINOP_M3_DIVIDE: res = fval1 / fval2;          break;
	  case BINOP_M3_MULT:   res = fval1 * fval2;          break;
	  case BINOP_M3_ADD:    res = fval1 + fval2;          break;
	  case BINOP_M3_MINUS:  res = fval1 - fval2;          break;
	  case BINOP_M3_MOD:    res = m3_modf (fval1, fval2); break;
	}
	return value_from_double (arg1_type, res);
      }
    }

    case UNOP_M3_NOT: {
      LONGEST val;
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      val  = ! m3_unpack_int2 (arg1);
      return m3_value_from_longest (builtin_type_m3_boolean, val);  }
	
    case BINOP_M3_AND: {
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      if (m3_unpack_int2 (arg1) == 0) {	return arg1; }
      return evaluate_subexp (NULL_TYPE, exp, pos, noside); }

    case BINOP_M3_OR: {
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      if (m3_unpack_int2 (arg1) == 1) {	return arg1; }
      return evaluate_subexp (NULL_TYPE, exp, pos, noside); }

    case BINOP_M3_EQUAL: {
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = evaluate_subexp (VALUE_TYPE (arg1), exp, pos, noside);
      tem = m3_value_equal (arg1, arg2);
      return m3_value_from_longest (builtin_type_m3_boolean, (LONGEST) tem); }

    case BINOP_M3_NE: {
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = evaluate_subexp (VALUE_TYPE (arg1), exp, pos, noside);
      tem = ! m3_value_equal (arg1, arg2);
      return value_from_longest (builtin_type_m3_boolean, (LONGEST) tem); }

    case BINOP_M3_LT: {
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = evaluate_subexp (VALUE_TYPE (arg1), exp, pos, noside);
      tem = value_less (arg1, arg2);
      return value_from_longest (builtin_type_m3_boolean, (LONGEST) tem); }

    case BINOP_M3_LE: {
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = evaluate_subexp (VALUE_TYPE (arg1), exp, pos, noside);
      tem = ! (value_less (arg2, arg1));
      return value_from_longest (builtin_type_m3_boolean, (LONGEST) tem); }

    case BINOP_M3_GT: {
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = evaluate_subexp (VALUE_TYPE (arg1), exp, pos, noside);
      tem = value_less (arg2, arg1);
      return value_from_longest (builtin_type_m3_boolean, (LONGEST) tem); }

    case BINOP_M3_GE: {
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = evaluate_subexp (VALUE_TYPE (arg1), exp, pos, noside);
      tem = ! (value_less (arg1, arg2));
      return value_from_longest (builtin_type_m3_boolean, (LONGEST) tem); }

    case BINOP_M3_MIN: {
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = evaluate_subexp (VALUE_TYPE (arg1), exp, pos, noside);
      return value_less (arg1, arg2) ? arg1 : arg2; }

    case BINOP_M3_MAX: {
      arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
      arg2 = evaluate_subexp (VALUE_TYPE (arg1), exp, pos, noside);
      return value_less (arg1, arg2) ? arg2 : arg1; }

    case BINOP_M3_CAT:
      error ("Not yet implemented: '&' text concatenation");
      return 0; 

    case BINOP_M3_IN:
      error ("Not yet implemented: 'IN' set membership test");
      return 0; 

    case OP_TYPE:
      error ("Attempt to use a type name as an expression");

      
    default:
      /* Removing this case and compiling with gcc -Wall reveals that
	 a lot of cases are hitting this case.  Some of these should
	 probably be removed from expression.h (e.g. do we need a BINOP_SCOPE
	 and an OP_SCOPE?); others are legitimate expressions which are
	 (apparently) not fully implemented.

	 If there are any cases landing here which mean a user error,
	 then they should be separate cases, with more descriptive
	 error messages.  */

      error ("\
GDB does not (yet) know how to evaluated that kind of expression");
    }

 nosideret:
  return value_from_longest (builtin_type_long, (LONGEST) 1);
}

/* Evaluate a subexpression of EXP, at index *POS,
   and return the address of that subexpression.
   Advance *POS over the subexpression.
   If the subexpression isn't an lvalue, get an error.
   NOSIDE may be EVAL_AVOID_SIDE_EFFECTS;
   then only the type of the result need be correct.  */

static value_ptr
evaluate_subexp_for_address (exp, pos, noside)
     register struct expression *exp;
     register int *pos;
     enum noside noside;
{
  enum exp_opcode op;
  register int pc;
  struct symbol *var;

  pc = (*pos);
  op = exp->elts[pc].opcode;

  switch (op)
    {
    case UNOP_IND:
      (*pos)++;
      return evaluate_subexp (NULL_TYPE, exp, pos, noside);

    case UNOP_MEMVAL:
      (*pos) += 3;
      return value_cast (lookup_pointer_type (exp->elts[pc + 1].type),
			 evaluate_subexp (NULL_TYPE, exp, pos, noside));

    case OP_VAR_VALUE:
      var = exp->elts[pc + 2].symbol;

      /* C++: The "address" of a reference should yield the address
       * of the object pointed to. Let value_addr() deal with it. */
      if (TYPE_CODE (SYMBOL_TYPE (var)) == TYPE_CODE_REF)
        goto default_case;

      (*pos) += 4;
      if (noside == EVAL_AVOID_SIDE_EFFECTS)
	{
	  struct type *type =
	    lookup_pointer_type (SYMBOL_TYPE (var));
	  enum address_class sym_class = SYMBOL_CLASS (var);

	  if (sym_class == LOC_CONST
	      || sym_class == LOC_CONST_BYTES
	      || sym_class == LOC_REGISTER
	      || sym_class == LOC_REGPARM)
	    error ("Attempt to take address of register or constant.");

	return
	  value_zero (type, not_lval);
	}
      else
	return
	  locate_var_value
	    (var,
	     block_innermost_frame (exp->elts[pc + 1].block));

    default:
    default_case:
      if (noside == EVAL_AVOID_SIDE_EFFECTS)
	{
	  value_ptr x = evaluate_subexp (NULL_TYPE, exp, pos, noside);
	  if (VALUE_LVAL (x) == lval_memory)
	    return value_zero (lookup_pointer_type (VALUE_TYPE (x)),
			       not_lval);
	  else
	    error ("Attempt to take address of non-lval");
	}
      return value_addr (evaluate_subexp (NULL_TYPE, exp, pos, noside));
    }
}

/* Evaluate like `evaluate_subexp' except coercing arrays to pointers.
   When used in contexts where arrays will be coerced anyway, this is
   equivalent to `evaluate_subexp' but much faster because it avoids
   actually fetching array contents (perhaps obsolete now that we have
   VALUE_LAZY).

   Note that we currently only do the coercion for C expressions, where
   arrays are zero based and the coercion is correct.  For other languages,
   with nonzero based arrays, coercion loses.  Use CAST_IS_CONVERSION
   to decide if coercion is appropriate.

   */

static value_ptr
evaluate_subexp_with_coercion (exp, pos, noside)
     register struct expression *exp;
     register int *pos;
     enum noside noside;
{
  register enum exp_opcode op;
  register int pc;
  register value_ptr val;
  struct symbol *var;

  pc = (*pos);
  op = exp->elts[pc].opcode;

  switch (op)
    {
    case OP_VAR_VALUE:
      var = exp->elts[pc + 2].symbol;
      if (TYPE_CODE (SYMBOL_TYPE (var)) == TYPE_CODE_ARRAY
	  && CAST_IS_CONVERSION)
	{
	  (*pos) += 4;
	  val =
	    locate_var_value
	      (var, block_innermost_frame (exp->elts[pc + 1].block));
	  return value_cast (lookup_pointer_type (TYPE_TARGET_TYPE (SYMBOL_TYPE (var))),
			     val);
	}
      /* FALLTHROUGH */

    default:
      return evaluate_subexp (NULL_TYPE, exp, pos, noside);
    }
}

/* Evaluate a subexpression of EXP, at index *POS,
   and return a value for the size of that subexpression.
   Advance *POS over the subexpression.  */

static value_ptr
evaluate_subexp_for_sizeof (exp, pos)
     register struct expression *exp;
     register int *pos;
{
  enum exp_opcode op;
  register int pc;
  value_ptr val;

  pc = (*pos);
  op = exp->elts[pc].opcode;

  switch (op)
    {
      /* This case is handled specially
	 so that we avoid creating a value for the result type.
	 If the result type is very big, it's desirable not to
	 create a value unnecessarily.  */
    case UNOP_IND:
      (*pos)++;
      val = evaluate_subexp (NULL_TYPE, exp, pos, EVAL_AVOID_SIDE_EFFECTS);
      return value_from_longest (builtin_type_int, (LONGEST)
		      TYPE_LENGTH (TYPE_TARGET_TYPE (VALUE_TYPE (val))));

    case UNOP_MEMVAL:
      (*pos) += 3;
      return value_from_longest (builtin_type_int, 
			      (LONGEST) TYPE_LENGTH (exp->elts[pc + 1].type));

    case OP_VAR_VALUE:
      (*pos) += 4;
      return
	value_from_longest
	  (builtin_type_int,
	   (LONGEST) TYPE_LENGTH (SYMBOL_TYPE (exp->elts[pc + 2].symbol)));

    default:
      val = evaluate_subexp (NULL_TYPE, exp, pos, EVAL_AVOID_SIDE_EFFECTS);
      return value_from_longest (builtin_type_int,
			      (LONGEST) TYPE_LENGTH (VALUE_TYPE (val)));
    }
}

/* Parse a type expression in the string [P..P+LENGTH). */

struct type *
parse_and_eval_type (p, length)
     char *p;
     int length;
{
    char *tmp = (char *)alloca (length + 4);
    struct expression *expr;
    tmp[0] = '(';
    memcpy (tmp+1, p, length);
    tmp[length+1] = ')';
    tmp[length+2] = '0';
    tmp[length+3] = '\0';
    expr = parse_expression (tmp);
    if (expr->elts[0].opcode != UNOP_CAST)
	error ("Internal error in eval_type.");
    return expr->elts[1].type;
}
