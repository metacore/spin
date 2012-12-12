/* Support for printing Chill values for GDB, the GNU debugger.
   Copyright 1986, 1988, 1989, 1991, 1992, 1993, 1994
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
#include "obstack.h"
#include "symtab.h"
#include "gdbtypes.h"
#include "valprint.h"
#include "expression.h"
#include "value.h"
#include "language.h"
#include "demangle.h"
#include "c-lang.h" /* For c_val_print */
#include "typeprint.h"
#include "ch-lang.h"

static void
chill_print_value_fields PARAMS ((struct type *, char *, GDB_FILE *, int, int,
				  enum val_prettyprint, struct type **));


/* Print the elements of an array.
   Similar to val_print_array_elements, but prints
   element indexes (in Chill syntax). */

static void
chill_val_print_array_elements (type, valaddr, address, stream,
				format, deref_ref, recurse, pretty)
     struct type *type;
     char *valaddr;
     CORE_ADDR address;
     GDB_FILE *stream;
     int format;
     int deref_ref;
     int recurse;
     enum val_prettyprint pretty;
{
  unsigned int i = 0;
  unsigned int things_printed = 0;
  unsigned len;
  struct type *elttype;
  struct type *range_type = TYPE_FIELD_TYPE (type, 0);
  struct type *index_type = TYPE_TARGET_TYPE (range_type);
  unsigned eltlen;
  /* Position of the array element we are examining to see
     whether it is repeated.  */
  unsigned int rep1;
  /* Number of repetitions we have detected so far.  */
  unsigned int reps;
  LONGEST low_bound =  TYPE_FIELD_BITPOS (range_type, 0);
  LONGEST high_bound = TYPE_FIELD_BITPOS (range_type, 1);
      
  elttype = TYPE_TARGET_TYPE (type);
  eltlen = TYPE_LENGTH (elttype);
  len = TYPE_LENGTH (type) / eltlen;

  annotate_array_section_begin (i, elttype);

  for (; i < len && things_printed < print_max; i++)
    {
      if (i != 0)
	{
	  if (prettyprint_arrays)
	    {
	      fprintf_filtered (stream, ",\n");
	      print_spaces_filtered (2 + 2 * recurse, stream);
	    }
	  else
	    {
	      fprintf_filtered (stream, ", ");
	    }
	}
      wrap_here (n_spaces (2 + 2 * recurse));

      rep1 = i + 1;
      reps = 1;
      while ((rep1 < len) && 
	     !memcmp (valaddr + i * eltlen, valaddr + rep1 * eltlen, eltlen))
	{
	  ++reps;
	  ++rep1;
	}

      fputs_filtered ("(", stream);
      print_type_scalar (index_type, low_bound + i, stream);
      if (reps > 1)
	{
	  fputs_filtered (":", stream);
	  print_type_scalar (index_type, low_bound + i + reps - 1, stream);
	  fputs_filtered ("): ", stream);
	  val_print (elttype, valaddr + i * eltlen, 0, stream, format,
		     deref_ref, recurse + 1, pretty);

	  i = rep1 - 1;
	  things_printed += 1;
	}
      else
	{
	  fputs_filtered ("): ", stream);
	  val_print (elttype, valaddr + i * eltlen, 0, stream, format,
		     deref_ref, recurse + 1, pretty);
	  annotate_elt ();
	  things_printed++;
	}
    }
  annotate_array_section_end ();
  if (i < len)
    {
      fprintf_filtered (stream, "...");
    }
}

/* Print data of type TYPE located at VALADDR (within GDB), which came from
   the inferior at address ADDRESS, onto stdio stream STREAM according to
   FORMAT (a letter or 0 for natural format).  The data at VALADDR is in
   target byte order.

   If the data are a string pointer, returns the number of string characters
   printed.

   If DEREF_REF is nonzero, then dereference references, otherwise just print
   them like pointers.

   The PRETTY parameter controls prettyprinting.  */

int
chill_val_print (type, valaddr, address, stream, format, deref_ref, recurse,
		 pretty)
     struct type *type;
     char *valaddr;
     CORE_ADDR address;
     GDB_FILE *stream;
     int format;
     int deref_ref;
     int recurse;
     enum val_prettyprint pretty;
{
  LONGEST val;
  unsigned int i = 0;		/* Number of characters printed.  */
  struct type *elttype;
  CORE_ADDR addr;

  switch (TYPE_CODE (type))
    {
    case TYPE_CODE_ARRAY:
      if (TYPE_LENGTH (type) > 0 && TYPE_LENGTH (TYPE_TARGET_TYPE (type)) > 0)
	{
	  if (prettyprint_arrays)
	    {
	      print_spaces_filtered (2 + 2 * recurse, stream);
	    }
	  fprintf_filtered (stream, "[");
	  chill_val_print_array_elements (type, valaddr, address, stream,
					  format, deref_ref, recurse, pretty);
	  fprintf_filtered (stream, "]");
	}
      else
	{
	  error ("unimplemented in chill_val_print; unspecified array length");
	}
      break;

    case TYPE_CODE_INT:
      format = format ? format : output_format;
      if (format)
	{
	  print_scalar_formatted (valaddr, type, format, 0, stream);
	}
      else
	{
	  val_print_type_code_int (type, valaddr, stream);
	}
      break;

    case TYPE_CODE_CHAR:
      format = format ? format : output_format;
      if (format)
	{
	  print_scalar_formatted (valaddr, type, format, 0, stream);
	}
      else
	{
	  LA_PRINT_CHAR ((unsigned char) unpack_long (type, valaddr),
			 stream);
	}
      break;

    case TYPE_CODE_FLT:
      if (format)
	{
	  print_scalar_formatted (valaddr, type, format, 0, stream);
	}
      else
	{
	  print_floating (valaddr, type, stream);
	}
      break;

    case TYPE_CODE_BOOL:
      format = format ? format : output_format;
      if (format)
	{
	  print_scalar_formatted (valaddr, type, format, 0, stream);
	}
      else
	{
	  /* FIXME: Why is this using builtin_type_chill_bool not type?  */
	  val = unpack_long (builtin_type_chill_bool, valaddr);
	  fprintf_filtered (stream, val ? "TRUE" : "FALSE");
	}
      break;

    case TYPE_CODE_UNDEF:
      /* This happens (without TYPE_FLAG_STUB set) on systems which don't use
	 dbx xrefs (NO_DBX_XREFS in gcc) if a file has a "struct foo *bar"
	 and no complete type for struct foo in that file.  */
      fprintf_filtered (stream, "<incomplete type>");
      break;

    case TYPE_CODE_PTR:
      if (format && format != 's')
	{
	  print_scalar_formatted (valaddr, type, format, 0, stream);
	  break;
	}
      addr = unpack_pointer (type, valaddr);
      elttype = TYPE_TARGET_TYPE (type);

      /* We assume a NULL pointer is all zeros ... */
      if (addr == 0)
	{
	  fputs_filtered ("NULL", stream);
	  return 0;
	}
      
      if (TYPE_CODE (elttype) == TYPE_CODE_FUNC)
	{
	  /* Try to print what function it points to.  */
	  print_address_demangle (addr, stream, demangle);
	  /* Return value is irrelevant except for string pointers.  */
	  return (0);
	}
      if (addressprint && format != 's')
	{
	  print_address_numeric (addr, 1, stream);
	}
      
      /* For a pointer to char or unsigned char, also print the string
	 pointed to, unless pointer is null.  */
      if (TYPE_LENGTH (elttype) == 1
	  && TYPE_CODE (elttype) == TYPE_CODE_CHAR
	  && (format == 0 || format == 's')
	  && addr != 0
	  && /* If print_max is UINT_MAX, the alloca below will fail.
		In that case don't try to print the string.  */
	  print_max < UINT_MAX)
	  {
	    i = val_print_string (addr, 0, stream);
	  }
      /* Return number of characters printed, plus one for the
	 terminating null if we have "reached the end".  */
      return (i + (print_max && i != print_max));
      break;

    case TYPE_CODE_STRING:
      if (format && format != 's')
	{
	  print_scalar_formatted (valaddr, type, format, 0, stream);
	  break;
	}
      i = TYPE_LENGTH (type);
      LA_PRINT_STRING (stream, valaddr, i, 0);
      /* Return number of characters printed, plus one for the terminating
	 null if we have "reached the end".  */
      return (i + (print_max && i != print_max));
      break;

    case TYPE_CODE_BITSTRING:
    case TYPE_CODE_SET:
      elttype = TYPE_FIELD_TYPE (type, 0);
      check_stub_type (elttype);
      if (TYPE_FLAGS (elttype) & TYPE_FLAG_STUB)
	{
	  fprintf_filtered (stream, "<incomplete type>");
	  gdb_flush (stream);
	  break;
	}
      {
	struct type *range = elttype;
	int low_bound = TYPE_LOW_BOUND (range);
	int high_bound = TYPE_HIGH_BOUND (range);
	int i;
	int is_bitstring = TYPE_CODE (type) == TYPE_CODE_BITSTRING;
	int need_comma = 0;

	if (is_bitstring)
	  fputs_filtered ("B'", stream);
	else
	  fputs_filtered ("[", stream);
	for (i = low_bound; i <= high_bound; i++)
	  {
	    int element = value_bit_index (type, valaddr, i);
	    if (is_bitstring)
	      fprintf_filtered (stream, "%d", element);
	    else if (element)
	      {
		if (need_comma)
		  fputs_filtered (", ", stream);
		print_type_scalar (TYPE_TARGET_TYPE (range), i, stream);
		need_comma = 1;

		/* Look for a continuous range of true elements. */
		if (i+1 <= high_bound && value_bit_index (type, valaddr, ++i))
		  {
		    int j = i; /* j is the upper bound so far of the range */
		    fputs_filtered (":", stream);
		    while (i+1 <= high_bound
			   && value_bit_index (type, valaddr, ++i))
		      j = i;
		    print_type_scalar (TYPE_TARGET_TYPE (range), j, stream);
		  }
	      }
	  }
	if (is_bitstring)
	  fputs_filtered ("'", stream);
	else
	  fputs_filtered ("]", stream);
      }
      break;

    case TYPE_CODE_STRUCT:
      if (chill_is_varying_struct (type))
	{
	  struct type *inner = TYPE_FIELD_TYPE (type, 1);
	  long length = unpack_long (TYPE_FIELD_TYPE (type, 0), valaddr);
	  char *data_addr = valaddr + TYPE_FIELD_BITPOS (type, 1) / 8;
	  
	  switch (TYPE_CODE (inner))
	    {
	    case TYPE_CODE_STRING:
	      if (length > TYPE_LENGTH (type))
		{
		  fprintf_filtered (stream,
				    "<dynamic length %d > static length %d>",
				    length, TYPE_LENGTH (type));
		}
	      LA_PRINT_STRING (stream, data_addr, length, 0);
	      return length;
	    default:
	      break;
	    }
	}
      chill_print_value_fields (type, valaddr, stream, format, recurse, pretty,
				0);
      break;

    case TYPE_CODE_REF:
      if (addressprint)
        {
	  fprintf_filtered (stream, "LOC(");
	  print_address_numeric
	    (extract_address (valaddr, TARGET_PTR_BIT / HOST_CHAR_BIT),
	     1,
	     stream);
	  fprintf_filtered (stream, ")");
	  if (deref_ref)
	    fputs_filtered (": ", stream);
        }
      /* De-reference the reference.  */
      if (deref_ref)
	{
	  if (TYPE_CODE (TYPE_TARGET_TYPE (type)) != TYPE_CODE_UNDEF)
	    {
	      value_ptr deref_val =
		value_at
		  (TYPE_TARGET_TYPE (type),
		   unpack_pointer (lookup_pointer_type (builtin_type_void),
				   valaddr));
	      val_print (VALUE_TYPE (deref_val),
			 VALUE_CONTENTS (deref_val),
			 VALUE_ADDRESS (deref_val), stream, format,
			 deref_ref, recurse + 1, pretty);
	    }
	  else
	    fputs_filtered ("???", stream);
	}
      break;

    case TYPE_CODE_ENUM:
      c_val_print (type, valaddr, address, stream, format,
		   deref_ref, recurse, pretty);
      break;

    case TYPE_CODE_RANGE:
      if (TYPE_TARGET_TYPE (type))
	chill_val_print (TYPE_TARGET_TYPE (type), valaddr, address, stream,
			 format, deref_ref, recurse, pretty);
      break;

    case TYPE_CODE_MEMBER:
    case TYPE_CODE_UNION:
    case TYPE_CODE_FUNC:
    case TYPE_CODE_VOID:
    case TYPE_CODE_ERROR:
    default:
      /* Let's defer printing to the C printer, rather than
	 print an error message.  FIXME! */
      c_val_print (type, valaddr, address, stream, format,
		   deref_ref, recurse, pretty);
    }
  gdb_flush (stream);
  return (0);
}

/* Mutually recursive subroutines of cplus_print_value and c_val_print to
   print out a structure's fields: cp_print_value_fields and cplus_print_value.

   TYPE, VALADDR, STREAM, RECURSE, and PRETTY have the
   same meanings as in cplus_print_value and c_val_print.

   DONT_PRINT is an array of baseclass types that we
   should not print, or zero if called from top level.  */

static void
chill_print_value_fields (type, valaddr, stream, format, recurse, pretty,
			  dont_print)
     struct type *type;
     char *valaddr;
     GDB_FILE *stream;
     int format;
     int recurse;
     enum val_prettyprint pretty;
     struct type **dont_print;
{
  int i, len;
  int fields_seen = 0;

  check_stub_type (type);

  fprintf_filtered (stream, "[");
  len = TYPE_NFIELDS (type);
  if (len == 0)
    {
      fprintf_filtered (stream, "<No data fields>");
    }
  else
    {
      for (i = 0; i < len; i++)
	{
	  if (fields_seen)
	    {
	      fprintf_filtered (stream, ", ");
	    }
	  fields_seen = 1;
	  if (pretty)
	    {
	      fprintf_filtered (stream, "\n");
	      print_spaces_filtered (2 + 2 * recurse, stream);
	    }
	  else 
	    {
	      wrap_here (n_spaces (2 + 2 * recurse));
	    }
	  fputs_filtered (".", stream);
	  fprintf_symbol_filtered (stream, TYPE_FIELD_NAME (type, i),
				   language_chill, DMGL_NO_OPTS);
	  fputs_filtered (": ", stream);
	  if (TYPE_FIELD_PACKED (type, i))
	    {
	      value_ptr v;

	      /* Bitfields require special handling, especially due to byte
		 order problems.  */
	      v = value_from_longest (TYPE_FIELD_TYPE (type, i),
				      unpack_field_as_long (type, valaddr, i));

	      chill_val_print (TYPE_FIELD_TYPE (type, i), VALUE_CONTENTS (v), 0,
			       stream, format, 0, recurse + 1, pretty);
	    }
	  else
	    {
	      chill_val_print (TYPE_FIELD_TYPE (type, i), 
			       valaddr + TYPE_FIELD_BITPOS (type, i) / 8,
			       0, stream, format, 0, recurse + 1, pretty);
	    }
	}
      if (pretty)
	{
	  fprintf_filtered (stream, "\n");
	  print_spaces_filtered (2 * recurse, stream);
	}
    }
  fprintf_filtered (stream, "]");
}

int
chill_value_print (val, stream, format, pretty)
     value_ptr val;
     GDB_FILE *stream;
     int format;
     enum val_prettyprint pretty;
{
  /* A "repeated" value really contains several values in a row.
     They are made by the @ operator.
     Print such values as if they were arrays.  */

  if (VALUE_REPEATED (val))
    {
      register unsigned int n = VALUE_REPETITIONS (val);
      register unsigned int typelen = TYPE_LENGTH (VALUE_TYPE (val));
      fprintf_filtered (stream, "[");
      /* Print arrays of characters using string syntax.  */
      if (typelen == 1 && TYPE_CODE (VALUE_TYPE (val)) == TYPE_CODE_INT
	  && format == 0)
	LA_PRINT_STRING (stream, VALUE_CONTENTS (val), n, 0);
      else
	{
	  value_print_array_elements (val, stream, format, pretty);
	}
      fprintf_filtered (stream, "]");
      return (n * typelen);
    }
  else
    {
      struct type *type = VALUE_TYPE (val);

      /* If it is a pointer, indicate what it points to.

	 Print type also if it is a reference.

         C++: if it is a member pointer, we will take care
	 of that when we print it.  */
      if (TYPE_CODE (type) == TYPE_CODE_PTR ||
	  TYPE_CODE (type) == TYPE_CODE_REF)
	{
	  char *valaddr = VALUE_CONTENTS (val);
	  CORE_ADDR addr = unpack_pointer (type, valaddr);
          if (TYPE_CODE (type) != TYPE_CODE_PTR || addr != 0)
	    {
	      int i;
	      char *name = TYPE_NAME (type);
	      if (name)
		fputs_filtered (name, stream);
	      else if (TYPE_CODE (TYPE_TARGET_TYPE (type)) == TYPE_CODE_VOID)
		fputs_filtered ("PTR", stream);
	      else
		{
		  fprintf_filtered (stream, "(");
		  type_print (type, "", stream, -1);
		  fprintf_filtered (stream, ")");
		}
	      fprintf_filtered (stream, "(");
	      i = val_print (type, valaddr, VALUE_ADDRESS (val),
			     stream, format, 1, 0, pretty);
	      fprintf_filtered (stream, ")");
	      return i;
	    }
	}
      return (val_print (type, VALUE_CONTENTS (val),
			 VALUE_ADDRESS (val), stream, format, 1, 0, pretty));
    }
}


