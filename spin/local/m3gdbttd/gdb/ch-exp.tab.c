
/*  A Bison parser, made from ./ch-exp.y with Bison version GNU Bison version 1.22
  */

#define YYBISON 1  /* Identify Bison output.  */

#define	FIXME_01	258
#define	FIXME_02	259
#define	FIXME_03	260
#define	FIXME_04	261
#define	FIXME_05	262
#define	FIXME_06	263
#define	FIXME_07	264
#define	FIXME_08	265
#define	FIXME_09	266
#define	FIXME_10	267
#define	FIXME_11	268
#define	FIXME_12	269
#define	FIXME_13	270
#define	FIXME_14	271
#define	FIXME_15	272
#define	FIXME_16	273
#define	FIXME_17	274
#define	FIXME_18	275
#define	FIXME_19	276
#define	FIXME_20	277
#define	FIXME_21	278
#define	FIXME_22	279
#define	FIXME_24	280
#define	FIXME_25	281
#define	FIXME_26	282
#define	FIXME_27	283
#define	FIXME_28	284
#define	FIXME_29	285
#define	FIXME_30	286
#define	INTEGER_LITERAL	287
#define	BOOLEAN_LITERAL	288
#define	CHARACTER_LITERAL	289
#define	FLOAT_LITERAL	290
#define	GENERAL_PROCEDURE_NAME	291
#define	LOCATION_NAME	292
#define	SET_LITERAL	293
#define	EMPTINESS_LITERAL	294
#define	CHARACTER_STRING_LITERAL	295
#define	BIT_STRING_LITERAL	296
#define	TYPENAME	297
#define	FIELD_NAME	298
#define	CASE	299
#define	OF	300
#define	ESAC	301
#define	LOGIOR	302
#define	ORIF	303
#define	LOGXOR	304
#define	LOGAND	305
#define	ANDIF	306
#define	NOTEQUAL	307
#define	GTR	308
#define	LEQ	309
#define	IN	310
#define	SLASH_SLASH	311
#define	MOD	312
#define	REM	313
#define	NOT	314
#define	POINTER	315
#define	RECEIVE	316
#define	UP	317
#define	IF	318
#define	THEN	319
#define	ELSE	320
#define	FI	321
#define	ELSIF	322
#define	ILLEGAL_TOKEN	323
#define	NUM	324
#define	PRED	325
#define	SUCC	326
#define	ABS	327
#define	CARD	328
#define	MAX_TOKEN	329
#define	MIN_TOKEN	330
#define	SIZE	331
#define	UPPER	332
#define	LOWER	333
#define	LENGTH	334
#define	GDB_REGNAME	335
#define	GDB_LAST	336
#define	GDB_VARIABLE	337
#define	GDB_ASSIGNMENT	338

#line 54 "./ch-exp.y"


#include "defs.h"
#include <string.h>
#include <ctype.h>
#include "expression.h"
#include "language.h"
#include "value.h"
#include "parser-defs.h"
#include "ch-lang.h"
#include "bfd.h" /* Required by objfiles.h.  */
#include "symfile.h" /* Required by objfiles.h.  */
#include "objfiles.h" /* For have_full_symbols and have_partial_symbols */

/* Remap normal yacc parser interface names (yyparse, yylex, yyerror, etc),
   as well as gratuitiously global symbol names, so we can have multiple
   yacc generated parsers in gdb.  Note that these are only the variables
   produced by yacc.  If other parser generators (bison, byacc, etc) produce
   additional global names that conflict at link time, then those parser
   generators need to be fixed instead of adding those names to this list. */

#define	yymaxdepth chill_maxdepth
#define	yyparse	chill_parse
#define	yylex	chill_lex
#define	yyerror	chill_error
#define	yylval	chill_lval
#define	yychar	chill_char
#define	yydebug	chill_debug
#define	yypact	chill_pact
#define	yyr1	chill_r1
#define	yyr2	chill_r2
#define	yydef	chill_def
#define	yychk	chill_chk
#define	yypgo	chill_pgo
#define	yyact	chill_act
#define	yyexca	chill_exca
#define	yyerrflag chill_errflag
#define	yynerrs	chill_nerrs
#define	yyps	chill_ps
#define	yypv	chill_pv
#define	yys	chill_s
#define	yy_yys	chill_yys
#define	yystate	chill_state
#define	yytmp	chill_tmp
#define	yyv	chill_v
#define	yy_yyv	chill_yyv
#define	yyval	chill_val
#define	yylloc	chill_lloc
#define	yyreds	chill_reds		/* With YYDEBUG defined */
#define	yytoks	chill_toks		/* With YYDEBUG defined */

#ifndef YYDEBUG
#define	YYDEBUG	0		/* Default to no yydebug support */
#endif

int
yyparse PARAMS ((void));

static int
yylex PARAMS ((void));

void
yyerror PARAMS ((char *));


#line 124 "./ch-exp.y"
typedef union
  {
    LONGEST lval;
    unsigned LONGEST ulval;
    struct {
      LONGEST val;
      struct type *type;
    } typed_val;
    double dval;
    struct symbol *sym;
    struct type *tval;
    struct stoken sval;
    struct ttype tsym;
    struct symtoken ssym;
    int voidval;
    struct block *bval;
    enum exp_opcode opcode;
    struct internalvar *ivar;

    struct type **tvec;
    int *ivec;
  } YYSTYPE;

#ifndef YYLTYPE
typedef
  struct yyltype
    {
      int timestamp;
      int first_line;
      int first_column;
      int last_line;
      int last_column;
      char *text;
   }
  yyltype;

#define YYLTYPE yyltype
#endif

#include <stdio.h>

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	YYFINAL		254
#define	YYFLAG		-32768
#define	YYNTBASE	99

#define YYTRANSLATE(x) ((unsigned)(x) <= 338 ? yytranslate[x] : 162)

static const char yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,    74,
    75,    64,    62,    98,    63,    44,    65,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,    46,    45,    59,
    55,    57,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
    72,     2,    73,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     1,     2,     3,     4,     5,
     6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
    16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
    26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
    36,    37,    38,    39,    40,    41,    42,    43,    47,    48,
    49,    50,    51,    52,    53,    54,    56,    58,    60,    61,
    66,    67,    68,    69,    70,    71,    76,    77,    78,    79,
    80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
    90,    91,    92,    93,    94,    95,    96,    97
};

#if YYDEBUG != 0
static const short yyprhs[] = {     0,
     0,     2,     4,     6,     8,    10,    12,    15,    17,    19,
    21,    23,    25,    27,    31,    33,    35,    37,    39,    41,
    43,    45,    47,    49,    51,    53,    55,    57,    59,    61,
    63,    65,    67,    69,    71,    73,    75,    77,    79,    81,
    83,    85,    87,    89,    91,    96,   103,   110,   111,   117,
   124,   131,   134,   137,   139,   141,   143,   145,   149,   151,
   153,   155,   161,   171,   174,   177,   182,   184,   189,   191,
   195,   199,   203,   205,   209,   213,   215,   219,   223,   227,
   231,   235,   239,   243,   245,   249,   253,   257,   259,   263,
   267,   271,   275,   277,   280,   283,   286,   289,   292,   294,
   298,   303,   308,   313,   318,   323,   328,   333,   338,   343,
   348,   353,   358,   360,   365,   370,   375,   377,   379,   381,
   383,   385,   387,   389,   391,   393,   395,   397,   399,   401,
   403,   405,   407,   409,   411,   413,   415,   417,   419,   421,
   423
};

static const short yyrhs[] = {   100,
     0,   138,     0,   122,     0,   101,     0,     3,     0,   103,
     0,   105,    70,     0,    37,     0,    95,     0,    94,     0,
    96,     0,     5,     0,   122,     0,   104,    98,   122,     0,
   106,     0,   107,     0,   108,     0,   109,     0,   110,     0,
   111,     0,   112,     0,   114,     0,   115,     0,   116,     0,
   117,     0,   118,     0,   119,     0,   120,     0,   121,     0,
   102,     0,   145,     0,   146,     0,   147,     0,   148,     0,
    36,     0,    32,     0,    33,     0,    34,     0,    35,     0,
    38,     0,    39,     0,    40,     0,    41,     0,     6,     0,
   149,    74,   150,    75,     0,   149,    74,   151,    46,   152,
    75,     0,   149,    74,   150,    76,   153,    75,     0,     0,
   141,    74,   113,   104,    75,     0,   141,    74,   154,    46,
   155,    75,     0,   141,    74,   156,    76,   153,    75,     0,
   105,    43,     0,   138,   121,     0,     7,     0,   136,     0,
     8,     0,     9,     0,    74,   122,    75,     0,   128,     0,
   135,     0,   123,     0,    77,   157,   124,   125,    80,     0,
    47,   158,    48,   127,    72,    79,   126,    73,    49,     0,
    78,   159,     0,    79,   159,     0,    81,   157,   124,   125,
     0,   122,     0,   160,    46,   126,    45,     0,   129,     0,
   128,    50,   129,     0,   128,    51,   129,     0,   128,    52,
   129,     0,   130,     0,   129,    53,   130,     0,   129,    54,
   130,     0,   131,     0,   130,    55,   131,     0,   130,    56,
   131,     0,   130,    57,   131,     0,   130,    58,   131,     0,
   130,    59,   131,     0,   130,    60,   131,     0,   130,    61,
   131,     0,   132,     0,   131,    62,   132,     0,   131,    63,
   132,     0,   131,    66,   132,     0,   133,     0,   132,    64,
   133,     0,   132,    65,   133,     0,   132,    67,   133,     0,
   132,    68,   133,     0,   134,     0,    63,   134,     0,    69,
   134,     0,   121,   108,     0,    70,   102,     0,    71,   161,
     0,   105,     0,   102,    97,   100,     0,    83,    74,   122,
    75,     0,    84,    74,   122,    75,     0,    85,    74,   122,
    75,     0,    86,    74,   122,    75,     0,    87,    74,   122,
    75,     0,    88,    74,   122,    75,     0,    89,    74,   122,
    75,     0,    90,    74,   122,    75,     0,    90,    74,   137,
    75,     0,    91,    74,   139,    75,     0,    92,    74,   139,
    75,     0,    93,    74,   140,    75,     0,   138,     0,   142,
    74,   122,    75,     0,   143,    74,   122,    75,     0,   144,
    74,   104,    75,     0,    42,     0,   122,     0,   138,     0,
   122,     0,   105,     0,    10,     0,    11,     0,    12,     0,
    13,     0,    14,     0,    15,     0,    16,     0,    17,     0,
    18,     0,    19,     0,    20,     0,    21,     0,    22,     0,
    23,     0,    24,     0,    27,     0,    28,     0,    29,     0,
    30,     0,    31,     0
};

#endif

#if YYDEBUG != 0
static const short yyrline[] = { 0,
   316,   317,   323,   327,   333,   341,   342,   350,   357,   363,
   369,   375,   383,   387,   394,   398,   402,   406,   410,   414,
   418,   422,   426,   430,   434,   438,   442,   446,   450,   458,
   466,   470,   474,   478,   482,   493,   500,   506,   513,   520,
   524,   528,   534,   544,   553,   561,   565,   573,   577,   587,
   591,   599,   608,   618,   626,   634,   642,   650,   658,   662,
   666,   672,   676,   682,   688,   692,   698,   704,   712,   716,
   720,   724,   732,   736,   740,   748,   752,   756,   760,   764,
   768,   772,   776,   785,   789,   793,   797,   805,   809,   813,
   817,   821,   829,   833,   837,   841,   851,   855,   859,   868,
   877,   882,   886,   890,   894,   898,   902,   906,   908,   913,
   917,   921,   927,   931,   935,   939,   945,   948,   952,   958,
   966,   975,   976,   977,   978,   979,   980,   981,   982,   983,
   984,   985,   986,   987,   988,   989,   990,   991,   992,   993,
   994
};

static const char * const yytname[] = {   "$","error","$illegal.","FIXME_01",
"FIXME_02","FIXME_03","FIXME_04","FIXME_05","FIXME_06","FIXME_07","FIXME_08",
"FIXME_09","FIXME_10","FIXME_11","FIXME_12","FIXME_13","FIXME_14","FIXME_15",
"FIXME_16","FIXME_17","FIXME_18","FIXME_19","FIXME_20","FIXME_21","FIXME_22",
"FIXME_24","FIXME_25","FIXME_26","FIXME_27","FIXME_28","FIXME_29","FIXME_30",
"INTEGER_LITERAL","BOOLEAN_LITERAL","CHARACTER_LITERAL","FLOAT_LITERAL","GENERAL_PROCEDURE_NAME",
"LOCATION_NAME","SET_LITERAL","EMPTINESS_LITERAL","CHARACTER_STRING_LITERAL",
"BIT_STRING_LITERAL","TYPENAME","FIELD_NAME","'.'","';'","':'","CASE","OF","ESAC",
"LOGIOR","ORIF","LOGXOR","LOGAND","ANDIF","'='","NOTEQUAL","'>'","GTR","'<'",
"LEQ","IN","'+'","'-'","'*'","'/'","SLASH_SLASH","MOD","REM","NOT","POINTER",
"RECEIVE","'['","']'","'('","')'","UP","IF","THEN","ELSE","FI","ELSIF","ILLEGAL_TOKEN",
"NUM","PRED","SUCC","ABS","CARD","MAX_TOKEN","MIN_TOKEN","SIZE","UPPER","LOWER",
"LENGTH","GDB_REGNAME","GDB_LAST","GDB_VARIABLE","GDB_ASSIGNMENT","','","start",
"value","undefined_value","location","access_name","expression_list","primitive_value",
"location_contents","value_name","literal","tuple","value_string_element","value_string_slice",
"value_array_element","@1","value_array_slice","value_structure_field","expression_conversion",
"value_procedure_call","value_built_in_routine_call","start_expression","zero_adic_operator",
"parenthesised_expression","expression","conditional_expression","then_alternative",
"else_alternative","sub_expression","value_case_alternative","operand_0","operand_1",
"operand_2","operand_3","operand_4","operand_5","operand_6","single_assignment_action",
"chill_value_built_in_routine_call","mode_argument","mode_name","upper_lower_argument",
"length_argument","array_primitive_value","array_mode_name","string_mode_name",
"variant_structure_mode_name","synonym_name","value_enumeration_name","value_do_with_name",
"value_receive_name","string_primitive_value","start_element","left_element",
"right_element","slice_size","lower_element","upper_element","first_element",
"boolean_expression","case_selector_list","subexpression","case_label_specification",
"buffer_location",""
};
#endif

static const short yyr1[] = {     0,
    99,    99,   100,   100,   101,   102,   102,   103,   103,   103,
   103,   103,   104,   104,   105,   105,   105,   105,   105,   105,
   105,   105,   105,   105,   105,   105,   105,   105,   105,   106,
   107,   107,   107,   107,   107,   108,   108,   108,   108,   108,
   108,   108,   108,   109,   110,   111,   111,   113,   112,   114,
   114,   115,   116,   117,   118,   119,   120,   121,   122,   122,
   122,   123,   123,   124,   125,   125,   126,   127,   128,   128,
   128,   128,   129,   129,   129,   130,   130,   130,   130,   130,
   130,   130,   130,   131,   131,   131,   131,   132,   132,   132,
   132,   132,   133,   133,   133,   133,   134,   134,   134,   135,
   136,   136,   136,   136,   136,   136,   136,   136,   136,   136,
   136,   136,   137,   137,   137,   137,   138,   139,   139,   140,
   141,   142,   143,   144,   145,   146,   147,   148,   149,   150,
   151,   152,   153,   154,   155,   156,   157,   158,   159,   160,
   161
};

static const short yyr2[] = {     0,
     1,     1,     1,     1,     1,     1,     2,     1,     1,     1,
     1,     1,     1,     3,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     4,     6,     6,     0,     5,     6,
     6,     2,     2,     1,     1,     1,     1,     3,     1,     1,
     1,     5,     9,     2,     2,     4,     1,     4,     1,     3,
     3,     3,     1,     3,     3,     1,     3,     3,     3,     3,
     3,     3,     3,     1,     3,     3,     3,     1,     3,     3,
     3,     3,     1,     2,     2,     2,     2,     2,     1,     3,
     4,     4,     4,     4,     4,     4,     4,     4,     4,     4,
     4,     4,     1,     4,     4,     4,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1
};

static const short yydefact[] = {     0,
     5,    12,    44,    54,    56,    57,   125,   126,   127,   128,
   129,    36,    37,    38,    39,    35,     8,    40,    41,    42,
    43,   117,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    10,     9,    11,     1,     4,    30,     6,    99,    15,    16,
    17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
    27,    28,    29,     3,    61,    59,    69,    73,    76,    84,
    88,    93,    60,    55,     2,     0,    31,    32,    33,    34,
     0,   138,     0,    30,    29,    94,     0,    95,    97,   121,
   141,    98,     0,   137,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,    52,     7,    96,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,    53,
    48,     0,     0,    58,     0,     0,     0,     0,     0,     0,
     0,     0,     0,   122,   123,   124,     0,     0,   113,     0,
     0,     0,   118,   119,     0,     0,   120,     0,   100,    70,
    71,    72,    74,    75,    77,    78,    79,    80,    81,    82,
    83,    85,    86,    87,    89,    90,    91,    92,   134,   136,
     0,     0,     0,   130,   131,     0,     0,   140,     0,     0,
   139,    64,     0,     0,     0,   101,   102,   103,   104,   105,
   106,   107,   108,   109,     0,     0,     0,   110,   111,   112,
     0,    13,     0,     0,    45,     0,     0,     0,     0,    65,
     0,    62,     0,     0,     0,    49,     0,   135,     0,   133,
     0,     0,   132,     0,     0,    67,     0,     0,   114,   115,
   116,    14,    50,    51,    47,    46,     0,    68,    66,     0,
    63,     0,     0,     0
};

static const short yydefgoto[] = {   252,
    44,    45,    46,    47,   211,    48,    49,    50,    51,    52,
    53,    54,    55,   181,    56,    57,    58,    59,    60,    61,
    62,    63,    64,    65,   136,   195,   237,   189,    66,    67,
    68,    69,    70,    71,    72,    73,    74,   148,    87,   155,
   158,    76,   150,   151,   152,    77,    78,    79,    80,    81,
   186,   187,   234,   231,   182,   229,   183,    95,    83,   192,
   190,    92
};

static const short yypact[] = {   127,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,     5,   495,   495,   587,    55,   311,    86,    40,
    41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
-32768,-32768,-32768,-32768,-32768,    28,-32768,   -35,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,   -16,-32768,-32768,   -38,    -5,   -29,    -7,   -14,
-32768,-32768,-32768,-32768,    53,    54,-32768,-32768,-32768,-32768,
    63,-32768,    81,-32768,-32768,-32768,    53,-32768,   -34,   -33,
-32768,-32768,    64,-32768,    67,   311,   311,   311,   311,   311,
   311,   311,   219,   311,   311,   311,   127,-32768,-32768,-32768,
   403,   403,   403,   403,   403,   403,   403,   403,   403,   403,
   403,   403,   403,   403,   403,   403,   403,   403,   403,-32768,
    60,     2,   108,-32768,   117,     4,    72,    73,    74,    75,
    76,    77,    79,-32768,-32768,-32768,    80,    82,    53,    84,
    96,    97,-32768,    53,    98,   100,-32768,   101,-32768,    -5,
    -5,    -5,   -29,   -29,    -7,    -7,    -7,    -7,    -7,    -7,
    -7,   -14,   -14,   -14,-32768,-32768,-32768,-32768,-32768,-32768,
   311,   110,   104,-32768,-32768,   -18,   126,-32768,   109,   136,
-32768,-32768,   117,    86,   103,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,   311,   311,   311,-32768,-32768,-32768,
   -64,-32768,   161,   164,-32768,   164,   166,   113,   311,-32768,
    67,-32768,   112,   114,   -60,-32768,   311,-32768,   118,-32768,
   119,   120,-32768,   125,   311,-32768,   143,     4,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,   129,-32768,-32768,   154,
-32768,   205,   206,-32768
};

static const short yypgoto[] = {-32768,
   102,-32768,   -20,-32768,     1,   211,-32768,-32768,   175,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,   -23,   -28,-32768,    18,     3,     8,-32768,-32768,   -46,
   -27,   -75,   -44,   -66,    65,-32768,-32768,-32768,     7,   135,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,    26,-32768,-32768,-32768,    51,-32768,    56,
-32768,-32768
};


#define	YYLAST		683


static const short yytable[] = {    93,
    85,    85,    85,    84,    84,    89,    75,   108,   -30,   108,
   226,   111,   112,   113,   241,    12,    13,    14,    15,   184,
   185,    18,    19,    20,    21,   116,   117,   118,   119,   120,
   121,   122,    82,   227,   109,   -30,   109,   227,  -121,   -30,
   165,   166,   167,   168,   169,   170,   171,   114,   115,   126,
   127,   130,   128,   129,   123,   124,   215,   216,   125,   175,
   176,   177,   178,   130,   160,   161,   162,   137,   138,   139,
   140,   141,   142,   143,   147,   153,   153,   157,   172,   173,
   174,   179,   193,   180,   194,    91,   163,   164,    86,    88,
    84,    84,    84,    84,    84,    84,    84,    84,    84,    84,
    84,    84,    84,    84,    84,    84,    84,    84,    84,   149,
   154,   154,    94,    96,    97,    98,    99,   100,   101,   102,
   103,   104,   105,   106,   107,   130,    28,   131,   133,     1,
   130,     2,     3,     4,     5,     6,   132,   188,   134,     7,
     8,     9,    10,    11,   135,   191,   196,   197,   198,   199,
   200,   201,   212,   202,   203,   213,   204,   205,    12,    13,
    14,    15,    16,    17,    18,    19,    20,    21,    22,   206,
   207,   217,   208,    23,   209,   210,   223,   224,   212,   214,
   218,   219,   222,   228,   230,   233,   239,   248,   240,    24,
   236,   235,   243,   244,   245,    25,    26,    27,   242,   246,
    28,   250,   251,    29,   253,   254,   236,   225,   159,    30,
    31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
    41,    42,    43,     2,     3,     4,     5,     6,   144,   145,
   146,     7,     8,     9,    10,    11,    90,   110,   238,   156,
   249,   232,   247,     0,   221,     0,     0,     0,   220,     0,
    12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
    22,     0,     0,     0,     0,    23,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,    24,     0,     0,     0,     0,     0,    25,    26,    27,
     0,     0,    28,     0,     0,    29,     0,     0,     0,     0,
     0,    30,    31,    32,    33,    34,    35,    36,    37,    38,
    39,    40,    41,    42,    43,     2,     3,     4,     5,     6,
     0,     0,     0,     7,     8,     9,    10,    11,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,    12,    13,    14,    15,    16,    17,    18,    19,
    20,    21,    22,     0,     0,     0,     0,    23,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,    24,     0,     0,     0,     0,     0,    25,
    26,    27,     0,     0,    28,     0,     0,    29,     0,     0,
     0,     0,     0,    30,    31,    32,    33,    34,    35,    36,
    37,    38,    39,    40,    41,    42,    43,     2,     3,     4,
     5,     6,     0,     0,     0,     7,     8,     9,    10,    11,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,    12,    13,    14,    15,    16,    17,
    18,    19,    20,    21,    22,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,    24,     0,     0,     0,     0,
     0,    25,    26,    27,     0,     0,    28,     0,     0,     0,
     0,     0,     0,     0,     0,    30,    31,    32,    33,    34,
    35,    36,    37,    38,    39,    40,    41,    42,    43,     2,
     3,     4,     5,     6,     0,     0,     0,     7,     8,     9,
    10,    11,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,    12,    13,    14,    15,
    16,    17,    18,    19,    20,    21,    22,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,    26,    27,     0,     0,    28,     0,
     0,     0,     0,     0,     0,     0,     0,    30,    31,    32,
    33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
    43,     2,     3,     4,     5,     6,     0,     0,     0,     7,
     8,     9,    10,    11,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,    12,    13,
    14,    15,    16,    17,    18,    19,    20,    21,    22,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    28,     0,     0,     0,     0,     0,     0,     0,     0,    30,
    31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
    41,    42,    43
};

static const short yycheck[] = {    28,
    24,    25,    26,    24,    25,    26,     0,    43,    43,    43,
    75,    50,    51,    52,    75,    32,    33,    34,    35,    18,
    19,    38,    39,    40,    41,    55,    56,    57,    58,    59,
    60,    61,    28,    98,    70,    70,    70,    98,    74,    74,
   116,   117,   118,   119,   120,   121,   122,    53,    54,    64,
    65,    75,    67,    68,    62,    63,    75,    76,    66,   126,
   127,   128,   129,    87,   111,   112,   113,    96,    97,    98,
    99,   100,   101,   102,   103,   104,   105,   106,   123,   124,
   125,    22,    79,    24,    81,    31,   114,   115,    24,    25,
   111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
   121,   122,   123,   124,   125,   126,   127,   128,   129,   103,
   104,   105,    27,    74,    74,    74,    74,    74,    74,    74,
    74,    74,    74,    74,    97,   149,    74,    74,    48,     3,
   154,     5,     6,     7,     8,     9,    74,    30,    75,    13,
    14,    15,    16,    17,    78,    29,    75,    75,    75,    75,
    75,    75,   181,    75,    75,    46,    75,    74,    32,    33,
    34,    35,    36,    37,    38,    39,    40,    41,    42,    74,
    74,    46,    75,    47,    75,    75,   205,   206,   207,    76,
    72,    46,    80,    23,    21,    20,    75,    45,    75,    63,
   219,    79,    75,    75,    75,    69,    70,    71,   227,    75,
    74,    73,    49,    77,     0,     0,   235,   207,   107,    83,
    84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
    94,    95,    96,     5,     6,     7,     8,     9,    10,    11,
    12,    13,    14,    15,    16,    17,    26,    63,   221,   105,
   238,   216,   235,    -1,   194,    -1,    -1,    -1,   193,    -1,
    32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
    42,    -1,    -1,    -1,    -1,    47,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    63,    -1,    -1,    -1,    -1,    -1,    69,    70,    71,
    -1,    -1,    74,    -1,    -1,    77,    -1,    -1,    -1,    -1,
    -1,    83,    84,    85,    86,    87,    88,    89,    90,    91,
    92,    93,    94,    95,    96,     5,     6,     7,     8,     9,
    -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    32,    33,    34,    35,    36,    37,    38,    39,
    40,    41,    42,    -1,    -1,    -1,    -1,    47,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    63,    -1,    -1,    -1,    -1,    -1,    69,
    70,    71,    -1,    -1,    74,    -1,    -1,    77,    -1,    -1,
    -1,    -1,    -1,    83,    84,    85,    86,    87,    88,    89,
    90,    91,    92,    93,    94,    95,    96,     5,     6,     7,
     8,     9,    -1,    -1,    -1,    13,    14,    15,    16,    17,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    32,    33,    34,    35,    36,    37,
    38,    39,    40,    41,    42,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    63,    -1,    -1,    -1,    -1,
    -1,    69,    70,    71,    -1,    -1,    74,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    83,    84,    85,    86,    87,
    88,    89,    90,    91,    92,    93,    94,    95,    96,     5,
     6,     7,     8,     9,    -1,    -1,    -1,    13,    14,    15,
    16,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    32,    33,    34,    35,
    36,    37,    38,    39,    40,    41,    42,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    70,    71,    -1,    -1,    74,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    83,    84,    85,
    86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
    96,     5,     6,     7,     8,     9,    -1,    -1,    -1,    13,
    14,    15,    16,    17,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    32,    33,
    34,    35,    36,    37,    38,    39,    40,    41,    42,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    83,
    84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
    94,    95,    96
};
/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "/uns/lib/bison.simple"

/* Skeleton output parser for bison,
   Copyright (C) 1984, 1989, 1990 Bob Corbett and Richard Stallman

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 1, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */


#ifndef alloca
#ifdef __GNUC__
#define alloca __builtin_alloca
#else /* not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__) || defined (__sparc) || defined (__sgi)
#include <alloca.h>
#else /* not sparc */
#if defined (MSDOS) && !defined (__TURBOC__)
#else /* not MSDOS, or __TURBOC__ */
#if defined(_AIX)
 #pragma alloca
#else /* not MSDOS, __TURBOC__, or _AIX */
#ifdef __hpux
#ifdef __cplusplus
extern "C" {
void *alloca (unsigned int);
};
#else /* not __cplusplus */
void *alloca ();
#endif /* not __cplusplus */
#endif /* __hpux */
#endif /* not _AIX */
#endif /* not MSDOS, or __TURBOC__ */
#endif /* not sparc.  */
#endif /* not GNU C.  */
#endif /* alloca not defined.  */

/* This is the parser code that is written into each bison parser
  when the %semantic_parser declaration is not specified in the grammar.
  It was written by Richard Stallman by simplifying the hairy parser
  used when %semantic_parser is specified.  */

/* Note: there must be only one dollar sign in this file.
   It is replaced by the list of actions, each action
   as one case of the switch.  */

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	return(0)
#define YYABORT 	return(1)
#define YYERROR		goto yyerrlab1
/* Like YYERROR except do call yyerror.
   This remains here temporarily to ease the
   transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto yyerrlab
#define YYRECOVERING()  (!!yyerrstatus)
#define YYBACKUP(token, value) \
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    { yychar = (token), yylval = (value);			\
      yychar1 = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { yyerror ("syntax error: cannot back up"); YYERROR; }	\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

#ifndef YYPURE
#define YYLEX		yylex()
#endif

#ifdef YYPURE
#ifdef YYLSP_NEEDED
#define YYLEX		yylex(&yylval, &yylloc)
#else
#define YYLEX		yylex(&yylval)
#endif
#endif

/* If nonreentrant, generate the variables here */

#ifndef YYPURE

int	yychar;			/*  the lookahead symbol		*/
YYSTYPE	yylval;			/*  the semantic value of the		*/
				/*  lookahead symbol			*/

#ifdef YYLSP_NEEDED
YYLTYPE yylloc;			/*  location data for the lookahead	*/
				/*  symbol				*/
#endif

int yynerrs;			/*  number of parse errors so far       */
#endif  /* not YYPURE */

#if YYDEBUG != 0
int yydebug;			/*  nonzero means print parse trace	*/
/* Since this is uninitialized, it does not stop multiple parsers
   from coexisting.  */
#endif

/*  YYINITDEPTH indicates the initial size of the parser's stacks	*/

#ifndef	YYINITDEPTH
#define YYINITDEPTH 200
#endif

/*  YYMAXDEPTH is the maximum size the stacks can grow to
    (effective only if the built-in stack extension method is used).  */

#if YYMAXDEPTH == 0
#undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
#define YYMAXDEPTH 10000
#endif

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
int yyparse (void);
#endif

#if __GNUC__ > 1		/* GNU C and GNU C++ define this.  */
#define __yy_bcopy(FROM,TO,COUNT)	__builtin_memcpy(TO,FROM,COUNT)
#else				/* not GNU C or C++ */
#ifndef __cplusplus

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_bcopy (from, to, count)
     char *from;
     char *to;
     int count;
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#else /* __cplusplus */

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_bcopy (char *from, char *to, int count)
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#endif
#endif

#line 184 "/uns/lib/bison.simple"
int
yyparse()
{
  register int yystate;
  register int yyn;
  register short *yyssp;
  register YYSTYPE *yyvsp;
  int yyerrstatus;	/*  number of tokens to shift before error messages enabled */
  int yychar1 = 0;		/*  lookahead token as an internal (translated) token number */

  short	yyssa[YYINITDEPTH];	/*  the state stack			*/
  YYSTYPE yyvsa[YYINITDEPTH];	/*  the semantic value stack		*/

  short *yyss = yyssa;		/*  refer to the stacks thru separate pointers */
  YYSTYPE *yyvs = yyvsa;	/*  to allow yyoverflow to xreallocate them elsewhere */

#ifdef YYLSP_NEEDED
  YYLTYPE yylsa[YYINITDEPTH];	/*  the location stack			*/
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;

#define YYPOPSTACK   (yyvsp--, yyssp--, yylsp--)
#else
#define YYPOPSTACK   (yyvsp--, yyssp--)
#endif

  int yystacksize = YYINITDEPTH;

#ifdef YYPURE
  int yychar;
  YYSTYPE yylval;
  int yynerrs;
#ifdef YYLSP_NEEDED
  YYLTYPE yylloc;
#endif
#endif

  YYSTYPE yyval;		/*  the variable used to return		*/
				/*  semantic values from the action	*/
				/*  routines				*/

  int yylen;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Starting parse\n");
#endif

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss - 1;
  yyvsp = yyvs;
#ifdef YYLSP_NEEDED
  yylsp = yyls;
#endif

/* Push a new state, which is found in  yystate  .  */
/* In all cases, when you get here, the value and location stacks
   have just been pushed. so pushing a state here evens the stacks.  */
yynewstate:

  *++yyssp = yystate;

  if (yyssp >= yyss + yystacksize - 1)
    {
      /* Give user a chance to xreallocate the stack */
      /* Use copies of these so that the &'s don't force the real ones into memory. */
      YYSTYPE *yyvs1 = yyvs;
      short *yyss1 = yyss;
#ifdef YYLSP_NEEDED
      YYLTYPE *yyls1 = yyls;
#endif

      /* Get the current used size of the three stacks, in elements.  */
      int size = yyssp - yyss + 1;

#ifdef yyoverflow
      /* Each stack pointer address is followed by the size of
	 the data in use in that stack, in bytes.  */
#ifdef YYLSP_NEEDED
      /* This used to be a conditional around just the two extra args,
	 but that might be undefined if yyoverflow is a macro.  */
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yyls1, size * sizeof (*yylsp),
		 &yystacksize);
#else
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yystacksize);
#endif

      yyss = yyss1; yyvs = yyvs1;
#ifdef YYLSP_NEEDED
      yyls = yyls1;
#endif
#else /* no yyoverflow */
      /* Extend the stack our own way.  */
      if (yystacksize >= YYMAXDEPTH)
	{
	  yyerror("parser stack overflow");
	  return 2;
	}
      yystacksize *= 2;
      if (yystacksize > YYMAXDEPTH)
	yystacksize = YYMAXDEPTH;
      yyss = (short *) alloca (yystacksize * sizeof (*yyssp));
      __yy_bcopy ((char *)yyss1, (char *)yyss, size * sizeof (*yyssp));
      yyvs = (YYSTYPE *) alloca (yystacksize * sizeof (*yyvsp));
      __yy_bcopy ((char *)yyvs1, (char *)yyvs, size * sizeof (*yyvsp));
#ifdef YYLSP_NEEDED
      yyls = (YYLTYPE *) alloca (yystacksize * sizeof (*yylsp));
      __yy_bcopy ((char *)yyls1, (char *)yyls, size * sizeof (*yylsp));
#endif
#endif /* no yyoverflow */

      yyssp = yyss + size - 1;
      yyvsp = yyvs + size - 1;
#ifdef YYLSP_NEEDED
      yylsp = yyls + size - 1;
#endif

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Stack size increased to %d\n", yystacksize);
#endif

      if (yyssp >= yyss + yystacksize - 1)
	YYABORT;
    }

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Entering state %d\n", yystate);
#endif

  goto yybackup;
 yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* yychar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (yychar == YYEMPTY)
    {
#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Reading a token: ");
#endif
      yychar = YYLEX;
    }

  /* Convert token to internal form (in yychar1) for indexing tables with */

  if (yychar <= 0)		/* This means end of input. */
    {
      yychar1 = 0;
      yychar = YYEOF;		/* Don't call YYLEX any more */

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Now at end of input.\n");
#endif
    }
  else
    {
      yychar1 = YYTRANSLATE(yychar);

#if YYDEBUG != 0
      if (yydebug)
	{
	  fprintf (stderr, "Next token is %d (%s", yychar, yytname[yychar1]);
	  /* Give the individual parser a way to print the precise meaning
	     of a token, for further debugging info.  */
#ifdef YYPRINT
	  YYPRINT (stderr, yychar, yylval);
#endif
	  fprintf (stderr, ")\n");
	}
#endif
    }

  yyn += yychar1;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != yychar1)
    goto yydefault;

  yyn = yytable[yyn];

  /* yyn is what to do for this token type in this state.
     Negative => reduce, -yyn is rule number.
     Positive => shift, yyn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrlab;

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting token %d (%s), ", yychar, yytname[yychar1]);
#endif

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  /* count tokens shifted since error; after three, turn off error status.  */
  if (yyerrstatus) yyerrstatus--;

  yystate = yyn;
  goto yynewstate;

/* Do the default action for the current state.  */
yydefault:

  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;

/* Do a reduction.  yyn is the number of a rule to reduce with.  */
yyreduce:
  yylen = yyr2[yyn];
  if (yylen > 0)
    yyval = yyvsp[1-yylen]; /* implement default value of the action */

#if YYDEBUG != 0
  if (yydebug)
    {
      int i;

      fprintf (stderr, "Reducing via rule %d (line %d), ",
	       yyn, yyrline[yyn]);

      /* Print the symbols being reduced, and their result.  */
      for (i = yyprhs[yyn]; yyrhs[i] > 0; i++)
	fprintf (stderr, "%s ", yytname[yyrhs[i]]);
      fprintf (stderr, " -> %s\n", yytname[yyr1[yyn]]);
    }
#endif


  switch (yyn) {

case 1:
#line 316 "./ch-exp.y"
{ ;
    break;}
case 2:
#line 318 "./ch-exp.y"
{ write_exp_elt_opcode(OP_TYPE);
			  write_exp_elt_type(yyvsp[0].tsym.type);
			  write_exp_elt_opcode(OP_TYPE);;
    break;}
case 3:
#line 324 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 4:
#line 328 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 5:
#line 334 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 7:
#line 343 "./ch-exp.y"
{
			  write_exp_elt_opcode (UNOP_IND);
			;
    break;}
case 8:
#line 351 "./ch-exp.y"
{
			  write_exp_elt_opcode (OP_VAR_VALUE);
			  write_exp_elt_block (NULL);
			  write_exp_elt_sym (yyvsp[0].ssym.sym);
			  write_exp_elt_opcode (OP_VAR_VALUE);
			;
    break;}
case 9:
#line 358 "./ch-exp.y"
{
			  write_exp_elt_opcode (OP_LAST);
			  write_exp_elt_longcst (yyvsp[0].lval);
			  write_exp_elt_opcode (OP_LAST); 
			;
    break;}
case 10:
#line 364 "./ch-exp.y"
{
			  write_exp_elt_opcode (OP_REGISTER);
			  write_exp_elt_longcst (yyvsp[0].lval);
			  write_exp_elt_opcode (OP_REGISTER); 
			;
    break;}
case 11:
#line 370 "./ch-exp.y"
{
			  write_exp_elt_opcode (OP_INTERNALVAR);
			  write_exp_elt_intern (yyvsp[0].ivar);
			  write_exp_elt_opcode (OP_INTERNALVAR); 
			;
    break;}
case 12:
#line 376 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 13:
#line 384 "./ch-exp.y"
{
			  arglist_len = 1;
			;
    break;}
case 14:
#line 388 "./ch-exp.y"
{
			  arglist_len++;
			;
    break;}
case 15:
#line 395 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 16:
#line 399 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 17:
#line 403 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 18:
#line 407 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 19:
#line 411 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 20:
#line 415 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 21:
#line 419 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 22:
#line 423 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 23:
#line 427 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 24:
#line 431 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 25:
#line 435 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 26:
#line 439 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 27:
#line 443 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 28:
#line 447 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 29:
#line 451 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 30:
#line 459 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 31:
#line 467 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 32:
#line 471 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 33:
#line 475 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 34:
#line 479 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 35:
#line 483 "./ch-exp.y"
{
			  write_exp_elt_opcode (OP_VAR_VALUE);
			  write_exp_elt_block (NULL);
			  write_exp_elt_sym (yyvsp[0].ssym.sym);
			  write_exp_elt_opcode (OP_VAR_VALUE);
			;
    break;}
case 36:
#line 494 "./ch-exp.y"
{
			  write_exp_elt_opcode (OP_LONG);
			  write_exp_elt_type (yyvsp[0].typed_val.type);
			  write_exp_elt_longcst ((LONGEST) (yyvsp[0].typed_val.val));
			  write_exp_elt_opcode (OP_LONG);
			;
    break;}
case 37:
#line 501 "./ch-exp.y"
{
			  write_exp_elt_opcode (OP_BOOL);
			  write_exp_elt_longcst ((LONGEST) yyvsp[0].ulval);
			  write_exp_elt_opcode (OP_BOOL);
			;
    break;}
case 38:
#line 507 "./ch-exp.y"
{
			  write_exp_elt_opcode (OP_LONG);
			  write_exp_elt_type (yyvsp[0].typed_val.type);
			  write_exp_elt_longcst ((LONGEST) (yyvsp[0].typed_val.val));
			  write_exp_elt_opcode (OP_LONG);
			;
    break;}
case 39:
#line 514 "./ch-exp.y"
{
			  write_exp_elt_opcode (OP_DOUBLE);
			  write_exp_elt_type (builtin_type_double);
			  write_exp_elt_dblcst (yyvsp[0].dval);
			  write_exp_elt_opcode (OP_DOUBLE);
			;
    break;}
case 40:
#line 521 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 41:
#line 525 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 42:
#line 529 "./ch-exp.y"
{
			  write_exp_elt_opcode (OP_STRING);
			  write_exp_string (yyvsp[0].sval);
			  write_exp_elt_opcode (OP_STRING);
			;
    break;}
case 43:
#line 535 "./ch-exp.y"
{
			  write_exp_elt_opcode (OP_BITSTRING);
			  write_exp_bitstring (yyvsp[0].sval);
			  write_exp_elt_opcode (OP_BITSTRING);
			;
    break;}
case 44:
#line 545 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 45:
#line 554 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 46:
#line 562 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 47:
#line 566 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 48:
#line 576 "./ch-exp.y"
{ start_arglist (); ;
    break;}
case 49:
#line 578 "./ch-exp.y"
{
			  write_exp_elt_opcode (MULTI_SUBSCRIPT);
			  write_exp_elt_longcst ((LONGEST) end_arglist ());
			  write_exp_elt_opcode (MULTI_SUBSCRIPT);
			;
    break;}
case 50:
#line 588 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 51:
#line 592 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 52:
#line 600 "./ch-exp.y"
{ write_exp_elt_opcode (STRUCTOP_STRUCT);
			  write_exp_string (yyvsp[0].sval);
			  write_exp_elt_opcode (STRUCTOP_STRUCT);
			;
    break;}
case 53:
#line 609 "./ch-exp.y"
{
			  write_exp_elt_opcode (UNOP_CAST);
			  write_exp_elt_type (yyvsp[-1].tsym.type);
			  write_exp_elt_opcode (UNOP_CAST);
			;
    break;}
case 54:
#line 619 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 55:
#line 627 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 56:
#line 635 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 57:
#line 643 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 58:
#line 651 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 59:
#line 659 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 60:
#line 663 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 61:
#line 667 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 62:
#line 673 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 63:
#line 677 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 64:
#line 683 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 65:
#line 689 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 66:
#line 693 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 67:
#line 699 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 68:
#line 705 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 69:
#line 713 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 70:
#line 717 "./ch-exp.y"
{
			  write_exp_elt_opcode (BINOP_BITWISE_IOR);
			;
    break;}
case 71:
#line 721 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 72:
#line 725 "./ch-exp.y"
{
			  write_exp_elt_opcode (BINOP_BITWISE_XOR);
			;
    break;}
case 73:
#line 733 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 74:
#line 737 "./ch-exp.y"
{
			  write_exp_elt_opcode (BINOP_BITWISE_AND);
			;
    break;}
case 75:
#line 741 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 76:
#line 749 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 77:
#line 753 "./ch-exp.y"
{
			  write_exp_elt_opcode (BINOP_EQUAL);
			;
    break;}
case 78:
#line 757 "./ch-exp.y"
{
			  write_exp_elt_opcode (BINOP_NOTEQUAL);
			;
    break;}
case 79:
#line 761 "./ch-exp.y"
{
			  write_exp_elt_opcode (BINOP_GTR);
			;
    break;}
case 80:
#line 765 "./ch-exp.y"
{
			  write_exp_elt_opcode (BINOP_GEQ);
			;
    break;}
case 81:
#line 769 "./ch-exp.y"
{
			  write_exp_elt_opcode (BINOP_LESS);
			;
    break;}
case 82:
#line 773 "./ch-exp.y"
{
			  write_exp_elt_opcode (BINOP_LEQ);
			;
    break;}
case 83:
#line 777 "./ch-exp.y"
{
			  write_exp_elt_opcode (BINOP_IN);
			;
    break;}
case 84:
#line 786 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 85:
#line 790 "./ch-exp.y"
{
			  write_exp_elt_opcode (BINOP_ADD);
			;
    break;}
case 86:
#line 794 "./ch-exp.y"
{
			  write_exp_elt_opcode (BINOP_SUB);
			;
    break;}
case 87:
#line 798 "./ch-exp.y"
{
			  write_exp_elt_opcode (BINOP_CONCAT);
			;
    break;}
case 88:
#line 806 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 89:
#line 810 "./ch-exp.y"
{
			  write_exp_elt_opcode (BINOP_MUL);
			;
    break;}
case 90:
#line 814 "./ch-exp.y"
{
			  write_exp_elt_opcode (BINOP_DIV);
			;
    break;}
case 91:
#line 818 "./ch-exp.y"
{
			  write_exp_elt_opcode (BINOP_MOD);
			;
    break;}
case 92:
#line 822 "./ch-exp.y"
{
			  write_exp_elt_opcode (BINOP_REM);
			;
    break;}
case 93:
#line 830 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 94:
#line 834 "./ch-exp.y"
{
			  write_exp_elt_opcode (UNOP_NEG);
			;
    break;}
case 95:
#line 838 "./ch-exp.y"
{
			  write_exp_elt_opcode (UNOP_LOGICAL_NOT);
			;
    break;}
case 96:
#line 844 "./ch-exp.y"
{
			  write_exp_elt_opcode (BINOP_CONCAT);
			;
    break;}
case 97:
#line 852 "./ch-exp.y"
{
			  write_exp_elt_opcode (UNOP_ADDR);
			;
    break;}
case 98:
#line 856 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 99:
#line 860 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 100:
#line 870 "./ch-exp.y"
{
			  write_exp_elt_opcode (BINOP_ASSIGN);
			;
    break;}
case 101:
#line 879 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 102:
#line 883 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 103:
#line 887 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 104:
#line 891 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 105:
#line 895 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 106:
#line 899 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 107:
#line 903 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 108:
#line 907 "./ch-exp.y"
{ write_exp_elt_opcode (UNOP_SIZEOF); ;
    break;}
case 109:
#line 909 "./ch-exp.y"
{ write_exp_elt_opcode (OP_LONG);
			  write_exp_elt_type (builtin_type_int);
			  write_exp_elt_longcst ((LONGEST) TYPE_LENGTH (yyvsp[-1].tval));
			  write_exp_elt_opcode (OP_LONG); ;
    break;}
case 110:
#line 914 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 111:
#line 918 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 112:
#line 922 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 113:
#line 928 "./ch-exp.y"
{
			  yyval.tval = yyvsp[0].tsym.type;
			;
    break;}
case 114:
#line 932 "./ch-exp.y"
{
			  yyval.tval = 0;	/* FIXME */
			;
    break;}
case 115:
#line 936 "./ch-exp.y"
{
			  yyval.tval = 0;	/* FIXME */
			;
    break;}
case 116:
#line 940 "./ch-exp.y"
{
			  yyval.tval = 0;	/* FIXME */
			;
    break;}
case 118:
#line 949 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 119:
#line 953 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 120:
#line 959 "./ch-exp.y"
{
			  yyval.voidval = 0;	/* FIXME */
			;
    break;}
case 121:
#line 967 "./ch-exp.y"
{
			  yyval.voidval = 0;
			;
    break;}
case 122:
#line 975 "./ch-exp.y"
{ yyval.voidval = 0; ;
    break;}
case 123:
#line 976 "./ch-exp.y"
{ yyval.voidval = 0; ;
    break;}
case 124:
#line 977 "./ch-exp.y"
{ yyval.voidval = 0; ;
    break;}
case 125:
#line 978 "./ch-exp.y"
{ yyval.voidval = 0; ;
    break;}
case 126:
#line 979 "./ch-exp.y"
{ yyval.voidval = 0; ;
    break;}
case 127:
#line 980 "./ch-exp.y"
{ yyval.voidval = 0; ;
    break;}
case 128:
#line 981 "./ch-exp.y"
{ yyval.voidval = 0; ;
    break;}
case 129:
#line 982 "./ch-exp.y"
{ yyval.voidval = 0; ;
    break;}
case 130:
#line 983 "./ch-exp.y"
{ yyval.voidval = 0; ;
    break;}
case 131:
#line 984 "./ch-exp.y"
{ yyval.voidval = 0; ;
    break;}
case 132:
#line 985 "./ch-exp.y"
{ yyval.voidval = 0; ;
    break;}
case 133:
#line 986 "./ch-exp.y"
{ yyval.voidval = 0; ;
    break;}
case 134:
#line 987 "./ch-exp.y"
{ yyval.voidval = 0; ;
    break;}
case 135:
#line 988 "./ch-exp.y"
{ yyval.voidval = 0; ;
    break;}
case 136:
#line 989 "./ch-exp.y"
{ yyval.voidval = 0; ;
    break;}
case 137:
#line 990 "./ch-exp.y"
{ yyval.voidval = 0; ;
    break;}
case 138:
#line 991 "./ch-exp.y"
{ yyval.voidval = 0; ;
    break;}
case 139:
#line 992 "./ch-exp.y"
{ yyval.voidval = 0; ;
    break;}
case 140:
#line 993 "./ch-exp.y"
{ yyval.voidval = 0; ;
    break;}
case 141:
#line 994 "./ch-exp.y"
{ yyval.voidval = 0; ;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */
#line 465 "/uns/lib/bison.simple"

  yyvsp -= yylen;
  yyssp -= yylen;
#ifdef YYLSP_NEEDED
  yylsp -= yylen;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

  *++yyvsp = yyval;

#ifdef YYLSP_NEEDED
  yylsp++;
  if (yylen == 0)
    {
      yylsp->first_line = yylloc.first_line;
      yylsp->first_column = yylloc.first_column;
      yylsp->last_line = (yylsp-1)->last_line;
      yylsp->last_column = (yylsp-1)->last_column;
      yylsp->text = 0;
    }
  else
    {
      yylsp->last_line = (yylsp+yylen-1)->last_line;
      yylsp->last_column = (yylsp+yylen-1)->last_column;
    }
#endif

  /* Now "shift" the result of the reduction.
     Determine what state that goes to,
     based on the state we popped back to
     and the rule number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTBASE] + *yyssp;
  if (yystate >= 0 && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTBASE];

  goto yynewstate;

yyerrlab:   /* here on detecting error */

  if (! yyerrstatus)
    /* If not already recovering from an error, report this error.  */
    {
      ++yynerrs;

#ifdef YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (yyn > YYFLAG && yyn < YYLAST)
	{
	  int size = 0;
	  char *msg;
	  int x, count;

	  count = 0;
	  /* Start X at -yyn if nec to avoid negative indexes in yycheck.  */
	  for (x = (yyn < 0 ? -yyn : 0);
	       x < (sizeof(yytname) / sizeof(char *)); x++)
	    if (yycheck[x + yyn] == x)
	      size += strlen(yytname[x]) + 15, count++;
	  msg = (char *) xmalloc(size + 15);
	  if (msg != 0)
	    {
	      strcpy(msg, "parse error");

	      if (count < 5)
		{
		  count = 0;
		  for (x = (yyn < 0 ? -yyn : 0);
		       x < (sizeof(yytname) / sizeof(char *)); x++)
		    if (yycheck[x + yyn] == x)
		      {
			strcat(msg, count == 0 ? ", expecting `" : " or `");
			strcat(msg, yytname[x]);
			strcat(msg, "'");
			count++;
		      }
		}
	      yyerror(msg);
	      free(msg);
	    }
	  else
	    yyerror ("parse error; also virtual memory exceeded");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror("parse error");
    }

  goto yyerrlab1;
yyerrlab1:   /* here on error raised explicitly by an action */

  if (yyerrstatus == 3)
    {
      /* if just tried and failed to reuse lookahead token after an error, discard it.  */

      /* return failure if at end of input */
      if (yychar == YYEOF)
	YYABORT;

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Discarding token %d (%s).\n", yychar, yytname[yychar1]);
#endif

      yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token
     after shifting the error token.  */

  yyerrstatus = 3;		/* Each real token shifted decrements this */

  goto yyerrhandle;

yyerrdefault:  /* current state does not do anything special for the error token. */

#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */
  yyn = yydefact[yystate];  /* If its default is to accept any token, ok.  Otherwise pop it.*/
  if (yyn) goto yydefault;
#endif

yyerrpop:   /* pop the current state because it cannot handle the error token */

  if (yyssp == yyss) YYABORT;
  yyvsp--;
  yystate = *--yyssp;
#ifdef YYLSP_NEEDED
  yylsp--;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "Error: state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

yyerrhandle:

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yyerrdefault;

  yyn += YYTERROR;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != YYTERROR)
    goto yyerrdefault;

  yyn = yytable[yyn];
  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrpop;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrpop;

  if (yyn == YYFINAL)
    YYACCEPT;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting error token, ");
#endif

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  yystate = yyn;
  goto yynewstate;
}
#line 996 "./ch-exp.y"


/* Implementation of a dynamically expandable buffer for processing input
   characters acquired through lexptr and building a value to return in
   yylval. */

static char *tempbuf;		/* Current buffer contents */
static int tempbufsize;		/* Size of allocated buffer */
static int tempbufindex;	/* Current index into buffer */

#define GROWBY_MIN_SIZE 64	/* Minimum amount to grow buffer by */

#define CHECKBUF(size) \
  do { \
    if (tempbufindex + (size) >= tempbufsize) \
      { \
	growbuf_by_size (size); \
      } \
  } while (0);

/* Grow the static temp buffer if necessary, including allocating the first one
   on demand. */

static void
growbuf_by_size (count)
     int count;
{
  int growby;

  growby = max (count, GROWBY_MIN_SIZE);
  tempbufsize += growby;
  if (tempbuf == NULL)
    {
      tempbuf = (char *) xmalloc (tempbufsize);
    }
  else
    {
      tempbuf = (char *) xrealloc (tempbuf, tempbufsize);
    }
}

/* Try to consume a simple name string token.  If successful, returns
   a pointer to a nullbyte terminated copy of the name that can be used
   in symbol table lookups.  If not successful, returns NULL. */

static char *
match_simple_name_string ()
{
  char *tokptr = lexptr;

  if (isalpha (*tokptr) || *tokptr == '_')
    {
      char *result;
      do {
	tokptr++;
      } while (isalnum (*tokptr) || (*tokptr == '_'));
      yylval.sval.ptr = lexptr;
      yylval.sval.length = tokptr - lexptr;
      lexptr = tokptr;
      result = copy_name (yylval.sval);
      return result;
    }
  return (NULL);
}

/* Start looking for a value composed of valid digits as set by the base
   in use.  Note that '_' characters are valid anywhere, in any quantity,
   and are simply ignored.  Since we must find at least one valid digit,
   or reject this token as an integer literal, we keep track of how many
   digits we have encountered. */
  
static int
decode_integer_value (base, tokptrptr, ivalptr)
  int base;
  char **tokptrptr;
  int *ivalptr;
{
  char *tokptr = *tokptrptr;
  int temp;
  int digits = 0;

  while (*tokptr != '\0')
    {
      temp = *tokptr;
      if (isupper (temp))
        temp = tolower (temp);
      tokptr++;
      switch (temp)
	{
	case '_':
	  continue;
	case '0':  case '1':  case '2':  case '3':  case '4':
	case '5':  case '6':  case '7':  case '8':  case '9':
	  temp -= '0';
	  break;
	case 'a':  case 'b':  case 'c':  case 'd':  case 'e': case 'f':
	  temp -= 'a';
	  temp += 10;
	  break;
	default:
	  temp = base;
	  break;
	}
      if (temp < base)
	{
	  digits++;
	  *ivalptr *= base;
	  *ivalptr += temp;
	}
      else
	{
	  /* Found something not in domain for current base. */
	  tokptr--;	/* Unconsume what gave us indigestion. */
	  break;
	}
    }
  
  /* If we didn't find any digits, then we don't have a valid integer
     value, so reject the entire token.  Otherwise, update the lexical
     scan pointer, and return non-zero for success. */
  
  if (digits == 0)
    {
      return (0);
    }
  else
    {
      *tokptrptr = tokptr;
      return (1);
    }
}

static int
decode_integer_literal (valptr, tokptrptr)
  int *valptr;
  char **tokptrptr;
{
  char *tokptr = *tokptrptr;
  int base = 0;
  int ival = 0;
  int explicit_base = 0;
  
  /* Look for an explicit base specifier, which is optional. */
  
  switch (*tokptr)
    {
    case 'd':
    case 'D':
      explicit_base++;
      base = 10;
      tokptr++;
      break;
    case 'b':
    case 'B':
      explicit_base++;
      base = 2;
      tokptr++;
      break;
    case 'h':
    case 'H':
      explicit_base++;
      base = 16;
      tokptr++;
      break;
    case 'o':
    case 'O':
      explicit_base++;
      base = 8;
      tokptr++;
      break;
    default:
      base = 10;
      break;
    }
  
  /* If we found an explicit base ensure that the character after the
     explicit base is a single quote. */
  
  if (explicit_base && (*tokptr++ != '\''))
    {
      return (0);
    }
  
  /* Attempt to decode whatever follows as an integer value in the
     indicated base, updating the token pointer in the process and
     computing the value into ival.  Also, if we have an explicit
     base, then the next character must not be a single quote, or we
     have a bitstring literal, so reject the entire token in this case.
     Otherwise, update the lexical scan pointer, and return non-zero
     for success. */

  if (!decode_integer_value (base, &tokptr, &ival))
    {
      return (0);
    }
  else if (explicit_base && (*tokptr == '\''))
    {
      return (0);
    }
  else
    {
      *valptr = ival;
      *tokptrptr = tokptr;
      return (1);
    }
}

/*  If it wasn't for the fact that floating point values can contain '_'
    characters, we could just let strtod do all the hard work by letting it
    try to consume as much of the current token buffer as possible and
    find a legal conversion.  Unfortunately we need to filter out the '_'
    characters before calling strtod, which we do by copying the other
    legal chars to a local buffer to be converted.  However since we also
    need to keep track of where the last unconsumed character in the input
    buffer is, we have transfer only as many characters as may compose a
    legal floating point value. */
    
static int
match_float_literal ()
{
  char *tokptr = lexptr;
  char *buf;
  char *copy;
  double dval;
  extern double strtod ();
  
  /* Make local buffer in which to build the string to convert.  This is
     required because underscores are valid in chill floating point numbers
     but not in the string passed to strtod to convert.  The string will be
     no longer than our input string. */
     
  copy = buf = (char *) alloca (strlen (tokptr) + 1);

  /* Transfer all leading digits to the conversion buffer, discarding any
     underscores. */

  while (isdigit (*tokptr) || *tokptr == '_')
    {
      if (*tokptr != '_')
	{
	  *copy++ = *tokptr;
	}
      tokptr++;
    }

  /* Now accept either a '.', or one of [eEdD].  Dot is legal regardless
     of whether we found any leading digits, and we simply accept it and
     continue on to look for the fractional part and/or exponent.  One of
     [eEdD] is legal only if we have seen digits, and means that there
     is no fractional part.  If we find neither of these, then this is
     not a floating point number, so return failure. */

  switch (*tokptr++)
    {
      case '.':
        /* Accept and then look for fractional part and/or exponent. */
	*copy++ = '.';
	break;

      case 'e':
      case 'E':
      case 'd':
      case 'D':
	if (copy == buf)
	  {
	    return (0);
	  }
	*copy++ = 'e';
	goto collect_exponent;
	break;

      default:
	return (0);
        break;
    }

  /* We found a '.', copy any fractional digits to the conversion buffer, up
     to the first nondigit, non-underscore character. */

  while (isdigit (*tokptr) || *tokptr == '_')
    {
      if (*tokptr != '_')
	{
	  *copy++ = *tokptr;
	}
      tokptr++;
    }

  /* Look for an exponent, which must start with one of [eEdD].  If none
     is found, jump directly to trying to convert what we have collected
     so far. */

  switch (*tokptr)
    {
      case 'e':
      case 'E':
      case 'd':
      case 'D':
	*copy++ = 'e';
	tokptr++;
	break;
      default:
	goto convert_float;
	break;
    }

  /* Accept an optional '-' or '+' following one of [eEdD]. */

  collect_exponent:
  if (*tokptr == '+' || *tokptr == '-')
    {
      *copy++ = *tokptr++;
    }

  /* Now copy an exponent into the conversion buffer.  Note that at the 
     moment underscores are *not* allowed in exponents. */

  while (isdigit (*tokptr))
    {
      *copy++ = *tokptr++;
    }

  /* If we transfered any chars to the conversion buffer, try to interpret its
     contents as a floating point value.  If any characters remain, then we
     must not have a valid floating point string. */

  convert_float:
  *copy = '\0';
  if (copy != buf)
      {
        dval = strtod (buf, &copy);
        if (*copy == '\0')
	  {
	    yylval.dval = dval;
	    lexptr = tokptr;
	    return (FLOAT_LITERAL);
	  }
      }
  return (0);
}

/* Recognize a string literal.  A string literal is a sequence
   of characters enclosed in matching single or double quotes, except that
   a single character inside single quotes is a character literal, which
   we reject as a string literal.  To embed the terminator character inside
   a string, it is simply doubled (I.E. "this""is""one""string") */

static int
match_string_literal ()
{
  char *tokptr = lexptr;

  for (tempbufindex = 0, tokptr++; *tokptr != '\0'; tokptr++)
    {
      CHECKBUF (1);
      if (*tokptr == *lexptr)
	{
	  if (*(tokptr + 1) == *lexptr)
	    {
	      tokptr++;
	    }
	  else
	    {
	      break;
	    }
	}
      tempbuf[tempbufindex++] = *tokptr;
    }
  if (*tokptr == '\0'					/* no terminator */
      || (tempbufindex == 1 && *tokptr == '\''))	/* char literal */
    {
      return (0);
    }
  else
    {
      tempbuf[tempbufindex] = '\0';
      yylval.sval.ptr = tempbuf;
      yylval.sval.length = tempbufindex;
      lexptr = ++tokptr;
      return (CHARACTER_STRING_LITERAL);
    }
}

/* Recognize a character literal.  A character literal is single character
   or a control sequence, enclosed in single quotes.  A control sequence
   is a comma separated list of one or more integer literals, enclosed
   in parenthesis and introduced with a circumflex character.

   EX:  'a'  '^(7)'  '^(7,8)'

   As a GNU chill extension, the syntax C'xx' is also recognized as a 
   character literal, where xx is a hex value for the character.

   Note that more than a single character, enclosed in single quotes, is
   a string literal.

   Also note that the control sequence form is not in GNU Chill since it
   is ambiguous with the string literal form using single quotes.  I.E.
   is '^(7)' a character literal or a string literal.  In theory it it
   possible to tell by context, but GNU Chill doesn't accept the control
   sequence form, so neither do we (for now the code is disabled).

   Returns CHARACTER_LITERAL if a match is found.
   */

static int
match_character_literal ()
{
  char *tokptr = lexptr;
  int ival = 0;
  
  if ((*tokptr == 'c' || *tokptr == 'C') && (*(tokptr + 1) == '\''))
    {
      /* We have a GNU chill extension form, so skip the leading "C'",
	 decode the hex value, and then ensure that we have a trailing
	 single quote character. */
      tokptr += 2;
      if (!decode_integer_value (16, &tokptr, &ival) || (*tokptr != '\''))
	{
	  return (0);
	}
      tokptr++;
    }
  else if (*tokptr == '\'')
    {
      tokptr++;

      /* Determine which form we have, either a control sequence or the
	 single character form. */
      
      if ((*tokptr == '^') && (*(tokptr + 1) == '('))
	{
#if 0     /* Disable, see note above. -fnf */
	  /* Match and decode a control sequence.  Return zero if we don't
	     find a valid integer literal, or if the next unconsumed character
	     after the integer literal is not the trailing ')'.
	     FIXME:  We currently don't handle the multiple integer literal
	     form. */
	  tokptr += 2;
	  if (!decode_integer_literal (&ival, &tokptr) || (*tokptr++ != ')'))
	    {
	      return (0);
	    }
#else
	  return (0);
#endif
	}
      else
	{
	  ival = *tokptr++;
	}
      
      /* The trailing quote has not yet been consumed.  If we don't find
	 it, then we have no match. */
      
      if (*tokptr++ != '\'')
	{
	  return (0);
	}
    }
  else
    {
      /* Not a character literal. */
      return (0);
    }
  yylval.typed_val.val = ival;
  yylval.typed_val.type = builtin_type_chill_char;
  lexptr = tokptr;
  return (CHARACTER_LITERAL);
}

/* Recognize an integer literal, as specified in Z.200 sec 5.2.4.2.
   Note that according to 5.2.4.2, a single "_" is also a valid integer
   literal, however GNU-chill requires there to be at least one "digit"
   in any integer literal. */

static int
match_integer_literal ()
{
  char *tokptr = lexptr;
  int ival;
  
  if (!decode_integer_literal (&ival, &tokptr))
    {
      return (0);
    }
  else 
    {
      yylval.typed_val.val = ival;
      yylval.typed_val.type = builtin_type_int;
      lexptr = tokptr;
      return (INTEGER_LITERAL);
    }
}

/* Recognize a bit-string literal, as specified in Z.200 sec 5.2.4.8
   Note that according to 5.2.4.8, a single "_" is also a valid bit-string
   literal, however GNU-chill requires there to be at least one "digit"
   in any bit-string literal. */

static int
match_bitstring_literal ()
{
  char *tokptr = lexptr;
  int mask;
  int bitoffset = 0;
  int bitcount = 0;
  int base;
  int digit;
  
  tempbufindex = 0;

  /* Look for the required explicit base specifier. */
  
  switch (*tokptr++)
    {
    case 'b':
    case 'B':
      base = 2;
      break;
    case 'o':
    case 'O':
      base = 8;
      break;
    case 'h':
    case 'H':
      base = 16;
      break;
    default:
      return (0);
      break;
    }
  
  /* Ensure that the character after the explicit base is a single quote. */
  
  if (*tokptr++ != '\'')
    {
      return (0);
    }
  
  while (*tokptr != '\0' && *tokptr != '\'')
    {
      digit = *tokptr;
      if (isupper (digit))
        digit = tolower (digit);
      tokptr++;
      switch (digit)
	{
	  case '_':
	    continue;
	  case '0':  case '1':  case '2':  case '3':  case '4':
	  case '5':  case '6':  case '7':  case '8':  case '9':
	    digit -= '0';
	    break;
	  case 'a':  case 'b':  case 'c':  case 'd':  case 'e': case 'f':
	    digit -= 'a';
	    digit += 10;
	    break;
	  default:
	    return (0);
	    break;
	}
      if (digit >= base)
	{
	  /* Found something not in domain for current base. */
	  return (0);
	}
      else
	{
	  /* Extract bits from digit, starting with the msbit appropriate for
	     the current base, and packing them into the bitstring byte,
	     starting at the lsbit. */
	  for (mask = (base >> 1); mask > 0; mask >>= 1)
	    {
	      bitcount++;
	      CHECKBUF (1);
	      if (digit & mask)
		{
		  tempbuf[tempbufindex] |= (1 << bitoffset);
		}
	      bitoffset++;
	      if (bitoffset == HOST_CHAR_BIT)
		{
		  bitoffset = 0;
		  tempbufindex++;
		}
	    }
	}
    }
  
  /* Verify that we consumed everything up to the trailing single quote,
     and that we found some bits (IE not just underbars). */

  if (*tokptr++ != '\'')
    {
      return (0);
    }
  else 
    {
      yylval.sval.ptr = tempbuf;
      yylval.sval.length = bitcount;
      lexptr = tokptr;
      return (BIT_STRING_LITERAL);
    }
}

/* Recognize tokens that start with '$'.  These include:

	$regname	A native register name or a "standard
			register name".
			Return token GDB_REGNAME.

	$variable	A convenience variable with a name chosen
			by the user.
			Return token GDB_VARIABLE.

	$digits		Value history with index <digits>, starting
			from the first value which has index 1.
			Return GDB_LAST.

	$$digits	Value history with index <digits> relative
			to the last value.  I.E. $$0 is the last
			value, $$1 is the one previous to that, $$2
			is the one previous to $$1, etc.
			Return token GDB_LAST.

	$ | $0 | $$0	The last value in the value history.
			Return token GDB_LAST.

	$$		An abbreviation for the second to the last
			value in the value history, I.E. $$1
			Return token GDB_LAST.

    Note that we currently assume that register names and convenience
    variables follow the convention of starting with a letter or '_'.

   */

static int
match_dollar_tokens ()
{
  char *tokptr;
  int regno;
  int namelength;
  int negate;
  int ival;

  /* We will always have a successful match, even if it is just for
     a single '$', the abbreviation for $$0.  So advance lexptr. */

  tokptr = ++lexptr;

  if (*tokptr == '_' || isalpha (*tokptr))
    {
      /* Look for a match with a native register name, usually something
	 like "r0" for example. */

      for (regno = 0; regno < NUM_REGS; regno++)
	{
	  namelength = strlen (reg_names[regno]);
	  if (STREQN (tokptr, reg_names[regno], namelength)
	      && !isalnum (tokptr[namelength]))
	    {
	      yylval.lval = regno;
	      lexptr += namelength;
	      return (GDB_REGNAME);
	    }
	}

      /* Look for a match with a standard register name, usually something
	 like "pc", which gdb always recognizes as the program counter
	 regardless of what the native register name is. */

      for (regno = 0; regno < num_std_regs; regno++)
	{
	  namelength = strlen (std_regs[regno].name);
	  if (STREQN (tokptr, std_regs[regno].name, namelength)
	      && !isalnum (tokptr[namelength]))
	    {
	      yylval.lval = std_regs[regno].regnum;
	      lexptr += namelength;
	      return (GDB_REGNAME);
	    }
	}

      /* Attempt to match against a convenience variable.  Note that
	 this will always succeed, because if no variable of that name
	 already exists, the lookup_internalvar will create one for us.
	 Also note that both lexptr and tokptr currently point to the
	 start of the input string we are trying to match, and that we
	 have already tested the first character for non-numeric, so we
	 don't have to treat it specially. */

      while (*tokptr == '_' || isalnum (*tokptr))
	{
	  tokptr++;
	}
      yylval.sval.ptr = lexptr;
      yylval.sval.length = tokptr - lexptr;
      yylval.ivar = lookup_internalvar (copy_name (yylval.sval));
      lexptr = tokptr;
      return (GDB_VARIABLE);
    }

  /* Since we didn't match against a register name or convenience
     variable, our only choice left is a history value. */

  if (*tokptr == '$')
    {
      negate = 1;
      ival = 1;
      tokptr++;
    }
  else
    {
      negate = 0;
      ival = 0;
    }

  /* Attempt to decode more characters as an integer value giving
     the index in the history list.  If successful, the value will
     overwrite ival (currently 0 or 1), and if not, ival will be
     left alone, which is good since it is currently correct for
     the '$' or '$$' case. */

  decode_integer_literal (&ival, &tokptr);
  yylval.lval = negate ? -ival : ival;
  lexptr = tokptr;
  return (GDB_LAST);
}

struct token
{
  char *operator;
  int token;
};

static const struct token idtokentab[] =
{
    { "length", LENGTH },
    { "lower", LOWER },
    { "upper", UPPER },
    { "andif", ANDIF },
    { "pred", PRED },
    { "succ", SUCC },
    { "card", CARD },
    { "size", SIZE },
    { "orif", ORIF },
    { "num", NUM },
    { "abs", ABS },
    { "max", MAX_TOKEN },
    { "min", MIN_TOKEN },
    { "mod", MOD },
    { "rem", REM },
    { "not", NOT },
    { "xor", LOGXOR },
    { "and", LOGAND },
    { "in", IN },
    { "or", LOGIOR }
};

static const struct token tokentab2[] =
{
    { ":=", GDB_ASSIGNMENT },
    { "//", SLASH_SLASH },
    { "->", POINTER },
    { "/=", NOTEQUAL },
    { "<=", LEQ },
    { ">=", GTR }
};

/* Read one token, getting characters through lexptr.  */
/* This is where we will check to make sure that the language and the
   operators used are compatible.  */

static int
yylex ()
{
    unsigned int i;
    int token;
    char *inputname;
    struct symbol *sym;

    /* Skip over any leading whitespace. */
    while (isspace (*lexptr))
	{
	    lexptr++;
	}
    /* Look for special single character cases which can't be the first
       character of some other multicharacter token. */
    switch (*lexptr)
	{
	    case '\0':
	        return (0);
	    case ',':
	    case '=':
	    case ';':
	    case '!':
	    case '+':
	    case '*':
	    case '(':
	    case ')':
	    case '[':
	    case ']':
		return (*lexptr++);
	}
    /* Look for characters which start a particular kind of multicharacter
       token, such as a character literal, register name, convenience
       variable name, string literal, etc. */
    switch (*lexptr)
      {
	case '\'':
	case '\"':
	  /* First try to match a string literal, which is any
	     sequence of characters enclosed in matching single or double
	     quotes, except that a single character inside single quotes
	     is a character literal, so we have to catch that case also. */
	  token = match_string_literal ();
	  if (token != 0)
	    {
	      return (token);
	    }
	  if (*lexptr == '\'')
	    {
	      token = match_character_literal ();
	      if (token != 0)
		{
		  return (token);
		}
	    }
	  break;
        case 'C':
        case 'c':
	  token = match_character_literal ();
	  if (token != 0)
	    {
	      return (token);
	    }
	  break;
	case '$':
	  token = match_dollar_tokens ();
	  if (token != 0)
	    {
	      return (token);
	    }
	  break;
      }
    /* See if it is a special token of length 2.  */
    for (i = 0; i < sizeof (tokentab2) / sizeof (tokentab2[0]); i++)
	{
	    if (STREQN (lexptr, tokentab2[i].operator, 2))
		{
		    lexptr += 2;
		    return (tokentab2[i].token);
		}
	}
    /* Look for single character cases which which could be the first
       character of some other multicharacter token, but aren't, or we
       would already have found it. */
    switch (*lexptr)
	{
	    case '-':
	    case ':':
	    case '/':
	    case '<':
	    case '>':
		return (*lexptr++);
	}
    /* Look for a float literal before looking for an integer literal, so
       we match as much of the input stream as possible. */
    token = match_float_literal ();
    if (token != 0)
	{
	    return (token);
	}
    token = match_bitstring_literal ();
    if (token != 0)
	{
	    return (token);
	}
    token = match_integer_literal ();
    if (token != 0)
	{
	    return (token);
	}

    /* Try to match a simple name string, and if a match is found, then
       further classify what sort of name it is and return an appropriate
       token.  Note that attempting to match a simple name string consumes
       the token from lexptr, so we can't back out if we later find that
       we can't classify what sort of name it is. */

    inputname = match_simple_name_string ();

    if (inputname != NULL)
      {
	char *simplename = (char*) alloca (strlen (inputname));

	char *dptr = simplename, *sptr = inputname;
	for (; *sptr; sptr++)
	  *dptr++ = isupper (*sptr) ? tolower(*sptr) : *sptr;

	/* See if it is a reserved identifier. */
	for (i = 0; i < sizeof (idtokentab) / sizeof (idtokentab[0]); i++)
	    {
		if (STREQ (simplename, idtokentab[i].operator))
		    {
			return (idtokentab[i].token);
		    }
	    }

	/* Look for other special tokens. */
	if (STREQ (simplename, "true"))
	    {
		yylval.ulval = 1;
		return (BOOLEAN_LITERAL);
	    }
	if (STREQ (simplename, "false"))
	    {
		yylval.ulval = 0;
		return (BOOLEAN_LITERAL);
	    }

	sym = lookup_symbol (inputname, expression_context_block,
			     VAR_NAMESPACE, (int *) NULL,
			     (struct symtab **) NULL);
	if (sym == NULL && strcmp (inputname, simplename) != 0)
	  {
	    sym = lookup_symbol (simplename, expression_context_block,
				 VAR_NAMESPACE, (int *) NULL,
				 (struct symtab **) NULL);
	  }
	if (sym != NULL)
	  {
	    yylval.ssym.stoken.ptr = NULL;
	    yylval.ssym.stoken.length = 0;
	    yylval.ssym.sym = sym;
	    yylval.ssym.is_a_field_of_this = 0;	/* FIXME, C++'ism */
	    switch (SYMBOL_CLASS (sym))
	      {
	      case LOC_BLOCK:
		/* Found a procedure name. */
		return (GENERAL_PROCEDURE_NAME);
	      case LOC_STATIC:
		/* Found a global or local static variable. */
		return (LOCATION_NAME);
	      case LOC_REGISTER:
	      case LOC_ARG:
	      case LOC_REF_ARG:
	      case LOC_REGPARM:
	      case LOC_REGPARM_ADDR:
	      case LOC_LOCAL:
	      case LOC_LOCAL_ARG:
	      case LOC_BASEREG:
	      case LOC_BASEREG_ARG:
		if (innermost_block == NULL
		    || contained_in (block_found, innermost_block))
		  {
		    innermost_block = block_found;
		  }
		return (LOCATION_NAME);
		break;
	      case LOC_CONST:
	      case LOC_LABEL:
		return (LOCATION_NAME);
		break;
	      case LOC_TYPEDEF:
		yylval.tsym.type = SYMBOL_TYPE (sym);
		return TYPENAME;
	      case LOC_UNDEF:
	      case LOC_CONST_BYTES:
	      case LOC_OPTIMIZED_OUT:
		error ("Symbol \"%s\" names no location.", inputname);
		break;
	      }
	  }
	else if (!have_full_symbols () && !have_partial_symbols ())
	  {
	    error ("No symbol table is loaded.  Use the \"file\" command.");
	  }
	else
	  {
	    error ("No symbol \"%s\" in current context.", inputname);
	  }
      }

    /* Catch single character tokens which are not part of some
       longer token. */

    switch (*lexptr)
      {
	case '.':			/* Not float for example. */
	  lexptr++;
	  while (isspace (*lexptr)) lexptr++;
	  inputname = match_simple_name_string ();
	  if (!inputname)
	    return '.';
	  return FIELD_NAME;
      }

    return (ILLEGAL_TOKEN);
}

void
yyerror (msg)
     char *msg;
{
  error ("A %s in expression, near `%s'.", (msg ? msg : "error"), lexptr);
}
