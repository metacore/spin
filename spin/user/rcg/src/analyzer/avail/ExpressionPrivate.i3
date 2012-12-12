
(*
 *
 * Copyright 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 *)

(*
 * HISTORY
 * 03-Jan-97  Wilson Hsieh (whsieh) at the University of Washington
 *	created
 *
 *
 *)



INTERFACE ExpressionPrivate;

IMPORT Expression;


  (************* types *******************)

TYPE
  ImmedData = RECORD
    immed: Expression.Immed;
    mark: BOOLEAN;
    offset: CARDINAL;
  END;

  ImmedArray = REF ARRAY OF ImmedData;

  TArray = REF ARRAY OF Expression.T;

  (************* revelations *************)

REVEAL
  Expression.Wildcard <: Expression.WildcardPublic OBJECT
    values: ImmedArray;
    next  : CARDINAL;
  END;

REVEAL
  Expression.Phi <: Expression.T OBJECT METHODS
    addExpr (t: Expression.T);
  END OBJECT
    exprs: TArray;
    next : CARDINAL;
  END;

END ExpressionPrivate.
