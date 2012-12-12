(*
 *
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 *)

(*
 * HISTORY
 *
 *)

INTERFACE AnalysisPrivate;

IMPORT Analysis, BasicBlock;

REVEAL
  Analysis.T = Analysis.PublicT BRANDED OBJECT
    name : TEXT;
    direction: Analysis.Direction;
  OVERRIDES
    Name := GetName;
    Direction := GetDirection;

    InvalidateResidueBlock := InvalidateResidueBlock;
  END;
    
PROCEDURE GetName (a: Analysis.T) : TEXT;
PROCEDURE GetDirection (a: Analysis.T) : Analysis.Direction;

PROCEDURE InvalidateResidueBlock(a: Analysis.T; block: BasicBlock.T);

END AnalysisPrivate.
