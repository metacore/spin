(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 10-Jun-97  Wilson Hsieh (whsieh) at the University of Washington
 *	add checks for import/export
 *
 * 04-Mar-97  Przemek Pardyak (pardy) at the University of Washington
 *      Removed the destruction code.
 *
 * 27-Feb-97  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Checks on domains.
 *)

INTERFACE DomainChecks;
IMPORT Domain;
IMPORT Mx, Wr;
IMPORT IntAuthTbl;

VAR
  TypeAuthTbl : IntAuthTbl.T;

PROCEDURE CheckM3LinkInfo(linkBase: Mx.LinkSet;
                          units: Mx.UnitList; 
                          nUnits: INTEGER;
                          wr: Wr.T;
                          dump: BOOLEAN;
                          static: BOOLEAN): BOOLEAN;

PROCEDURE CheckSubtypes(domain: Domain.T): BOOLEAN;

PROCEDURE CheckImportExport (units: Mx.UnitList; domain: Domain.T) : BOOLEAN;

END DomainChecks.
