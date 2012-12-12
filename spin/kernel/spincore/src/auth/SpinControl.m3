(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 13-Jun-97  Wilson Hsieh (whsieh) at the University of Washington
 *	call into Domain after static domains are initialized
 *
 * 11-Feb-97  Robert Grimm (rgrimm) at the University of Washington
 *      Removed Identity and most of Auth
 *
 * 28-Jan-97  Robert Grimm (rgrimm) at the University of Washington
 *      added support for SecurityPublic and SecurityPrivate
 *
 * 06-Mar-96  Devid Becker (becker) at the University of Washington
 *	nameserver fixup and quieted the authorizor
 *
 * 29-Feb-96  Brian Bershad (bershad) at the University of Washington
 *	Revised for simpler NameServer interface.
 *
 * 28-Oct-95  Brian Bershad (bershad) at the University of Washington
 *	Required to make UNSAFE to import interfaces with EXTERNS.
 *
 * 11-Oct-95  Marc Fiuczynski (mef) at the University of Washington
 *	Changed to merge the psuedo domains SpinPublic_C with SpinPublic
 *	and SpinTrusted_C with SpinTrusted.
 *
 * 01-Aug-95  Brian Bershad (bershad) at the University of Washington
 *	Created
 *)

(* "SpinControl" exports the (psuedo) Public and Trusted interfaces.
 
   This module is UNSAFE because it imports EXTERNS. These imports
   should be removed once we have INTERFACE_UNIT working. *)

UNSAFE MODULE SpinControl;

IMPORT SpinPublic, SpinPublicInterface,  SpinPublicPrivate, SpinPublicExtern;
IMPORT SpinTrusted, SpinTrustedInterface,  SpinTrustedPrivate, SpinTrustedExtern;
IMPORT SecurityPublicInterface;
IMPORT SecurityTrustedInterface;

IMPORT Domain, DomainPrivate, Auth, NameServer;
IMPORT IO, Fmt;<*NOWARN*>

VAR exactlyonce: BOOLEAN := FALSE;
PROCEDURE Init (verbose: BOOLEAN) =
  VAR
    public, trusted: TEXT;
  BEGIN
    IF NOT exactlyonce THEN
      exactlyonce := TRUE;
      EVAL SpinPublicInterface.Export( NEW(Auth.AuthAlways) );
      public := SpinPublic.Brand;
      
      EVAL SpinTrustedInterface.Export( NEW(Auth.AuthAlways) );
      trusted := SpinTrusted.Brand;
      
      EVAL SecurityPublicInterface.Export( NEW(Auth.AuthAlways) );
      EVAL SecurityTrustedInterface.Export( NEW(Auth.AuthAlways) );
      
      TRY
        (* merge public domains *)

        WITH
	  nsentry   = NameServer.Lookup(NIL, "/svc/domains/" & public),
	  holder    = NARROW(nsentry,Domain.T),
	  subdomain = DomainPrivate.CreateFromSymbols("SpinPublicCcode", 
                             SpinPublicExtern.domainSymbols,
                             SpinPublicExtern.domainSize)
         DO
          Domain.Add(holder, subdomain);
          SpinPublicPrivate.domain := holder;
        END;
        
        (* merge trusted domains *)
        WITH
	  nsentry     = NameServer.Lookup(NIL,"/svc/domains/" & trusted),
	  holder      = NARROW(nsentry, Domain.T),
	  subdomain   = DomainPrivate.CreateFromSymbols("SpinTrustedCcode",
                             SpinTrustedExtern.domainSymbols,
                             SpinTrustedExtern.domainSize)
          
         DO
          Domain.Add(holder, subdomain);
          SpinTrustedPrivate.domain := holder;
        END;

        IF verbose THEN
          IO.Put("SpinControl init done for " & 
            public & ", " & trusted & ".\n");
        END;
      EXCEPT
      ELSE
        IO.Put("ERROR >> SpinControl: domain merge failed\n");
      END;

      Domain.StaticDomainsInitialized ();
    END;
  END Init;

BEGIN
END SpinControl.

