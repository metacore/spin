(*
 * HISTORY
 * 10-May-97  Tian Fung Lim (tian) at the University of Washington
 *	Added time mode.
 *
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added quiet and verbose modes.
 *
 * 26-Mar-96  Brian Bershad (bershad) at the University of Washington
 *	Created.
 *
 *)


INTERFACE Threat;


(*
 * Threat is "domain control" interface. It's purpose is much like inetd,
 * providing a friendly on-demand loader for many popular SPIN services.
 * It works by registering domain names with the nameserver, using
 * the auth callback service to on-demand load a domain when someone asks
 * for it.  At that point, nanny takes itself out of the naming loop for
 * the loaded domain and waits until the loaded domain is unloaded, at which
 * point, nanny kicks back in for the domain.
 *
 *)

IMPORT ParseParams;


CONST CommandName = "threat";
CONST CommandHelp = "stress scheduler. args: zap - uninstall, verbose - more output, quiet - less output, time s - quit after s seconds";
PROCEDURE Run(pp: ParseParams.T): BOOLEAN; 


END Threat.


 

  

