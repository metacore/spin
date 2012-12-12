(* 
 * HISTORY
 * 02-May-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *)

INTERFACE EtherClient;
PROCEDURE Init(verbose:BOOLEAN); 
CONST Brand  = "EtherClient";
CONST timing = FALSE;

(* shell command support *)
CONST EtherClassificationCommandName = "etherclassification";
      EtherClassificationCommandHelp = " -debug level";

CONST EtherGenCommandName = "ethergen";
      EtherGenCommandHelp = " -debug level";

CONST EtherArpCommandName = "etherarp";
      EtherArpCommandHelp = " -debug level| -dump| -del num num num num";

CONST EtherDefaultCommandName = "etherdefault";
      EtherDefaultCommandHelp = " -debug level| -packets";

END EtherClient.
