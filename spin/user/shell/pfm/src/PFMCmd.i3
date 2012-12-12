(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 24-Feb-97  Yasushi Saito (yasushi) at the University of Washington
 *	Added pfm interrupt handling.
 * html
 *)
INTERFACE PFMCmd;

(*
   The "pfm" shell command is used to control the Alpha performance monitor.
   It is usually loaded by issuing "nanny touch PFM" from the shell.

   | pfm [open|enable |disable |close |setitems |counters |ipl |profiling |setmux |clear |getcount ] args...

   <dl>
   <dt>"open"
   <dd>Opens the pfm device. Takes no argument. Doesn't have to be called
   usually.
   <dt>"enable"
   <dd>Enables interrupts from the PFM device. Takes no argument.
   <dt>"disable"
   <dd>Disables interrupts from the PFM device. Takes no argument.
   Profiling info can be obtained only after "disable" is called.
   <dt>"close"
   <dd>Closes the pfm device. Takes no argument. Doesn't have to be called
   usually.
   <dt>"setitems"
   <dd>Selects what to do at each interrupt. Arguments are a sequence of
   following subcommands.
      <dl>
      <dt>"counters"
      <dd>just count the # of interrupts. This is the default.
      <dt>"ipl"
      <dd>just count the # of interrupts. This is the default.
      <dt>"profiling"
      <dd>take PC profile.
      </dl>
   
   <dt>"setmux"
   <dd>PFM has two counters, and this command chooses the item to be counted
   by each counter. Arguments are a sequence of following subcommands.
   See pfm(8) for what they mean.
   
   <pre>off0 off1 freq0 freq1 issues pipedry loadi pipefrozen
   branchi cycles palmode nonissues dcache icache dual branchmiss
   fpinst intops storei</pre>

   <dt>"clear"
   <dd>Clear the profiling and counter data.
   
   <dt>"getcount"
   <dd>Get the number of interrupts.

   </dl>
   
*)

END PFMCmd.
