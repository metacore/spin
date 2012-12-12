INTERFACE Video; 

(* base video support *)

(* XXX fix me. *)
(* -change CARDINAL to unsigned short. *)
(* -fix ADDRESS to be REFANY, once we can gen M3 types in M3. *)
(* the video_framebuffer() interface directly exports pointer to the framebuffer *)

TYPE T = ADDRESS;

<* EXTERNAL *> PROCEDURE video_clear_screen(video:T);

<* EXTERNAL *> PROCEDURE video_frame_width(video:T):CARDINAL;

<* EXTERNAL *> PROCEDURE video_frame_height(video:T):CARDINAL;

<* EXTERNAL *> PROCEDURE video_frame_bitpxel(video:T):CARDINAL;

<* EXTERNAL *> PROCEDURE video_frame_unit(video:T):CARDINAL;

<* EXTERNAL *> PROCEDURE video_set_screen_pos(video:T; xoff: CARDINAL; yoff:CARDINAL);

(* <* EXTERNAL *> PROCEDURE video_framebuffer(video:T):ADDRESS; *)

<* EXTERNAL *> PROCEDURE video_set_cmap_entry(video:T ; index:CARDINAL ; red:CARDINAL ; green:CARDINAL ; blue:CARDINAL);

<* EXTERNAL *> PROCEDURE video_new(unit:CARDINAL):T;

<* EXTERNAL *> PROCEDURE video_dealloc(video:T);

END Video.
