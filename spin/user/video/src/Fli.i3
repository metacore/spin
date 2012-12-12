INTERFACE Fli; 
IMPORT Video;

(* fli video format *)
TYPE T = ARRAY OF CHAR;
CONST 
  FILE_HEADER_SIZE  = 128;
  FRAME_HEADER_SIZE =  16;
  CHUNK_HEADER_SIZE =   6;
  FRAME_HEADER_SIZE_OFFSET = 0;
  CHUNK_HEADER_SIZE_OFFSET = 0;


<* EXTERNAL fli_clr1 *> PROCEDURE fli_clr(dpy:Video.T; VAR chunk_buff: ARRAY OF CHAR; lsb: BOOLEAN );
<* EXTERNAL fli_lc1 *> PROCEDURE fli_lc(dpy:Video.T; VAR chunk_buff: ARRAY OF CHAR; lsb: BOOLEAN );
<* EXTERNAL fli_brun1 *> PROCEDURE fli_brun(dpy:Video.T; height:CARDINAL; width: CARDINAL; VAR chunk_buff: ARRAY OF CHAR; lsb: BOOLEAN);
<* EXTERNAL fli_display_frame1 *> PROCEDURE fli_display_frame(VAR fh:T; dpy:Video.T; height:CARDINAL; width:CARDINAL; VAR buff: ARRAY OF CHAR; lsb: BOOLEAN);
<* EXTERNAL fli_play1 *> PROCEDURE fli_play(VAR fh:ARRAY OF CHAR; dpy:Video.T; height: CARDINAL; width: CARDINAL; frames: CARDINAL; repeat: CARDINAL);
<* EXTERNAL fli_frames1 *> PROCEDURE fli_frames(VAR fh:T):CARDINAL;
<* EXTERNAL fli_width1 *> PROCEDURE fli_width(VAR fh:T):CARDINAL;
<* EXTERNAL fli_height1 *> PROCEDURE fli_height(VAR fh:T):CARDINAL;
<* EXTERNAL fli_magic1 *> PROCEDURE fli_magic(VAR fh:T):BOOLEAN;
<* EXTERNAL fli_total_size1 *> PROCEDURE fli_total_size(VAR fh:T):CARDINAL;
<* EXTERNAL fli_read_frame_header1 *> PROCEDURE fli_read_frame_header(VAR b:ARRAY OF CHAR; VAR fh:T; VAR offset: CARDINAL; VAR lsb: BOOLEAN):BOOLEAN;
<* EXTERNAL fli_size1 *> PROCEDURE fli_size(VAR fh:T; offset: CARDINAL; lsb: BOOLEAN):CARDINAL;
<* EXTERNAL fli_dump1 *> PROCEDURE fli_dump(VAR fh:T);
<* EXTERNAL fli_get_frame1 *> PROCEDURE fli_get_frame(VAR buff:ARRAY OF CHAR; frame: REF T):BOOLEAN;
<* EXTERNAL fli_do_demo *> PROCEDURE fli_do_demo();

END Fli.
