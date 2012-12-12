(*
 *
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 08-May-96  Emin Gun Sirer (egs) at the University of Washington
 *	Changed to not perform any allocation via calls to Text.Sub.
 *
 * 29-Nov-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Mime related routines.
 *
 *)
MODULE Mime;
IMPORT IO, Text;

CONST
  NumOfSuffixes = 21;
TYPE 
  SuffixArray = ARRAY [1..NumOfSuffixes] OF TEXT;
  SuffixLenArray = ARRAY [1..NumOfSuffixes] OF CARDINAL;
CONST 
  suffix   = SuffixArray{".html",
                         ".gif",
                         ".jpeg",
                         ".jpg",
                         ".txt",
                         ".tsv", 
                         ".tiff",
                         ".xbm",
                         ".ps",
                         ".PS",
                         ".eps",
                         ".rtf",
                         ".mif",
                         ".dvi",
                         ".man",
                         ".tar",
                         ".au",
                         ".snd",
                         ".mpeg",
                         ".mpg",
                         ".qt"
  };
  
  mimetype  = SuffixArray{"text/html",
                          "image/gif",
                          "image/jpeg",
                          "image/jpeg",
                          "text/plain",
                          "text/tab-separated-values",
                          "image/tiff",
                          "image/x-xbitmap",
                          "application/postscript",
                          "application/postscript",
                          "application/postscript",
                          "application/rtf",
                          "application/x-mif",
                          "application/x-dvi",
                          "application/x-troff-man",
                          "application/x-tar",
                          "audio/basic",
                          "audio/basic",
                          "video/mpeg",
                          "video/mpeg",
                          "video/quicktime"
  };

VAR
  suffixlen: SuffixLenArray;

(*
 * Determine url type.
 *)
PROCEDURE FindMimeType(READONLY url: ARRAY OF CHAR; urllen: CARDINAL) : TEXT =
  VAR 
    j: INTEGER;
  BEGIN
    FOR i := FIRST(suffix) TO LAST(suffix) DO
      IF urllen > suffixlen[i] THEN
        j := suffixlen[i];
        WHILE j >= 0 DO
          IF Text.GetChar(suffix[i], j) # 
             url[urllen - 1 - suffixlen[i] + j] THEN
            EXIT;
          END;
          DEC(j);
        END;
        IF j < 0 THEN
          RETURN mimetype[i];
        END;
      END;
    END;
    RETURN "text/plain";
  END FindMimeType;

(*
 * XXX Add suffix to encoding mappings.
        .Z              x-compress
        .gz             x-gzip
 *)

BEGIN
  FOR i := FIRST(suffix) TO LAST(suffix) DO
    suffixlen[i] := Text.Length(suffix[i]) - 1;
  END;
  IO.Put("Mime types initialized\n");
END Mime.

