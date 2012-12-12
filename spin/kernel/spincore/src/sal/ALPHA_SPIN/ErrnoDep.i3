(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 07-Mar-96  Marc Fiuczynski (mef) at the University of Washington
 *	Cleaned up errno constant definition and errormessages.
 *
 * 12-Sep-95  Marc Fiuczynski (mef) at the University of Washington
 *	OSF/1 3.2 sys/errno.h
 *
 *)

INTERFACE ErrnoDep;

CONST ESUCCESS        = 0;           (* Successful                                     *)
CONST EPERM           = 1;           (* Not owner                                      *)
CONST ENOENT          = 2;           (* No such file or directory                      *)
CONST ESRCH           = 3;           (* No such process                                *)
CONST EINTR           = 4;           (* Interrupted system call                        *)
CONST EIO             = 5;           (* IO error                                       *)
CONST ENXIO           = 6;           (* No such deviceor address                       *)
CONST E2BIG           = 7;           (* Arg list too long                              *)
CONST ENOEXEC         = 8;           (* Exec format error                              *)
CONST EBADF           = 9;           (* Bad file number                                *)
CONST ECHILD          = 10;          (* No children                                    *)
CONST EDEADLK         = 11;          (* Operation would cause deadlock                 *)
CONST ENOMEM          = 12;          (* Notenough core                                 *)
CONST EACCES          = 13;          (* Permission denied                              *)
CONST EFAULT          = 14;          (* Bad address                                    *)
CONST ENOTBLK         = 15;          (* Block device required                          *)
CONST EBUSY           = 16;          (* Mount device busy                              *)
CONST EEXIST          = 17;          (* File exists                                    *)
CONST EXDEV           = 18;          (* Cross device link                              *)
CONST ENODEV          = 19;          (* No such device                                 *)
CONST ENOTDIR         = 20;          (* Not a director                                 *)
CONST EISDIR          = 21;          (* Is a directory                                 *)
CONST EINVAL          = 22;          (* Invalid argument                               *)
CONST ENFILE          = 23;          (* File table overflow                            *)
CONST EMFILE          = 24;          (* Too many open files                            *)
CONST ENOTTY          = 25;          (* Nota typewriter                                *)
CONST ETXTBSY         = 26;          (* Text file busy                                 *)
CONST EFBIG           = 27;          (* File too large                                 *)
CONST ENOSPC          = 28;          (* No space lefton device                         *)
CONST ESPIPE          = 29;          (* Illegal seek                                   *)
CONST EROFS           = 30;          (* Read-only file system                          *)
CONST EMLINK          = 31;          (* Too many links                                 *)
CONST EPIPE           = 32;          (* Broken pipe                                    *)
                                     (* math software                                  *)
CONST EDOM            = 33;          (* Argument too large                             *)
CONST ERANGE          = 34;          (* Result too large                               *)

                                     (* STREAMS: packet size of of configured range    *)
                                     (* non-blocking and interrupt i/o                 *)
CONST EWOULDBLOCK     = 35;          (* Operation would block                          *)
CONST EAGAIN          = EWOULDBLOCK; (* Operation would block                          *)
CONST EINPROGRESS     = 36;          (* Operation now in progress                      *)
CONST EALREADY        = 37;          (* Operation already in progress                  *)
                                     (* ipc/network software                           *)
                                     (* argument errors                                *)
CONST ENOTSOCK        = 38;          (* Socket operation on non-socket                 *)
CONST EDESTADDRREQ    = 39;          (* Destination address required                   *)
CONST EMSGSIZE        = 40;          (* Message too long                               *)
CONST EPROTOTYPE      = 41;          (* Protocol wrong type for socket                 *)
CONST ENOPROTOOPT     = 42;          (* Protocol not available                         *)
CONST EPROTONOSUPPORT = 43;          (* Protocol not supported                         *)
CONST ESOCKTNOSUPPORT = 44;          (* Socket type not supported                      *)
CONST EOPNOTSUPP      = 45;          (* Operation not supportedon socket               *)
CONST EPFNOSUPPORT    = 46;          (* Protocol family not supported                  *)
CONST EAFNOSUPPORT    = 47;          (* Address family not supported byprotocol family *)
CONST EADDRINUSE      = 48;          (* Address already in use                         *)
CONST EADDRNOTAVAIL   = 49;          (* Can't assign requested address                 *)

                                     (* operational errors                             *)
CONST ENETDOWN        = 50;          (* Network is down                                *)
CONST ENETUNREACH     = 51;          (* Network is unreachable                         *)
CONST ENETRESET       = 52;          (* Network dropped connection on reset            *)
CONST ECONNABORTED    = 53;          (* Software caused connection abort               *)
CONST ECONNRESET      = 54;          (* Connection reset by peer                       *)
CONST ENOBUFS         = 55;          (* No buffer space available                      *)
CONST EISCONN         = 56;          (* Socket is already connected                    *)
CONST ENOTCONN        = 57;          (* Socket is not connected                        *)
CONST ESHUTDOWN       = 58;          (* Can't send aftersocket shutdown                *)
CONST ETOOMANYREFS    = 59;          (* Too many references: cant splice               *)
CONST ETIMEDOUT       = 60;          (* Connection timed out                           *)
CONST ECONNREFUSED    = 61;          (* Connection refused                             *)
CONST ELOOP           = 62;          (* Too many levels of symbolic links              *)
CONST ENAMETOOLONG    = 63;          (* File name too long                             *)
CONST EHOSTDOWN       = 64;          (* Host is down                                   *)
CONST EHOSTUNREACH    = 65;          (* No route to host                               *)
CONST ENOTEMPTY       = 66;          (* Directory not empty                            *)
CONST EPROCLIM        = 67;          (* Too many processes                             *)
CONST EUSERS          = 68;          (* Too many users                                 *)
CONST EDQUOT          = 69;          (* Disc quota exceeded                            *)

                                     (* NFS errors.                                    *)
CONST ESTALE          = 70;          (* Stale NFS file handle                          *)
CONST EREMOTE         = 71;          (* Too many levels of remote in path              *)
CONST EBADRPC         = 72;          (* RPC struct is bad                              *)
CONST ERPCMISMATCH    = 73;          (* RPC version wrong                              *)
CONST EPROGUNAVAIL    = 74;          (* RPC prog. not avail                            *)
CONST EPROGMISMATCH   = 75;          (* Program version wrong                          *)
CONST EPROCUNAVAIL    = 76;          (* Bad procedure for program                      *)
CONST ENOLCK          = 77;          (* No locks available                             *)
CONST ENOSYS          = 78;          (* Function not implemented                       *)
CONST EFTYPE          = 79;          (* Inappropriate file type or format              *)
CONST ENOMSG          = 80;          (* No msg matches receive request                 *)
CONST EIDRM           = 81;          (* Msg queue id has been removed                  *)
                                     (* STREAMS                                        *)
CONST ENOSR           = 82;          (* Out of STREAMS resources                       *)
CONST ETIME           = 83;          (* System call timed out                          *)
CONST EBADMSG         = 84;          (* Next message has wrong type                    *)
CONST EPROTO          = 85;          (* STREAMS protocol error                         *)
CONST ENODATA         = 86;          (* No message on stream headread q                *)
CONST ENOSTR          = 87;          (* fd not associated witha stream                 *)
CONST ECLONEME        = 88;          (* Tells open to clonethe device                  *)
                                     (* Filesystem                                     *)
CONST EDIRTY          = 89;          (* Mounting a dirty fs wo force                   *)
                                     (* Loader errors                                  *)
CONST EDUPPKG         = 90;          (* duplicate package nameon install               *)
CONST EVERSION        = 91;          (* version number mismatch                        *)
CONST ENOPKG          = 92;          (* unresolved package name                        *)
CONST ENOSYM          = 93;          (* unresolved symbol name                         *)
CONST ECANCELED       = 94;          (* operation canceled                             *)
CONST EFAIL           = 95;          (* cannot start operation                         *)
CONST EINPROG         = 97;          (* operation (now) in progress                    *)
CONST EMTIMERS        = 98;          (* too many timers                                *)
CONST ENOTSUP         = 99;          (* function not implemented                       *)
CONST EAIO            = 100;         (* internal AIO operation complete                *)

                                     (* BSD 4.3 RENO                                   *)
CONST EILSEQ          = 116;         (* Invalid wide character                         *)

                                     (* Internal Disk/Block Device error codes         *)
CONST ESOFT           = 123;         (* soft ECC error                                 *)
CONST EMEDIA          = 124;         (* hard ECC error                                 *)
CONST ERELOCATED      = 125;         (* XXX                                            *)

CONST FirstError = ESUCCESS;
      LastError  = ERELOCATED;

CONST
  ErrorMessages = ARRAY [FirstError .. LastError] OF TEXT 
  {
  (* ESUCCESS           = 0;   *) "Successful",
  (* EPERM              = 1;   *) "Not owner",
  (* ENOENT             = 2;   *) "No such file or directory",
  (* ESRCH              = 3;   *) "No such process",
  (* EINTR              = 4;   *) "Interrupted system call",
  (* EIO                = 5;   *) "IO error",
  (* ENXIO              = 6;   *) "No such deviceor address",
  (* E2BIG              = 7;   *) "Arg list too long",
  (* ENOEXEC            = 8;   *) "Exec format error",
  (* EBADF              = 9;   *) "Bad file number",
  (* ECHILD             = 10;  *) "No children",
  (* EDEADLK            = 11;  *) "Operation would cause deadlock",
  (* ENOMEM             = 12;  *) "Notenough core",
  (* EACCES             = 13;  *) "Permission denied",
  (* EFAULT             = 14;  *) "Bad address",
  (* ENOTBLK            = 15;  *) "Block device required",
  (* EBUSY              = 16;  *) "Mount device busy",
  (* EEXIST             = 17;  *) "File exists",
  (* EXDEV              = 18;  *) "Cross device link",
  (* ENODEV             = 19;  *) "No such device",
  (* ENOTDIR            = 20;  *) "Not a director",
  (* EISDIR             = 21;  *) "Is a directory",
  (* EINVAL             = 22;  *) "Invalid argument",
  (* ENFILE             = 23;  *) "File table overflow",
  (* EMFILE             = 24;  *) "Too many open files",
  (* ENOTTY             = 25;  *) "Nota typewriter",
  (* ETXTBSY            = 26;  *) "Text file busy",
  (* EFBIG              = 27;  *) "File too large",
  (* ENOSPC             = 28;  *) "No space lefton device",
  (* ESPIPE             = 29;  *) "Illegal seek",
  (* EROFS              = 30;  *) "Read-only file system",
  (* EMLINK             = 31;  *) "Too many links",
  (* EPIPE              = 32;  *) "Broken pipe",
  (* EDOM               = 33;  *) "Argument too large",
  (* ERANGE             = 34;  *) "Result too large",
  (* EWOULDBLOCK/EAGAIN = 35;  *) "Operation would block",
  (* EINPROGRESS        = 36;  *) "Operation now in progress",
  (* EALREADY           = 37;  *) "Operation already in progress",
  (* ENOTSOCK           = 38;  *) "Socket operation on non-socket",
  (* EDESTADDRREQ       = 39;  *) "Destination address required",
  (* EMSGSIZE           = 40;  *) "Message too long",
  (* EPROTOTYPE         = 41;  *) "Protocol wrong type for socket",
  (* ENOPROTOOPT        = 42;  *) "Protocol not available",
  (* EPROTONOSUPPORT    = 43;  *) "Protocol not supported",
  (* ESOCKTNOSUPPORT    = 44;  *) "Socket type not supported",
  (* EOPNOTSUPP         = 45;  *) "Operation not supportedon socket",
  (* EPFNOSUPPORT       = 46;  *) "Protocol family not supported",
  (* EAFNOSUPPORT       = 47;  *) "Address family not supported byprotocol family",
  (* EADDRINUSE         = 48;  *) "Address already in use",
  (* EADDRNOTAVAIL      = 49;  *) "Can't assign requested address",
  (* ENETDOWN           = 50;  *) "Network is down",
  (* ENETUNREACH        = 51;  *) "Network is unreachable",
  (* ENETRESET          = 52;  *) "Network dropped connection on reset",
  (* ECONNABORTED       = 53;  *) "Software caused connection abort",
  (* ECONNRESET         = 54;  *) "Connection reset by peer",
  (* ENOBUFS            = 55;  *) "No buffer space available",
  (* EISCONN            = 56;  *) "Socket is already connected",
  (* ENOTCONN           = 57;  *) "Socket is not connected",
  (* ESHUTDOWN          = 58;  *) "Can't send aftersocket shutdown",
  (* ETOOMANYREFS       = 59;  *) "Too many references: cant splice",
  (* ETIMEDOUT          = 60;  *) "Connection timed out",
  (* ECONNREFUSED       = 61;  *) "Connection refused",
  (* ELOOP              = 62;  *) "Too many levels of symbolic links",
  (* ENAMETOOLONG       = 63;  *) "File name too long",
  (* EHOSTDOWN          = 64;  *) "Host is down",
  (* EHOSTUNREACH       = 65;  *) "No route to host",
  (* ENOTEMPTY          = 66;  *) "Directory not empty",
  (* EPROCLIM           = 67;  *) "Too many processes",
  (* EUSERS             = 68;  *) "Too many users",
  (* EDQUOT             = 69;  *) "Disc quota exceeded",
  (* ESTALE             = 70;  *) "Stale NFS file handle",
  (* EREMOTE            = 71;  *) "Too many levels of remote in path",
  (* EBADRPC            = 72;  *) "RPC struct is bad",
  (* ERPCMISMATCH       = 73;  *) "RPC version wrong",
  (* EPROGUNAVAIL       = 74;  *) "RPC prog. not avail",
  (* EPROGMISMATCH      = 75;  *) "Program version wrong",
  (* EPROCUNAVAIL       = 76;  *) "Bad procedure for program",
  (* ENOLCK             = 77;  *) "No locks available",
  (* ENOSYS             = 78;  *) "Function not implemented",
  (* EFTYPE             = 79;  *) "Inappropriate file type or format",
  (* ENOMSG             = 80;  *) "No msg matches receive request",
  (* EIDRM              = 81;  *) "Msg queue id has been removed",
  (* ENOSR              = 82;  *) "Out of STREAMS resources",
  (* ETIME              = 83;  *) "System call timed out",
  (* EBADMSG            = 84;  *) "Next message has wrong type",
  (* EPROTO             = 85;  *) "STREAMS protocol error",
  (* ENODATA            = 86;  *) "No message on stream headread q",
  (* ENOSTR             = 87;  *) "fd not associated witha stream",
  (* ECLONEME           = 88;  *) "Tells open to clonethe device",
  (* EDIRTY             = 89;  *) "Mounting a dirty fs wo force",
  (* EDUPPKG            = 90;  *) "duplicate package nameon install",
  (* EVERSION           = 91;  *) "version number mismatch",
  (* ENOPKG             = 92;  *) "unresolved package name",
  (* ENOSYM             = 93;  *) "unresolved symbol name",
  (* ECANCELED          = 94;  *) "operation canceled",
  (* EFAIL              = 95;  *) "cannot start operation",
  (* 96                        *) "XXX Unused",
  (* EINPROG            = 97;  *) "operation (now) in progress",
  (* EMTIMERS           = 98;  *) "too many timers",
  (* ENOTSUP            = 99;  *) "function not implemented",
  (* EAIO               = 100; *) "internal AIO operation complete",
  (* 101                       *) "XXX Unused",
  (* 102                       *) "XXX Unused",
  (* 103                       *) "XXX Unused",
  (* 104                       *) "XXX Unused",
  (* 105                       *) "XXX Unused",
  (* 106                       *) "XXX Unused",
  (* 107                       *) "XXX Unused",
  (* 108                       *) "XXX Unused",
  (* 109                       *) "XXX Unused",
  (* 110                       *) "XXX Unused",
  (* 111                       *) "XXX Unused",
  (* 112                       *) "XXX Unused",
  (* 113                       *) "XXX Unused",
  (* 114                       *) "XXX Unused",
  (* 115                       *) "XXX Unused",
  (* EILSEQ             = 116; *) "Invalid wide character",
  (* 117                       *) "XXX Unused",
  (* 118                       *) "XXX Unused",
  (* 119                       *) "XXX Unused",
  (* 120                       *) "XXX Unused",
  (* 121                       *) "XXX Unused",
  (* 122                       *) "XXX Unused",
  (* ESOFT              = 123; *) "soft ECC error",
  (* EMEDIA             = 124; *) "hard ECC error",
  (* ERELOCATED         = 125; *) "XXX"
  };
  
END ErrnoDep.
