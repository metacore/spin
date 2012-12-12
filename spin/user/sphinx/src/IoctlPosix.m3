(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 11-Jun-96 oystr at the University of Washington
 *	Added [O]SIOCGIFCONF, FIONBIO.
 *
 * 08-Sep-95  Marc Fiuczynski (mef) at the University of Washington
 *      This module initializes the ioctl cmd values for the POSIX ioctl.
 *
 *)

MODULE IoctlPosix;
IMPORT Ioctl, Ctypes;
IMPORT If;

PROCEDURE Init() =
  BEGIN
    (* initialize cmd values defined in sys/ioctl.h *)

    (* cmd values for ioctls using the ifreq struct as parameter *)
    OSIOCGIFADDR := Ioctl.IOWR('i',13, BYTESIZE(If.ifreq));     (* #define OSIOCGIFADDR    _IOWR('i',13, struct ifreq)     /* get ifnet address  */ *)
    SIOCGIFADDR := Ioctl.IOWR('i',33, BYTESIZE(If.ifreq));      (* #define SIOCGIFADDR     _IOWR('i',33, struct ifreq)     /* get ifnet address  */ *)
    OSIOCGIFDSTADDR := Ioctl.IOWR('i',15, BYTESIZE(If.ifreq));  (* #define OSIOCGIFDSTADDR _IOWR('i',15, struct ifreq)     /* get p-p address    */ *)
    SIOCGIFDSTADDR := Ioctl.IOWR('i',34, BYTESIZE(If.ifreq));   (* #define SIOCGIFDSTADDR  _IOWR('i',34, struct ifreq)     /* get p-p address    */ *)
    OSIOCGIFBRDADDR := Ioctl.IOWR('i',18, BYTESIZE(If.ifreq));  (* #define OSIOCGIFBRDADDR _IOWR('i',18, struct ifreq)     /* get broadcast addr */ *)
    SIOCGIFBRDADDR := Ioctl.IOWR('i',35, BYTESIZE(If.ifreq));   (* #define SIOCGIFBRDADDR  _IOWR('i',35, struct ifreq)     /* get broadcast addr */ *)
    OSIOCGIFNETMASK := Ioctl.IOWR('i',21, BYTESIZE(If.ifreq));  (* #define OSIOCGIFNETMASK _IOWR('i',21, struct ifreq)     /* get net addr mask  */ *)
    SIOCGIFNETMASK := Ioctl.IOWR('i',37, BYTESIZE(If.ifreq));   (* #define SIOCGIFNETMASK  _IOWR('i',37, struct ifreq)     /* get net addr mask  */ *)
    SIOCSIFADDR := Ioctl.IOW('i', 12, BYTESIZE(If.ifreq));      (* #define SIOCSIFADDR     _IOW('i', 12, struct ifreq)     /* set ifnet address  */ *)
    SIOCSIFDSTADDR := Ioctl.IOW('i', 14, BYTESIZE(If.ifreq));   (* #define SIOCSIFDSTADDR  _IOW('i', 14, struct ifreq)     /* set p-p address    */ *)
    SIOCSIFFLAGS := Ioctl.IOW('i', 16, BYTESIZE(If.ifreq));     (* #define SIOCSIFFLAGS    _IOW('i', 16, struct ifreq)     /* set ifnet flags    */ *)
    SIOCGIFFLAGS := Ioctl.IOWR('i',17, BYTESIZE(If.ifreq));     (* #define SIOCGIFFLAGS    _IOWR('i',17, struct ifreq)     /* get ifnet flags    */ *)
    SIOCSIFBRDADDR := Ioctl.IOW('i', 19, BYTESIZE(If.ifreq));   (* #define SIOCSIFBRDADDR  _IOW('i', 19, struct ifreq)     /* set broadcast addr */ *)
    SIOCSIFNETMASK := Ioctl.IOW('i', 22, BYTESIZE(If.ifreq));   (* #define SIOCSIFNETMASK  _IOW('i', 22, struct ifreq)     /* set net addr mask  */ *)
    SIOCGIFMETRIC := Ioctl.IOWR('i',23, BYTESIZE(If.ifreq));    (* #define SIOCGIFMETRIC   _IOWR('i',23, struct ifreq)     /* get IF metric      */ *)
    SIOCSIFMETRIC := Ioctl.IOW('i', 24, BYTESIZE(If.ifreq));    (* #define SIOCSIFMETRIC   _IOW('i', 24, struct ifreq)     /* set IF metric      */ *)
    SIOCDIFADDR := Ioctl.IOW('i', 25, BYTESIZE(If.ifreq));      (* #define SIOCDIFADDR     _IOW('i', 25, struct ifreq)     /* delete IF addr     */ *)
    SIOCAIFADDR := Ioctl.IOW('i', 26, BYTESIZE(If.ifaliasreq)); (* #define SIOCAIFADDR     _IOW('i', 26, struct ifaliasreq)/* add/chg IF alias   */ *)
    SIOCPIFADDR := Ioctl.IOW('i', 29, BYTESIZE(If.ifaliasreq)); (* #define SIOCAIFADDR     _IOW('i', 29, struct ifaliasreq)/* add/chg IF alias   */ *)
    SIOCARPREQ := Ioctl.IOWR('i',40, BYTESIZE(If.ifreq));       (* #define SIOCARPREQ      _IOWR('i',40, struct ifreq)     /* arp request pkt    */ *)
    SIOCIFRESET := Ioctl.IOW('i',47, BYTESIZE(If.ifreq));       (* #define SIOCIFRESET     _IOW('i',47, struct ifreq)      /* Reset interface    */ *)
    SIOCADDMULTI := Ioctl.IOW('i', 49, BYTESIZE(If.ifreq));     (* #define SIOCADDMULTI    _IOW('i', 49, struct ifreq)     /* add m'cast addr    */ *)
    SIOCDELMULTI := Ioctl.IOW('i', 50, BYTESIZE(If.ifreq));     (* #define SIOCDELMULTI    _IOW('i', 50, struct ifreq)     /* del m'cast addr    */ *)
    SIOCENABLBACK := Ioctl.IOW('i', 60, BYTESIZE(If.ifreq));    (* #define SIOCENABLBACK   _IOW('i', 60, struct ifreq)     /* Enable loopback    */ *)
    SIOCDISABLBACK := Ioctl.IOW('i', 61, BYTESIZE(If.ifreq));   (* #define SIOCDISABLBACK  _IOW('i', 61, struct ifreq)     /* Disable loopback   */ *)
    SIOCSMACSPEED := Ioctl.IOW('i', 65, BYTESIZE(If.ifreq));    (* #define SIOCSMACSPEED   _IOW('i', 65, struct ifreq)     /* Set MAC speed      */ *)
    SIOCRMACSPEED := Ioctl.IOWR('i', 66, BYTESIZE(If.ifreq));   (* #define SIOCRMACSPEED   _IOWR('i', 66, struct ifreq)    /* Read MAC speed     */ *)
    SIOCSIPMTU := Ioctl.IOW('i', 67, BYTESIZE(If.ifreq));       (* #define SIOCSIPMTU      _IOW('i', 67, struct ifreq)     /* Set intf. IP MTU   */ *)
    SIOCRIPMTU := Ioctl.IOWR('i', 68, BYTESIZE(If.ifreq));      (* #define SIOCRIPMTU      _IOWR('i', 68, struct ifreq)    /* Read intf. IP MTU  */ *)
    (* cmd values for ioctls using the ifdevea struct as parameter *)
    SIOCRPHYSADDR := Ioctl.IOWR('i', 62, BYTESIZE(If.ifdevea)); (* #define SIOCRPHYSADDR   _IOWR('i', 62, struct ifdevea)  /* Read Phys addr     */ *)
    SIOCSPHYSADDR := Ioctl.IOWR('i', 63, BYTESIZE(If.ifdevea)); (* #define SIOCSPHYSADDR   _IOWR('i', 63, struct ifdevea)  /* Set addr           */ *)
    OSIOCGIFCONF := Ioctl.IOWR('i',20,BYTESIZE(If.ifconf));
    SIOCGIFCONF  := Ioctl.IOWR('i',36,BYTESIZE(If.ifconf));
    
    FIONBIO := Ioctl.IOW('f',126,BYTESIZE(Ctypes.int));
    FIONREAD := Ioctl.IOR('f', 127, BYTESIZE(Ctypes.int));
    FIOASYNC := Ioctl.IOW('f', 125, BYTESIZE(Ctypes.int));
    FIOSETOWN := Ioctl.IOW('f', 124, BYTESIZE(Ctypes.int));
    FIOGETOWN := Ioctl.IOR('f', 123, BYTESIZE(Ctypes.int));

    TIOCGPGRP := Ioctl.IOR('t', 119, BYTESIZE(Ctypes.int));
    TIOCSPGRP := Ioctl.IOW('t', 118, BYTESIZE(Ctypes.int));

  END Init;

BEGIN
  Init();
END IoctlPosix.
