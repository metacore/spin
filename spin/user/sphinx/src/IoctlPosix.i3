(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 06-Jul-96  Yasushi Saito (yasushi) at the University of Washington
 *	Added FIOASYNC and others.
 * 11-Jun-96 oystr at the University of Washington
 *	Added [O]SIOCGIFCONF, FIONBIO.
 *
 *)

INTERFACE IoctlPosix;
IMPORT Ioctl;

VAR
  (* BSD4.4 sockaddr format accepted and returned - also see sys/socket.h *)
  (* #define OSIOCGIFADDR    _IOWR('i',13, struct ifreq)     /* get ifnet address */ *)
  OSIOCGIFADDR : Ioctl.T;

  (* #define SIOCGIFADDR     _IOWR('i',33, struct ifreq)     /* get ifnet address */ *)
  SIOCGIFADDR : Ioctl.T;

  (* #define OSIOCGIFDSTADDR _IOWR('i',15, struct ifreq)     /* get p-p address */ *)
  OSIOCGIFDSTADDR : Ioctl.T;

  (* #define SIOCGIFDSTADDR  _IOWR('i',34, struct ifreq)     /* get p-p address */ *)
  SIOCGIFDSTADDR : Ioctl.T;

  (* #define OSIOCGIFBRDADDR _IOWR('i',18, struct ifreq)     /* get broadcast addr */ *)
  OSIOCGIFBRDADDR : Ioctl.T;

  (* #define SIOCGIFBRDADDR  _IOWR('i',35, struct ifreq)     /* get broadcast addr */ *)
  SIOCGIFBRDADDR : Ioctl.T;

  (* #define OSIOCGIFCONF _IOWR('i',20, struct ifconf) /* get ifnet list */ *)
  OSIOCGIFCONF : Ioctl.T;

  (* #define SIOCGIFCONF _IOWR('i',36, struct ifconf) /* get ifnet list */ *)
  SIOCGIFCONF  : Ioctl.T;

  (* #define OSIOCGIFNETMASK _IOWR('i',21, struct ifreq)     /* get net addr mask */ *)
  OSIOCGIFNETMASK : Ioctl.T;

  (* #define SIOCGIFNETMASK  _IOWR('i',37, struct ifreq)     /* get net addr mask */ *)
  SIOCGIFNETMASK : Ioctl.T;

  (* #define SIOCSIFADDR     _IOW('i', 12, struct ifreq)     /* set ifnet address */ *)
  SIOCSIFADDR : Ioctl.T;

  (* #define SIOCSIFDSTADDR  _IOW('i', 14, struct ifreq)     /* set p-p address */ *)
  SIOCSIFDSTADDR : Ioctl.T;

  (* #define SIOCSIFFLAGS    _IOW('i', 16, struct ifreq)     /* set ifnet flags */ *)
  SIOCSIFFLAGS : Ioctl.T;

  (* #define SIOCGIFFLAGS    _IOWR('i',17, struct ifreq)     /* get ifnet flags */ *)
  SIOCGIFFLAGS : Ioctl.T;

  (* #define SIOCSIFBRDADDR  _IOW('i', 19, struct ifreq)     /* set broadcast addr */ *)
  SIOCSIFBRDADDR : Ioctl.T;

  (* #define SIOCSIFNETMASK  _IOW('i', 22, struct ifreq)     /* set net addr mask */ *)
  SIOCSIFNETMASK : Ioctl.T;

  (* #define SIOCGIFMETRIC   _IOWR('i',23, struct ifreq)     /* get IF metric */ *)
  SIOCGIFMETRIC : Ioctl.T;

  (* #define SIOCSIFMETRIC   _IOW('i', 24, struct ifreq)     /* set IF metric */ *)
  SIOCSIFMETRIC : Ioctl.T;

  (* #define SIOCDIFADDR     _IOW('i', 25, struct ifreq)     /* delete IF addr */ *)
  SIOCDIFADDR : Ioctl.T;

  (* #define SIOCAIFADDR     _IOW('i', 26, struct ifaliasreq)/* add/chg IF alias */ *)
  SIOCAIFADDR : Ioctl.T;

  (* #define SIOCPIFADDR     _IOW('i', 29, struct ifaliasreq)/* add/chg IF alias */ *)
  SIOCPIFADDR : Ioctl.T;

  SIOCARPREQ : Ioctl.T; (* #define SIOCARPREQ      _IOWR('i',40, struct ifreq)     /* arp request pkt */ *)

  (* #define SIOCIFRESET     _IOW('i',47, struct ifreq)      /* Reset interface */ *)
  SIOCIFRESET : Ioctl.T;

  (* #define SIOCADDMULTI    _IOW('i', 49, struct ifreq)     /* add m'cast addr */ *)
  SIOCADDMULTI : Ioctl.T;

  (* #define SIOCDELMULTI    _IOW('i', 50, struct ifreq)     /* del m'cast addr */ *)
  SIOCDELMULTI : Ioctl.T;

  (* #define SIOCENABLBACK   _IOW('i', 60, struct ifreq)     /* Enable loopback */ *)
  SIOCENABLBACK : Ioctl.T;

  (* #define SIOCDISABLBACK  _IOW('i', 61, struct ifreq)     /* Disable loopback */ *)
  SIOCDISABLBACK : Ioctl.T;

  (* #define SIOCRPHYSADDR        _IOWR('i', 62, struct ifdevea)  /* Read Phys addr */ *)
  SIOCRPHYSADDR : Ioctl.T;

  (* #define SIOCSPHYSADDR   _IOWR('i', 63, struct ifdevea)  /* Set addr */*)
  SIOCSPHYSADDR : Ioctl.T;

  (* #define SIOCIFSETCHAR   _IOWR('i', 64, struct ifchar)   /* Set characteristic */ *)
  SIOCIFSETCHAR : Ioctl.T;

  (* #define SIOCSMACSPEED   _IOW('i', 65, struct ifreq)     /* Set MAC speed */ *)
  SIOCSMACSPEED : Ioctl.T;

  (* #define SIOCRMACSPEED   _IOWR('i', 66, struct ifreq)    /* Read MAC speed */ *)
  SIOCRMACSPEED : Ioctl.T;

  (* #define SIOCSIPMTU      _IOW('i', 67, struct ifreq)     /* Set intf. IP MTU */ *)
  SIOCSIPMTU : Ioctl.T;

  (* #define SIOCRIPMTU   _IOWR('i', 68, struct ifreq)    /* Read intf. IP MTU */ *)
  SIOCRIPMTU : Ioctl.T;


  FIONREAD : Ioctl.T;	(*_IOR('f', 127, int)   get number of bytes to read *)
  FIONBIO : Ioctl.T;	(*_IOW('f', 126, int)	set/clear non-blocking i/o *)
  FIOASYNC : Ioctl.T;	(*_IOW('f', 125, int)	set/clear async i/o *)
  FIOSETOWN : Ioctl.T;(*_IOW('f', 124, int)	set owner *)
  FIOGETOWN : Ioctl.T;(*_IOR('f', 123, int)	get owner *)

  TIOCGPGRP : Ioctl.T;(*_IOR('t', 119, pid_t)	get pgrp of tty *)
  TIOCSPGRP : Ioctl.T;(*_IOW('t', 118, pid_t)	set pgrp of tty *)


PROCEDURE Init();

END IoctlPosix.
