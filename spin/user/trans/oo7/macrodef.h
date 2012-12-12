#ifndef __MACRODEF_H_
#define __MACRODEF_H_

/* The code generated will depend on what is defined below
   All dependencies for each system are encapsulated in the files
   <system>def.h and <system>dep.c e.g. All opal dependencies can be 
   found in the files opaldef.h and opaldep.h
 */

//#define OPAL
// #define TEXAS
// #define EXODUS
// #define ODI
#define RVM

#ifdef OPAL 
#include "opaldef.h"
#endif

#ifdef TEXAS
#include "texasdef.h"
#define BEGIN_NEW(x,y) pnew(x,y)
#endif

#ifdef EXODUS 
#include "exodusdef.h"
#define BEGIN_NEW(x,y) new(x)
#endif

#ifdef ODI
#include "odidef.h"
#endif 

#ifdef RVM
#include "rvmdef.h"
#endif 


/* headers that are common to **ALL** systems only, should go in here */
#include "commondef.h"

#endif __MACRODEF_H_
