#include <iostream.h>
#include "macrodef.h"
#include <fstream.h>
extern void *DesignLib;

#include "GenList.h"
#include "GenVHSet.h"
#include "GenVHBag.h"
#include "GenBBag.h"
#include "GenAVLIndex.h"

#include "OO7.h"
#include "BenchParams.h"
#include "GenParams.h"
#include "VarParams.h"

/* number beyond which bags and sets become "large" */
#define THRESHOLD 100

int TotalComplexAssm;

int AssemblyHierarchySize = 0;
int DesignLibSize = 0;
int DBContainersSize = 0;
int TotalDBSize = 0;

#define SMALL 0
#define BIG   1
#define VARIABLE 2

int
sizeOfSet(int ct, int numElements)
{
  if(ct==SMALL)
    {
      return  (3*sizeof(int)+3*sizeof(int *))
	+ (2*sizeof(int)+2*sizeof(int *))
	+ (2*sizeof(int *)*numElements);
    }
  else if(ct==BIG)
    {
      int capacity = 101;
      int wastedSpace = 0;
      while(capacity < numElements)
	{
	  wastedSpace += capacity * 9;
	  capacity = capacity << 1;
	}
      return ( wastedSpace + capacity * 9 + 40);
    }
  else if(ct==VARIABLE)
    {
      if(numElements < THRESHOLD)
	return sizeOfSet(SMALL, numElements);
      else
	return sizeOfSet(BIG, numElements);
    }
  else
    {
      printf("sizeofSet: unknown type passed in\n");
      exit(1);
    }
}

int
sizeOfBag(int ct, int numElements)
{
  if(ct==SMALL)
    {
      return  (4*sizeof(int)+3*sizeof(int *))
	+ (2*sizeof(int)+2*sizeof(int *))
	+ (2*sizeof(int *)*numElements);
    }
  else if(ct==BIG)
    {
      int capacity = 101;
      int wastedSpace = 0;
      while(capacity < numElements)
	{
	  wastedSpace += capacity * 9;
	  capacity = capacity << 1;
	}
      return ( wastedSpace + capacity * 9 + 48);
    }
  else if(ct==VARIABLE)
    {
      if(numElements < THRESHOLD)
	return sizeOfBag(SMALL, numElements);
      else
	return sizeOfBag(BIG, numElements);
    }
  else
    {
      printf("sizeofBag: unknown type passed in\n");
      exit(1);
    }
}

int
sizeOfIndex(int numElements)
{
  return (sizeof(int)+3*sizeof(int *))
    + (5*sizeof(int *))*numElements;
}

int
sizeOfList(int numElements)
{
  return  (2*sizeof(int)+2*sizeof(int *))
    +  (2*sizeof(int *)*numElements);
}

int
sizeOfBBag(int numElements)
{
  return (2*sizeof(int)+sizeof(int *))
    + (sizeof(int *)*numElements);
}

int
numAssm(int numLevels, int branchingFactor)
{
  int i, total = 1;
  if(numLevels==1)
    return 1;
  else
    {
      for(i=0; i< branchingFactor; i++)
	total += numAssm(numLevels-1, branchingFactor);
      return total;
    }
}

int
numComplexAssm(int numLevels, int branchingFactor)
{
  return ( numAssm(numLevels-1,branchingFactor) );
}


int
numBaseAssm(int numLevels, int branchingFactor)
{
  return ( numAssm(numLevels,branchingFactor)
	  - numAssm(numLevels-1,branchingFactor) );
}

void
Compute_Database_Sizes()
{
  int modassmsize, subassmsize, componentssize;
  int partssize, usedinsize, tofromsize;
  int   designlibdbsize,  atomicdbsize , documentdbsize, 
     baseassmdb, moduledb , designlibidindexsize, atomicidindexsize, 
     atomicdateindexsize, documentidindexsize, documenttitleindexsize, 
     baseassmidindexsize, moduleidindexsize,  cpsize ;

  TotalBaseAssm = numBaseAssm(NumAssmLevels,NumAssmPerAssm);
  TotalComplexAssm = numComplexAssm(NumAssmLevels,NumAssmPerAssm) + 1;

  /* --------  Assembly Hierarchy size ---------- */
  printf("--------------------- ASSEMBLY HIERARCHY ------------------\n");
  printf("TotalBaseAssm = %d, TotalComplexAssm = %d\n", 
	 TotalBaseAssm, TotalComplexAssm);
  modassmsize = sizeOfSet(VARIABLE, numAssm(NumAssmLevels,NumAssmPerAssm));
  subassmsize = sizeOfSet(VARIABLE, NumAssmPerAssm);
  componentssize = 2 * sizeOfBag(VARIABLE, NumCompPerAssm);
  printf("sizeof struct Module = %d, sizeof struct Manual = %d\n",
	 sizeof(struct Module), sizeof(struct Manual));
  printf("sizeof struct BaseAssm = %d, sizeof struct ComplexAssm = %d\n",
	 sizeof(struct BaseAssembly), sizeof(struct ComplexAssembly));
  printf("Manual size = %d\n", ManualSize);
  printf("modassmsize = %d , subassmsize = %d , componentssize = %d\n",
	 modassmsize, subassmsize, componentssize);

  AssemblyHierarchySize = TotalModules * 
    (sizeof(struct Module)+ modassmsize +
     sizeof(struct Manual) + ManualSize +
     (  TotalComplexAssm * ( sizeof(struct ComplexAssembly) + subassmsize)) +
     ( TotalBaseAssm * ( sizeof(struct BaseAssembly) + componentssize))
    );
  printf("AssemblyHierarchySize = %d\n",AssemblyHierarchySize );

  /* -------------- Design library size  ------------ */

  printf("------------- DesignLibrary -----------------------\n");
  partssize = sizeOfSet(VARIABLE, NumAtomicPerComp) ;
  usedinsize = 2 * sizeOfBag(VARIABLE, NumCompPerAssm);
  tofromsize =  2 * sizeOfBBag(NumConnPerAtomic);

  printf("sizeof CompositePart = %d, sizeof AtomicPart = %d\n",
	 sizeof(struct CompositePart), sizeof(struct AtomicPart));
  printf("sizeof Connection = %d , sizeof Document = %d\n",
	 sizeof(struct Connection), sizeof(struct Document));
  printf("Documentsize = %d\n", DocumentSize);
  printf("partssize =  %d, usedinsize = %d, tofromsize = %d\n",
	 partssize, usedinsize, tofromsize);
  printf("TotalCompParts = %d, NumAtomicPerComp = %d, NumConnPerAtomic = %d\n",
	 TotalCompParts, NumAtomicPerComp, NumConnPerAtomic);

  DesignLibSize = TotalCompParts *
    ( sizeof(struct CompositePart) + partssize + usedinsize +
     sizeof(struct Document) + DocumentSize +
     NumAtomicPerComp * ( sizeof(struct AtomicPart) + tofromsize) + 
     NumAtomicPerComp * NumConnPerAtomic * sizeof(struct Connection)
    );
  printf("DesignLibSize = %d\n", DesignLibSize);

  printf("------------------ Database Containers ---------------\n");
  designlibdbsize = sizeOfSet(VARIABLE, TotalCompParts);
  atomicdbsize = sizeOfSet(VARIABLE, TotalAtomicParts);
  documentdbsize = sizeOfSet(VARIABLE, TotalCompParts);
  baseassmdb  =      sizeOfSet(VARIABLE, TotalBaseAssm);
  moduledb =   sizeOfSet(VARIABLE, TotalModules) ;
  designlibidindexsize   =  sizeOfIndex(TotalCompParts);
  atomicidindexsize =    sizeOfIndex(TotalAtomicParts);
  atomicdateindexsize =   sizeOfIndex(TotalAtomicParts);
  documentidindexsize =     sizeOfIndex(TotalCompParts);
  documenttitleindexsize =  sizeOfIndex(TotalCompParts);
  baseassmidindexsize =  sizeOfIndex(TotalBaseAssm);
  moduleidindexsize =    sizeOfIndex(TotalModules);
  cpsize =     2 * TotalCompParts * 
    ( sizeof(int *) + sizeOfList(NumCompPerAssm)) ;

  printf(" designlibdbsize = %d, atomicdbsize = %d, documentdbsize = %d\n",
     designlibdbsize,  atomicdbsize, documentdbsize);
  printf(" baseassmdb = %d , moduledb = %d, designlibidindexsize = %d\n",
     baseassmdb, moduledb, designlibidindexsize);
  printf("atomidindsize = %d, atomdateindsize = %d,documentidindsize = %d\n",
     atomicidindexsize, atomicdateindexsize, documentidindexsize );
  printf("doctitleindsz = %d, baidindsz = %d, modidindsz = %d, cpsz = %d\n",
     documenttitleindexsize, baseassmidindexsize, moduleidindexsize, cpsize);

   DBContainersSize = 
     designlibdbsize +
     atomicdbsize +
     documentdbsize +
     baseassmdb +
     moduledb +
     designlibidindexsize +
     atomicidindexsize +
     atomicdateindexsize +
     documentidindexsize +
     documenttitleindexsize +
     baseassmidindexsize +
     moduleidindexsize +
     cpsize ;

  printf("DBContainersSize = %d\n", DBContainersSize);

  printf("----------------------------------------------------------\n");
  TotalDBSize = AssemblyHierarchySize + DesignLibSize + DBContainersSize;
  printf("TotalDBSize *WITHOUT* struct mhead overhead = %d\n", TotalDBSize);
}
