#ifndef __GENVHBAG_H_
#define __GENVHBAG_H_

#pragma interface

#include "GenList.h"

#ifndef _Pix__
#define Pix    void *
#endif _Pix__

/* codes for status fields */
#define EMPTYCELL   0
#define VALIDCELL   1
#define DELETEDCELL 2

#define DEFAULT_INITIAL_CAPACITY 101
#define HASHTABLE_TOO_CROWDED(COUNT, SIZE) ((SIZE) - ((SIZE) >> 3) <= (COUNT))

// for now just use the modulo function for primary hash values
#define THASH(key)  (unsigned int) key

// This bag uses simple linked lists for bags that are small
// and hash tables for LARGE (>100 elements) bags.
// Once a bag has been classified as small, it's implementation will be
// a list forever (even if it grows beyond 100). So, if you think that 
// the bag can become large, you should initialize it to a value >= LARGE
// for the purposes of efficiency.
// If you don't pass in any arguments to the constructor it will create 
// a large bag.

#define LARGE 100

template<class T>
class GenVHBag
{
private:
  int  large; // 1 if bag is LARGE
  GenList<T> *Tlist; // used only if bag is SMALL
  T*  tab;            // a table (array) containing bag values
  char*  status;      // indicates if table entry is empt/valid/deleted
  unsigned int  size; // current size of table
  unsigned int count; // current number of elements in the bag
  unsigned int memusage; // total number of bytes malloced
  /* 
     * hashing method: double hash based on high bits of hash fct,
     * followed by linear probe. Can't do too much better if table
     * sizes not constrained to be prime.
     */
    static inline unsigned int doublehashinc(unsigned int h, unsigned int s)
      {
	unsigned int dh =  ((h / s) % s);
	return (dh > 1)? dh : 1;
      }
public:
  GenVHBag(unsigned int sz = DEFAULT_INITIAL_CAPACITY);
  GenVHBag(GenVHBag& a);  // WARNING!!! This should be used only for large bags
  ~GenVHBag();
  Pix  add(T  item);
  int del(T  key);
  int remove(T key);
  int nof(T  item);
  int contains(T  key);
  void clear();
  Pix  first();
  int next(Pix& i);
  T& operator () (Pix i);
  Pix  seek(T  key, Pix p = 0);
  int capacity();
  int cardinality();
  void resize(unsigned int newsize = 0);  // used only by LARGE bags
  int OK(); // used only by LARGE bags
  unsigned int  MemUsage();
};

#endif __GENVHBAG_H_
