#ifndef __GENBBAG_H_
#define __GENBBAG_H_

#pragma interface

/*******************************************************************/
/*       Look at the file GenBBag.README for more information     */
/*       on how to use this package.                               */
/*******************************************************************/
#ifndef _Pix__
#define Pix    void *
#endif _Pix__

#define DEFAULT_BBAG_SIZE 9

template<class T>
class GenBBag 
{
private:
  T*  tab;            // an array containing set values
  int size;          // max size of table
  int index;         // index of first free element in the array
                     // the array contains 'index' valid values
public:
  GenBBag(int sz = DEFAULT_BBAG_SIZE);
  GenBBag(GenBBag& a);
  ~GenBBag();
  Pix  add(T  item);
  int del(T  key);
  int remove(T key);
  int nof(T  item);
  int contains(T  key);
  void clear();
  Pix  first();
  int next(Pix& i);
  T& operator () (Pix i);
  Pix  seek(T  key);
  int capacity();
  int cardinality();
};

#endif __GENBBAG_H_
