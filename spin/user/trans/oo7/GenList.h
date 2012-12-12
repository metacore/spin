#ifndef __GENLIST_H_
#define __GENLIST_H_

#pragma interface

#ifndef _Pix__
#define Pix    void *
#endif _Pix__

template<class T> class Element; // forward declaration

template<class T>
class Element {
 public:
    T data;
    Element *next;
    Element();
};

template<class T>
class GenList {
 private:
  int sz;
  Element<T> *front, *rear;
  unsigned int memusage;
 public:
  GenList();
  ~GenList();

  int   cmpitem(T key1, T key2);
  Pix Prepend(T val);
  Pix Append(T val);
  Pix InsertSorted(T val, int (*cmpfunc)(T val1,T val2));
  Pix Locate(T val);
  T *Remove(T val);
  Pix RemoveGetNext(Pix prev);
  void Iterate(void (*pf)(T&,void *), void *arg);
  int Size();
  int IsEmpty();
  Pix First();
  void Next(Pix &i);
  T& operator() (Pix i);
  Pix Nth(int n);
  unsigned int MemUsage();
};

#endif __GENLIST_H_
