#ifndef __GENBBAG_CC_
#define __GENBBAG_CC_

#pragma interface

#include "GenBBag.h"


template<class T>
GenBBag<T>::GenBBag(int sz = DEFAULT_BBAG_SIZE)
{
  SETRANGE((char *)this, sizeof(*this));

  tab = BEGIN_NEW(DesignLib,T[size = sz])
  END_NEW
  index = 0;
}


template<class T>
GenBBag<T>::GenBBag(GenBBag& a)
{
  SETRANGE((char *)this, sizeof(*this));

  tab = BEGIN_NEW(DesignLib,T[size = a.size])
  END_NEW
  for (Pix p = a.first(); p; a.next(p)) add(a(p));

}

template<class T>
GenBBag<T>::~GenBBag()
{
  delete [] tab;
}

template<class T>
Pix
GenBBag<T>::add(T item)
{
  SETRANGE((char *)this, sizeof(*this));

  if(index >= size)
    {
      printf("Bag is full: resizing bag\n");
      int temp = index;
      size = 2*size;
      T *newtab = BEGIN_NEW(DesignLib, T[size])
      END_NEW
	SETRANGE((char *)newtab, sizeof(T)*size);
      for(int i = 0; i < index; i++) newtab[i] = tab[i];
      // delete tab;     
      // free up old space
      tab = newtab;
      SETRANGE((char *)tab, sizeof(T)*size);
      tab[index++] = item;
      return (Pix)(&tab[temp]);
    }
  else
    {
      int temp = index;
      SETRANGE((char *)tab, sizeof(T)*size);
      tab[index++] = item;
      return (Pix) (&tab[temp]);
    }
}

template<class T>
int
GenBBag<T>::del(T key)
{
  SETRANGE((char *)this, sizeof(*this));

  if(index <= 0)
    printf("cannot delete from empty Bag\n");
  else
    {
      SETRANGE((char *)tab, sizeof(T)*size);
      for(int i = 0; i < index; i++)
	if(tab[i]==key)
	  {
	    // found element -- delete it and compact the remaining
	    for(int j=i;j<index-1;j++)
	      tab[j] = tab[j+1];
	    index--;
	    return 0;
	  }
      printf("BBag del: element not found\n");
    }

  return 0;
}

template<class T>
int
GenBBag<T>::remove(T key)
{
  SETRANGE((char *)this, sizeof(*this));

  // remove all occurrences of key from bag
  if(index <= 0)
    printf("cannot delete from empty Bag\n");
  else
    {
      SETRANGE((char *)tab, sizeof(T)*size);
      for(int i = 0; i < index; i++)
	if(tab[i]==key)
	  {
	    // found element -- delete it and compact the remaining
	    for(int j=i;j<index-1;j++)
	      tab[j] = tab[j+1];
	    index--;
	  }
      printf("BBag remove: element not found\n");
    }

  return 0;
}

template<class T>
int
GenBBag<T>::nof(T item)
{
  int count = 0;
  for(int i = 0; i < index; i++)
    if(tab[i]==item)
      count++;
  return count;
}

template<class T>
int
GenBBag<T>::contains(T key)
{
  return seek(key) != 0;
}

template<class T>
void
GenBBag<T>::clear()
{
  for (int i = 0; i < size; ++i)
    tab[i] = 0;
  index = 0;
}

template<class T>
Pix
GenBBag<T>::first()
{
  if(index>0)
    return (Pix)(&tab[0]);
  else
    return 0;
}

template<class T>
int
GenBBag<T>::next(Pix &i)
{
  int pos;
  if (i != 0) 
    {
      pos = ((unsigned)i - (unsigned)tab) / sizeof(T) + 1;
      if(pos < index)
	{
	  i =  (Pix)(&tab[pos]);
	}
      else
	i = 0;
    }
  return 0;
}

template<class T>
T&
GenBBag<T>::operator() (Pix i)
{
  if (i == 0) printf("null Pix\n");
  return *((T*)i);
}

template<class T>
Pix
GenBBag<T>::seek(T key)
{
  for(int i =0; i< index; i++)
    if(tab[i]==key)
      return (Pix) (&tab[i]);
  return 0;
}

template<class T>
int
GenBBag<T>::capacity()
{
  return size;
}

template<class T>
int
GenBBag<T>::cardinality()
{
  return index;
}

#endif __GENBBAG_CC_
