#ifndef __GENVHBAG_CC_
#define __GENVHBAG_CC

#pragma interface

#include "GenList.h"

#include "GenVHBag.h"

template<class T>
GenVHBag<T>::GenVHBag(unsigned int sz = DEFAULT_INITIAL_CAPACITY)
{
#ifndef NO_SETRANGE
  rvm_tid_t tid;

  BeginTransaction(&tid);
  SETRANGE((char *)this, sizeof(*this));
#endif

  memusage = 0;
  // Hacked for small bag size
  //	if(sz >= LARGE)
  sz = sz; // To make the compiler shut up.
  if(0)
    {
      large = 1;
      tab = BEGIN_NEW(DesignLib, T[size = sz])
      END_NEW
      status = BEGIN_NEW(DesignLib, char[size])
      END_NEW
      memusage += sizeof(T)*size + sizeof(char)*size;
#ifndef NO_SETRANGE
      SETRANGE((char *)status, sizeof(char)*size);
#endif
      for (unsigned int i = 0; i < size; ++i) status[i] = EMPTYCELL;
      count = 0;	
    }
  else
    {
      large = 0;
      Tlist = BEGIN_NEW(DesignLib, GenList<T>)
      END_NEW
      memusage += sizeof(GenList<T>);
    }
#ifndef NO_SETRANGE
  Commit(&tid);
#endif
}

template<class T>
GenVHBag<T>::GenVHBag(GenVHBag& a)
{
#ifndef NO_SETRANGE
  rvm_tid_t tid;
  
  BeginTransaction(&tid);
  SETRANGE((char *)this, sizeof(*this));
#endif

  tab = BEGIN_NEW(DesignLib, T[size = a.size])
  END_NEW
  status = BEGIN_NEW(DesignLib, char[size])
  END_NEW
#ifndef NO_SETRANGE
    SETRANGE((char *)status, sizeof(char)*size);
#endif
  for (unsigned int i = 0; i < size; ++i) status[i] = EMPTYCELL;
  count = 0;
  for (Pix p = a.first(); p; a.next(p)) add(a(p));

#ifndef NO_SETRANGE
  Commit(&tid);
#endif

}

template<class T>
GenVHBag<T>::~GenVHBag()
{
  if(large)
    {
      delete [] tab;
      delete status;
    }
  else
    {
      // call destructor on list
    }
}

template<class T>
Pix
GenVHBag<T>::add(T item)
{
#ifndef NO_SETRANGE
  rvm_tid_t tid;
  
  BeginTransaction(&tid);
  SETRANGE((char *)this, sizeof(*this));
#endif

  if(large)
    {
      if (HASHTABLE_TOO_CROWDED(count, size))
	resize();
      unsigned int bestspot = size;
      unsigned int hashval = THASH(item);
      unsigned int h = hashval % size;
      for (unsigned int i = 0; i <= size; ++i)
	{
	  if (status[h] == EMPTYCELL)
	    {
	      if (bestspot >= size) bestspot = h;
#ifndef NO_SETRANGE
	      SETRANGE((char *)tab, sizeof(T)*size);
	      SETRANGE((char *)status, sizeof(char)*size);
#endif
	      tab[bestspot] = item;
	      status[bestspot] = VALIDCELL;
	      ++count;
#ifndef NO_SETRANGE
	      Commit(&tid);
#endif
	      return (Pix)(&tab[bestspot]);
	    }
	  else if (status[h] == DELETEDCELL)
	    {
	      if (bestspot >= size) bestspot = h;
	    }
	  
	  if (i == 0)
	    h = (h + doublehashinc(hashval, size)) % size;
	  else if (++h >= size)
	    h -= size;
	}
#ifndef NO_SETRANGE
	      SETRANGE((char *)tab, sizeof(T)*size);
	      SETRANGE((char *)status, sizeof(char)*size);
#endif
      tab[bestspot] = item;
      status[bestspot] = VALIDCELL;
      ++count;
#ifndef NO_SETRANGE
	      Commit(&tid);
#endif
      return (Pix)(&tab[bestspot]);
    }
  else
    {
#ifndef NO_SETRANGE
	      Commit(&tid);
#endif
      return Tlist->Append(item);
    }
}

template<class T>
int
GenVHBag<T>::del(T key)
{
#ifndef NO_SETRANGE
  rvm_tid_t tid;
  
  BeginTransaction(&tid);
  SETRANGE((char *)this, sizeof(*this));
#endif

  if(large)
    {
      unsigned int hashval = THASH(key);
      unsigned int h = hashval % size;
      for (unsigned int i = 0; i <= size; ++i)
	{
	  if (status[h] == EMPTYCELL)
	    {
#ifndef NO_SETRANGE
	      Commit(&tid);
#endif
	      return 0;
	    }
	  else if (status[h] == VALIDCELL && (key==tab[h]))
	    {
#ifndef NO_SETRANGE
	      SETRANGE((char *)status, sizeof(char)*size);
#endif
	      status[h] = DELETEDCELL;
	      --count;
#ifndef NO_SETRANGE
	      Commit(&tid);
#endif
	      return 0;
	    }
	  if (i == 0)
	    h = (h + doublehashinc(hashval, size)) % size;
	  else if (++h >= size)
	    h -= size;
	}
#ifndef NO_SETRANGE
      Commit(&tid);
#endif
      return 0;
    }
  else
    {
#ifndef NO_SETRANGE
      Commit(&tid);
#endif
      Tlist->Remove(key);
      return 0;
    }
}

template<class T>
int
GenVHBag<T>::remove(T key)
{
#ifndef NO_SETRANGE
  rvm_tid_t tid;
  
  BeginTransaction(&tid);
  SETRANGE((char *)this, sizeof(*this));
#endif

  if(large)
    {
      unsigned int hashval = THASH(key);
      unsigned int h = hashval % size;
      for (unsigned int i = 0; i <= size; ++i)
	{
	  if (status[h] == EMPTYCELL)
	    {
#ifndef NO_SETRANGE
	      Commit(&tid);
#endif
	      return 0;
	    }
	  else if (status[h] == VALIDCELL && (key==tab[h]))
	    {
	      status[h] = DELETEDCELL;
	      --count;
	    }
	  if (i == 0)
	    h = (h + doublehashinc(hashval, size)) % size;
	  else if (++h >= size)
	    h -= size;
	}
#ifndef NO_SETRANGE
      Commit(&tid);
#endif
      return 0;
    }
  else
    {
#ifndef NO_SETRANGE
      Commit(&tid);
#endif
      while(Tlist->Locate(key))
	Tlist->Remove(key);
      return 0;
    }
}

template<class T>
int
GenVHBag<T>::nof(T item)
{
  int n = 0;
  if(large)
    {
      unsigned int hashval = THASH(item);
      unsigned int h = hashval % size;
      unsigned int firsth = size;
      for (unsigned int i = 0; i <= size; ++i)
	{
	  if (status[h] == EMPTYCELL)
	    return n;
	  else if (h != firsth && status[h] == VALIDCELL && (item==tab[h]))
	    {
	      ++n;
	      if (firsth >= size)
		firsth = h;
	    }
	  if (i == 0)
	    h = (h + doublehashinc(hashval, size)) % size;
	  else if (++h >= size)
	    h -= size;
	}
    }
  else
    {
      //ugly but works
      while(Tlist->Locate(item))
	{ n++; Tlist->Remove(item);}
      for(int j=0; j<n;j++)
	Tlist->Append(item);
    }
  return n;
}

template<class T>
int
GenVHBag<T>::contains(T key)
{
  if(large)
    return seek(key) != 0;
  else
    return (Tlist->Locate(key) != 0);
}

template<class T>
void
GenVHBag<T>::clear()
{
#ifndef NO_SETRANGE
  rvm_tid_t tid;
  
  BeginTransaction(&tid);
  SETRANGE((char *)this, sizeof(*this));
#endif

  if(large)
    {
#ifndef NO_SETRANGE
      SETRANGE((char *)status, sizeof(char)*size);
#endif
      for (unsigned int i = 0; i < size; ++i) status[i] = EMPTYCELL;
      count = 0;	
    }
  else
    {
      // remove all items from list
      for(Pix p = Tlist->First(); p ; Tlist->Next(p))
	Tlist->Remove((*Tlist)(p));
    }

#ifndef NO_SETRANGE
  Commit(&tid);
#endif
}

template<class T>
Pix
GenVHBag<T>::first()
{
  if(large)
    {
      for (unsigned int pos = 0; pos < size; ++pos)
	if (status[pos] == VALIDCELL) return (Pix)(&tab[pos]);
      return 0;
    }
  else
    return Tlist->First();
}

template<class T>
int
GenVHBag<T>::next(Pix& i)
{
  if(large)
    {
      //	    if (i == 0) return 0;
      unsigned int pos = ((unsigned)i - (unsigned)tab) / sizeof(T) + 1;
      for (; pos < size; ++pos)
	if (status[pos] == VALIDCELL)
	  {
	    i = (Pix)(&tab[pos]);
	    return 0;
	  }
      i = 0;
      return 0;
    }
  else {
    Tlist->Next(i);
    return 0;
  }
}

template<class T>
T&
GenVHBag<T>::operator() (Pix i)
{
  if(large)
    {
      if (i == 0) printf("null Pix\n");
      return *((T*)i);
    }
  else
    {
      return (*Tlist)(i);
    }
}

template<class T>
Pix
GenVHBag<T>::seek(T key, Pix p = 0)
{
  if(large)
    {
      T* t = (T*) p;
      if (t == 0 || !(*t==key))
	{
	  unsigned int hashval = THASH(key);
	  unsigned int h = hashval % size;
	  for (unsigned int i = 0; i <= size; ++i)
	    {
	      if (status[h] == EMPTYCELL)
		return 0;
	      else if (status[h] == VALIDCELL && (key==tab[h]))
		return (Pix)(&tab[h]);
	      if (i == 0)
		h = (h + doublehashinc(hashval, size)) % size;
	      else if (++h >= size)
		h -= size;
	    }
	  return 0;
	}
      else
	{
	  int seent = 0;
	  unsigned int hashval = THASH(key);
	  unsigned int h = hashval % size;
	  for (unsigned int i = 0; i <= size; ++i)
	    {
	      if (status[h] == EMPTYCELL)
		return 0;
	      else if (&tab[h] == t)
		seent = 1;
	      else if (seent && status[h] == VALIDCELL && (key==tab[h]))
		return (Pix)(&tab[h]);
	      if (i == 0)
		h = (h + doublehashinc(hashval, size)) % size;
	      else if (++h >= size)
		h -= size;
	    }
	  return 0;
	}
    }
  else
    {
      return Tlist->Locate(key);
    }
}

template<class T>
int
GenVHBag<T>::capacity()
{
  if(large)
    return size;
  else
    return (LARGE-1);
}

template<class T>
int
GenVHBag<T>::cardinality()
{
  if(large)
    return count;
  else
    return Tlist->Size();
}

template<class T>
void
GenVHBag<T>::resize(unsigned int newsize = 0)
{
  int i;
#ifndef NO_SETRANGE
  rvm_tid_t tid;
  
  BeginTransaction(&tid);
  SETRANGE((char *)this, sizeof(*this));
#endif

  if (newsize <= count)
    {
      newsize = DEFAULT_INITIAL_CAPACITY;
      while (HASHTABLE_TOO_CROWDED(count, newsize)) newsize <<= 1;
    }
  T* oldtab = tab;
  char* oldstatus = status;
  unsigned int oldsize = size;
  tab = BEGIN_NEW(DesignLib, T[size = newsize])
  END_NEW
  status = BEGIN_NEW(DesignLib, char[size])
  END_NEW
  memusage += sizeof(T)*size + sizeof(char)*size;
#ifndef NO_SETRANGE
  SETRANGE((char *)status, sizeof(char)*size);
#endif
  for (i = 0; i < size; ++i) status[i] = EMPTYCELL;
  count = 0;
  for (i = 0; i < oldsize; ++i) if (oldstatus[i] == VALIDCELL) add(oldtab[i]);
  delete [] oldtab;
  delete oldstatus;

#ifndef NO_SETRANGE
  Commit(&tid);
#endif
}

template<class T>
int
GenVHBag<T>::OK()
{
  int v = tab != 0;
  v &= status != 0;
  int n = 0;
  for (unsigned int i = 0; i < size; ++i) 
    {
      if (status[i] == VALIDCELL) ++n;
      else if (status[i] != DELETEDCELL && status[i] != EMPTYCELL)
	v = 0;
    }
  v &= n == count;
  if (!v) printf("invariant failure\n");
  return v;
}

template<class T>
unsigned int
GenVHBag<T>::MemUsage()
{
  if(large)
    return memusage;
  else
    return (memusage + Tlist->MemUsage());
}

#endif __GENVHBAG_CC
