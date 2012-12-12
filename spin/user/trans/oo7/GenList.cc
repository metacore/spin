#ifndef __GENLIST_CC_
#define __GENLIST_CC_

#pragma interface

#include <iostream.h>
#include "GenList.h"

template<class T>
Element<T>::Element()
{
  SETRANGE((char *)this, sizeof(*this));
}

template<class T>
GenList<T>::GenList()
{
  SETRANGE((char *)this, sizeof(*this));

  sz = 0; front = rear = 0; memusage = 0  ;
}

template<class T>
GenList<T>::~GenList()
{
  // delete entire list
  // if data is a pointer, then all that may become garbage
  // to clean that up call Iterate on list with a cleanup function
  // before calling destructor
  Element<T> *temp, *prev;
  for(temp=front,prev=front;temp;)
    {
      temp = temp->next;
      delete prev;
      prev = temp;
    }
}

template<class T>
int
GenList<T>::cmpitem(T key1, T key2)
{
  return (key1 == key2);
}


template<class T>
Pix
GenList<T>::Prepend(T val)
{
  SETRANGE((char *)this, sizeof(*this));

  // insert val at front of list; O(1) operation
  Element<T> *temp;
  if(sz==0) { 
    front = rear = BEGIN_NEW(DesignLib, Element<T>)
    END_NEW
    SETRANGE((char *)front, sizeof(*front));
    front->data = val; front->next = 0;
    memusage += sizeof(Element<T>);
  }
  else
    {
      temp = front; 
      front = BEGIN_NEW(DesignLib, Element<T>)
      END_NEW
    SETRANGE((char *)front, sizeof(*front));
      front->data = val; front->next = temp;
      memusage += sizeof(Element<T>);
    }
  sz++;

  return (Pix)(&front->data);
}

template<class T>
Pix
GenList<T>::Append(T val)
{
  SETRANGE((char *)this, sizeof(*this));

  // add val to end of list;   O(1) operation                   
  if(sz==0) { 
    front = rear = BEGIN_NEW(DesignLib, Element<T>)
    END_NEW
    SETRANGE((char *)rear, sizeof(*rear));
    rear->data = val; rear->next = 0;
    memusage += sizeof(Element<T>);
  }
  else{
    rear->next = BEGIN_NEW(DesignLib, Element<T>)
    END_NEW
    rear = rear->next;
    SETRANGE((char *)rear, sizeof(*rear));
    rear->data = val; rear->next = 0;
    memusage += sizeof(Element<T>);
  }
  sz++;

  return (Pix) (&rear->data);
}

template<class T>
Pix
GenList<T>::InsertSorted(T val, int (*cmpfunc)(T, T))
{

  SETRANGE((char *)this, sizeof(*this));

  Element<T> *temp, *prev, *e;
  // add val to end of list;   O(1) operation                   
  if(sz==0) 
    { 
      front = rear = BEGIN_NEW(DesignLib, Element<T>)
      END_NEW

    SETRANGE((char *)rear, sizeof(*rear));

      rear->data = val; rear->next = 0;
      memusage += sizeof(Element<T>);
      sz++;
      return (Pix) (&front->data);
    }
  else
    {
      for(prev=0,temp=front;temp;prev=temp,temp=temp->next)
	{
	  // insert if an element is found which is "bigger than" val
	  if(cmpfunc(val,temp->data)<0)
	    {
	      if(temp==front)
		{
		  front = BEGIN_NEW(DesignLib, Element<T>)
		  END_NEW
		    SETRANGE((char *)front, sizeof(*front));

		  front->data = val; front->next = temp; e = front;
		}
	      else
		{

		    SETRANGE((char *)prev, sizeof(*prev));

		  prev->next = BEGIN_NEW(DesignLib, Element<T>)
		  END_NEW

		    SETRANGE((char *)prev->next, sizeof(*(prev->next)));

		  prev->next->data = val; prev->next->next = temp; 
		  e = prev->next;
		}
	      sz++; memusage += sizeof(Element<T>);
	      return (Pix) (&e->data);
	    }
	}
      // the new element goes to the end of the list

    SETRANGE((char *)rear, sizeof(*rear));

      rear->next = BEGIN_NEW(DesignLib, Element<T>)
      END_NEW
      rear = rear->next;

    SETRANGE((char *)rear, sizeof(*rear));

      rear->data = val; rear->next = 0;
      memusage += sizeof(Element<T>);
      sz++;
      return (Pix) (&rear->data);
    }
}

template<class T>
Pix
GenList<T>::Locate(T val)
{
  // return Pix if data found based on comparison function
  // O(N) operation;
  Element<T> *temp;
  for(temp=front;temp;temp=temp->next)
    {
      if(cmpitem(val,temp->data))
	return (Pix) (&temp->data); 
    }
  return 0; // not found
}

template<class T>
T *
GenList<T>::Remove(T val)
{
  SETRANGE((char *)this, sizeof(*this));

  // remove element from list if found 
  //and return pointer to data; O(N) operation
  Element<T> *temp, *prev=0;
  T *pdata;
  for(temp=front;temp;temp=temp->next)
    {
      if(cmpitem(val,temp->data))
	{
	  if(sz==1) {front = rear = 0;}
	  else if(temp==front) {front = temp->next;}
	  else if(temp==rear) {rear = prev;}
	  else 
	    {

	      SETRANGE((char *)prev, sizeof(*prev));

	      prev->next = temp->next;
	    }
	  sz--;  pdata = &(temp->data); delete temp; 
	  return pdata;
	}
      prev = temp;
    }


  return 0; // not found
}

template<class T>
Pix
GenList<T>::RemoveGetNext(Pix prev)
{
  SETRANGE((char *)this, sizeof(*this));

  // remove element from list given a Pix to the prev element
  // and return Pix to the next element
  Element<T> *temp;
  if(prev==0)
    { // special case -- remove first element
      if(sz==0) 
	{
	  return (Pix) 0;
	}
      if(sz==1) 
	{
	  delete front; sz--; front = rear = 0; 
	  return (Pix)0; 
	}
      else 
	{
	  temp = front; front = front->next; sz--; 
	  return (Pix)front;
	}
    }
  else if(((Element<T> *)prev)->next==rear)
    { // remove last element
      { 
	delete rear; sz--; rear = (Element<T> *)prev; 

	SETRANGE((char *)rear, sizeof(*rear));

	rear->next = 0; 
	return (Pix)0;
      }
    }
  else
    { // normal case. neither front nor rear affected.
      temp = ((Element<T> *)prev)->next; // element to remove
      SETRANGE((char *)prev, sizeof(*prev));
      ((Element<T> *)prev)->next =  ((Element<T> *)prev)->next->next;
      sz--; delete temp;
      return (Pix)((Element<T> *)prev)->next;
    }
}

template<class T>
void
GenList<T>::Iterate(void (*pf)(T&,void *), void *arg)
{
  // apply pf on each element of list, passing in argument arg
  Element<T> *temp;
  for(temp=front;temp;temp=temp->next)
    pf(temp->data, arg); // passed by reference; so data can be modified
}

template<class T>
int
GenList<T>::Size()
{
  // return size of list
  return sz;
}

template<class T>
int
GenList<T>::IsEmpty()
{
  // returns 0 if not empty 1 if empty
  return (sz==0);
}

template<class T>
Pix
GenList<T>::First()
{
  // returns a Pix to the "first" element if it exists, 0 otherwise
  if(front)
    return (Pix) (&front->data);
  else
    return 0;
}

template<class T>
void
GenList<T>::Next(Pix &i)
{
  // retuns Pix to the next element in the list if it exists, 0 otherwise
  // hack hack!! we know that i is really a ptr to the Element
  if(((Element<T> *)i)->next)
    {
      i =  (Pix)( &((Element<T> *)i)->next->data);
    }
  else
    i = 0;
}

template<class T>
Pix
GenList<T>::Nth(int n)
{
  // retuns Pix to the nth element in the list if it exists, 0 otherwise
  int i=1;
  Element<T> *temp;
  // check to see if n is valid.
  if(n<1 || n>sz)
    return (Pix)0;
  else
    {
      for(temp=front;temp;temp=temp->next,i++)
	{
	  if(i==n)
	    return (Pix)temp;
	}
    }
  // not reached
}

template<class T>
T&
GenList<T>::operator()(Pix i)
{
  if (i == 0) 
    cout << "null Pix:" << endl;
  return ((Element<T> *)i)->data;
}

template<class T>
unsigned int
GenList<T>::MemUsage()
{
  return memusage;
}

#endif __GENLIST_CC_
