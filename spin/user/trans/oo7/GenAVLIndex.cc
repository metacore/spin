#ifndef __GENAVLINDEX_CC_
#define __GENAVLINDEX_CC_

#pragma interface

#include <iostream.h>
#include "GenAVLIndex.h"

template<class K, class V>
GenAVLNode<K,V>*
GenAVLIndex<K,V>::leftmost()
{
  GenAVLNode<K,V>* t = root;
  if (t != 0) while (t->lt != 0) t = t->lt;
  return t;
}

template<class K, class V>
GenAVLNode<K,V>*
GenAVLIndex<K,V>::rightmost()
{
  GenAVLNode<K,V>* t = root;
  if (t != 0) while (t->rt != 0) t = t->rt;
  return t;
}

template<class K, class V>
GenAVLNode<K,V>*
GenAVLIndex<K,V>::pred(GenAVLNode<K,V>* t)
{
  GenAVLNode<K,V>* l = t->lt;
  if (!lthread(t)) while (!rthread(l)) l = l->rt;
  return l;
}

template<class K, class V>
GenAVLNode<K,V>*
GenAVLIndex<K,V>::succ(GenAVLNode<K,V>* t)
{
  GenAVLNode<K,V>* r = t->rt;
  if (!rthread(t)) while (!lthread(r)) r = r->lt;
  return r;
}

template<class K, class V>
void
GenAVLIndex<K,V>::_kill(GenAVLNode<K,V>* t)
{
  if (t != 0)
    {
	SETRANGE((char *)t, sizeof(*t));
      if (!lthread(t)) _kill(t->lt);
      if (!rthread(t)) _kill(t->rt);
      delete t;
    }

}

template<class K, class V>
void
GenAVLIndex<K,V>::_add(GenAVLNode<K,V>*& t, GenAVLNode<K,V>*& _found_node,
	     K* _target_item, int& _need_rebalancing)
{
  SETRANGE((char *)this, sizeof(*this));

  int cmp = pf(*_target_item, t->item);
  if (cmp == 0)
    {
      _found_node = t;
      return;
    }
  else if (cmp < 0)
    {
      if (lthread(t))
	{
	  ++count;
	  _found_node = BEGIN_NEW(DesignLib, GenAVLNodeKV)
	    (*_target_item, def)
	  END_NEW
	  set_lthread(_found_node, 1);
	  set_rthread(_found_node, 1);
	  SETRANGE((char *)_found_node, sizeof(*_found_node));
	  SETRANGE((char *)t, sizeof(*t));
	  _found_node->lt = t->lt;
	  _found_node->rt = t;
	  t->lt = _found_node;
	  set_lthread(t, 0);
	  _need_rebalancing = 1;
	}
      else
	_add(t->lt,_found_node, _target_item, _need_rebalancing);
      if (_need_rebalancing)
	{
	  switch(bf(t))
	    {
	    case AVLRIGHTHEAVY:
	      set_bf(t, AVLBALANCED);
	      _need_rebalancing = 0;
	      return;
	    case AVLBALANCED:
	      set_bf(t, AVLLEFTHEAVY);
	      return;
	    case AVLLEFTHEAVY:
	      GenAVLNode<K,V>* l = t->lt;
	      SETRANGE((char *)t, sizeof(*t));
	      SETRANGE((char *)l, sizeof(*l));
	      if (bf(l) == AVLLEFTHEAVY)
		{
		  if (rthread(l))
		    t->lt = l;
		  else
		    t->lt = l->rt;
		  set_lthread(t, rthread(l));
		  l->rt = t;
		  set_rthread(l, 0);
		  set_bf(t, AVLBALANCED);
		  set_bf(l, AVLBALANCED);
		  t = l;
		  _need_rebalancing = 0;
		}
	      else
		{
		  GenAVLNode<K,V>* r = l->rt;

		  SETRANGE((char *)l, sizeof(*l));
		  SETRANGE((char *)r, sizeof(*r));
		  SETRANGE((char *)t, sizeof(*t));

		  set_rthread(l, lthread(r));
		  if (lthread(r))
		    l->rt = r;
		  else
		    l->rt = r->lt;
		  r->lt = l;
		  set_lthread(r, 0);
		  set_lthread(t, rthread(r));
		  if (rthread(r))
		    t->lt = r;
		  else
		    t->lt = r->rt;
		  r->rt = t;
		  set_rthread(r, 0);
		  if (bf(r) == AVLLEFTHEAVY)
		    set_bf(t, AVLRIGHTHEAVY);
		  else
		    set_bf(t, AVLBALANCED);
		  if (bf(r) == AVLRIGHTHEAVY)
		    set_bf(l, AVLLEFTHEAVY);
		  else
		    set_bf(l, AVLBALANCED);
		  set_bf(r, AVLBALANCED);
		  t = r;
		  _need_rebalancing = 0;
		  return;
		}
	    }
	}
    }
  else
    {
      if (rthread(t))
	{
	  ++count;
	  _found_node = BEGIN_NEW(DesignLib, GenAVLNodeKV)
	    (*_target_item, def)
	  END_NEW
	  set_rthread(t, 0);
	  set_lthread(_found_node, 1);
	  set_rthread(_found_node, 1);
	  SETRANGE((char *)_found_node, sizeof(*_found_node));
	  SETRANGE((char *)t, sizeof(*t));
	  _found_node->lt = t;
	  _found_node->rt = t->rt;
	  t->rt = _found_node;
	  _need_rebalancing = 1;
	}
      else
	_add(t->rt,_found_node, _target_item, _need_rebalancing);
      if (_need_rebalancing)
	{
	  switch(bf(t))
	    {
	    case AVLLEFTHEAVY:
	      set_bf(t, AVLBALANCED);
	      _need_rebalancing = 0;
	      return;
	    case AVLBALANCED:
	      set_bf(t, AVLRIGHTHEAVY);
	      return;
	    case AVLRIGHTHEAVY:
	      GenAVLNode<K,V>* r = t->rt;
	      SETRANGE((char *)t, sizeof(*t));
	      SETRANGE((char *)r, sizeof(*r));
	      if (bf(r) == AVLRIGHTHEAVY)
		{
		  if (lthread(r))
		    t->rt = r;
		  else
		    t->rt = r->lt;
		  set_rthread(t, lthread(r));
		  r->lt = t;
		  set_lthread(r, 0);
		  set_bf(t, AVLBALANCED);
		  set_bf(r, AVLBALANCED);
		  t = r;
		  _need_rebalancing = 0;
		}
	      else
		{
		  GenAVLNode<K,V>* l = r->lt;
		  SETRANGE((char *)l, sizeof(*l));
		  set_lthread(r, rthread(l));
		  if (rthread(l))
		    r->lt = l;
		  else
		    r->lt = l->rt;
		  l->rt = r;
		  set_rthread(l, 0);
		  set_rthread(t, lthread(l));
		  if (lthread(l))
		    t->rt = l;
		  else
		    t->rt = l->lt;
		  l->lt = t;
		  set_lthread(l, 0);
		  if (bf(l) == AVLRIGHTHEAVY)
		    set_bf(t, AVLLEFTHEAVY);
		  else
		    set_bf(t, AVLBALANCED);
		  if (bf(l) == AVLLEFTHEAVY)
		    set_bf(r, AVLRIGHTHEAVY);
		  else
		    set_bf(r, AVLBALANCED);
		  set_bf(l, AVLBALANCED);
		  t = l;
		  _need_rebalancing = 0;
		  return;
		}
	    }
	}
    }
}

template<class K, class V>
void
GenAVLIndex<K,V>::_del(GenAVLNode<K,V>* par, GenAVLNode<K,V>*& t, 
	    GenAVLNode<K,V>*& _found_node,
	    K* _target_item, int& _need_rebalancing, int& _already_found)
{
    SETRANGE((char *)this, sizeof(*this));

  int comp;
  if (_already_found)
    {
      if (rthread(t))
	comp = 0;
      else
	comp = 1;
    }
  else 
    comp = pf(*_target_item, t->item);
  if (comp == 0)
    {
      if (lthread(t) && rthread(t))
	{
	  _found_node = t;
	  SETRANGE((char *)par, sizeof(*par));
	  if (t == par->lt)
	    {
	      set_lthread(par, 1);
	      par->lt = t->lt;
	    }
	  else
	    {
	      set_rthread(par, 1);
	      par->rt = t->rt;
	    }
	  _need_rebalancing = 1;
	  return;
	}
      else if (lthread(t))
	{
	  _found_node = t;
	  GenAVLNode<K,V>* s = succ(t);
	  SETRANGE((char *)s, sizeof(*s));
	  SETRANGE((char *)t, sizeof(*t));
	  if (s != 0 && lthread(s))
	    s->lt = t->lt;
	  t = t->rt;
	  _need_rebalancing = 1;
	  return;
	}
      else if (rthread(t))
	{
	  _found_node = t;
	  GenAVLNode<K,V>* p = pred(t);
	  SETRANGE((char *)p, sizeof(*p));
	  SETRANGE((char *)t, sizeof(*t));
	  if (p != 0 && rthread(p))
	    p->rt = t->rt;
	  t = t->lt;
	  _need_rebalancing = 1;
	  return;
	}
      else                        // replace item & find someone deletable
	{
	    SETRANGE((char *)t, sizeof(*t));
	  GenAVLNode<K,V>* p = pred(t);
	  t->item = p->item;
	  t->cont = p->cont;
	  _already_found = 1;
	  comp = -1;                // fall through below to left
	}
    }
  
  if (comp < 0)
    {
      if (lthread(t))
	return;
      _del(t, t->lt,_found_node, _target_item, 
	   _need_rebalancing,_already_found);
      if (!_need_rebalancing)
	return;
      switch (bf(t))
	{
	case AVLLEFTHEAVY:
	  set_bf(t, AVLBALANCED);
	  return;
	case AVLBALANCED:
	  set_bf(t, AVLRIGHTHEAVY);
	  _need_rebalancing = 0;
	  return;
	case AVLRIGHTHEAVY:
	  GenAVLNode<K,V>* r = t->rt;
	  SETRANGE((char *)t, sizeof(*t));
	  SETRANGE((char *)r, sizeof(*r));
	  switch (bf(r))
	    {
	    case AVLBALANCED:
	      if (lthread(r))
		t->rt = r;
	      else
		t->rt = r->lt;
	      set_rthread(t, lthread(r));
	      r->lt = t;
	      set_lthread(r, 0);
	      set_bf(t, AVLRIGHTHEAVY);
	      set_bf(r, AVLLEFTHEAVY);
	      _need_rebalancing = 0;
	      t = r;
	      return;
	    case AVLRIGHTHEAVY:
	      if (lthread(r))
		t->rt = r;
	      else
		t->rt = r->lt;
	      set_rthread(t, lthread(r));
	      r->lt = t;
	      set_lthread(r, 0);
	      set_bf(t, AVLBALANCED);
	      set_bf(r, AVLBALANCED);
	      t = r;
	      return;
	    case AVLLEFTHEAVY:
	      GenAVLNode<K,V>* l = r->lt;
	      SETRANGE((char *)l, sizeof(*l));
	      set_lthread(r, rthread(l));
	      if (rthread(l))
		r->lt = l;
	      else
		r->lt = l->rt;
	      l->rt = r;
	      set_rthread(l, 0);
	      set_rthread(t, lthread(l));
	      if (lthread(l))
		t->rt = l;
	      else
		t->rt = l->lt;
	      l->lt = t;
	      set_lthread(l, 0);
	      if (bf(l) == AVLRIGHTHEAVY)
		set_bf(t, AVLLEFTHEAVY);
	      else
		set_bf(t, AVLBALANCED);
	      if (bf(l) == AVLLEFTHEAVY)
		set_bf(r, AVLRIGHTHEAVY);
	      else
		set_bf(r, AVLBALANCED);
	      set_bf(l, AVLBALANCED);
	      t = l;
	      return;
	    }
	}
    }
  else
    {
      if (rthread(t))
	{
	  return;
	}
      _del(t, t->rt,_found_node, _target_item,
	   _need_rebalancing, _already_found);
      if (!_need_rebalancing)
	{
	  return;
	}
      switch (bf(t))
	{
	case AVLRIGHTHEAVY:
	  set_bf(t, AVLBALANCED);
	  return;
	case AVLBALANCED:
	  set_bf(t, AVLLEFTHEAVY);
	  _need_rebalancing = 0;
	  return;
	case AVLLEFTHEAVY:
	  GenAVLNode<K,V>* l = t->lt;
	  SETRANGE((char *)t, sizeof(*t));
	  SETRANGE((char *)l, sizeof(*l));
	  switch (bf(l))
	    {
	    case AVLBALANCED:
	      if (rthread(l))
		t->lt = l;
	      else
		t->lt = l->rt;
	      set_lthread(t, rthread(l));
	      l->rt = t;
	      set_rthread(l, 0);
	      set_bf(t, AVLLEFTHEAVY);
	      set_bf(l, AVLRIGHTHEAVY);
	      _need_rebalancing = 0;
	      t = l;
	      return;
	    case AVLLEFTHEAVY:
	      if (rthread(l))
		t->lt = l;
	      else
		t->lt = l->rt;
	      set_lthread(t, rthread(l));
	      l->rt = t;
	      set_rthread(l, 0);
	      set_bf(t, AVLBALANCED);
	      set_bf(l, AVLBALANCED);
	      t = l;
	      return;
	    case AVLRIGHTHEAVY:
	      GenAVLNode<K,V>* r = l->rt;
	      SETRANGE((char *)r, sizeof(*r));
	      set_rthread(l, lthread(r));
	      if (lthread(r))
		l->rt = r;
	      else
		l->rt = r->lt;
	      r->lt = l;
	      set_lthread(r, 0);
	      set_lthread(t, rthread(r));
	      if (rthread(r))
		t->lt = r;
	      else
		t->lt = r->rt;
	      r->rt = t;
	      set_rthread(r, 0);
	      if (bf(r) == AVLLEFTHEAVY)
		set_bf(t, AVLRIGHTHEAVY);
	      else
		set_bf(t, AVLBALANCED);
	      if (bf(r) == AVLRIGHTHEAVY)
		set_bf(l, AVLLEFTHEAVY);
	      else
		set_bf(l, AVLBALANCED);
	      set_bf(r, AVLBALANCED);
	      t = r;
	      return;
	    }
	}
    }
}

template<class K, class V>
V&
GenAVLIndex<K,V>::operator[](K& item)
{
    SETRANGE((char *)this, sizeof(*this));

  GenAVLNode<K,V>* _found_node;
  int _need_rebalancing;
  if (root == 0)
    {
      ++count;
      root = BEGIN_NEW(DesignLib, GenAVLNodeKV)(item, def)
      END_NEW
      set_rthread(root, 1);
      set_lthread(root, 1);
      return root->cont;
    }
  else
    {
      K* _target_item = &item;
      _need_rebalancing = 0;
      _add(root,_found_node, _target_item, _need_rebalancing);
      return _found_node->cont;
    }

}

template<class K, class V>
int
GenAVLIndex<K,V>::del(K& item)
{
    SETRANGE((char *)this, sizeof(*this));

  GenAVLNode<K,V>* _found_node;
  K* _target_item;
  int _need_rebalancing;
  int _already_found;
  if (root != 0) // fix this
    {
      _need_rebalancing = 0;
      _already_found = 0;
      _found_node = 0;
      _target_item = &item;
      _del(root, root, _found_node, _target_item, 
	   _need_rebalancing, _already_found);
      if (_found_node)
	{

	    SETRANGE((char *)_found_node, sizeof(*_found_node));
	  delete(_found_node);
	  if (--count == 0)
	    root = 0;
	}
    }
    return 0;
}

template<class K, class V>
Pix
GenAVLIndex<K,V>::first()
{
  return (Pix)(leftmost());
}

template<class K, class V>
void
GenAVLIndex<K,V>::next(Pix& i)
{
  if (i != 0) i = (Pix)(succ((GenAVLNode<K,V>*)i));
}

template<class K, class V>
K&
GenAVLIndex<K,V>::key(Pix i)
{
  if (i == 0) 
    cout << "null Pix";  
  return ((GenAVLNode<K,V>*)i)->item;
}

template<class K, class V>
V&
GenAVLIndex<K,V>::contents(Pix i)
{
  if (i == 0) 
    cout << "null Pix";  
  return ((GenAVLNode<K,V>*)i)->cont;
}

template<class K, class V>
Pix
GenAVLIndex<K,V>::seek(K& key)
{
  GenAVLNode<K,V>* t = root;
  if (t == 0)
    return 0;
  for (;;)
    {
      int cmp = pf(key, t->item);
      if (cmp == 0)
	return (Pix)(t);
      else if (cmp < 0)
	{
	  if (lthread(t))
	    {
	      return 0;
	    }
	  else
	    {
	      t = t->lt;
	    }
	}
      else if (rthread(t))
	return 0;
      else
	{
	  t = t->rt;
	}
    }
}

template<class K, class V>
int
GenAVLIndex<K,V>::contains(K& key)
{
  return seek(key) != 0;
}

template<class K, class V>
void
GenAVLIndex<K,V>::clear()
{
    SETRANGE((char *)this, sizeof(*this));
    _kill(root); count = 0; root = 0;

}

template<class K, class V>
Pix
GenAVLIndex<K,V>::last()
{
  return (Pix)(rightmost());
}

template<class K, class V>
void
GenAVLIndex<K,V>::prev(Pix& i)
{
  if (i != 0) 
    i = (Pix)(pred((GenAVLNode<K,V>*)i));
}

template<class K, class V>
int
GenAVLIndex<K,V>::OK()
{
  int v = 1;
  if (root == 0) 
    v = count == 0;
  else
    {
      int n = 1;
      GenAVLNode<K,V>* trail = leftmost();
      GenAVLNode<K,V>* t = succ(trail);
      while (t != 0)
	{
	  ++n;
	  v &= pf(trail->item, t->item) < 0;
	  trail = t;
	  t = succ(t);
	}
      v &= n == count;
    }
  if (!v) 
    cout << "invariant failure";
  return v;
}


#endif __GENAVLINDEX_CC_
