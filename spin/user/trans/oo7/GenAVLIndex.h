#ifndef __GENAVLINDEX_H_
#define __GENAVLINDEX_H_

#pragma interface

#ifndef _Pix__
#define Pix    void *
#endif _Pix__

#define AVLBALANCEMASK    3
#define AVLBALANCED       0
#define AVLLEFTHEAVY      1
#define AVLRIGHTHEAVY     2

#define LTHREADBIT        4
#define RTHREADBIT        8

#define CMP(a,b)  ( (a)==(b) ) ? 0 : ( (a) < (b) ? -1 : 1)

#define GenAVLNodeKV GenAVLNode<K,V>

template<class K, class V>
class GenIndex
{
protected:
  int count;
  V   def;
  
public:
  GenIndex(V  dflt) :def(dflt) 
    {
#ifndef NO_SETRANGE
  rvm_tid_t tid;

  BeginTransaction(&tid);
  SetRange(&tid, (char *)this, sizeof(*this));
#endif
      count = 0;
#ifndef NO_SETRANGE
  Commit(&tid);
#endif
    }
  virtual ~GenIndex() {};
  
  int length(){ return count;} // current number of items
  int empty() {return count==0;}
  
  virtual int           contains(K& key)=0;      // is key mapped?
  
  virtual void          clear()=0;                 // delete all items
  
  virtual V&          operator [] (K& key) = 0; // access contents by key

  virtual int          del(K& key) = 0;       // delete entry
  
  virtual Pix           first() = 0;             // Pix of first item or 0
  virtual void          next(Pix& i) = 0;        // advance to next or 0
  virtual K&          key(Pix i) = 0;          // access key at i
  virtual V&          contents(Pix i) = 0;     // access contents at i
  
//  virtual int           owns(Pix i);             // is i a valid Pix  ?
  virtual Pix           seek(K& key)=0;          // Pix of key
  
  V&  dflt() { return def;}                  // access default val
  
  virtual int           OK() = 0;                // rep invariant
};

template<class K, class V>
class GenAVLNode
{
public:
  K   item;
  V   cont;
  GenAVLNode<K,V>* lt;
  GenAVLNode<K,V>* rt;
  char   stat;
  GenAVLNode(K& h, V  c, GenAVLNode<K,V>* l=0, GenAVLNode<K,V>* r=0)
    :item(h), cont(c), lt(l), rt(r), stat(0) 
      {
#ifndef NO_SETRANGE
  rvm_tid_t tid;

  BeginTransaction(&tid);
  SetRange(&tid, (char *)this, sizeof(*this));
#endif
#ifndef NO_SETRANGE
  Commit(&tid);
#endif
      }
  ~GenAVLNode(){}
};

template<class K, class V>
class GenAVLIndex : public GenIndex<K,V>
{
protected:
  GenAVLNode<K,V>*   root;
  int (*pf)(K,K);
  GenAVLNode<K,V>*   leftmost();
  GenAVLNode<K,V>*   rightmost();
  GenAVLNode<K,V>*   pred(GenAVLNode<K,V>* t);
  GenAVLNode<K,V>*   succ(GenAVLNode<K,V>* t);
  void  _kill(GenAVLNode<K,V>* t);
  void  _add(GenAVLNode<K,V>*& t, GenAVLNode<K,V>*& _found_node,
	     K* _target_item, int& _need_rebalancing);
  void _del(GenAVLNode<K,V>* par, GenAVLNode<K,V>*& t, 
	    GenAVLNode<K,V>*& _found_node,
	    K* _target_item, int& _need_rebalancing, int& _already_found);
public:
  GenAVLIndex(V  dflt, int (*cmpfunc)(K,K)) :GenIndex<K,V>(dflt) 
    {
#ifndef NO_SETRANGE
  rvm_tid_t tid;

  BeginTransaction(&tid);
  SetRange(&tid, (char *)this, sizeof(*this));
#endif
      root = 0; pf = cmpfunc;
#ifndef NO_SETRANGE
  Commit(&tid);
#endif
    }
  GenAVLIndex(GenAVLIndex& b) :GenIndex<K,V>(0)
    { 
#ifndef NO_SETRANGE
  rvm_tid_t tid;

  BeginTransaction(&tid);
  SetRange(&tid, (char *)this, sizeof(*this));
#endif
  root = 0;
  count = 0;
  for (Pix i = b.first(); i != 0; b.next(i)) 
    (*this)[b.key(i)] = b.contents(i);
#ifndef NO_SETRANGE
  Commit(&tid);
#endif
    }
  ~GenAVLIndex(){  _kill(root);}
  static inline int bf(GenAVLNode<K,V>* t)
    {
      return t->stat & AVLBALANCEMASK;
    }
  static inline void set_bf(GenAVLNode<K,V>* t, int b)
    {
#ifndef NO_SETRANGE
      rvm_tid_t tid;
      BeginTransaction(&tid);
      SetRange(&tid, (char *)t, sizeof(*t));
#endif
      t->stat = (t->stat & ~AVLBALANCEMASK) | (b & AVLBALANCEMASK);
#ifndef NO_SETRANGE
  Commit(&tid);
#endif

    }
  static inline int rthread(GenAVLNode<K,V>* t)
    {
      return t->stat & RTHREADBIT;
    }
  static inline void set_rthread(GenAVLNode<K,V>* t, int b)
    {
#ifndef NO_SETRANGE
      rvm_tid_t tid;
      BeginTransaction(&tid);
      SetRange(&tid, (char *)t, sizeof(*t));
#endif
      if (b)
	t->stat |= RTHREADBIT;
      else
	t->stat &= ~RTHREADBIT;
#ifndef NO_SETRANGE
  Commit(&tid);
#endif
    }
  static inline int lthread(GenAVLNode<K,V>* t)
    {
      return t->stat & LTHREADBIT;
    }
  static inline void set_lthread(GenAVLNode<K,V>* t, int b)
    {
#ifndef NO_SETRANGE
      rvm_tid_t tid;
      BeginTransaction(&tid);
      SetRange(&tid, (char *)t, sizeof(*t));
#endif
      if (b)
	t->stat |= LTHREADBIT;
      else
	t->stat &= ~LTHREADBIT;
#ifndef NO_SETRANGE
  Commit(&tid);
#endif
    }
  V& operator [] (K& item);
  int  del(K& item);
  Pix first();
  void next(Pix& i);
  K& key(Pix i);
  V&  contents(Pix i);
  Pix seek(K& key);
  int contains(K& key);
  void clear();
  Pix  last();
  void prev(Pix& i);
  int OK();
};




#endif __GENAVLINDEX_H_
