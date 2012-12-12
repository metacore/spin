#ifndef _OO7_H_
#define _OO7_H_

#define TypeSize	10
#define TitleSize	40
#define DummySize	1000

extern const int FALSe;
extern const int TRUe;

typedef enum {Complex, Base} AssemblyType;

typedef enum { Trav1, Trav1WW, Trav2a, Trav2b, Trav2c, Trav3a, Trav3b,
               Trav3c, Trav4, Trav5do, Trav5undo, Trav6, Trav7, Trav8, 
               Trav9, Trav10, Query1, Query1WW, Query2, Query3, Query4,
	       Query5, Query6, Query7, Query8, Insert, Delete, Reorg1, Reorg2,
	       WarmUpdate, MultiTrav1, MultiTrav2, MultiTrav3, MultiTrav4, 
	       MultiTrav5, MultiTrav6 } BenchmarkOp;

typedef enum {UpdateOne, UpdateAll, UpdateRepeat} UpdateType;

typedef enum {UpdateDirectionDo, UpdateDirectionUndo} UpdateDirectionType;


#include "PartIdSet.h"
// #include "GenList.h"
//--------------------------------------------------------------------------
// AtomicPart objects are the primitives for building up designs
//	- modeled after the Sun/OO1 benchmark's parts
//--------------------------------------------------------------------------

class Connection;
class CompositePart;

class AtomicPart {
public:
  int  			id ;
  char			type[TypeSize];
  int  			buildDate;
  int  			x, y;
  int                         docId;
  GenBBag<Connection*>	*to;
  GenBBag<Connection*>	*from;
  CompositePart*		partOf;
  AtomicPart(int ptId);
  ~AtomicPart();
  void AddToConnection(Connection *);
  void AddFromConnection(Connection *);
  void swapXY();
  void toggleDate();
  int traverse(BenchmarkOp op, PartIdSet& visitedIds);
  void DoNothing();
};


//--------------------------------------------------------------------------
// Connection objects are used to wire AtomicParts together
//	- similarly, modeled after Sun/OO1 connections
//--------------------------------------------------------------------------

class Connection {
public:
    char			type[TypeSize];
    int  			length;
    AtomicPart*			from;
    AtomicPart*			to;

    Connection(AtomicPart* fromPart,
    	       AtomicPart* toPart);
    // accessor functions.
    AtomicPart *To();
    AtomicPart *From();
};


//--------------------------------------------------------------------------
// CompositeParts are parts constructed from AtomicParts
//	- entry in a library of reusesable components
//	- implementation is a graph of atomic parts
//	- provides unit of significant access locality
//	- each has an associated (unique) document object
//--------------------------------------------------------------------------

class Document;
class BaseAssembly;

class CompositePart {
public:
     int  			id;
    char			type[TypeSize];
    int  			buildDate;
    Document*			documentation;
    GenVHBag<BaseAssembly*>	*usedInPriv;
    GenVHBag<BaseAssembly*>	*usedInShar;
    GenVHSet<AtomicPart*>	*parts;
    AtomicPart*			rootPart;
    CompositePart(int cpId);
    ~CompositePart();
    int traverse(BenchmarkOp op);
    int traverse7();
    int reorg1();
    int reorg2();
};


//--------------------------------------------------------------------------
// Document objects are used to hold a description of some particular
// CompositePart object
//--------------------------------------------------------------------------

class Document {
public:
    char title[TitleSize];
    int	 id;
    char *text;
    CompositePart* part;

    Document(int cpId, CompositePart *cp);
    ~Document();
    int searchText(char c);
    int replaceText(char *oldString, char* newString);
};


//--------------------------------------------------------------------------
// Manual objects are used to hold a description of some particular
// module.  Really just big documents, only associated with modules
// instead of CompositeParts.
//--------------------------------------------------------------------------

class Module;
class Document;
class Manual;

class Manual {
public:
    char			title[TitleSize];
    int                         id;
    char			*text;
    int                         textLen;  
    Module*		        mod;

    Manual(int cpId);
    ~Manual();
    int searchText(char c);
    int replaceText(char *oldString, char* newString);
    int firstLast();
};

//--------------------------------------------------------------------------
// Assembly objects are design instances built up recursively from
// from other Assembly objects and (at the leaves only) CompositeParts
//	- hierarchical (tree) structure for designs
//	- may share composite parts with other assemblies
//	- nonleaf and leaf assembly subtypes
//--------------------------------------------------------------------------
class Assembly;
class ComplexAssembly;
class BaseAssembly;

class Assembly {
public:
     int  			id;
    char			type[TypeSize];
    int  			buildDate;
    ComplexAssembly*		superAssembly;
    Module*			module;

    Assembly(int);
    virtual int traverse(BenchmarkOp op) = 0;
//    virtual int traverse7(PartIdSet& visitedComplexIds) = 0;
    virtual AssemblyType myType() = 0;
    void    DoNothing();
};


class ComplexAssembly: public Assembly {
public:
    GenVHSet<Assembly *> *subAssemblies;

    ComplexAssembly(int, ComplexAssembly*, int, Module*);
    int traverse(BenchmarkOp op);
//    int traverse7(PartIdSet& visitedComplexIds);
    virtual AssemblyType myType() { return Complex; };
};


class BaseAssembly: public Assembly {
public:
    GenVHBag<CompositePart*>	*componentsPriv;
    GenVHBag<CompositePart*>	*componentsShar;
    BaseAssembly(int asId, ComplexAssembly* parentAssembly, Module* mod);
//    ~BaseAssembly();
    int traverse(BenchmarkOp op);
//    int traverse7(PartIdSet& visitedComplexIds);
    virtual AssemblyType myType() { return Base; };
};


//--------------------------------------------------------------------------
// Modules are the designs resulting from Assembly composition
//	- unit of scaleup for the benchmark database
//	- may share composite parts with other modules
//--------------------------------------------------------------------------

class Module {
public:
    int  			id;
    char			type[TypeSize];
    int  			buildDate;
    Manual*                     man;
    GenVHSet<Assembly*>		*assemblies;
    ComplexAssembly*		designRoot;
    Module(int modId);
    ~Module();
    int traverse(BenchmarkOp op);
    int scanManual();
    int firstLast();
};


class DBPoolRoot {
public:
  GenVHSet<CompositePart *> *DesignLib;
  GenVHSet<AtomicPart *> *AtomicDB;
  GenVHSet<Document *> *DocumentDB;
  GenAVLIndex<int, CompositePart *> *DesignLibIdIndex;
  GenAVLIndex<int, AtomicPart *> *AtomicIdIndex;
  GenAVLIndex<int, AtomicPart *> *AtomicDateIndex;
  GenAVLIndex<int, Document *> *DocumentIdIndex;
  GenAVLIndex<char *, Document *> *DocumentTitleIndex;
  GenVHSet<BaseAssembly *> *BaseAssemblyDB;
  GenAVLIndex<int, BaseAssembly *> *BaseAssemblyIdIndex;  
  GenVHSet<Module *> *ModuleDB;
  GenAVLIndex<int, Module *> *ModuleIdIndex;
  GenList<BaseAssembly *> *private_cp;
  GenList<BaseAssembly *> *shared_cp;
  DBPoolRoot(GenVHSet<CompositePart *> *a,
		 GenVHSet<AtomicPart *> *b,
		 GenVHSet<Document *> *c,
		 GenAVLIndex<int, CompositePart *> *d,
		 GenAVLIndex<int, AtomicPart *> *e,
		 GenAVLIndex<int, AtomicPart *> *f,
		 GenAVLIndex<int, Document *> *g,
		 GenAVLIndex<char *, Document *> *h,
		 GenVHSet<BaseAssembly *> *i,
		 GenAVLIndex<int, BaseAssembly *> *j,
		 GenVHSet<Module *> *k,
		 GenAVLIndex<int, Module *> *l,
		 GenList<BaseAssembly *> *m,GenList<BaseAssembly *> *n)
    { 
#ifndef NO_SETRANGE
  rvm_tid_t tid;

  BeginTransaction(&tid);
  SetRange(&tid, (char *)this, sizeof(*this));
#endif
DesignLib = a; AtomicDB = b; DocumentDB = c; 
      DesignLibIdIndex = d; AtomicIdIndex = e; AtomicDateIndex = f;
      DocumentIdIndex = g; DocumentTitleIndex = h;
      BaseAssemblyDB = i; BaseAssemblyIdIndex  = j;
      ModuleDB = k; ModuleIdIndex = l;
      private_cp = m; shared_cp = n;
#ifndef NO_SETRANGE
  Commit(&tid);
#endif
}
};

//--------------------------------------------------------------------------
// Dummy objects -- used for cache purging
//--------------------------------------------------------------------------

class DummyObj {
public:
    char			dummy[DummySize];
};

#endif _OO7_H_
