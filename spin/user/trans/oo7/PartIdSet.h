

#ifndef PARTID_H
#define PARTID_H

#define HashTableSize 511

class Member {
public:
    int		value;
    Member*	next;
};

class PartIdSet {
private:
    Member* hashTable[HashTableSize];
    int     emptySet;
    int hash (int val) 
    { return((unsigned) ((unsigned) val * 12345 + 6789) % HashTableSize); }

public:
    PartIdSet();
    ~PartIdSet() { clear(); };
    void clear();
    void insert(int val);
    int  contains(int val);
    int	 empty() { return(emptySet); };
};

#endif

