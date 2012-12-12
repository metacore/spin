/*
 * HISTORY
 * 12-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created. 
 */

/* Interface to Dan Sleater's(who is also the original inventor of the
   algorithm!) Splay tree implementation. */

#ifndef __SPLAY_H

typedef long splay_key_t;

typedef struct tree_node Tree;
struct tree_node {
    Tree * left, * right;
    splay_key_t key;
    int size;   /* maintained to be the number of nodes rooted here */
};

Tree *splay_splay(splay_key_t i, Tree *root);
/* Bring the node I to the root. */

Tree *splay_insert(Tree *new, Tree *root, int *existed);
/* Insert the item NEW into the tree ROOT.
   Returns the new tree. EXISTED will be 1 when the item with the same
   key already existed, otherwise it will be 0. */

Tree *splay_delete(splay_key_t i, Tree *t, Tree **elem);
/* Deletes i from the tree if it's there.               
   Return a pointer to the resulting tree.              
   ELEM points to the deleted element.
   */

Tree *splay_delete_root(Tree *t, Tree **elem);

Tree *splay_find_rank(int r, Tree *t);
/* Find the Rth element in the tree. */

void splay_printtree(Tree * t, int d);
/* Print the tree. D specifies the current depth of the node. */

#define node_size(x) (((x)==NULL) ? 0 : ((x)->size))
/* This macro returns the size of a node.  Unlike "x->size",     */
/* it works even if x=NULL.  The test could be avoided by using  */
/* a special version of NULL which was a real node with size 0.  */

#endif
