/*
 * HISTORY
 * 31-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Fixed the signature of subarray.
 *      Added copyright.
 */

struct openarray {
    void *start;
    unsigned long size;
};

/* This function returns an UNTRACED REF ARRAY type */
extern struct openarray *
subarray(char *, unsigned long, unsigned long);
