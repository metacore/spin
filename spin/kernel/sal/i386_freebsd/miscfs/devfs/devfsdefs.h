#ifdef DEVFS_DEBUG
#define DBPRINT(A) printf(A)
#else
#define DBPRINT(A)
#endif

/*
 * Written by Julian Elischer (julian@DIALIX.oz.au)
 *
 * $Header: /afs/cs/project/spin/cvsroot/spin/kernel/sal/i386_freebsd/miscfs/devfs/devfsdefs.h,v 1.1.1.1 1996/08/15 03:23:48 fgray Exp $
 */

/* first a couple of defines for compatibility with inodes */

#define	ISUID		04000		/* set user identifier when exec'ing */
#define	ISGID		02000		/* set group identifier when exec'ing */
#define	ISVTX		01000		/* save execution information on exit */
#define	IREAD		0400		/* read permission */
#define	IWRITE		0200		/* write permission */
#define	IEXEC		0100		/* execute permission */


#define	ILOCKED		0x0001		/* inode is locked */
#define	IWANT		0x0002		/* some process waiting on lock */
#define	IRENAME		0x0004		/* inode is being renamed */
#define	IUPD		0x0010		/* file has been modified */
#define	IACC		0x0020		/* inode access time to be updated */
#define	ICHG		0x0040		/* inode has been changed */
#define	IMOD		0x0080		/* inode has been modified */
#define	ISHLOCK		0x0100		/* file has shared lock */
#define	IEXLOCK		0x0200		/* file has exclusive lock */
#define	ILWAIT		0x0400		/* someone waiting on file lock */

/*
 * Lock and unlock inodes.
 */
#ifdef notdef
#define	DNLOCK(ip) { \
	while ((ip)->i_flag & ILOCKED) { \
		(ip)->i_flag |= IWANT; \
		(void) sleep((caddr_t)(ip), PINOD); \
	} \
	(ip)->i_flag |= ILOCKED; \
}

#define	DNUNLOCK(ip) { \
	(ip)->i_flag &= ~ILOCKED; \
	if ((ip)->i_flag&IWANT) { \
		(ip)->i_flag &= ~IWANT; \
		wakeup((caddr_t)(ip)); \
	} \
}
#else
#define DNLOCK(ip)
#define DNUNLOCK(ip)
#endif


#define DEVMAXNAMESIZE 32
#define DEVMAXPATHSIZE 128
#define	DEV_DIR 1
#define DEV_BDEV 2
#define DEV_CDEV 3
#define DEV_DDEV 4
#define	DEV_ALIAS 5
#define DEV_SLNK 6


extern int (**devfs_vnodeop_p)();	/* our own vector array for dirs */
extern int (**dev_spec_vnodeop_p)();	/* our own vector array for devs */

typedef struct dev_name *devnm_p;
typedef	struct devnode	*dn_p;

struct	devnode	/* the equivalent of an INODE */
{
	u_short type;
	int	flags;		/* more inode compatible for now *//*XXXkill*/
	u_short	mode;		/* basically inode compatible (drwxrwxrwx) */
	u_short	uid;		/* basically inode compatible  */
	u_short	gid;		/* basically inode compatible  */
	struct timespec	atime;	/* time of last access */
	struct timespec	mtime;	/* time of last modification */
	struct timespec	ctime;	/* time file changed */
	int	links;		/* how many file links does this node have? */
	struct	devfsmount *dvm; /* the mount structure for this 'plane' */
	struct	vnode *vn;	/* address of last vnode that represented us */
	u_long	vn_id;		/* make sure we have the right vnode */
	int (***ops)();		/* yuk... pointer to pointer(s) to funcs */
	int	len;		/* of any associated info (e.g. dir data) */
	union  typeinfo {
		struct {
			struct	cdevsw	*cdevsw;
			dev_t	dev;
		}Cdev;
		struct {
			struct	bdevsw	*bdevsw;
			dev_t	dev;
		}Bdev;
		struct {
			int (***ops)();	 /* duplicate, used in dev_add_node */
			int	arg;
		}Ddev;
		struct {
			devnm_p	dirlist;
			devnm_p	*dirlast;
			dn_p	parent;
			devnm_p	myname;
			int	entrycount;
		}Dir;
		struct {
			char	*name;	/* must be allocated separatly */
			int	namelen;
		}Slnk;
		struct {
			devnm_p	realthing;
			devnm_p	next;
		}Alias;
	}by;
};
typedef	struct devnode	devnode_t;

struct	dev_name
{
	/*-----------------------directory entry fields-------------*/
	char	name[DEVMAXNAMESIZE];
	dn_p	dnp;		/* the "inode" (devnode) pointer */
	dn_p	parent;		/* backpointer to the directory itself */
	devnm_p	next;		/* next object in this directory */
	devnm_p	*prevp;		/* previous pointer in directory linked list */
	/*-----------------------aliases or backing nodes----------*/
	union {
		struct {
			devnm_p	aliases;	/* aliase chain (kill with us)*/
			int	alias_count;	/* # 'alias' nodes for us. */
		} back;
		struct {
			devnm_p	realthing;	/* ptr to the backing node */
			devnm_p	file_node;	/* our file node */

		} front;
	} as;
	/*-----------------------the front-back chain-------------*/
	devnm_p	next_front;	/* the linked list of all our front nodes */
	devnm_p	*prev_frontp;	/* the end of the front node chain */
	int	frontcount;	/* number of front nodes that reference us*/
};

typedef struct dev_name devnm_t;
extern devnm_p dev_root;


/*
 * Rules for front nodes:
 * Dirs hava a strict 1:1 relationship with their OWN devnode
 * Symlinks similarly
 * Device Nodes ALWAYS point to the devnode that is linked
 * to the Backing node. (with a ref count)
 */

/*
 * DEVFS specific per/mount information, used to link a monted fs to a
 * particular 'plane' of front nodes.
 */
struct devfsmount
{
	struct mount *mount;		/* vfs mount struct for this fs	*/
	devnm_p	plane_root;		/* the root of this 'plane'	*/
	int	flags;			/* usefule some day 8-) 	*/
};

struct dev_vn_data
{
	char	magic[6];		/* = "devfs" if correct */
	devnm_p	front;
	devnm_p	back;
};

extern struct vnodeops spec_vnodeops,devfs_vnodeops;
/*
 * Prototypes for DEVFS virtual filesystem operations
 */
#include "devfs_proto.h"
