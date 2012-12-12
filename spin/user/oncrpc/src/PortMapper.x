/* PortMapper.x -- Sun port mapper protocol
   David Nichols (from Sun docs)
   August, 1991 */

const PMAP_PORT = 111;

/*
 * A mapping of (program, version, protocol) to port number
 */

struct mapping {
    unsigned int prog;
    unsigned int vers;
    unsigned int prot;
    unsigned int port;
};

/*
 * Supported values for the "prot" field
 */
const IPPROTO_TCP = 6;		/* protocol number for TCP/IP */
const IPPROTO_UDP = 17;		/* protocol number for UDP/IP */

/*
 * A list of mappings
 */
struct pmap {
    mapping map;
    pmap *next;
};
typedef pmap *pmaplist;

/*
 * Arguments to callit
 */
struct call_args {
    unsigned int prog;
    unsigned int vers;
    unsigned int proc;
    opaque args<>;
};

/*
 * Results of callit
 */
struct call_result {
    unsigned int port;
    opaque res<>;
};

/*
 * Port mapper procedures
 */
program PMAP_PROG {
    version PMAP_VERS {
	bool Set(mapping) = 1;
	bool Unset(mapping) = 2;
	unsigned int GetPort(mapping) = 3;
	pmaplist Dump(void) = 4;
	call_result CallIt(call_args) = 5;
    } = 2;
} = 100000;
