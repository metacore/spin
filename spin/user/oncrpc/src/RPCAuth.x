const AUTH_NONE  = 0;
const AUTH_UNIX  = 1;
const AUTH_SHORT = 2;
const AUTH_DES   = 3;

struct auth_unix {
	unsigned int stamp;
	string machinename<255>;
	unsigned int uid;
	unsigned int gid;
	unsigned int gids<16>;
};

typedef opaque opaque_auth<400>;

union Credentials switch (unsigned int flavor){
 case 0 /* AUTH_NONE */:
	opaque_auth oa;
 case 1 /* AUTH_UNIX */:
	opaque_auth oa;
 case 2 /* AUTH_SHORT */:
	opaque_auth oa;
 default: 
	opaque_auth oa;
};

