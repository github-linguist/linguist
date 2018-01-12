/* rpc.x extracted from RFC5531 */

const RPC_VERS = 2;

enum auth_flavor {
	AUTH_NONE       = 0,
	AUTH_SYS        = 1,
	AUTH_SHORT      = 2,
	AUTH_DH         = 3,
	AUTH_KERB       = 4, /* RFC2695 */
	AUTH_RSA        = 5,
	RPCSEC_GSS      = 6  /* RFC2203 */
	/* and more to be defined */
};

typedef opaque opaque_auth_body<400>;

struct opaque_auth {
	int flavor; /* may be "pseudo" value outside enum */
	opaque_auth_body body;
};

enum msg_type {
	CALL  = 0,
	REPLY = 1
};

enum reply_stat {
	MSG_ACCEPTED = 0,
	MSG_DENIED   = 1
};

enum accept_stat {
	SUCCESS       = 0, /* RPC executed successfully       */
	PROG_UNAVAIL  = 1, /* remote hasn't exported program  */
	PROG_MISMATCH = 2, /* remote can't support version #  */
	PROC_UNAVAIL  = 3, /* program can't support procedure */
	GARBAGE_ARGS  = 4, /* procedure can't decode params   */
	SYSTEM_ERR    = 5  /* e.g. memory allocation failure  */
};

enum reject_stat {
	RPC_MISMATCH = 0, /* RPC version number != 2          */
	AUTH_ERROR = 1    /* remote can't authenticate caller */
};

enum auth_stat {
	AUTH_OK           = 0,  /* success                        */
	/*
	 * failed at remote end
	 */
	AUTH_BADCRED      = 1,  /* bad credential (seal broken)   */
	AUTH_REJECTEDCRED = 2,  /* client must begin new session  */
	AUTH_BADVERF      = 3,  /* bad verifier (seal broken)     */
	AUTH_REJECTEDVERF = 4,  /* verifier expired or replayed   */
	AUTH_TOOWEAK      = 5,  /* rejected for security reasons  */
	/*
	 * failed locally
	 */
	AUTH_INVALIDRESP  = 6,  /* bogus response verifier        */
	AUTH_FAILED       = 7,  /* reason unknown                 */
	/*
	 * AUTH_KERB errors; deprecated.  See [[139]RFC2695]
	 */
	AUTH_KERB_GENERIC = 8,  /* kerberos generic error */
	AUTH_TIMEEXPIRE = 9,    /* time of credential expired */
	AUTH_TKT_FILE = 10,     /* problem with ticket file */
	AUTH_DECODE = 11,       /* can't decode authenticator */
	AUTH_NET_ADDR = 12,     /* wrong net address in ticket */
	/*
	 * RPCSEC_GSS GSS related errors
	 */
	RPCSEC_GSS_CREDPROBLEM = 13, /* no credentials for user */
	RPCSEC_GSS_CTXPROBLEM = 14   /* problem with context */
};

struct rpc_msg {
	unsigned int xid;
	union rpc_msg_body body;
};

union rpc_msg_body switch (msg_type mtype) {
	case CALL:
		call_body cbody;
	case REPLY:
		reply_body rbody;
};

struct call_body {
	unsigned int rpcvers;       /* must be equal to two (2) */
	unsigned int prog;
	unsigned int vers;
	unsigned int proc;
	opaque_auth cred;
	opaque_auth verf;
	/* procedure-specific parameters start here */
};

union reply_body switch (reply_stat stat) {
	case MSG_ACCEPTED:
		accepted_reply areply;
	case MSG_DENIED:
		rejected_reply rreply;
} /*reply*/;

struct accepted_reply {
	opaque_auth verf;
	union accepted_reply_data reply_data;
};

union accepted_reply_data switch (accept_stat stat) {
	case SUCCESS:
		void /* opaque results[0] */;
		/*
		 * procedure-specific results start here
		 */
	case PROG_MISMATCH:
		struct {
			unsigned int low;
			unsigned int high;
		} mismatch_info;
	default:
		/*
		 * Void.  Cases include PROG_UNAVAIL, PROC_UNAVAIL,
		 * GARBAGE_ARGS, and SYSTEM_ERR.
		 */
		void;
};

union rejected_reply switch (reject_stat stat) {
	case RPC_MISMATCH:
		struct {
			unsigned int low;
			unsigned int high;
		} mismatch_info;
	case AUTH_ERROR:
		auth_stat auth_stat; /* renamed to avoid conflict with discriminator */
};

struct authsys_parms {
	unsigned int stamp;
	string machinename<255>;
	unsigned int uid;
	unsigned int gid;
	unsigned int gids<16>;
};
