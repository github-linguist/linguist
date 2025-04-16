##! Base DNS analysis script which tracks and logs DNS queries along with
##! their responses.

@load base/utils/queue
@load ./consts

module DNS;

export {
	## The DNS logging stream identifier.
	redef enum Log::ID += { LOG };

	## The record type which contains the column fields of the DNS log.
	type Info: record {
		## The earliest time at which a DNS protocol message over the
		## associated connection is observed.
		ts:            time               &log;
		## A unique identifier of the connection over which DNS messages
		## are being transferred.
		uid:           string             &log;
		## The connection's 4-tuple of endpoint addresses/ports.
		id:            conn_id            &log;
		## The transport layer protocol of the connection.
		proto:         transport_proto    &log;
		## A 16-bit identifier assigned by the program that generated
		## the DNS query.  Also used in responses to match up replies to
		## outstanding queries.
		trans_id:      count              &log &optional;
		## Round trip time for the query and response. This indicates
		## the delay between when the request was seen until the
		## answer started.
		rtt:           interval           &log &optional;
		## The domain name that is the subject of the DNS query.
		query:         string             &log &optional;
		## The QCLASS value specifying the class of the query.
		qclass:        count              &log &optional;
		## A descriptive name for the class of the query.
		qclass_name:   string             &log &optional;
		## A QTYPE value specifying the type of the query.
		qtype:         count              &log &optional;
		## A descriptive name for the type of the query.
		qtype_name:    string             &log &optional;
		## The response code value in DNS response messages.
		rcode:         count              &log &optional;
		## A descriptive name for the response code value.
		rcode_name:    string             &log &optional;
		## The Authoritative Answer bit for response messages specifies
		## that the responding name server is an authority for the
		## domain name in the question section.
		AA:            bool               &log &default=F;
		## The Truncation bit specifies that the message was truncated.
		TC:            bool               &log &default=F;
		## The Recursion Desired bit in a request message indicates that
		## the client wants recursive service for this query.
		RD:            bool               &log &default=F;
		## The Recursion Available bit in a response message indicates
		## that the name server supports recursive queries.
		RA:            bool               &log &default=F;
		## A reserved field that is usually zero in
		## queries and responses.
		Z:             count              &log &default=0;
		## The set of resource descriptions in the query answer.
		answers:       vector of string   &log &optional;
		## The caching intervals of the associated RRs described by the
		## *answers* field.
		TTLs:          vector of interval &log &optional;
		## The DNS query was rejected by the server.
		rejected:      bool               &log &default=F;

		## The total number of resource records in a reply message's
		## answer section.
		total_answers: count           &optional;
		## The total number of resource records in a reply message's
		## answer, authority, and additional sections.
		total_replies: count           &optional;

		## Whether the full DNS query has been seen.
		saw_query: bool                &default=F;
		## Whether the full DNS reply has been seen.
		saw_reply: bool                &default=F;
	};

	## An event that can be handled to access the :bro:type:`DNS::Info`
	## record as it is sent to the logging framework.
	global log_dns: event(rec: Info);

	## This is called by the specific dns_*_reply events with a "reply"
	## which may not represent the full data available from the resource
	## record, but it's generally considered a summarization of the
	## responses.
	##
	## c: The connection record for which to fill in DNS reply data.
	##
	## msg: The DNS message header information for the response.
	##
	## ans: The general information of a RR response.
	##
	## reply: The specific response information according to RR type/class.
	global do_reply: hook(c: connection, msg: dns_msg, ans: dns_answer, reply: string);

	## A hook that is called whenever a session is being set.
	## This can be used if additional initialization logic needs to happen
	## when creating a new session value.
	##
	## c: The connection involved in the new session.
	##
	## msg: The DNS message header information.
	##
	## is_query: Indicator for if this is being called for a query or a response.
	global set_session: hook(c: connection, msg: dns_msg, is_query: bool);

	## Yields a queue of :bro:see:`DNS::Info` objects for a given
	## DNS message query/transaction ID.
	type PendingMessages: table[count] of Queue::Queue;

	## Give up trying to match pending DNS queries or replies for a given
	## query/transaction ID once this number of unmatched queries or replies
	## is reached (this shouldn't happen unless either the DNS server/resolver
	## is broken, Bro is not seeing all the DNS traffic, or an AXFR query
	## response is ongoing).
	option max_pending_msgs = 50;

	## Give up trying to match pending DNS queries or replies across all
	## query/transaction IDs once there is at least one unmatched query or
	## reply across this number of different query IDs.
	option max_pending_query_ids = 50;

	## A record type which tracks the status of DNS queries for a given
	## :bro:type:`connection`.
	type State: record {
		## Indexed by query id, returns Info record corresponding to
		## queries that haven't been matched with a response yet.
		pending_queries: PendingMessages;

		## Indexed by query id, returns Info record corresponding to
		## replies that haven't been matched with a query yet.
		pending_replies: PendingMessages;
	};
}


redef record connection += {
	dns:       Info  &optional;
	dns_state: State &optional;
};

const ports = { 53/udp, 53/tcp, 137/udp, 5353/udp, 5355/udp };
redef likely_server_ports += { ports };

event bro_init() &priority=5
	{
	Log::create_stream(DNS::LOG, [$columns=Info, $ev=log_dns, $path="dns"]);
	Analyzer::register_for_ports(Analyzer::ANALYZER_DNS, ports);
	}

function new_session(c: connection, trans_id: count): Info
	{
	local info: Info;
	info$ts       = network_time();
	info$id       = c$id;
	info$uid      = c$uid;
	info$proto    = get_conn_transport_proto(c$id);
	info$trans_id = trans_id;
	return info;
	}

function log_unmatched_msgs_queue(q: Queue::Queue)
	{
	local infos: vector of Info;
	Queue::get_vector(q, infos);

	for ( i in infos )
		{
		Log::write(DNS::LOG, infos[i]);
		}
	}

function log_unmatched_msgs(msgs: PendingMessages)
	{
	for ( trans_id in msgs )
		{
		log_unmatched_msgs_queue(msgs[trans_id]);
		}

	clear_table(msgs);
	}

function enqueue_new_msg(msgs: PendingMessages, id: count, msg: Info)
	{
	if ( id !in msgs )
		{
		if ( |msgs| > max_pending_query_ids )
			{
			# Throw away all unmatched on assumption they'll never be matched.
			log_unmatched_msgs(msgs);
			}

		msgs[id] = Queue::init();
		}
	else
		{
		if ( Queue::len(msgs[id]) > max_pending_msgs )
			{
			log_unmatched_msgs_queue(msgs[id]);
			# Throw away all unmatched on assumption they'll never be matched.
			msgs[id] = Queue::init();
			}
		}

	Queue::put(msgs[id], msg);
	}

function pop_msg(msgs: PendingMessages, id: count): Info
	{
	local rval: Info = Queue::get(msgs[id]);

	if ( Queue::len(msgs[id]) == 0 )
		delete msgs[id];

	return rval;
	}

hook set_session(c: connection, msg: dns_msg, is_query: bool) &priority=5
	{
	if ( ! c?$dns_state )
		{
		local state: State;
		c$dns_state = state;
		}

	if ( is_query )
		{
		if ( msg$id in c$dns_state$pending_replies &&
		     Queue::len(c$dns_state$pending_replies[msg$id]) > 0 )
			{
			# Match this DNS query w/ what's at head of pending reply queue.
			c$dns = pop_msg(c$dns_state$pending_replies, msg$id);
			}
		else
			{
			# Create a new DNS session and put it in the query queue so
			# we can wait for a matching reply.
			c$dns = new_session(c, msg$id);
			enqueue_new_msg(c$dns_state$pending_queries, msg$id, c$dns);
			}
		}
	else
		{
		if ( msg$id in c$dns_state$pending_queries &&
		     Queue::len(c$dns_state$pending_queries[msg$id]) > 0 )
			{
			# Match this DNS reply w/ what's at head of pending query queue.
			c$dns = pop_msg(c$dns_state$pending_queries, msg$id);
			}
		else
			{
			# Create a new DNS session and put it in the reply queue so
			# we can wait for a matching query.
			c$dns = new_session(c, msg$id);
			enqueue_new_msg(c$dns_state$pending_replies, msg$id, c$dns);
			}
		}

	if ( ! is_query )
		{
		c$dns$rcode = msg$rcode;
		c$dns$rcode_name = base_errors[msg$rcode];

		if ( ! c$dns?$total_answers )
			c$dns$total_answers = msg$num_answers;

		if ( ! c$dns?$total_replies )
			c$dns$total_replies = msg$num_answers + msg$num_addl + msg$num_auth;

		if ( msg$rcode != 0 && msg$num_queries == 0 )
			c$dns$rejected = T;
		}
	}

event dns_message(c: connection, is_orig: bool, msg: dns_msg, len: count) &priority=5
	{
	if ( msg$opcode != 0 )
		# Currently only standard queries are tracked.
		return;

	hook set_session(c, msg, ! msg$QR);
	}

hook DNS::do_reply(c: connection, msg: dns_msg, ans: dns_answer, reply: string) &priority=5
	{
	if ( msg$opcode != 0 )
		# Currently only standard queries are tracked.
		return;

	if ( ! msg$QR )
		# This is weird: the inquirer must also be providing answers in
		# the request, which is not what we want to track.
		return;

	if ( ans$answer_type == DNS_ANS )
		{
		if ( ! c$dns?$query )
			c$dns$query = ans$query;

		c$dns$AA    = msg$AA;
		c$dns$RA    = msg$RA;

		if ( ! c$dns?$rtt )
			{
			c$dns$rtt = network_time() - c$dns$ts;
			# This could mean that only a reply was seen since 
			# we assume there must be some passage of time between
			# request and response.
			if ( c$dns$rtt == 0secs )
				delete c$dns$rtt;
			}

		if ( reply != "" )
			{
			if ( ! c$dns?$answers )
				c$dns$answers = vector();
			c$dns$answers += reply;

			if ( ! c$dns?$TTLs )
				c$dns$TTLs = vector();
			c$dns$TTLs += ans$TTL;
			}
		}
	}

event dns_end(c: connection, msg: dns_msg) &priority=5
	{
	if ( ! c?$dns )
		return;

	if ( msg$QR )
		c$dns$saw_reply = T;
	else
		c$dns$saw_query = T;
	}

event dns_end(c: connection, msg: dns_msg) &priority=-5
	{
	if ( c?$dns && c$dns$saw_reply && c$dns$saw_query )
		{
		Log::write(DNS::LOG, c$dns);
		delete c$dns;
		}
	}

event dns_request(c: connection, msg: dns_msg, query: string, qtype: count, qclass: count) &priority=5
	{
	if ( msg$opcode != 0 )
		# Currently only standard queries are tracked.
		return;

	c$dns$RD          = msg$RD;
	c$dns$TC          = msg$TC;
	c$dns$qclass      = qclass;
	c$dns$qclass_name = classes[qclass];
	c$dns$qtype       = qtype;
	c$dns$qtype_name  = query_types[qtype];
	c$dns$Z           = msg$Z;

	# Decode netbios name queries
	# Note: I'm ignoring the name type for now.  Not sure if this should be
	#       worked into the query/response in some fashion.
	if ( c$id$resp_p == 137/udp )
		{
		query = decode_netbios_name(query);
		if ( c$dns$qtype_name == "SRV" )
			{
			# The SRV RFC used the ID used for NetBios Status RRs.
			# So if this is NetBios Name Service we name it correctly.
			c$dns$qtype_name = "NBSTAT";
			}
		}
	c$dns$query = query;
	}


event dns_unknown_reply(c: connection, msg: dns_msg, ans: dns_answer) &priority=5
	{
	hook DNS::do_reply(c, msg, ans, fmt("<unknown type=%s>", ans$qtype));
	}

event dns_A_reply(c: connection, msg: dns_msg, ans: dns_answer, a: addr) &priority=5
	{
	hook DNS::do_reply(c, msg, ans, fmt("%s", a));
	}

event dns_TXT_reply(c: connection, msg: dns_msg, ans: dns_answer, strs: string_vec) &priority=5
	{
	local txt_strings: string = "";

	for ( i in strs )
		{
		if ( i > 0 )
			txt_strings += " ";

		txt_strings += fmt("TXT %d %s", |strs[i]|, strs[i]);
		}

	hook DNS::do_reply(c, msg, ans, txt_strings);
	}

event dns_AAAA_reply(c: connection, msg: dns_msg, ans: dns_answer, a: addr) &priority=5
	{
	hook DNS::do_reply(c, msg, ans, fmt("%s", a));
	}

event dns_A6_reply(c: connection, msg: dns_msg, ans: dns_answer, a: addr) &priority=5
	{
	hook DNS::do_reply(c, msg, ans, fmt("%s", a));
	}

event dns_NS_reply(c: connection, msg: dns_msg, ans: dns_answer, name: string) &priority=5
	{
	hook DNS::do_reply(c, msg, ans, name);
	}

event dns_CNAME_reply(c: connection, msg: dns_msg, ans: dns_answer, name: string) &priority=5
	{
	hook DNS::do_reply(c, msg, ans, name);
	}

event dns_MX_reply(c: connection, msg: dns_msg, ans: dns_answer, name: string,
                   preference: count) &priority=5
	{
	hook DNS::do_reply(c, msg, ans, name);
	}

event dns_PTR_reply(c: connection, msg: dns_msg, ans: dns_answer, name: string) &priority=5
	{
	hook DNS::do_reply(c, msg, ans, name);
	}

event dns_SOA_reply(c: connection, msg: dns_msg, ans: dns_answer, soa: dns_soa) &priority=5
	{
	hook DNS::do_reply(c, msg, ans, soa$mname);
	}

event dns_WKS_reply(c: connection, msg: dns_msg, ans: dns_answer) &priority=5
	{
	hook DNS::do_reply(c, msg, ans, "");
	}

event dns_SRV_reply(c: connection, msg: dns_msg, ans: dns_answer, target: string, priority: count, weight: count, p: count) &priority=5
	{
	hook DNS::do_reply(c, msg, ans, target);
	}

# TODO: figure out how to handle these
#event dns_EDNS(c: connection, msg: dns_msg, ans: dns_answer)
#	{
#
#	}
#
#event dns_EDNS_addl(c: connection, msg: dns_msg, ans: dns_edns_additional)
#	{
#
#	}
#
#event dns_TSIG_addl(c: connection, msg: dns_msg, ans: dns_tsig_additional)
#	{
#
#	}

event dns_rejected(c: connection, msg: dns_msg, query: string, qtype: count, qclass: count) &priority=5
	{
	if ( c?$dns )
		c$dns$rejected = T;
	}

event connection_state_remove(c: connection) &priority=-5
	{
	if ( ! c?$dns_state )
		return;

	# If Bro is expiring state, we should go ahead and log all unmatched
	# queries and replies now.
	log_unmatched_msgs(c$dns_state$pending_queries);
	log_unmatched_msgs(c$dns_state$pending_replies);
	}
