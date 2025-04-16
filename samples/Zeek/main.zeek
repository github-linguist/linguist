##! Implements base functionality for HTTP analysis.  The logging model is
##! to log request/response pairs and all relevant metadata together in
##! a single record.

@load base/utils/numbers
@load base/utils/files
@load base/frameworks/tunnels

module HTTP;

export {
	redef enum Log::ID += { LOG };

	## Indicate a type of attack or compromise in the record to be logged.
	type Tags: enum {
		## Placeholder.
		EMPTY
	};

	## This setting changes if passwords used in Basic-Auth are captured or
	## not.
	option default_capture_password = F;

	## The record type which contains the fields of the HTTP log.
	type Info: record {
		## Timestamp for when the request happened.
		ts:                      time      &log;
		## Unique ID for the connection.
		uid:                     string    &log;
		## The connection's 4-tuple of endpoint addresses/ports.
		id:                      conn_id   &log;
		## Represents the pipelined depth into the connection of this
		## request/response transaction.
		trans_depth:             count     &log;
		## Verb used in the HTTP request (GET, POST, HEAD, etc.).
		method:                  string    &log &optional;
		## Value of the HOST header.
		host:                    string    &log &optional;
		## URI used in the request.
		uri:                     string    &log &optional;
		## Value of the "referer" header.  The comment is deliberately
		## misspelled like the standard declares, but the name used here
		## is "referrer" spelled correctly.
		referrer:                string    &log &optional;
		## Value of the version portion of the request.
		version:		string	   &log &optional;
		## Value of the User-Agent header from the client.
		user_agent:              string    &log &optional;
		## Value of the Origin header from the client.
		origin:                  string    &log &optional;
		## Actual uncompressed content size of the data transferred from
		## the client.
		request_body_len:        count     &log &default=0;
		## Actual uncompressed content size of the data transferred from
		## the server.
		response_body_len:       count     &log &default=0;
		## Status code returned by the server.
		status_code:             count     &log &optional;
		## Status message returned by the server.
		status_msg:              string    &log &optional;
		## Last seen 1xx informational reply code returned by the server.
		info_code:               count     &log &optional;
		## Last seen 1xx informational reply message returned by the server.
		info_msg:                string    &log &optional;
		## A set of indicators of various attributes discovered and
		## related to a particular request/response pair.
		tags:                    set[Tags] &log;

		## Username if basic-auth is performed for the request.
		username:                string    &log &optional;
		## Password if basic-auth is performed for the request.
		password:                string    &log &optional;

		## Determines if the password will be captured for this request.
		capture_password:        bool      &default=default_capture_password;

		## All of the headers that may indicate if the request was proxied.
		proxied:                 set[string] &log &optional;

		## Indicates if this request can assume 206 partial content in
		## response.
		range_request:           bool      &default=F;
	};

	## Structure to maintain state for an HTTP connection with multiple
	## requests and responses.
	type State: record {
		## Pending requests.
		pending:          table[count] of Info;
		## Current request in the pending queue.
		current_request:  count                &default=0;
		## Current response in the pending queue.
		current_response: count                &default=0;
		## Track the current deepest transaction.
		## This is meant to cope with missing requests
		## and responses.
		trans_depth:      count                &default=0;
	};

	## A list of HTTP headers typically used to indicate proxied requests.
	option proxy_headers: set[string] = {
		"FORWARDED",
		"X-FORWARDED-FOR",
		"X-FORWARDED-FROM",
		"CLIENT-IP",
		"VIA",
		"XROXY-CONNECTION",
		"PROXY-CONNECTION",
	};

	## A list of HTTP methods. Other methods will generate a weird. Note
	## that the HTTP analyzer will only accept methods consisting solely
	## of letters ``[A-Za-z]``.
	option http_methods: set[string] = {
		"GET", "POST", "HEAD", "OPTIONS",
		"PUT", "DELETE", "TRACE", "CONNECT",
		# HTTP methods for distributed authoring:
		"PROPFIND", "PROPPATCH", "MKCOL",
		"COPY", "MOVE", "LOCK", "UNLOCK",
		"POLL", "REPORT", "SUBSCRIBE", "BMOVE",
		"SEARCH"
	};

	## Event that can be handled to access the HTTP record as it is sent on
	## to the logging framework.
	global log_http: event(rec: Info);
}

# Add the http state tracking fields to the connection record.
redef record connection += {
	http:        Info  &optional;
	http_state:  State &optional;
};

const ports = {
	80/tcp, 81/tcp, 631/tcp, 1080/tcp, 3128/tcp,
	8000/tcp, 8080/tcp, 8888/tcp,
};
redef likely_server_ports += { ports };

# Initialize the HTTP logging stream and ports.
event zeek_init() &priority=5
	{
	Log::create_stream(HTTP::LOG, [$columns=Info, $ev=log_http, $path="http"]);
	Analyzer::register_for_ports(Analyzer::ANALYZER_HTTP, ports);
	}

function code_in_range(c: count, min: count, max: count) : bool
	{
	return c >= min && c <= max;
	}

function new_http_session(c: connection): Info
	{
	local tmp: Info;
	tmp$ts=network_time();
	tmp$uid=c$uid;
	tmp$id=c$id;
	tmp$trans_depth = ++c$http_state$trans_depth;
	return tmp;
	}

function set_state(c: connection, is_orig: bool)
	{
	if ( ! c?$http_state )
		{
		local s: State;
		c$http_state = s;
		}

	# These deal with new requests and responses.
	if ( is_orig )
		{
		if ( c$http_state$current_request !in c$http_state$pending )
			c$http_state$pending[c$http_state$current_request] = new_http_session(c);

		c$http = c$http_state$pending[c$http_state$current_request];
		}
	else
		{
		if ( c$http_state$current_response !in c$http_state$pending )
			c$http_state$pending[c$http_state$current_response] = new_http_session(c);

		c$http = c$http_state$pending[c$http_state$current_response];
		}
	}

event http_request(c: connection, method: string, original_URI: string,
                   unescaped_URI: string, version: string) &priority=5
	{
	if ( ! c?$http_state )
		{
		local s: State;
		c$http_state = s;
		}

	++c$http_state$current_request;
	set_state(c, T);

	c$http$method = method;
	c$http$uri = unescaped_URI;

	if ( method !in http_methods )
		Reporter::conn_weird("unknown_HTTP_method", c, method);
	}

event http_reply(c: connection, version: string, code: count, reason: string) &priority=5
	{
	if ( ! c?$http_state )
		{
		local s: State;
		c$http_state = s;
		}

	# If the last response was an informational 1xx, we're still expecting
	# the real response to the request, so don't create a new Info record yet.
	if ( c$http_state$current_response !in c$http_state$pending ||
	     (c$http_state$pending[c$http_state$current_response]?$status_code &&
	       ! code_in_range(c$http_state$pending[c$http_state$current_response]$status_code, 100, 199)) )
		{
		++c$http_state$current_response;
		}
	set_state(c, F);

	c$http$status_code = code;
	c$http$status_msg = reason;
	c$http$version = version;

	if ( code_in_range(code, 100, 199) )
		{
		c$http$info_code = code;
		c$http$info_msg = reason;
		}

	if ( c$http?$method && c$http$method == "CONNECT" && code == 200 )
		{
		# Copy this conn_id and set the orig_p to zero because in the case of CONNECT
		# proxies there will be potentially many source ports since a new proxy connection
		# is established for each proxied connection.  We treat this as a singular
		# "tunnel".
		local tid = copy(c$id);
		tid$orig_p = 0/tcp;
		Tunnel::register([$cid=tid, $tunnel_type=Tunnel::HTTP]);
		}
	}

event http_header(c: connection, is_orig: bool, name: string, value: string) &priority=5
	{
	set_state(c, is_orig);

	if ( is_orig ) # client headers
		{
		if ( name == "REFERER" )
			c$http$referrer = value;

		else if ( name == "HOST" )
			# The split is done to remove the occasional port value that shows up here.
			c$http$host = split_string1(value, /:/)[0];

		else if ( name == "RANGE" )
			c$http$range_request = T;

		else if ( name == "ORIGIN" )
			c$http$origin = value;

		else if ( name == "USER-AGENT" )
			c$http$user_agent = value;

		else if ( name in proxy_headers )
				{
				if ( ! c$http?$proxied )
					c$http$proxied = set();
				add c$http$proxied[fmt("%s -> %s", name, value)];
				}

		else if ( name == "AUTHORIZATION" || name == "PROXY-AUTHORIZATION" )
			{
			if ( /^[bB][aA][sS][iI][cC] / in value )
				{
				local userpass = decode_base64_conn(c$id, sub(value, /[bB][aA][sS][iI][cC][[:blank:]]/, ""));
				local up = split_string(userpass, /:/);
				if ( |up| >= 2 )
					{
					c$http$username = up[0];
					if ( c$http$capture_password )
						c$http$password = up[1];
					}
				else
					{
					c$http$username = fmt("<problem-decoding> (%s)", value);
					if ( c$http$capture_password )
						c$http$password = userpass;
					}
				}
			}
		}
	}

event http_message_done(c: connection, is_orig: bool, stat: http_message_stat) &priority = 5
	{
	set_state(c, is_orig);

	if ( is_orig )
		c$http$request_body_len = stat$body_length;
	else
		c$http$response_body_len = stat$body_length;
	}

event http_message_done(c: connection, is_orig: bool, stat: http_message_stat) &priority = -5
	{
	# The reply body is done so we're ready to log.
	if ( ! is_orig )
		{
		# If the response was an informational 1xx, we're still expecting
		# the real response later, so we'll continue using the same record.
		if ( ! (c$http?$status_code && code_in_range(c$http$status_code, 100, 199)) )
			{
			Log::write(HTTP::LOG, c$http);
			delete c$http_state$pending[c$http_state$current_response];
			}
		}
	}

event connection_state_remove(c: connection) &priority=-5
	{
	# Flush all pending but incomplete request/response pairs.
	if ( c?$http_state )
		{
		for ( r, info in c$http_state$pending )
			{
			# We don't use pending elements at index 0.
			if ( r == 0 ) next;
			Log::write(HTTP::LOG, info);
			}
		}
	}

