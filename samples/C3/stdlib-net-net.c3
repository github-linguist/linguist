// Source: https://github.com/c3lang/c3c/blob/master/lib/std/net/net.c3
// Permalink: https://github.com/c3lang/c3c/blob/2b0857b/lib/std/net/net.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::net;
import std::io;

fault NetError
{
	INVALID_URL,
	URL_TOO_LONG,
	INVALID_SOCKET,
	GENERAL_ERROR,
	INVALID_IP_STRING,
	ADDRINFO_FAILED,
	CONNECT_FAILED,
	LISTEN_FAILED,
	ACCEPT_FAILED,
	WRITE_FAILED,
	READ_FAILED,
	SOCKOPT_FAILED,

	SOCKETS_NOT_INITIALIZED,
	STILL_PROCESSING_CALLBACK,
	BAD_SOCKET_DESCRIPTOR,
	NOT_A_SOCKET,
	CONNECTION_REFUSED,
	CONNECTION_TIMED_OUT,
	ADDRESS_IN_USE,
	CONNECTION_ALREADY_IN_PROGRESS,
	ALREADY_CONNECTED,
	NETWORK_UNREACHABLE,
	OPERATION_NOT_SUPPORTED_ON_SOCKET,
	CONNECTION_RESET,
}

fn uint! ipv4toint(String s)
{
	uint out;
	int element;
	int current = -1;
	foreach (c : s)
	{
		if (c == '.')
		{
			if (current < 0) return NetError.INVALID_IP_STRING?;
			out = out << 8 + current;
			current = -1;
			element++;
			continue;
		}
		if (element > 3 || c < '0' || c > '9') return NetError.INVALID_IP_STRING?;
		if (current < 0)
		{
			current = c - '0';
			continue;
		}
		current = current * 10 + c - '0';
	}
	if (element != 3 || current < 0) return NetError.INVALID_IP_STRING?;
	out = out << 8 + current;
	return out;
}

fn String! int_to_new_ipv4(uint val, Allocator allocator = allocator::heap())
{
	char[3 * 4 + 3 + 1] buffer;
	String res = (String)io::bprintf(&buffer, "%d.%d.%d.%d", val >> 24, (val >> 16) & 0xFF, (val >> 8) & 0xFF, val & 0xFF)!;
	return res.copy(allocator);
}

fn String! int_to_temp_ipv4(uint val)
{
	return int_to_new_ipv4(val, allocator::temp());
}
