// Source: https://github.com/c3lang/c3c/blob/master/lib/std/net/socket_private.c3
// Permalink: https://github.com/c3lang/c3c/blob/748c737/lib/std/net/socket_private.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::net @if(os::SUPPORTS_INET);
import std::time, libc, std::os;

macro apply_sockoptions(sockfd, options) @private
{
	Socket sock = { .sock = sockfd };
	foreach (o : options) sock.set_option(o, true)!;
}

fn Socket! connect_from_addrinfo(AddrInfo* addrinfo, SocketOption[] options) @private
{
	@loop_over_ai(addrinfo; NativeSocket sockfd, AddrInfo* ai)
	{
		apply_sockoptions(sockfd, options)!;
		int errcode = os::connect(sockfd, ai.ai_addr, ai.ai_addrlen);
		// Keep the first successful connection.
		if (!errcode) return new_socket(sockfd, ai);
	};
	return os::socket_error()?;
}

fn bool last_error_is_delayed_connect()
{
	$switch
		$case env::WIN32:
		switch (win32_WSAGetLastError())
		{
			case wsa::EWOULDBLOCK:
			case wsa::EINPROGRESS: return true;
			default: return false;
		}
    	$default:
    	Errno err = libc::errno();
    	return err == errno::EINPROGRESS || err == errno::EAGAIN || err == errno::EWOULDBLOCK;
	$endswitch
}

fn Socket! connect_with_timeout_from_addrinfo(AddrInfo* addrinfo, SocketOption[] options, Duration timeout) @private
{
	Clock c = 0;
	@loop_over_ai(addrinfo; NativeSocket sockfd, AddrInfo* ai)
	{
		apply_sockoptions(sockfd, options)!;
		sockfd.set_non_blocking(true)!;
		int errcode = os::connect(sockfd, ai.ai_addr, ai.ai_addrlen);
		if (!errcode)
		{
			// It worked, restore blocking.
			sockfd.set_non_blocking(false)!;
			return new_socket(sockfd, ai);
		}
		if (last_error_is_delayed_connect())
		{
			Duration timeout_left = timeout;
			if (c)
			{
				Duration to_remove = c.to_now().to_duration();
				if (to_remove >= timeout_left)
				{
					return NetError.CONNECTION_TIMED_OUT?;
				}
				timeout_left -= to_remove;
			}
			else
			{
				c = clock::now();
			}
			Poll poll_request = { sockfd, SUBSCRIBE_ANY_WRITE, 0 };
			if (!poll((&poll_request)[:1], timeout_left)!)
			{
				return NetError.CONNECTION_TIMED_OUT?;
			}
			if (poll_request.revents & POLL_EVENT_WRITE)
			{
				sockfd.set_non_blocking(false)!;
				return new_socket(sockfd, ai);
			}
		}
	};
	return os::socket_error()?;
}

fn Socket! connect_async_from_addrinfo(AddrInfo* addrinfo, SocketOption[] options) @private
{
	@loop_over_ai(addrinfo; NativeSocket sockfd, AddrInfo* ai)
	{
		apply_sockoptions(sockfd, options)!;
		sockfd.set_non_blocking(true)!;
		int errcode = os::connect(sockfd, ai.ai_addr, ai.ai_addrlen);
		if (!errcode || last_error_is_delayed_connect())
		{
			// Keep the first successful connection.
			return new_socket(sockfd, ai);
		}
	};
	return os::socket_error()?;
}

macro @network_loop_over_ai(network, host, port; @body(fd, ai)) @private
{
	AddrInfo* ai = network.addrinfo(host, port)!;
	AddrInfo* first = ai;
	defer os::freeaddrinfo(first);
	while (ai)
	{
		NativeSocket sockfd = os::socket(ai.ai_family, ai.ai_socktype, ai.ai_protocol);
		if (sockfd > 0)
		{
			@body(sockfd, ai);
		}
		ai = ai.ai_next;
	}
}
