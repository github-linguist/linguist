// Source: https://github.com/c3lang/c3c/blob/master/lib/std/net/tcp.c3
// Permalink: https://github.com/c3lang/c3c/blob/16510d2/lib/std/net/tcp.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::net::tcp @if(os::SUPPORTS_INET);
import std::net @public;
import std::time, libc;

distinct TcpSocket = inline Socket;
distinct TcpServerSocket = inline Socket;

fn TcpSocket! connect(String host, uint port, Duration timeout = 0, SocketOption... options, IpProtocol ip_protocol = UNSPECIFIED)
{
	AddrInfo* ai =  net::addrinfo(host, port, ip_protocol.ai_family, os::SOCK_STREAM)!;
	defer os::freeaddrinfo(ai);
	if (timeout > 0)
	{
		return (TcpSocket)net::connect_with_timeout_from_addrinfo(ai, options, timeout)!;
	}
	return connect_to(ai, ...options);
}

fn TcpSocket! connect_async(String host, uint port, SocketOption... options, IpProtocol ip_protocol = UNSPECIFIED)
{
	AddrInfo* ai = net::addrinfo(host, port, ip_protocol.ai_family, os::SOCK_STREAM)!;
	defer os::freeaddrinfo(ai);
	return connect_async_to(ai, ...options);
}

fn TcpSocket! connect_to(AddrInfo* ai, SocketOption... options)
{
	return (TcpSocket)net::connect_from_addrinfo(ai, options);
}

fn TcpSocket! connect_async_to(AddrInfo* ai, SocketOption... options)
{
	return (TcpSocket)net::connect_async_from_addrinfo(ai, options);
}

fn TcpServerSocket! listen(String host, uint port, uint backlog, SocketOption... options, IpProtocol ip_protocol = UNSPECIFIED)
{
	AddrInfo* ai = net::addrinfo(host, port, ip_protocol.ai_family, os::SOCK_STREAM)!;
	defer os::freeaddrinfo(ai);
	return listen_to(ai, backlog, ...options);
}

fn TcpSocket! accept(TcpServerSocket* server_socket)
{
	TcpSocket socket;
	socket.ai_addrlen = socket.ai_addr_storage.len;
	socket.sock = os::accept(server_socket.sock, (SockAddrPtr)&socket.ai_addr_storage, &socket.ai_addrlen);
	if (!socket.sock.is_valid()) return NetError.ACCEPT_FAILED?;
	return socket;
}

fn TcpServerSocket! listen_to(AddrInfo* ai, uint backlog, SocketOption... options)
{
	net::@loop_over_ai(ai; NativeSocket sockfd, AddrInfo* ai_candidate)
	{
		net::apply_sockoptions(sockfd, options)!;
		bool err = os::bind(sockfd, ai_candidate.ai_addr, ai_candidate.ai_addrlen) || os::listen(sockfd, backlog);
		if (!err) return (TcpServerSocket)net::new_socket(sockfd, ai_candidate);
	};
	return os::socket_error()?;
}


