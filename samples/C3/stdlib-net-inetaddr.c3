// Source: https://github.com/c3lang/c3c/blob/master/lib/std/net/inetaddr.c3
// Permalink: https://github.com/c3lang/c3c/blob/e293c43/lib/std/net/inetaddr.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::net;
import std::io;
import std::ascii;

enum IpProtocol : char (AIFamily ai_family)
{
	UNSPECIFIED = os::AF_UNSPEC,
	IPV4        = os::AF_INET,
	IPV6        = os::AF_INET6,
}

struct InetAddress (Printable)
{
	bool is_ipv6;
	union
	{
		bitstruct ipv6 : char[16] @bigendian
		{
			ushort a : 0..15;
			ushort b : 16..31;
			ushort c : 32..47;
			ushort d : 48..63;
			ushort e : 64..79;
			ushort f : 80..95;
			ushort g : 96..111;
			ushort h : 112..127;
		}
		bitstruct ip6 : char[16] @bigendian
		{
			uint128 val : 0..127;
		}
		UShortBE[8] ipv6arr;
		bitstruct ipv4 : char[16] @bigendian
		{
			char a : 96..103;
			char b : 104..111;
			char c : 112..119;
			char d : 120..127;
		}
		bitstruct ip4 : char[16] @bigendian
		{
			uint val : 96..127;
		}
	}
}


fn usz! InetAddress.to_format(InetAddress* addr, Formatter* formatter) @dynamic
{
	if (addr.is_ipv6)
	{
		return formatter.printf("%04x:%04x:%04x:%04x:%04x:%04x:%04x:%04x",
			addr.ipv6.a, addr.ipv6.b, addr.ipv6.c, addr.ipv6.d,
			addr.ipv6.e, addr.ipv6.f, addr.ipv6.g, addr.ipv6.h)!;
	}
	return formatter.printf("%d.%d.%d.%d", addr.ipv4.a, addr.ipv4.b, addr.ipv4.c, addr.ipv4.d)!;
}

fn String InetAddress.to_new_string(InetAddress* addr, Allocator allocator = allocator::heap()) @dynamic
{
	if (addr.is_ipv6)
	{
		char[8 * 5 + 1] buffer;
		String res = (String)io::bprintf(&buffer, "%04x:%04x:%04x:%04x:%04x:%04x:%04x:%04x",
			addr.ipv6.a, addr.ipv6.b, addr.ipv6.c, addr.ipv6.d,
			addr.ipv6.e, addr.ipv6.f, addr.ipv6.g, addr.ipv6.h)!!;
		return res.copy(allocator);
	}
	char[3 * 4 + 3 + 1] buffer;
	String res = (String)io::bprintf(&buffer, "%d.%d.%d.%d", addr.ipv4.a, addr.ipv4.b, addr.ipv4.c, addr.ipv4.d)!!;
	return res.copy(allocator);
}

fn InetAddress! ipv6_from_str(String s)
{
	uint sections = 0;
	if (s.len < 2) return NetError.INVALID_IP_STRING?;
	foreach (c : s) if (c == ':') sections++;
	int zero_segment_len = s[0] == ':' || s[^1] == ':' ? 9 - sections : 8 - sections;
	if (zero_segment_len == 7 && s.len == 2) return { .is_ipv6 = true };
	if (zero_segment_len > 7) return NetError.INVALID_IP_STRING?;
	usz index = 0;
	bool last_was_colon, found_zero;
	int current = -1;
	InetAddress addr = { .is_ipv6 = true };
	foreach (i, c : s)
	{
		if (c == ':')
		{
			if (!last_was_colon)
			{
				if (current == -1)
				{
					last_was_colon = true;
					continue;
				}
				if (current < 0 || current > 65535) return NetError.INVALID_IP_STRING?;
				addr.ipv6arr[index++].val = (ushort)current;
				current = -1;
				last_was_colon = true;
				continue;
			}
			assert(current == -1);
			// Check that this is the first ::
			if (found_zero) return NetError.INVALID_IP_STRING?;
			// Also check that the zeroed section is at least 2
			if (zero_segment_len < 2) return NetError.INVALID_IP_STRING?;
			// Skip (will be zero by default
			index += zero_segment_len;
			found_zero = true;
			last_was_colon = false;
			continue;
		}
		last_was_colon = false;
		if (index > 7 || !c.is_xdigit()) return NetError.INVALID_IP_STRING?;
		if (current < 0) current = 0;
		current = current * 16 + (c <= '9' ? c - '0' : (c | 32) - 'a' + 10);
	}
	// Ends with ::
	if (index == 8 && current == -1) return addr;

	// Ends with number
	if (index != 7 || current < 0 || current > 65535) return NetError.INVALID_IP_STRING?;
	addr.ipv6arr[7].val = (ushort)current;
	return addr;
}

fn InetAddress! ipv4_from_str(String s)
{
	InetAddress addr;
	int element;
	int current = -1;
	foreach (c : s)
	{
		if (c == '.')
		{
			if (current < 0) return NetError.INVALID_IP_STRING?;
			if (current > 255) return NetError.INVALID_IP_STRING?;
			switch (element)
			{
				case 0: addr.ipv4.a = (char)current;
				case 1: addr.ipv4.b = (char)current;
				case 2: addr.ipv4.c = (char)current;
				default: return NetError.INVALID_IP_STRING?;
			}
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
	if (element != 3 || current < 0 || current > 255) return NetError.INVALID_IP_STRING?;
	addr.ipv4.d = (char)current;
	return addr;
}

fn bool InetAddress.is_loopback(InetAddress* addr)
{
	if (addr.is_ipv6)
	{
		return addr.ip6.val == 1;
	}
	return addr.ipv4.a == 127;
}

fn bool InetAddress.is_any_local(InetAddress* addr)
{
	if (addr.is_ipv6)
	{
		return addr.ip6.val == 0;
	}
	return addr.ip4.val == 0;
}

fn bool InetAddress.is_link_local(InetAddress* addr)
{
	if (addr.is_ipv6)
	{
		return addr.ipv6.a == 0xfa && (addr.ipv6.b & 0xc0) == 0x80;
	}
	return addr.ipv4.a == 169 && addr.ipv4.b == 254;
}

fn bool InetAddress.is_site_local(InetAddress* addr)
{
	if (addr.is_ipv6)
	{
		return addr.ipv6.a == 0xfe && (addr.ipv6.b & 0xc0) == 0xc0;
	}
	// 10/8 172.16/12 192.168/16
	switch
	{
		case addr.ipv4.a == 10:
		case addr.ipv4.a == 172 && (addr.ipv4.b & 0xF0) == 16:
		case addr.ipv4.a == 192 && addr.ipv4.b == 168:
			return true;
		default:
			return false;
	}
}

fn bool InetAddress.is_multicast(InetAddress* addr)
{
	if (addr.is_ipv6)
	{
		return addr.ipv6.a == 0xff;
	}
	return addr.ip4.val & 0xf0000000 == 0xe0000000;
}

fn bool InetAddress.is_multicast_global(InetAddress* addr)
{
	if (addr.is_ipv6)
	{
		return addr.ipv6.a == 0xff && (addr.ipv6.b & 0x0f) == 0x0e;
	}
	return addr.ipv4.a >= 224 && addr.ipv4.a <= 238
		&& !(addr.ipv4.a == 224 && addr.ipv4.b == 0 && addr.ipv4.c == 0);
}

fn bool InetAddress.is_multicast_node_local(InetAddress* addr)
{
	if (!addr.is_ipv6) return false;
	return addr.ipv6.a == 0xff && addr.ipv6.b & 0x0f == 0x01;
}

fn bool InetAddress.is_multicast_site_local(InetAddress* addr)
{
	if (addr.is_ipv6)
	{
		return addr.ipv6.a == 0xff && addr.ipv6.b & 0x0f == 0x05;
	}
	return addr.ipv4.a == 239 && addr.ipv4.b == 255;
}

fn bool InetAddress.is_multicast_org_local(InetAddress* addr)
{
	if (addr.is_ipv6)
	{
		return addr.ipv6.a == 0xff && addr.ipv6.b & 0x0f == 0x08;
	}
	return addr.ipv4.a == 239 && addr.ipv4.b >= 192 && addr.ipv4.b <= 195;
}

fn bool InetAddress.is_multicast_link_local(InetAddress* addr)
{
	if (addr.is_ipv6)
	{
		return addr.ipv6.a == 0xff && (addr.ipv6.b & 0x0f) == 0x02;
	}
	return addr.ipv4.a == 224 && addr.ipv4.b == 0 && addr.ipv4.c == 0;
}

fn AddrInfo*! addrinfo(String host, uint port, AIFamily ai_family, AISockType ai_socktype) @if(os::SUPPORTS_INET)
{
	@pool()
	{
		ZString zhost = host.zstr_tcopy();
		DString str = dstring::temp_with_capacity(32);
		str.appendf("%d", port);
		AddrInfo hints = { .ai_family = ai_family, .ai_socktype = ai_socktype };
		AddrInfo* ai;
		if (os::getaddrinfo(zhost, str.zstr_view(), &hints, &ai)) return NetError.ADDRINFO_FAILED?;
		return ai;
	};
}
