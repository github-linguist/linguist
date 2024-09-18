// Source: https://github.com/c3lang/c3c/blob/master/lib/std/collections/tuple.c3
// Permalink: https://github.com/c3lang/c3c/blob/ad0e97a/lib/std/collections/tuple.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::collections::tuple(<Type1, Type2>);

struct Tuple @deprecated
{
	Type1 first;
	Type2 second;
}

module std::collections::triple(<Type1, Type2, Type3>);

struct Triple @deprecated
{
	Type1 first;
	Type2 second;
	Type3 third;
}