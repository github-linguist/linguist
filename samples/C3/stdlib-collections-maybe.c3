// Source: https://github.com/c3lang/c3c/blob/master/lib/std/collections/maybe.c3
// Permalink: https://github.com/c3lang/c3c/blob/cd7a03c/lib/std/collections/maybe.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::collections::maybe(<Type>);

struct Maybe
{
	Type value;
	bool has_value;
}

fn Maybe value(Type val)
{
	return { .value = val, .has_value = true };
}

const Maybe EMPTY = { };

macro Type! Maybe.get(self)
{
	return self.has_value ? self.value : SearchResult.MISSING?;
}
