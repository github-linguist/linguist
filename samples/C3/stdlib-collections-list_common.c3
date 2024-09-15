// Source: https://github.com/c3lang/c3c/blob/master/lib/std/collections/list_common.c3
// Permalink: https://github.com/c3lang/c3c/blob/a248511/lib/std/collections/list_common.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::collections::list_common;

/**
 * IMPORTANT The returned array must be freed using free_aligned.
 **/
macro list_to_new_aligned_array($Type, self, Allocator allocator)
{
	if (!self.size) return $Type[] {};
	$Type[] result = allocator::alloc_array_aligned(allocator, $Type, self.size);
	result[..] = self.entries[:self.size];
	return result;
}

macro list_to_new_array($Type, self, Allocator allocator)
{
	if (!self.size) return $Type[] {};
	$Type[] result = allocator::alloc_array(allocator, $Type, self.size);
	result[..] = self.entries[:self.size];
	return result;
}

macro void list_reverse(self)
{
	if (self.size < 2) return;
	usz half = self.size / 2U;
	usz end = self.size - 1;
	for (usz i = 0; i < half; i++)
	{
		@swap(self.entries[i], self.entries[end - i]);
	}
}

macro usz list_remove_using_test(self, filter, bool $invert, ctx)
{
	usz size = self.size;
	for (usz i = size, usz k = size; k > 0; k = i)
	{
		// Find last index of item to be deleted.
		$if $invert:
			while (i > 0 && !filter(&self.entries[i - 1], ctx)) i--;
		$else
			while (i > 0 && filter(&self.entries[i - 1], ctx)) i--;
		$endif
		// Remove the items from this index up to the one not to be deleted.
		usz n = self.size - k;
		self.entries[i:n] = self.entries[k:n];
		self.size -= k - i;
		// Find last index of item not to be deleted.
		$if $invert:
			while (i > 0 && filter(&self.entries[i - 1], ctx)) i--;
		$else
			while (i > 0 && !filter(&self.entries[i - 1], ctx)) i--;
		$endif
	}
	return size - self.size;
}

macro usz list_compact(self)
{
	usz size = self.size;
	for (usz i = size; i > 0; i--)
	{
		if (self.entries[i - 1]) continue;
		for (usz j = i; j < size; j++)
		{
			self.entries[j - 1] = self.entries[j];
		}
		self.size--;
	}
	return size - self.size;
}

macro usz list_remove_item(self, value)
{
	usz size = self.size;
	for (usz i = size; i > 0; i--)
	{
		if (!equals(self.entries[i - 1], value)) continue;
		for (usz j = i; j < self.size; j++)
		{
			self.entries[j - 1] = self.entries[j];
		}
		self.size--;
	}
	return size - self.size;
}


macro usz list_remove_if(self, filter, bool $invert)
{
	usz size = self.size;
	for (usz i = size, usz k = size; k > 0; k = i)
	{
		// Find last index of item to be deleted.
		$if $invert:
			while (i > 0 && !filter(&self.entries[i - 1])) i--;
		$else
			while (i > 0 && filter(&self.entries[i - 1])) i--;
		$endif
		// Remove the items from this index up to the one not to be deleted.
		usz n = self.size - k;
		self.entries[i:n] = self.entries[k:n];
		self.size -= k - i;
		// Find last index of item not to be deleted.
		$if $invert:
			while (i > 0 && filter(&self.entries[i - 1])) i--;
		$else
			while (i > 0 && !filter(&self.entries[i - 1])) i--;
		$endif
	}
	return size - self.size;
}
