// Source: https://github.com/c3lang/c3c/blob/master/lib/std/collections/anylist.c3
// Permalink: https://github.com/c3lang/c3c/blob/ed5d338/lib/std/collections/anylist.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

// Copyright (c) 2024 Christoffer Lerno. All rights reserved.
// Use of self source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::collections::anylist;
import std::io,std::math;

def AnyPredicate = fn bool(any value);
def AnyTest = fn bool(any type, any context);

struct AnyList (Printable)
{
	usz size;
	usz capacity;
	Allocator allocator;
	any* entries;
}


/**
 * @param initial_capacity "The initial capacity to reserve"
 * Use `init` for to use a custom allocator.
 **/
fn AnyList* AnyList.new_init(&self, usz initial_capacity = 16, Allocator allocator = null)
{
	return self.init(allocator ?: allocator::heap(), initial_capacity) @inline;
}

/**
 * @param [&inout] allocator "The allocator to use"
 * @param initial_capacity "The initial capacity to reserve"
 **/
fn AnyList* AnyList.init(&self, Allocator allocator, usz initial_capacity = 16)
{
	self.allocator = allocator;
	self.size = 0;
	if (initial_capacity > 0)
	{
		initial_capacity = math::next_power_of_2(initial_capacity);
		self.entries = allocator::alloc_array(allocator, any, initial_capacity);
	}
	else
	{
		self.entries = null;
	}
	self.capacity = initial_capacity;
	return self;
}

/**
 * Initialize the list using the temp allocator.
 *
 * @param initial_capacity "The initial capacity to reserve"
 **/
fn AnyList* AnyList.temp_init(&self, usz initial_capacity = 16)
{
	return self.init(allocator::temp(), initial_capacity) @inline;
}

fn usz! AnyList.to_format(&self, Formatter* formatter) @dynamic
{
	switch (self.size)
	{
		case 0:
			return formatter.print("[]")!;
		case 1:
			return formatter.printf("[%s]", self.entries[0])!;
		default:
			usz n = formatter.print("[")!;
			foreach (i, element : self.entries[:self.size])
			{
				if (i != 0) formatter.print(", ")!;
				n += formatter.printf("%s", element)!;
			}
			n += formatter.print("]")!;
			return n;
	}
}

fn String AnyList.to_new_string(&self, Allocator allocator = null) @dynamic
{
	return string::format("%s", *self, allocator: allocator ?: allocator::heap());
}


fn String AnyList.to_string(&self, Allocator allocator) @dynamic
{
	return string::format("%s", *self, allocator: allocator);
}

fn String AnyList.to_tstring(&self) => string::tformat("%s", *self);

/**
 * Push an element on the list by cloning it.
 **/
macro void AnyList.push(&self, element)
{
	if (!self.allocator) self.allocator = allocator::heap();
	self.append_internal(allocator::clone(self.allocator, element));
}

fn void AnyList.append_internal(&self, any element) @local
{
	self.ensure_capacity();
	self.entries[self.size++] = element;
}

/**
 * Free a retained element removed using *_retained.
 **/
fn void AnyList.free_element(&self, any element) @inline
{
	allocator::free(self.allocator, element.ptr);
}

/**
 * Pop a value who's type is known. If the type is incorrect, this
 * will still pop the element.
 *
 * @return! CastResult.TYPE_MISMATCH, IteratorResult.NO_MORE_ELEMENT
 **/
macro AnyList.pop(&self, $Type)
{
	if (!self.size) return IteratorResult.NO_MORE_ELEMENT?;
	defer self.free_element(self.entries[self.size]);
	return *anycast(self.entries[--self.size], $Type);
}

/**
 * Pop the last value and allocate the copy using the given allocator.
 * @return! IteratorResult.NO_MORE_ELEMENT
 **/
fn any! AnyList.copy_pop(&self, Allocator allocator = allocator::heap())
{
	if (!self.size) return IteratorResult.NO_MORE_ELEMENT?;
	defer self.free_element(self.entries[self.size]);
	return allocator::clone_any(allocator, self.entries[--self.size]);
}

/**
 * Pop the last value and allocate the copy using the given allocator.
 * @return! IteratorResult.NO_MORE_ELEMENT
 * @deprecated `use copy_pop`
 **/
fn any! AnyList.new_pop(&self, Allocator allocator = allocator::heap())
{
	return self.copy_pop(allocator);
}

/**
 * Pop the last value and allocate the copy using the temp allocator
 * @return! IteratorResult.NO_MORE_ELEMENT
 * @deprecated `use tcopy_pop`
 **/
fn any! AnyList.temp_pop(&self) => self.copy_pop(allocator::temp());

/**
 * Pop the last value and allocate the copy using the temp allocator
 * @return! IteratorResult.NO_MORE_ELEMENT
 **/
fn any! AnyList.tcopy_pop(&self) => self.copy_pop(allocator::temp());

/**
 * Pop the last value. It must later be released using list.free_element()
 * @return! IteratorResult.NO_MORE_ELEMENT
 **/
fn any! AnyList.pop_retained(&self)
{
	if (!self.size) return IteratorResult.NO_MORE_ELEMENT?;
	return self.entries[--self.size];
}

fn void AnyList.clear(&self)
{
	for (usz i = 0; i < self.size; i++)
	{
		self.free_element(self.entries[i]);
	}
	self.size = 0;
}

/**
 * Same as pop() but pops the first value instead.
 **/
macro AnyList.pop_first(&self, $Type)
{
	if (!self.size) return IteratorResult.NO_MORE_ELEMENT?;
	defer self.remove_at(0);
	return *anycast(self.entries[0], $Type);
}

/**
 * Same as pop_retained() but pops the first value instead.
 **/
fn any! AnyList.pop_first_retained(&self)
{
	if (!self.size) return IteratorResult.NO_MORE_ELEMENT?;
	defer self.remove_at(0);
	return self.entries[0];
}

/**
 * Same as new_pop() but pops the first value instead.
 * @deprecated `use copy_pop_first`
 **/
fn any! AnyList.new_pop_first(&self, Allocator allocator = allocator::heap())
{
	return self.copy_pop_first(allocator) @inline;
}

/**
 * Same as new_pop() but pops the first value instead.
 **/
fn any! AnyList.copy_pop_first(&self, Allocator allocator = allocator::heap())
{
	if (!self.size) return IteratorResult.NO_MORE_ELEMENT?;
	defer self.free_element(self.entries[self.size]);
	defer self.remove_at(0);
	return allocator::clone_any(allocator, self.entries[0]);
}

/**
 * Same as temp_pop() but pops the first value instead.
 **/
fn any! AnyList.tcopy_pop_first(&self) => self.copy_pop_first(allocator::temp());

/**
 * Same as temp_pop() but pops the first value instead.
 * @deprecated `use tcopy_pop_first`
 **/
fn any! AnyList.temp_pop_first(&self) => self.new_pop_first(allocator::temp());

/**
 * @require index < self.size
 **/
fn void AnyList.remove_at(&self, usz index)
{
	if (!--self.size || index == self.size) return;
	self.free_element(self.entries[index]);
	self.entries[index .. self.size - 1] = self.entries[index + 1 .. self.size];
}

fn void AnyList.add_all(&self, AnyList* other_list)
{
	if (!other_list.size) return;
	self.reserve(other_list.size);
	foreach (value : other_list)
	{
		self.entries[self.size++] = allocator::clone_any(self.allocator, value);
	}
}

/**
 * Reverse the elements in a list.
 **/
fn void AnyList.reverse(&self)
{
	if (self.size < 2) return;
	usz half = self.size / 2U;
	usz end = self.size - 1;
	for (usz i = 0; i < half; i++)
	{
		self.swap(i, end - i);
	}
}

fn any[] AnyList.array_view(&self)
{
	return self.entries[:self.size];
}

/**
 * Push an element to the front of the list.
 **/
macro void AnyList.push_front(&self, type)
{
	self.insert_at(0, type);
}

/**
 * @require index < self.size
 **/
macro void AnyList.insert_at(&self, usz index, type) @local
{
	any value = allocator::copy(self.allocator, type);
	self.insert_at_internal(self, index, value);
}

/**
 * @require index < self.size
 **/
fn void AnyList.insert_at_internal(&self, usz index, any value) @local
{
	self.ensure_capacity();
	for (usz i = self.size; i > index; i--)
	{
		self.entries[i] = self.entries[i - 1];
	}
	self.size++;
	self.entries[index] = value;
}


/**
 * @require self.size > 0
 **/
fn void AnyList.remove_last(&self)
{
	self.free_element(self.entries[--self.size]);
}

/**
 * @require self.size > 0
 **/
fn void AnyList.remove_first(&self)
{
	self.remove_at(0);
}

macro AnyList.first(&self, $Type)
{
	return *anycast(self.first_any(), $Type);
}

fn any! AnyList.first_any(&self) @inline
{
	return self.size ? self.entries[0] : IteratorResult.NO_MORE_ELEMENT?;
}

macro AnyList.last(&self, $Type)
{
	return *anycast(self.last_any(), $Type);
}

fn any! AnyList.last_any(&self) @inline
{
	return self.size ? self.entries[self.size - 1] : IteratorResult.NO_MORE_ELEMENT?;
}

fn bool AnyList.is_empty(&self) @inline
{
	return !self.size;
}

fn usz AnyList.len(&self) @operator(len) @inline
{
	return self.size;
}

/**
 * @require index < self.size "Index out of range"
 **/
macro AnyList.get(&self, usz index, $Type)
{
	return *anycast(self.entries[index], $Type);
}

/**
 * @require index < self.size "Index out of range"
 **/
fn any AnyList.get_any(&self, usz index) @inline
{
	return self.entries[index];
}

fn void AnyList.free(&self)
{
	if (!self.allocator) return;
	self.clear();
	allocator::free(self.allocator, self.entries);
	self.capacity = 0;
	self.entries = null;
}

fn void AnyList.swap(&self, usz i, usz j)
{
	any temp = self.entries[i];
	self.entries[i] = self.entries[j];
	self.entries[j] = temp;
}

/**
 * @param filter "The function to determine if it should be removed or not"
 * @return "the number of deleted elements"
 **/
fn usz AnyList.remove_if(&self, AnyPredicate filter)
{
	return self._remove_if(filter, false);
}

/**
 * @param selection "The function to determine if it should be kept or not"
 * @return "the number of deleted elements"
 **/
fn usz AnyList.retain_if(&self, AnyPredicate selection)
{
	return self._remove_if(selection, true);
}

macro usz AnyList._remove_if(&self, AnyPredicate filter, bool $invert) @local
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
		for (usz j = i; j < k; j++) self.free_element(self.entries[j]);
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

fn usz AnyList.remove_using_test(&self, AnyTest filter, any context)
{
	return self._remove_using_test(filter, false, context);
}

fn usz AnyList.retain_using_test(&self, AnyTest filter, any context)
{
	return self._remove_using_test(filter, true, context);
}

macro usz AnyList._remove_using_test(&self, AnyTest filter, bool $invert, ctx) @local
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
		for (usz j = i; j < k; j++) self.free_element(self.entries[j]);
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

/**
 * Reserve at least min_capacity
 **/
fn void AnyList.reserve(&self, usz min_capacity)
{
	if (!min_capacity) return;
	if (self.capacity >= min_capacity) return;
	if (!self.allocator) self.allocator = allocator::heap();
	min_capacity = math::next_power_of_2(min_capacity);
	self.entries = allocator::realloc(self.allocator, self.entries, any.sizeof * min_capacity);
	self.capacity = min_capacity;
}

macro any AnyList.@item_at(&self, usz index) @operator([])
{
	return self.entries[index];
}

/**
 * @require index <= self.size "Index out of range"
 **/
macro void AnyList.set(&self, usz index, value)
{
	if (index == self.size)
	{
		self.push(value);
		return;
	}
	self.free_element(self.entries[index]);
	self.entries[index] = allocator::copy(self.allocator, value);
}

fn void AnyList.ensure_capacity(&self, usz added = 1) @inline @private
{
	usz new_size = self.size + added;
	if (self.capacity >= new_size) return;

	assert(new_size < usz.max / 2U);
	usz new_capacity = self.capacity ? 2U * self.capacity : 16U;
	while (new_capacity < new_size) new_capacity *= 2U;
	self.reserve(new_capacity);
}
