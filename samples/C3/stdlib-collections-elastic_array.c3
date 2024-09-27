// Source: https://github.com/c3lang/c3c/blob/master/lib/std/collections/elastic_array.c3
// Permalink: https://github.com/c3lang/c3c/blob/ed5d338/lib/std/collections/elastic_array.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

// Copyright (c) 2021-2024 Christoffer Lerno. All rights reserved.
// Use of self source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
/**
 * @require MAX_SIZE >= 1 `The size must be at least 1 element big.`
 **/
module std::collections::elastic_array(<Type, MAX_SIZE>);
import std::io, std::math, std::collections::list_common;

def ElementPredicate = fn bool(Type *type);
def ElementTest = fn bool(Type *type, any context);
const ELEMENT_IS_EQUATABLE = types::is_equatable_type(Type);
const ELEMENT_IS_POINTER = Type.kindof == POINTER;
macro type_is_overaligned() => Type.alignof > mem::DEFAULT_MEM_ALIGNMENT;

struct ElasticArray (Printable)
{
	usz size;
	Type[MAX_SIZE] entries;
}

fn usz! ElasticArray.to_format(&self, Formatter* formatter) @dynamic
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

fn String ElasticArray.to_string(&self, Allocator allocator) @dynamic
{
	return string::format("%s", *self, allocator: allocator);
}

fn String ElasticArray.to_new_string(&self, Allocator allocator = nul) @dynamic
{
	return string::format("%s", *self, allocator: allocator ?: allocator::heap());
}

fn String ElasticArray.to_tstring(&self)
{
	return string::tformat("%s", *self);
}

fn void! ElasticArray.push_try(&self, Type element) @inline
{
	if (self.size == MAX_SIZE) return AllocationFailure.OUT_OF_MEMORY?;
	self.entries[self.size++] = element;
}

/**
 * @require self.size < MAX_SIZE `Tried to exceed the max size`
 **/
fn void ElasticArray.push(&self, Type element) @inline
{
	self.entries[self.size++] = element;
}

fn Type! ElasticArray.pop(&self)
{
	if (!self.size) return IteratorResult.NO_MORE_ELEMENT?;
	return self.entries[--self.size];
}

fn void ElasticArray.clear(&self)
{
	self.size = 0;
}

/**
 * @require self.size > 0
 **/
fn Type! ElasticArray.pop_first(&self)
{
	if (!self.size) return IteratorResult.NO_MORE_ELEMENT?;
	defer self.remove_at(0);
	return self.entries[0];
}

/**
 * @require index < self.size
 **/
fn void ElasticArray.remove_at(&self, usz index)
{
	if (!--self.size || index == self.size) return;
	self.entries[index .. self.size - 1] = self.entries[index + 1 .. self.size];
}

/**
 * @require other_list.size + self.size <= MAX_SIZE
 **/
fn void ElasticArray.add_all(&self, ElasticArray* other_list)
{
	if (!other_list.size) return;
	foreach (&value : other_list)
	{
		self.entries[self.size++] = *value;
	}
}

/**
 * Add as many elements as possible to the new array,
 * returning the number of elements that didn't fit.
 **/
fn usz ElasticArray.add_all_to_limit(&self, ElasticArray* other_list)
{
	if (!other_list.size) return 0;
	foreach (i, &value : other_list)
	{
		if (self.size == MAX_SIZE) return other_list.size - i;
		self.entries[self.size++] = *value;
	}
	return 0;
}

/**
 * Add as many values from this array as possible, returning the
 * number of elements that didn't fit.
 *
 * @param [in] array
 **/
fn usz ElasticArray.add_array_to_limit(&self, Type[] array)
{
	if (!array.len) return 0;
	foreach (i, &value : array)
	{
		if (self.size == MAX_SIZE) return array.len - i;
		self.entries[self.size++] = *value;
	}
	return 0;
}

/**
 * Add the values of an array to this list.
 *
 * @param [in] array
 * @require array.len + self.size <= MAX_SIZE `Size would exceed max.`
 * @ensure self.size >= array.len
 **/
fn void ElasticArray.add_array(&self, Type[] array)
{
	if (!array.len) return;
	foreach (&value : array)
	{
		self.entries[self.size++] = *value;
	}
}



/**
 * IMPORTANT The returned array must be freed using free_aligned.
 **/
fn Type[] ElasticArray.to_new_aligned_array(&self)
{
	return list_common::list_to_new_aligned_array(Type, self, allocator::heap());
}

/**
 * IMPORTANT The returned array must be freed using free_aligned.
 **/
fn Type[] ElasticArray.to_aligned_array(&self, Allocator allocator)
{
	return list_common::list_to_new_aligned_array(Type, self, allocator);
}

/**
 * @require !type_is_overaligned() : "This function is not available on overaligned types"
 **/
macro Type[] ElasticArray.to_new_array(&self)
{
	return list_common::list_to_array(Type, self, allocator::heap());
}

/**
 * @require !type_is_overaligned() : "This function is not available on overaligned types"
 **/
macro Type[] ElasticArray.to_array(&self, Allocator allocator)
{
	return list_common::list_to_new_array(Type, self, allocator);
}

fn Type[] ElasticArray.to_tarray(&self)
{
	$if type_is_overaligned():
	return self.to_aligned_array(allocator::temp());
	$else
	return self.to_array(allocator::temp());
	$endif;
}

/**
 * Reverse the elements in a list.
 **/
fn void ElasticArray.reverse(&self)
{
	list_common::list_reverse(self);
}

fn Type[] ElasticArray.array_view(&self)
{
	return self.entries[:self.size];
}

/**
 * @require self.size < MAX_SIZE `List would exceed max size`
 **/
fn void ElasticArray.push_front(&self, Type type) @inline
{
	self.insert_at(0, type);
}

/**
 * @require self.size < MAX_SIZE `List would exceed max size`
 **/
fn void! ElasticArray.push_front_try(&self, Type type) @inline
{
	return self.insert_at_try(0, type);
}

/**
 * @require index <= self.size
 **/
fn void! ElasticArray.insert_at_try(&self, usz index, Type value)
{
	if (self.size == MAX_SIZE) return AllocationFailure.OUT_OF_MEMORY?;
	self.insert_at(index, value);
}

/**
 * @require self.size < MAX_SIZE `List would exceed max size`
 * @require index <= self.size
 **/
fn void ElasticArray.insert_at(&self, usz index, Type type)
{
	for (usz i = self.size; i > index; i--)
	{
		self.entries[i] = self.entries[i - 1];
	}
	self.size++;
	self.entries[index] = type;
}

/**
 * @require index < self.size
 **/
fn void ElasticArray.set_at(&self, usz index, Type type)
{
	self.entries[index] = type;
}

fn void! ElasticArray.remove_last(&self) @maydiscard
{
	if (!self.size) return IteratorResult.NO_MORE_ELEMENT?;
	self.size--;
}

fn void! ElasticArray.remove_first(&self) @maydiscard
{
	if (!self.size) return IteratorResult.NO_MORE_ELEMENT?;
	self.remove_at(0);
}

fn Type! ElasticArray.first(&self)
{
	if (!self.size) return IteratorResult.NO_MORE_ELEMENT?;
	return self.entries[0];
}

fn Type! ElasticArray.last(&self)
{
	if (!self.size) return IteratorResult.NO_MORE_ELEMENT?;
	return self.entries[self.size - 1];
}

fn bool ElasticArray.is_empty(&self) @inline
{
	return !self.size;
}

fn usz ElasticArray.byte_size(&self) @inline
{
	return Type.sizeof * self.size;
}

fn usz ElasticArray.len(&self) @operator(len) @inline
{
	return self.size;
}

fn Type ElasticArray.get(&self, usz index) @inline
{
	return self.entries[index];
}

fn void ElasticArray.swap(&self, usz i, usz j)
{
	@swap(self.entries[i], self.entries[j]);
}

/**
 * @param filter "The function to determine if it should be removed or not"
 * @return "the number of deleted elements"
 **/
fn usz ElasticArray.remove_if(&self, ElementPredicate filter)
{
	return list_common::list_remove_if(self, filter, false);
}

/**
 * @param selection "The function to determine if it should be kept or not"
 * @return "the number of deleted elements"
 **/
fn usz ElasticArray.retain_if(&self, ElementPredicate selection)
{
	return list_common::list_remove_if(self, selection, true);
}

fn usz ElasticArray.remove_using_test(&self, ElementTest filter, any context)
{
	return list_common::list_remove_using_test(self, filter, false, context);
}

fn usz ElasticArray.retain_using_test(&self, ElementTest filter, any context)
{
	return list_common::list_remove_using_test(self, filter, true, context);
}


macro Type ElasticArray.@item_at(&self, usz index) @operator([])
{
	return self.entries[index];
}

fn Type* ElasticArray.get_ref(&self, usz index) @operator(&[]) @inline
{
	return &self.entries[index];
}

fn void ElasticArray.set(&self, usz index, Type value) @operator([]=)
{
	self.entries[index] = value;
}


// Functions for equatable types
fn usz! ElasticArray.index_of(&self, Type type) @if(ELEMENT_IS_EQUATABLE)
{
	foreach (i, v : self)
	{
		if (equals(v, type)) return i;
	}
	return SearchResult.MISSING?;
}

fn usz! ElasticArray.rindex_of(&self, Type type) @if(ELEMENT_IS_EQUATABLE)
{
	foreach_r (i, v : self)
	{
		if (equals(v, type)) return i;
	}
	return SearchResult.MISSING?;
}

fn bool ElasticArray.equals(&self, ElasticArray other_list) @if(ELEMENT_IS_EQUATABLE)
{
	if (self.size != other_list.size) return false;
	foreach (i, v : self)
	{
		if (!equals(v, other_list.entries[i])) return false;
	}
	return true;
}

/**
 * Check for presence of a value in a list.
 *
 * @param [&in] self "the list to find elements in"
 * @param value "The value to search for"
 * @return "True if the value is found, false otherwise"
 **/
fn bool ElasticArray.contains(&self, Type value) @if(ELEMENT_IS_EQUATABLE)
{
	foreach (i, v : self)
	{
		if (equals(v, value)) return true;
	}
	return false;
}

/**
 * @param [&inout] self "The list to remove elements from"
 * @param value "The value to remove"
 * @return "true if the value was found"
 **/
fn bool ElasticArray.remove_last_item(&self, Type value) @if(ELEMENT_IS_EQUATABLE)
{
	return @ok(self.remove_at(self.rindex_of(value)));
}

/**
 * @param [&inout] self "The list to remove elements from"
 * @param value "The value to remove"
 * @return "true if the value was found"
 **/
fn bool ElasticArray.remove_first_item(&self, Type value) @if(ELEMENT_IS_EQUATABLE)
{
	return @ok(self.remove_at(self.index_of(value)));
}

/**
 * @param [&inout] self "The list to remove elements from"
 * @param value "The value to remove"
 * @return "the number of deleted elements."
 **/
fn usz ElasticArray.remove_item(&self, Type value) @if(ELEMENT_IS_EQUATABLE)
{
	return list_common::list_remove_item(self, value);
}



fn void ElasticArray.remove_all_from(&self, ElasticArray* other_list) @if(ELEMENT_IS_EQUATABLE)
{
	if (!other_list.size) return;
	foreach (v : other_list) self.remove_item(v);
}

/**
 * @param [&in] self
 * @return "The number non-null values in the list"
 **/
fn usz ElasticArray.compact_count(&self) @if(ELEMENT_IS_POINTER)
{
	usz vals = 0;
	foreach (v : self) if (v) vals++;
	return vals;
}

fn usz ElasticArray.compact(&self) @if(ELEMENT_IS_POINTER)
{
	return list_common::list_compact(self);
}