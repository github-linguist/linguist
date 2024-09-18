// Source: https://github.com/c3lang/c3c/blob/master/lib/std/collections/map.c3
// Permalink: https://github.com/c3lang/c3c/blob/840b3b3/lib/std/collections/map.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

// Copyright (c) 2023 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::collections::map(<Key, Value>);
import std::math;

const uint DEFAULT_INITIAL_CAPACITY = 16;
const uint MAXIMUM_CAPACITY = 1u << 31;
const float DEFAULT_LOAD_FACTOR = 0.75;
const VALUE_IS_EQUATABLE = Value.is_eq;
const bool COPY_KEYS = types::implements_copy(Key);

distinct Map = void*;

struct MapImpl
{
	Entry*[] table;
	Allocator allocator;
	uint count; // Number of elements
	uint threshold; // Resize limit
	float load_factor;
}

/**
 * @require capacity > 0 "The capacity must be 1 or higher"
 * @require load_factor > 0.0 "The load factor must be higher than 0"
 * @require capacity < MAXIMUM_CAPACITY "Capacity cannot exceed maximum"
 **/
fn Map new(uint capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR, Allocator allocator = allocator::heap())
{
	MapImpl* map = allocator::alloc(allocator, MapImpl);
	_init(map, capacity, load_factor, allocator);
	return (Map)map;
}

/**
 * @require capacity > 0 "The capacity must be 1 or higher"
 * @require load_factor > 0.0 "The load factor must be higher than 0"
 * @require capacity < MAXIMUM_CAPACITY "Capacity cannot exceed maximum"
 **/
fn Map temp(uint capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
	MapImpl* map = mem::temp_alloc(MapImpl);
	_init(map, capacity, load_factor, allocator::temp());
	return (Map)map;
}

/**
 * @param [&inout] allocator "The allocator to use"
 * @require $vacount % 2 == 0 "There must be an even number of arguments provided for keys and values"
 * @require capacity > 0 "The capacity must be 1 or higher"
 * @require load_factor > 0.0 "The load factor must be higher than 0"
 * @require capacity < MAXIMUM_CAPACITY "Capacity cannot exceed maximum"
 **/
macro Map new_init_with_key_values(..., uint capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR, Allocator allocator = allocator::heap())
{
	Map map = new(capacity, load_factor, allocator);
	$for (var $i = 0; $i < $vacount; $i += 2)
		map.set($vaarg[$i], $vaarg[$i+1]);
	$endfor
	return map;
}

/**
 * @param [in] keys "Array of keys for the Map entries"
 * @param [in] values "Array of values for the Map entries"
 * @param [&inout] allocator "The allocator to use"
 * @require keys.len == values.len "Both keys and values arrays must be the same length"
 * @require capacity > 0 "The capacity must be 1 or higher"
 * @require load_factor > 0.0 "The load factor must be higher than 0"
 * @require capacity < MAXIMUM_CAPACITY "Capacity cannot exceed maximum"
 **/
fn Map new_init_from_keys_and_values(Key[] keys, Value[] values, uint capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR, Allocator allocator = allocator::heap())
{
	assert(keys.len == values.len);
	Map map = new(capacity, load_factor, allocator);
	for (usz i = 0; i < keys.len; i++)
	{
		map.set(keys[i], values[i]);
	}
	return map;
}

/**
 * @require $vacount % 2 == 0 "There must be an even number of arguments provided for keys and values"
 * @require capacity > 0 "The capacity must be 1 or higher"
 * @require load_factor > 0.0 "The load factor must be higher than 0"
 * @require capacity < MAXIMUM_CAPACITY "Capacity cannot exceed maximum"
 **/
macro Map temp_new_with_key_values(..., uint capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
	Map map = temp(capacity, load_factor);
	$for (var $i = 0; $i < $vacount; $i += 2)
		map.set($vaarg[$i], $vaarg[$i+1]);
	$endfor
	return map;
}

/**
 * @param [in] keys "The keys for the HashMap entries"
 * @param [in] values "The values for the HashMap entries"
 * @param [&inout] allocator "The allocator to use"
 * @require keys.len == values.len "Both keys and values arrays must be the same length"
 * @require capacity > 0 "The capacity must be 1 or higher"
 * @require load_factor > 0.0 "The load factor must be higher than 0"
 * @require capacity < MAXIMUM_CAPACITY "Capacity cannot exceed maximum"
 **/
fn Map temp_init_from_keys_and_values(Key[] keys, Value[] values, uint capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR, Allocator allocator = allocator::heap())
{
	assert(keys.len == values.len);
	Map map = temp(capacity, load_factor);
	for (usz i = 0; i < keys.len; i++)
	{
		map.set(keys[i], values[i]);
	}
	return map;
}

/**
 * @param [&in] other_map "The map to copy from."
 **/
fn Map new_from_map(Map other_map, Allocator allocator = null)
{
	MapImpl* other_map_impl = (MapImpl*)other_map;
	if (!other_map_impl)
	{
		if (allocator) return new(allocator: allocator);
		return null;
	}
	MapImpl* map = (MapImpl*)new(other_map_impl.table.len, other_map_impl.load_factor, allocator ?: allocator::heap());
	if (!other_map_impl.count) return (Map)map;
	foreach (Entry *e : other_map_impl.table)
    {
    	if (!e) continue;
    	map._put_for_create(e.key, e.value);
    }
	return (Map)map;
}

/**
 * @param [&in] other_map "The map to copy from."
 **/
fn Map temp_from_map(Map other_map)
{
	return new_from_map(other_map, allocator::temp());
}

fn bool Map.is_empty(map) @inline
{
	return !map || !((MapImpl*)map).count;
}

fn usz Map.len(map) @inline
{
	return map ? ((MapImpl*)map).count : 0;
}

fn Value*! Map.get_ref(self, Key key)
{
	MapImpl *map = (MapImpl*)self;
	if (!map || !map.count) return SearchResult.MISSING?;
	uint hash = rehash(key.hash());
	for (Entry *e = map.table[index_for(hash, map.table.len)]; e != null; e = e.next)
	{
		if (e.hash == hash && equals(key, e.key)) return &e.value;
	}
	return SearchResult.MISSING?;
}

fn Entry*! Map.get_entry(map, Key key)
{
	MapImpl *map_impl = (MapImpl*)map;
	if (!map_impl || !map_impl.count) return SearchResult.MISSING?;
	uint hash = rehash(key.hash());
	for (Entry *e = map_impl.table[index_for(hash, map_impl.table.len)]; e != null; e = e.next)
	{
		if (e.hash == hash && equals(key, e.key)) return e;
	}
	return SearchResult.MISSING?;
}

/**
 * Get the value or update and
 * @require $assignable(#expr, Value)
 **/
macro Value Map.@get_or_set(&self, Key key, Value #expr)
{
	MapImpl *map = (MapImpl*)*self;
	if (!map || !map.count)
	{
		Value val = #expr;
		map.set(key, val);
		return val;
	}
	uint hash = rehash(key.hash());
	uint index = index_for(hash, map.table.len);
	for (Entry *e = map.table[index]; e != null; e = e.next)
	{
		if (e.hash == hash && equals(key, e.key)) return e.value;
	}
	Value val = #expr;
	map.add_entry(hash, key, val, index);
	return val;
}

fn Value! Map.get(map, Key key) @operator([])
{
	return *map.get_ref(key) @inline;
}

fn bool Map.has_key(map, Key key)
{
	return @ok(map.get_ref(key));
}

macro Value Map.set_value_return(&map, Key key, Value value) @operator([]=)
{
	map.set(key, value);
	return value;
}

fn bool Map.set(&self, Key key, Value value)
{
	// If the map isn't initialized, use the defaults to initialize it.
	if (!*self) *self = new();
	MapImpl* map = (MapImpl*)*self;
	uint hash = rehash(key.hash());
	uint index = index_for(hash, map.table.len);
	for (Entry *e = map.table[index]; e != null; e = e.next)
	{
		if (e.hash == hash && equals(key, e.key))
		{
			e.value = value;
			return true;
		}
	}
	map._add_entry(hash, key, value, index);
	return false;
}

fn void! Map.remove(map, Key key) @maydiscard
{
	if (!map || !((MapImpl*)map)._remove_entry_for_key(key)) return SearchResult.MISSING?;
}

fn void Map.clear(self)
{
	MapImpl* map = (MapImpl*)self;
	if (!map || !map.count) return;
	foreach (Entry** &entry_ref : map.table)
	{
		Entry* entry = *entry_ref;
		if (!entry) continue;
		Entry *next = entry.next;
		while (next)
		{
			Entry *to_delete = next;
			next = next.next;
			map._free_entry(to_delete);
		}
		map._free_entry(entry);
		*entry_ref = null;
	}
	map.count = 0;
}

fn void Map.free(self)
{
	if (!self) return;
	MapImpl* map = (MapImpl*)self;
	self.clear();
	map._free_internal(map.table.ptr);
	map.table = {};
	allocator::free(map.allocator, map);
}

fn Key[] Map.temp_keys_list(map)
{
	return map.new_keys_list(allocator::temp()) @inline;
}

fn Key[] Map.new_keys_list(self, Allocator allocator = allocator::heap())
{
	MapImpl* map = (MapImpl*)self;
	if (!map || !map.count) return {};

	Key[] list = allocator::alloc_array(allocator, Key, map.count);
	usz index = 0;
	foreach (Entry* entry : map.table)
	{
		while (entry)
		{
			list[index++] = entry.key;
			entry = entry.next;
		}
	}
	return list;
}

macro Map.@each(map; @body(key, value))
{
	map.@each_entry(; Entry* entry) {
		@body(entry.key, entry.value);
	};
}

macro Map.@each_entry(self; @body(entry))
{
	MapImpl *map = (MapImpl*)self;
	if (!map || !map.count) return;
	foreach (Entry* entry : map.table)
	{
		while (entry)
		{
			@body(entry);
			entry = entry.next;
		}
	}
}

fn Value[] Map.temp_values_list(map)
{
	return map.new_values_list(allocator::temp()) @inline;
}

fn Value[] Map.new_values_list(self, Allocator allocator = allocator::heap())
{
	MapImpl* map = (MapImpl*)self;
	if (!map || !map.count) return {};
	Value[] list = allocator::alloc_array(allocator, Value, map.count);
	usz index = 0;
	foreach (Entry* entry : map.table)
	{
		while (entry)
		{
			list[index++] = entry.value;
			entry = entry.next;
		}
	}
	return list;
}

fn bool Map.has_value(self, Value v) @if(VALUE_IS_EQUATABLE)
{
	MapImpl* map = (MapImpl*)self;
	if (!map || !map.count) return false;
	foreach (Entry* entry : map.table)
	{
		while (entry)
		{
			if (equals(v, entry.value)) return true;
			entry = entry.next;
		}
	}
	return false;
}

// --- private methods

fn void MapImpl._add_entry(&map, uint hash, Key key, Value value, uint bucket_index) @private
{
	$if COPY_KEYS:
	key = key.copy(map.allocator);
	$endif
	Entry* entry = allocator::new(map.allocator, Entry, { .hash = hash, .key = key, .value = value, .next = map.table[bucket_index] });
	map.table[bucket_index] = entry;
	if (map.count++ >= map.threshold)
	{
		map._resize(map.table.len * 2);
	}
}

fn void MapImpl._resize(&map, uint new_capacity) @private
{
	Entry*[] old_table = map.table;
	uint old_capacity = old_table.len;
	if (old_capacity == MAXIMUM_CAPACITY)
	{
		map.threshold = uint.max;
		return;
	}
	Entry*[] new_table = allocator::new_array(map.allocator, Entry*, new_capacity);
	map._transfer(new_table);
	map.table = new_table;
	map._free_internal(old_table.ptr);
	map.threshold = (uint)(new_capacity * map.load_factor);
}

fn uint rehash(uint hash) @inline @private
{
	hash ^= (hash >> 20) ^ (hash >> 12);
	return hash ^ ((hash >> 7) ^ (hash >> 4));
}

macro uint index_for(uint hash, uint capacity) @private
{
	return hash & (capacity - 1);
}

fn void MapImpl._transfer(&map, Entry*[] new_table) @private
{
	Entry*[] src = map.table;
	uint new_capacity = new_table.len;
	foreach (uint j, Entry *e : src)
	{
		if (!e) continue;
		do
		{
			Entry* next = e.next;
			uint i = index_for(e.hash, new_capacity);
			e.next = new_table[i];
			new_table[i] = e;
			e = next;
		}
		while (e);
	}
}

fn void _init(MapImpl* impl, uint capacity, float load_factor, Allocator allocator) @private
{
	capacity = math::next_power_of_2(capacity);
	*impl = {
		.allocator = allocator,
    	.load_factor = load_factor,
    	.threshold = (uint)(capacity * load_factor),
    	.table = allocator::new_array(allocator, Entry*, capacity)
	};
}

fn void MapImpl._put_for_create(&map, Key key, Value value) @private
{
	uint hash = rehash(key.hash());
	uint i = index_for(hash, map.table.len);
	for (Entry *e = map.table[i]; e != null; e = e.next)
	{
		if (e.hash == hash && equals(key, e.key))
		{
			e.value = value;
			return;
		}
	}
	map._create_entry(hash, key, value, i);
}

fn void MapImpl._free_internal(&map, void* ptr) @inline @private
{
	allocator::free(map.allocator, ptr);
}

fn bool MapImpl._remove_entry_for_key(&map, Key key) @private
{
	if (!map.count) return false;
	uint hash = rehash(key.hash());
	uint i = index_for(hash, map.table.len);
	Entry* prev = map.table[i];
	Entry* e = prev;
	while (e)
	{
		Entry *next = e.next;
		if (e.hash == hash && equals(key, e.key))
		{
			map.count--;
			if (prev == e)
			{
				map.table[i] = next;
			}
			else
			{
				prev.next = next;
			}
			map._free_entry(e);
			return true;
		}
		prev = e;
		e = next;
	}
	return false;
}

fn void MapImpl._create_entry(&map, uint hash, Key key, Value value, int bucket_index) @private
{
	Entry *e = map.table[bucket_index];
	$if COPY_KEYS:
	key = key.copy(map.allocator);
	$endif
	Entry* entry = allocator::new(map.allocator, Entry, { .hash = hash, .key = key, .value = value, .next = map.table[bucket_index] });
	map.table[bucket_index] = entry;
	map.count++;
}

fn void MapImpl._free_entry(&self, Entry *entry) @local
{
	$if COPY_KEYS:
	allocator::free(self.allocator, entry.key);
	$endif
	self._free_internal(entry);
}

struct Entry
{
	uint hash;
	Key key;
	Value value;
	Entry* next;
}
