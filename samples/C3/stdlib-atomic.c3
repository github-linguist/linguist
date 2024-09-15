// Source: https://github.com/c3lang/c3c/blob/master/lib/std/atomic.c3
// Permalink: https://github.com/c3lang/c3c/blob/e293c43/lib/std/atomic.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

// Copyright (c) 2023 Eduardo José Gómez Hernández. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::atomic::types(<Type>);

struct Atomic
{
	Type data;
}

/**
 * Loads data atomically, by default this uses SEQ_CONSISTENT ordering.
 *
 * @param ordering "The ordering, cannot be release or acquire-release."
 * @require ordering != RELEASE && ordering != ACQUIRE_RELEASE : "Release and acquire-release are not valid for load"
 **/
macro Type Atomic.load(&self, AtomicOrdering ordering = SEQ_CONSISTENT)
{
	Type* data = &self.data;
	switch(ordering)
	{
		case NOT_ATOMIC: return $$atomic_load(data, false, AtomicOrdering.NOT_ATOMIC.ordinal);
		case UNORDERED: return $$atomic_load(data, false, AtomicOrdering.UNORDERED.ordinal);
		case RELAXED: return $$atomic_load(data, false, AtomicOrdering.RELAXED.ordinal);
		case ACQUIRE: return $$atomic_load(data, false, AtomicOrdering.ACQUIRE.ordinal);
		case SEQ_CONSISTENT: return $$atomic_load(data, false, AtomicOrdering.SEQ_CONSISTENT.ordinal);
		case ACQUIRE_RELEASE:
		case RELEASE: unreachable("Invalid ordering.");
	}
}
/**
 * Stores data atomically, by default this uses SEQ_CONSISTENT ordering.
 *
 * @param ordering "The ordering, cannot be acquire or acquire-release."
 * @require ordering != ACQUIRE && ordering != ACQUIRE_RELEASE : "Acquire and acquire-release are not valid for store"
 **/
macro void Atomic.store(&self, Type value, AtomicOrdering ordering = SEQ_CONSISTENT)
{
	Type* data = &self.data;
	switch(ordering) 
	{
		case NOT_ATOMIC: $$atomic_store(data, value, false, AtomicOrdering.NOT_ATOMIC.ordinal);
		case UNORDERED: $$atomic_store(data, value, false, AtomicOrdering.UNORDERED.ordinal);
		case RELAXED: $$atomic_store(data, value, false, AtomicOrdering.RELAXED.ordinal);
		case RELEASE: $$atomic_store(data, value, false, AtomicOrdering.RELEASE.ordinal);
		case SEQ_CONSISTENT: $$atomic_store(data, value, false, AtomicOrdering.SEQ_CONSISTENT.ordinal);
		case ACQUIRE_RELEASE:
		case ACQUIRE: unreachable("Invalid ordering.");
	}
}

macro Type Atomic.add(&self, Type value, AtomicOrdering ordering = SEQ_CONSISTENT)
{
	Type* data = &self.data;
	return @atomic_exec(atomic::fetch_add, data, value, ordering);
}

macro Type Atomic.sub(&self, Type value, AtomicOrdering ordering = SEQ_CONSISTENT)
{
	Type* data = &self.data;
	return @atomic_exec(atomic::fetch_sub, data, value, ordering);
}

macro Type Atomic.mul(&self, Type value, AtomicOrdering ordering = SEQ_CONSISTENT)
{
	Type* data = &self.data;
	return @atomic_exec(atomic::fetch_mul, data, value, ordering);
}

macro Type Atomic.div(&self, Type value, AtomicOrdering ordering = SEQ_CONSISTENT)
{
	Type* data = &self.data;
	return @atomic_exec(atomic::fetch_div, data, value, ordering);
}

macro Type Atomic.max(&self, Type value, AtomicOrdering ordering = SEQ_CONSISTENT)
{
	Type* data = &self.data;
	return @atomic_exec(atomic::fetch_div, data, value, ordering);
}

macro Type Atomic.min(&self, Type value, AtomicOrdering ordering = SEQ_CONSISTENT)
{
	Type* data = &self.data;
	return @atomic_exec(atomic::fetch_min, data, value, ordering);
}

macro Type Atomic.or(&self, uint value, AtomicOrdering ordering = SEQ_CONSISTENT) @if(!types::is_float(Type))
{
	Type* data = &self.data;
	return @atomic_exec(atomic::fetch_or, data, value, ordering);
}

fn Type Atomic.xor(&self, uint value, AtomicOrdering ordering = SEQ_CONSISTENT) @if(!types::is_float(Type))
{
	Type* data = &self.data;
	return @atomic_exec(atomic::fetch_xor, data, value, ordering);
}

macro Type Atomic.and(&self, uint value, AtomicOrdering ordering = SEQ_CONSISTENT) @if(!types::is_float(Type))
{
	Type* data = &self.data;
	return @atomic_exec(atomic::fetch_and, data, value, ordering);
}

macro Type Atomic.shift_right(&self, uint amount, AtomicOrdering ordering = SEQ_CONSISTENT) @if(!types::is_float(Type))
{
	Type* data = &self.data;
	return @atomic_exec(atomic::fetch_shift_right, data, amount, ordering);
}

macro Type Atomic.shift_left(&self, uint amount, AtomicOrdering ordering = SEQ_CONSISTENT) @if(!types::is_float(Type))
{
	Type* data = &self.data;
	return @atomic_exec(atomic::fetch_shift_left, data, amount, ordering);
}

macro @atomic_exec(#func, data, value, ordering) @local
{
	switch(ordering) 
	{
		case RELAXED: return #func(data, value, RELAXED);
		case ACQUIRE: return #func(data, value, ACQUIRE);
		case RELEASE: return #func(data, value, RELEASE);
		case ACQUIRE_RELEASE: return #func(data, value, ACQUIRE_RELEASE);
		case SEQ_CONSISTENT: return #func(data, value, SEQ_CONSISTENT);
		default: unreachable("Ordering may not be non-atomic or unordered.");
	}
}

module std::atomic;
import std::math;

/**
 * @param [&in] ptr "the variable or dereferenced pointer to the data."
 * @param [in] y "the value to be added to ptr."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @return "returns the old value of ptr"
 *
 * @require !$alignment || math::is_power_of_2($alignment) "Alignment must be a power of two."
 * @require types::is_int($typeof(*ptr)) || types::is_float($typeof(*ptr)) "Only integer/float pointers may be used."
 * @require $ordering != AtomicOrdering.NOT_ATOMIC && $ordering != AtomicOrdering.UNORDERED "Acquire ordering is not valid."
 **/
macro fetch_add(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT, bool $volatile = false, usz $alignment = 0)
{
	$if $alignment == 0:
		$alignment = $typeof(*ptr).sizeof;
	$endif
	return $$atomic_fetch_add(ptr, y, $volatile, $ordering.ordinal, $alignment);
}

/**
 * @param [&in] ptr "the variable or dereferenced pointer to the data."
 * @param [in] y "the value to be added to ptr."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @return "returns the old value of ptr"
 * 
 * @require !$alignment || math::is_power_of_2($alignment) "Alignment must be a power of two."
 * @require types::is_int($typeof(*ptr)) || types::is_float($typeof(*ptr)) "Only integer/float pointers may be used."
 * @require $ordering != AtomicOrdering.NOT_ATOMIC && $ordering != AtomicOrdering.UNORDERED "Acquire ordering is not valid."
 **/
macro fetch_sub(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT, bool $volatile = false, usz $alignment = 0)
{
	$if $alignment == 0:
		$alignment = $typeof(*ptr).sizeof;
	$endif
	return $$atomic_fetch_sub(ptr, y, $volatile, $ordering.ordinal, $alignment);
}

/**
 * @param [&in] ptr "the variable or dereferenced pointer to the data."
 * @param [in] y "the value to be added to ptr."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @return "returns the old value of ptr"
 * 
 * @require types::is_int($typeof(*ptr)) || types::is_float($typeof(*ptr)) "Only integer/float pointers may be used."
 * @require $ordering != AtomicOrdering.NOT_ATOMIC && $ordering != AtomicOrdering.UNORDERED "Acquire ordering is not valid."
 **/
macro fetch_mul(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	var $load_ordering = $ordering;
	$if $ordering == RELEASE || $ordering == ACQUIRE_RELEASE:
	    $load_ordering = AtomicOrdering.SEQ_CONSISTENT;
	$endif
	
	var $StorageType = $typefrom(types::lower_to_atomic_compatible_type($typeof(*ptr)));

	$StorageType* storage_ptr = ($StorageType*)ptr;

	$typeof(*ptr) old_value;
	$typeof(*ptr) new_value;

	$StorageType storage_old_value;
	$StorageType storage_new_value;


	do {
		storage_old_value = $$atomic_load(storage_ptr, false, $load_ordering.ordinal);
		old_value = bitcast(storage_old_value, $typeof(*ptr));
		new_value = old_value * y;
		storage_new_value = bitcast(new_value, $StorageType);
	} while (mem::compare_exchange(storage_ptr, storage_old_value, storage_new_value, $ordering, $load_ordering) != storage_old_value);

	return old_value;
}

/**
 * @param [&in] ptr "the variable or dereferenced pointer to the data."
 * @param [in] y "the value to be added to ptr."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @return "returns the old value of ptr"
 * 
* @require types::is_int($typeof(*ptr)) || types::is_float($typeof(*ptr)) "Only integer/float pointers may be used."
* @require $ordering != AtomicOrdering.NOT_ATOMIC && $ordering != AtomicOrdering.UNORDERED "Acquire ordering is not valid."
 **/
macro fetch_div(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	var $load_ordering = $ordering;
	$if $ordering == RELEASE || $ordering == ACQUIRE_RELEASE:
	    $load_ordering = AtomicOrdering.SEQ_CONSISTENT;
	$endif
	
	var $StorageType = $typefrom(types::lower_to_atomic_compatible_type($typeof(*ptr)));

	$StorageType* storage_ptr = ($StorageType*)ptr;

	$typeof(*ptr) old_value;
	$typeof(*ptr) new_value;

	$StorageType storage_old_value;
	$StorageType storage_new_value;
	
	do {
		storage_old_value = $$atomic_load(storage_ptr, false, $load_ordering.ordinal);
		old_value = bitcast(storage_old_value, $typeof(*ptr));
		new_value = old_value / y;
		storage_new_value = bitcast(new_value, $StorageType);
	} while (mem::compare_exchange(storage_ptr, storage_old_value, storage_new_value, $ordering, $load_ordering) != storage_old_value);

	return old_value;
}

/**
 * @param [&in] ptr "the variable or dereferenced pointer to the data."
 * @param [in] y "the value to be added to ptr."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @return "returns the old value of ptr"
 * 
 * @require !$alignment || math::is_power_of_2($alignment) "Alignment must be a power of two."
 * @require types::is_int($typeof(*ptr)) "Only integer pointers may be used."
 * @require types::is_int($typeof(y)) "The value for or must be an int"
 * @require $ordering != AtomicOrdering.NOT_ATOMIC && $ordering != AtomicOrdering.UNORDERED "Acquire ordering is not valid."
 **/
macro fetch_or(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT, bool $volatile = false, usz $alignment = 0)
{
	$if	types::is_int($typeof(*ptr)):
		return $$atomic_fetch_or(ptr, y, $volatile, $ordering.ordinal, $alignment);
	$endif

	var $load_ordering = $ordering;
	$if $ordering == RELEASE || $ordering == ACQUIRE_RELEASE:
	    $load_ordering = AtomicOrdering.SEQ_CONSISTENT;
	$endif
	
	var $StorageType = $typefrom(types::lower_to_atomic_compatible_type($typeof(*ptr)));

	$StorageType* storage_ptr = ($StorageType*)ptr;

	$typeof(*ptr) old_value;
	$typeof(*ptr) new_value;

	$StorageType storage_old_value;
	$StorageType storage_new_value;
	$StorageType storage_y = ($StorageType)y;
	
	do {
		storage_old_value = $$atomic_load(storage_ptr, false, $load_ordering.ordinal);
		old_value = bitcast(storage_old_value, $typeof(*ptr));
		new_value = storage_old_value | storage_y;
		storage_new_value = bitcast(new_value, $StorageType);
	} while (mem::compare_exchange(storage_ptr, storage_old_value, storage_new_value, $ordering, $load_ordering) != storage_old_value);

	return old_value;
}

/**
 * @param [&in] ptr "the variable or dereferenced pointer to the data."
 * @param [in] y "the value to be added to ptr."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @return "returns the old value of ptr"
 * 
 * @require !$alignment || math::is_power_of_2($alignment) "Alignment must be a power of two."
 * @require types::is_int($typeof(*ptr)) "Only integer pointers may be used."
 * @require types::is_int($typeof(y)) "The value for or must be an int"
 * @require $ordering != AtomicOrdering.NOT_ATOMIC && $ordering != AtomicOrdering.UNORDERED "Acquire ordering is not valid."
 **/
macro fetch_xor(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT, bool $volatile = false, usz $alignment = 0)
{
	$if	types::is_int($typeof(*ptr)):
		return $$atomic_fetch_xor(ptr, y, $volatile, $ordering.ordinal, $alignment);
	$endif

	var $load_ordering = $ordering;
	$if $ordering == RELEASE || $ordering == ACQUIRE_RELEASE:
	    $load_ordering = AtomicOrdering.SEQ_CONSISTENT;
	$endif
	
	var $StorageType = $typefrom(types::lower_to_atomic_compatible_type($typeof(*ptr)));

	$StorageType* storage_ptr = ($StorageType*)ptr;

	$typeof(*ptr) old_value;
	$typeof(*ptr) new_value;

	$StorageType storage_old_value;
	$StorageType storage_new_value;
	$StorageType storage_y = ($StorageType)y;
	
	do {
		storage_old_value = $$atomic_load(storage_ptr, false, $load_ordering.ordinal);
		old_value = bitcast(storage_old_value, $typeof(*ptr));
		new_value = storage_old_value ^ storage_y;
		storage_new_value = bitcast(new_value, $StorageType);
	} while (mem::compare_exchange(storage_ptr, storage_old_value, storage_new_value, $ordering, $load_ordering) != storage_old_value);

	return old_value;
}

/**
 * @param [&in] ptr "the variable or dereferenced pointer to the data."
 * @param [in] y "the value to be added to ptr."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @return "returns the old value of ptr"
 * 
 * @require !$alignment || math::is_power_of_2($alignment) "Alignment must be a power of two."
 * @require types::is_int($typeof(*ptr)) "Only integer pointers may be used."
 * @require types::is_int($typeof(y)) "The value for or must be an int"
 * @require $ordering != AtomicOrdering.NOT_ATOMIC && $ordering != AtomicOrdering.UNORDERED "Acquire ordering is not valid."
 **/
macro fetch_and(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT, bool $volatile = false, usz $alignment = 0)
{
	$if	types::is_int($typeof(*ptr)):
		return $$atomic_fetch_and(ptr, y, $volatile, $ordering.ordinal, $alignment);
	$endif

	var $load_ordering = $ordering;
	$if $ordering == RELEASE || $ordering == ACQUIRE_RELEASE:
	    $load_ordering = AtomicOrdering.SEQ_CONSISTENT;
	$endif
	
	var $StorageType = $typefrom(types::lower_to_atomic_compatible_type($typeof(*ptr)));

	$StorageType* storage_ptr = ($StorageType*)ptr;

	$typeof(*ptr) old_value;
	$typeof(*ptr) new_value;

	$StorageType storage_old_value;
	$StorageType storage_new_value;
	$StorageType storage_y = ($StorageType)y;
	
	do {
		storage_old_value = $$atomic_load(storage_ptr, false, $load_ordering.ordinal);
		old_value = bitcast(storage_old_value, $typeof(*ptr));
		new_value = storage_old_value & storage_y;
		storage_new_value = bitcast(new_value, $StorageType);
	} while (mem::compare_exchange(storage_ptr, storage_old_value, storage_new_value, $ordering, $load_ordering) != storage_old_value);

	return old_value;
}

/**
 * @param [&in] ptr "the variable or dereferenced pointer to the data."
 * @param [in] y "the value to be added to ptr."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @return "returns the old value of ptr"
 * 
 * @require types::is_int($typeof(*ptr)) "Only integer pointers may be used."
 * @require types::is_int($typeof(y)) "The value for or must be an int"
 * @require $ordering != AtomicOrdering.NOT_ATOMIC && $ordering != AtomicOrdering.UNORDERED "Acquire ordering is not valid."
 **/
macro fetch_shift_right(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	var $load_ordering = $ordering;
	$if $ordering == RELEASE || $ordering == ACQUIRE_RELEASE:
	    $load_ordering = AtomicOrdering.SEQ_CONSISTENT;
	$endif
	
	var $StorageType = $typefrom(types::lower_to_atomic_compatible_type($typeof(*ptr)));

	$StorageType* storage_ptr = ($StorageType*)ptr;

	$typeof(*ptr) old_value;
	$typeof(*ptr) new_value;

	$StorageType storage_old_value;
	$StorageType storage_new_value;
	$StorageType storage_y = ($StorageType)y;
	
	do {
		storage_old_value = $$atomic_load(storage_ptr, false, $load_ordering.ordinal);
		old_value = bitcast(storage_old_value, $typeof(*ptr));
		new_value = storage_old_value >> storage_y;
		storage_new_value = bitcast(new_value, $StorageType);
	} while (mem::compare_exchange(storage_ptr, storage_old_value, storage_new_value, $ordering, $load_ordering) != storage_old_value);

	return old_value;
}

/**
 * @param [&in] ptr "the variable or dereferenced pointer to the data."
 * @param [in] y "the value to be added to ptr."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @return "returns the old value of ptr"
 * 
 * @require types::is_int($typeof(*ptr)) "Only integer pointers may be used."
 * @require types::is_int($typeof(y)) "The value for or must be an int"
 * @require $ordering != AtomicOrdering.NOT_ATOMIC && $ordering != AtomicOrdering.UNORDERED "Acquire ordering is not valid."
 **/
macro fetch_shift_left(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	var $load_ordering = $ordering;
	$if $ordering == RELEASE || $ordering == ACQUIRE_RELEASE:
	    $load_ordering = AtomicOrdering.SEQ_CONSISTENT;
	$endif
	
	var $StorageType = $typefrom(types::lower_to_atomic_compatible_type($typeof(*ptr)));

	$StorageType* storage_ptr = ($StorageType*)ptr;

	$typeof(*ptr) old_value;
	$typeof(*ptr) new_value;

	$StorageType storage_old_value;
	$StorageType storage_new_value;
	$StorageType storage_y = ($StorageType)y;
	
	do {
		storage_old_value = $$atomic_load(storage_ptr, false, $load_ordering.ordinal);
		old_value = bitcast(storage_old_value, $typeof(*ptr));
		new_value = storage_old_value << storage_y;
		storage_new_value = bitcast(new_value, $StorageType);
	} while (mem::compare_exchange(storage_ptr, storage_old_value, storage_new_value, $ordering, $load_ordering) != storage_old_value);

	return old_value;
}

/**
 * @param [&in] ptr "the variable or dereferenced pointer to the data."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @return "returns the old value of ptr"
 * 
 * @require types::is_int($typeof(*ptr)) "Only integer pointers may be used."
 * @require $ordering != AtomicOrdering.NOT_ATOMIC && $ordering != AtomicOrdering.UNORDERED "Acquire ordering is not valid."
 **/
macro flag_set(ptr, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	$typeof(*ptr) old_value;
	$typeof(*ptr) new_value = true;

	do {
		old_value = $$atomic_load(ptr, false, $ordering.ordinal);
	} while (mem::compare_exchange(ptr, old_value, new_value, $ordering, $load_ordering) != old_value);
	
	return old_value;
}

/**
 * @param [&in] ptr "the variable or dereferenced pointer to the data."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @return "returns the old value of ptr"
 * 
 * @require types::is_int($typeof(*ptr)) "Only integer pointers may be used."
 * @require $ordering != AtomicOrdering.NOT_ATOMIC && $ordering != AtomicOrdering.UNORDERED "Acquire ordering is not valid."
 **/
macro flag_clear(ptr, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	$typeof(*ptr) old_value;
	$typeof(*ptr) new_value = false;

	do {
		old_value = $$atomic_load(ptr, false, $ordering.ordinal);
	} while (mem::compare_exchange(ptr, old_value, new_value, $ordering, $load_ordering) != old_value);

	return old_value;
}

/**
 * @param [&in] ptr "the variable or dereferenced pointer to the data."
 * @param [in] y "the value to be added to ptr."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @return "returns the old value of ptr"
 * 
 * @require types::is_int($typeof(*ptr)) || types::is_float($typeof(*ptr)) "Only integer/float pointers may be used."
 * @require $ordering != AtomicOrdering.NOT_ATOMIC && $ordering != AtomicOrdering.UNORDERED "Acquire ordering is not valid."
 **/
macro fetch_max(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT, bool $volatile = false, usz $alignment = 0)
{
	$if $alignment == 0:
		$alignment = $typeof(*ptr).sizeof;
	$endif
	return $$atomic_fetch_max(ptr, y, $volatile, $ordering.ordinal, $alignment);
}

/**
 * @param [&in] ptr "the variable or dereferenced pointer to the data."
 * @param [in] y "the value to be added to ptr."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @return "returns the old value of ptr"
 * 
 * @require types::is_int($typeof(*ptr)) || types::is_float($typeof(*ptr)) "Only integer/float pointers may be used."
 * @require $ordering != AtomicOrdering.NOT_ATOMIC && $ordering != AtomicOrdering.UNORDERED "Acquire ordering is not valid."
 **/
macro fetch_min(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT, bool $volatile = false, usz $alignment = 0)
{
	$if $alignment == 0:
		$alignment = $typeof(*ptr).sizeof;
	$endif
	return $$atomic_fetch_min(ptr, y, $volatile, $ordering.ordinal, $alignment);
}
