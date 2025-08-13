/* 
MIT License

Copyright (c) 2025 m0tholith

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.


Source: https://codeberg.org/m0tholith/c3-ecs/src/branch/main/src/ecs_entity.c3

*/



module ecs::component;

import std::io;
import std::collections::map;
import std::collections::list;
import std::collections::dynamic_list;

struct ECSComponentGroup (Printable)
{
	DynamicList group;
	List{uint} idList;
	bool isTag;
}
fn usz? ECSComponentGroup.to_format(&self, Formatter* formatter) @dynamic
	=> formatter.printf("{ group: %s, idList: %s }", self.group, self.idList);
macro usz ECSComponentGroup.len(&self) @operator(len) => self.idList.len();
macro any ECSComponentGroup.get(&self, usz index) @operator([]) => self.group.get(index);
macro ECSComponentGroup.init(&self, Allocator allocator, $Type, bool is_tag = false)
{
	self.group.init(allocator, $Type);
	self.idList.init(allocator);
	self.isTag = is_tag;
}
fn void ECSComponentGroup.free(&self)
{
	self.group.free();
	self.idList.free();
}
fn void ECSComponentGroup.copy(&result, Allocator allocator, ECSComponentGroup source)
{
	result.group.copy(allocator, source.group);
	result.idList.init_with_array(allocator, source.idList.array_view());
}
faultdef DUPLICATE_COMPONENT;
<*
	Creates a new component associated with a specific ID
	@return? DUPLICATE_COMPONENT
*>
macro ECSComponentGroup.add_component(&self, uint id, value = EMPTY_MACRO_SLOT)
{
	var is_empty = $typeof(value).typeid == $typeof(EMPTY_MACRO_SLOT).typeid; 
	assert(is_empty ^ ($typeof(value).typeid == self.group.type), "Types don't match");
	switch (self.len())
	{
		case 0:
			self.idList.push(id);
			if (!self.isTag) self.group.push(value);
			return;
	}
	if (id < self.idList[0])
	{
		self.idList.insert_at(0, id);
		if (!self.isTag) self.group.insert_at(0, value);
		return;
	}
	if (id > self.idList[^1])
	{
		self.idList.push(id);
		if (!self.isTag) self.group.push(value);
		return;
	}

	uint l;
	uint r = (uint)self.len();
	uint m @noinit;

	while (l < r)
	{
		m = (l + r) / 2;
		if (self.idList[m] == id)
		{
			return DUPLICATE_COMPONENT?;
		}
		else if (self.idList[m] < id)
		{
			l = m + 1;
		}
		else if (self.idList[m] > id)
		{
			r = m - 1;
		}
	}
	m++;

	self.idList.insert_at(m, id);
	if (!self.isTag) self.group.insert_at(m, id);
}
faultdef COMPONENT_NOT_FOUND;
<*
	Creates a new tag associated with a specific ID
	@return? DUPLICATE_COMPONENT
*>
macro ECSComponentGroup.add_tag(&self, uint id)
	=> self.add_component(id);
<*
	Uses binary search to find index of component by ID
	@return? COMPONENT_NOT_FOUND
*>
fn usz? ECSComponentGroup.find_idx(&self, uint id)
{
	if (self.len() == 0) return COMPONENT_NOT_FOUND?;
	
	uint l = 0;
	uint r = (uint)self.len() - 1;
	uint m @noinit;

	while (l <= r)
	{
		m = (uint)((l + r) / 2);
		if (self.idList[m] == id)
		{
			return m;
		}
		else if (self.idList[m] < id)
		{
			if (m == usz.max) break;
			l = (usz)m + 1;
		}
		else if (self.idList[m] > id)
		{
			if (m == 0) break;
			r = m - 1;
		}
	}
	return COMPONENT_NOT_FOUND?;
}
<*
	Removes the component associated with a specific ID
	@return? COMPONENT_NOT_FOUND
*>
fn void? ECSComponentGroup.remove_component(&self, uint id)
{
	usz index = self.find_idx(id)!;
	if (!self.isTag) self.group.remove_at(index);
	self.idList.remove_at(index);
}
<*
	Removes the tag associated with a specific ID
	@return? COMPONENT_NOT_FOUND
*>
fn void? ECSComponentGroup.remove_tag(&self, uint id) @inline
	=> self.remove_component(id);

alias ECSComponentManager = HashMap{typeid, ECSComponentGroup};
fn void ECSComponentManager.copy(&result, Allocator allocator, ECSComponentManager source)
{
	result.init(allocator);
	ECSComponentGroup throwaway;
	foreach (kv : source.iter())
	{
		throwaway.copy(allocator, kv.value);
		result.set(kv.key, throwaway);
	}
}
<*
	Frees base type (hashmap) along with all components under
*>
fn void ECSComponentManager.free_full(&self)
{
	foreach (component : self.iter())
	{
		/// frees "valid component" entry in hashmap
		component.value.free();
	}
	self.free();
}
<*
	Registers component type into map
	@require !self.has_key($Component.typeid) : "Component already exists"
*>
macro ECSComponentManager.new_component(&self, $Component, is_tag = false)
{
	ECSComponentGroup group;
	group.init(self.allocator, $Component, is_tag: is_tag);
	self.set($Component.typeid, group);
}
<*
	Registers tag type into map
	@require !self.has_key($Tag.typeid) : "Tag already exists"
*>
macro ECSComponentManager.new_tag(&self, $Tag)
	=> self.new_component($Tag, is_tag: true);
<*
	Check if entity has specified component
	@require self.has_key($Component.typeid) : "Component doesn't exist"
*>
macro bool ECSComponentManager.has_component(&self, uint id, $Component)
{
	return @ok(self.get_ref($Component.typeid).find_idx(id));
}
<*
	Check if entity has all specified components (in vaargs)
*>
macro bool ECSComponentManager.has_components(&self, uint id, ...)
{
	$for var $i = 0; $i < $vacount; $i++:
		if (!self.has_component(id, $vatype[$i])) return false;
	$endfor
	return true;
}
<*
	Check if entity has specified tag
	@require self.has_key($Tag.typeid) : "Tag doesn't exist"
*>
macro bool ECSComponentManager.has_tag(&self, uint id, $Tag)
	=> self.has_component(id, $Tag);
<*
	Check if entity has all specified tags (in vaargs)
*>
macro bool ECSComponentManager.has_tags(&self, uint id, ...)
	=> self.has_component(id, $vasplat);
<*
	Get the component associated with a specific ID
	@require self.has_key($Component.typeid) : "Component doesn't exist"
	@return? COMPONENT_NOT_FOUND
*>
macro ECSComponentManager.get_component(&self, uint id, $Component)
{
	ECSComponentGroup* components = self.get_ref($Component.typeid)!!;
	usz idx = components.find_idx(id)!;
	return ($Component*)components.group.get_ptr(idx);
}
<*
	Creates a new component associated with a specific ID
	@require self.has_key($typeof(value).typeid) : "Component doesn't exist"
*>
macro ECSComponentManager.add_component(&self, uint id, value)
	=> self.get_ref($typeof(value).typeid).add_component(id, value)!;
<*
	Creates a new tag associated with a specific ID
	@require self.has_key($Tag.typeid) : "tag doesn't exist"
*>
macro ECSComponentManager.add_tag(&self, uint id, $Tag)
	=> self.get_ref($Tag.typeid).add_tag(id)!;
<*
	Removes the component associated with a specific ID
	@require self.has_key($Component.typeid) : "Component doesn't exist"
*>
macro ECSComponentManager.remove_component(&self, uint id, $Component)
	=> self.get_ref($Component.typeid).remove_component(id);
<*
	Removes the tag associated with a specific ID
	@require self.has_key($Tag.typeid) : "Tag doesn't exist"
*>
macro ECSComponentManager.remove_tag(&self, uint id, $Tag)
	=> self.remove_component(id, $Tag);

<*
	Loops over all components of specific type
	@require self.has_key($Component.typeid) : "Component doesn't exist"
*>
macro ECSComponentManager.@foreach(&self, $Component; @body(uint id, component))
{
	ECSComponentGroup group = self.get($Component.typeid)!!;
	foreach (i, &component : (($Component*)group.group.data)[:group.len()])
	{
		@body(group.idList[i], component);
	}
}