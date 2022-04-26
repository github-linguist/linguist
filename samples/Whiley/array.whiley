package std

import uint from std::integer

// ===================================================================
// Properties
// ===================================================================

// Check if two arrays equal for a given subrange
public property equals<T>(T[] lhs, T[] rhs, int start, int end) -> (bool r):
    // Arrays must be big enough to hold subrange
    |lhs| >= end && |rhs| >= end &&
    // All items in subrange match
    all { i in start..end | lhs[i] == rhs[i] }

// Check if two array subranges are equal
public property equals<T>(T[] l, int l_start, T[] r, int r_start, int length) -> bool:
    // Arrays must be big enough to hold subrange
    |l| >= (l_start + length) && |r| >= (r_start + length) &&
    // All items in subrange match
    all { k in 0..length | l[l_start+k] == r[r_start+k] }

public property contains<T>(T[] lhs, T item, int start, int end) -> (bool r):
    // Some index in given range contains item
    some { i in start..end | lhs[i] == item }

// Check whether a subsequence is contained with an array
public property matches<T>(T[] arr, T[] subseq) -> (bool r):
    |subseq| <= |arr| && matches<T>(arr,subseq,0,|arr| - |subseq|)

// Check whether a subsequence is contained with an array slice
public property matches<T>(T[] arr, T[] subseq, int start, int end) -> (bool r):
    some { i in start..end | equals(arr,i,subseq,0,|subseq|) }

// Check whether a given index is the first match of a subsequence
public property first_match<T>(T[] arr, T[] subseq, int index) -> (bool r):
    equals(arr,index,subseq,0,|subseq|) && !matches<T>(arr,subseq,0,index)

// Ensure all elements in an array (upto a given point) are unique
public property unique_elements<T>(T[] items, int end) -> (bool r):
    // All items upto end are unique
    all { i in 0..end, j in (i+1)..end | items[i] != items[j] }

// ===================================================================
// Lemmas
// ===================================================================

// A simple lemma which establishes that, if two arrays are equal for
// a given range, then they are also equal for subranges.
native public function lemma_equals<T>(T[] l, uint i, T[] r, uint j, uint n, uint m)
requires n <= m
requires equals<T>(l,i,r,j,m)
ensures equals<T>(l,i,r,j,n)

native public function lemma_matches<T>(T[] arr, T[] subseq, uint i)
requires equals<T>(arr,i,subseq,0,|subseq|)
ensures matches<T>(arr,subseq)
    
// ===================================================================
// Queries
// ===================================================================

// find first index in list which matches character.  If no match,
// then return null.
public function first_index_of<T>(T[] items, T item) -> (uint|null index)
// If int returned, element at this position matches item
ensures index is uint ==> items[index] == item
// If int returned, element at this position is first match
ensures index is uint ==> all { i in 0 .. index | items[i] != item }
// If null returned, no element in items matches item
ensures index is null ==> all { i in 0 .. |items| | items[i] != item }:
    //
    return first_index_of(items,item,0)

// find first index after a given start point in list which matches character.
// If no match, then return null.
public function first_index_of<T>(T[] items, T item, uint start) -> (uint|null index)
// Starting point cannot be beyond array
requires start <= |items|
// If int returned, element at this position matches item
ensures index is uint ==> items[index] == item
// If int returned, element at this position is first match
ensures index is uint ==> !contains(items,item,start,index)
// If null returned, no element in items matches item
ensures index is null ==> !contains(items,item,start,|items|):
    //
    for i in start .. |items|
    // No element seen so far matches item
    where !contains(items,item,start,i):
        //
        if items[i] == item:
            return (uint) i
    //
    return null

// find first index in list which matches items.  If no match, then return null.
public function first_index_of<T>(T[] items, T[] item) -> (uint|null index)
// Must be actually looking for something
requires |item| > 0
// If int returned, sequence from this position matches item
ensures (index is uint) ==> equals(items,index,item,0,|item|)
// If null returned, no position matches item in items
ensures (index is null) ==> all { i in 0.. (|items|-|item|) | !equals(items,i,item,0,|item|) }:
    // Sanity check
    if |item| <= |items|:
        return first_index_of<T>(items,item,0,|items|-|item|)
    else:
        return null

// find first index after a given start point in list which matches items.
// If no match, then return null.
public function first_index_of<T>(T[] items, T[] item, uint start, uint end) -> (uint|null index)
// Starting point cannot be beyond array
requires start <= end
// End point cannot be beyond array
requires end + |item| <= |items|
// Must be actually looking for something
requires |item| > 0
// If int returned, sequence from this position matches item
ensures (index is uint) ==> equals(items,index,item,0,|item|)
// If null returned, no position matches item in items
ensures (index == null) ==> all { i in start .. end | !equals(items,i,item,0,|item|) }:
    // Possible
    for i in start .. end
    // Nothing we've seen so far matches
    where all { k in start .. i | !equals(items,k,item,0,|item|) }:
        uint j = 0
        // for match
        if equals(items,i,item,0,|item|):
            return (uint) i
    //
    return null

// find last index in list which matches character.  If no match,
// then return null.
public function last_index_of<T>(T[] items, T item) -> (uint|null index)
// If int returned, element at this position matches item
ensures index is uint ==> items[index] == item
// If int returned, element at this position is last match
ensures index is uint ==> all { i in (index+1) .. |items| | items[i] != item }
// If null returned, no element in items matches item
ensures index is null ==> all { i in 0 .. |items| | items[i] != item }:
    //
    int i = |items|
    //
    while i > 0
    where i <= |items|
    // No element seen so far matches item
    where all { j in i..|items| | items[j] != item }:
        //
        i = i - 1
        if items[i] == item:
            return (uint) i
    //
    return null

// ===================================================================
// Mutators
// ===================================================================

// replace all occurrences of "old" with "new" in list "items".
public function replace_all<T>(T[] items, T o, T n) -> (T[] r)
// Every position in items matching o replaced with n
ensures all { i in 0..|items| | (items[i] == o) ==> r[i] == n }
// Every other position remains the same
ensures all { i in 0..|items| | (items[i] != o) ==> r[i] == items[i] }
// Size of resulting array remains the same
ensures |items| == |r|:
    //
    T[] oldItems = items // ghost
    //
    for i in 0..|items|
    where |items| == |oldItems|
    where all { k in 0..i | (oldItems[k] == o) ==> (items[k] == n) }
    where all { k in 0..|items| | (oldItems[k] != o) ==> (items[k] == oldItems[k]) }:
        if oldItems[i] == o:
            items[i] = n
    //
    return items

// replace first occurrence of "old" with "new" in list "items".
public function replace_first<T>(T[] items, T o, T n) -> (T[] r)
// Size of resulting array remains the same
ensures |items| == |r|
// Can only replace old with new
ensures all { i in 0..|items| | items[i] == r[i] || (items[i] == o && r[i] == n) }
// Must replace first match of old
ensures all { i in 0..|items| | (items[i] == o && !contains(items,o,0,i)) ==> r[i] == n }
// Must not replace any other matches
ensures all { i in 0..|items| | (items[i] == o && contains(items,o,0,i)) ==> r[i] == o }:
    //
    T[] oldItems = items // ghost
    //
    for i in 0..|items|
    where |items| == |oldItems|
    // old not seen so far
    where !contains(items,o,0,i):
        // look for item
        if oldItems[i] == o:
            // done
            items[i] = n
            return items
    //
    return items

// replace first occurrence of "old" with "new" in list "items".
unsafe public function replace_first<T>(T[] items, T[] o, T[] n) -> (T[] r)
// Must actually be replacing something
requires |o| > 0
// Length may differ after match
ensures matches(items,o) ==> (|r| + |o|) == (|items| + |n|)
// Length doesn't differ if no match
ensures !matches(items,o) ==> (|r| == |items|)
// First match always replaced
ensures all { i in 0 .. (|items|-|o|) | first_match(items,o,i) ==> equals(r,i,n,0,|n|) }
// Items below first match are retained
ensures all { i in 0..|items| | first_match(items,o,i) ==> equals(items,0,r,0,i) }
// Items above first match are retained
ensures all { i in 0..|items| | first_match(items,o,i) ==> equals(items,i+|o|,r,i+|n|,|items| - (i+|o|)) }:
    // Look for match
    uint|null i = first_index_of<T>(items,o)
    // Check whether found
    if i is null:
        assert all { j in 0 .. |items| - |o| | !equals(items,j,o,0,|o|) }
        assert all { j in 0 .. |items| - |o| | !first_match(items,o,j) }
        assert !matches(items,o)
        // nothing found
        return items
    else if |o| == |n|:
        // easy case, can perform in place
        return copy(n,0,items,i,|o|)
    else:
        lemma_matches<T>(items,o,i)
        // hard case, must resize array
        uint size = (|items| - |o|) + |n|
        // Resize with temporary fill
        T[] nitems = resize<T>(items,size,o[0])
        // copy new over old
        nitems = copy<T>(n,0,nitems,i,|n|)
        // Calculate size of remainder
        uint remainder = size - i - |n|
        // copy remainder back
        return copy<T>(items,i+|o|,nitems,i+|n|,remainder)

// replace all occurrences of "old" with "new" in list "items".
public function replace_all<T>(T[] items, T[] o, T[] n) -> (T[] r)
// must have something to replace
requires |o| > 0:
    //
    // NOTE: this is an horifically poor implementation which obviously
    // needs updating at some point.    
    while first_index_of<T>(items,o) != null:
        items = replace_first<T>(items,o,n)
    //
    return items

// replace occurrences of "old" with corresponding occurences in order
public function replace<T>(T[] items, T[] o, T[][] nn) -> (T[] r)
// must have something to replace
requires |o| > 0:
    // NOTE: this is an horifically poor implementation which obviously
    // needs updating at some point.    
    uint i = 0
    //
    while i < |nn| && first_index_of<T>(items,o) != null:
        items = replace_first<T>(items,o,nn[i])
        i = i + 1
    //
    return items


// Extract slice of items array between start and up to (but not including) end.
public function slice<T>(T[] items, uint start, uint end) -> (T[] r)
// Given region to slice must make sense
requires start <= end && end <= |items|
// Size of slice determined by difference between start and end
ensures |r| == (end - start)
// Items returned in slice match those in region from start
ensures all { i in 0..|r| | items[i+start] == r[i] }:
    //
    if start == end:
        return []
    else:    
        T[] nitems = [items[0]; end-start]
        return copy(items,start,nitems,0,|nitems|)

public function append<T>(T[] lhs, T[] rhs) -> (T[] r)
// Resulting array exactly size of s1 and s2 together 
ensures |r| == |lhs| + |rhs|
// Elements of lhs are stored first in result
ensures all { k in 0..|lhs| | r[k] == lhs[k] }
// Elemnts of rhs are stored after those of lhs
ensures all { k in 0..|rhs| | r[k+|lhs|] == rhs[k] }:
    //
    if |lhs| == 0:
        return rhs
    else:
        // resize array
        T[] rs = resize<T>(lhs, |lhs| + |rhs|, lhs[0])
        // copy over new items
        return copy(rhs,0,rs,|lhs|,|rhs|)

public function append<T>(T[] items, T item) -> (T[] r)
// Every item from original array is retained
ensures all { k in 0..|items| | r[k] == items[k] }
// Last item in result matches item appended
ensures r[|items|] == item
// Size of array is one larger than original
ensures |r| == |items|+1:
    //
    T[] nitems = [item; |items| + 1]
    //
    for i in 0..|items|
    where |nitems| == |items|+1
    where nitems[|items|] == item
    where all { k in 0..i | nitems[k] == items[k] }:
        nitems[i] = items[i]
    //
    return nitems

public function append<T>(T item, T[] items) -> (T[] r)
// Every item from original array is retained
ensures all { k in 0..|items| | r[k+1] == items[k] }
// First item in result matches item appended
ensures r[0] == item
// Size of array is one larger than original
ensures |r| == |items|+1:
    //
    T[] nitems = [item; |items| + 1]
    //
    for i in 0..|items|
    where |nitems| == |items|+1
    where nitems[0] == item
    where all { k in 0..i | nitems[k+1] == items[k] }:
        nitems[i+1] = items[i]
    //
    return nitems

public function resize<T>(T[] src, uint size) -> (T[] result)
// Cannot create larger array (as how to fill new elements?)
requires size <= |src|
// Resulting array must have desired size
ensures |result| == size
// All elements must be copied over if increasing in size
ensures all { k in 0..size | result[k] == src[k] }:
    //    
    if |src| == 0:
        // handle empty array case
        return src
    else:
        result = [src[0]; size]
        // copy what we can over
        for i in 0..size
        // result size unchanged
        where |result| == size
        // everything so far is unchanged
        where all { k in 0..i | result[k] == src[k] }:
            result[i] = src[i]
        //
        return result

public function resize<T>(T[] items, uint size, T item) -> (T[] result)
// Resulting array must have desired size
ensures |result| == size
// All elements must be copied over if increasing in size
ensures (|items| <= size) ==> all { k in 0..|items| | result[k] == items[k] }
// Must fill upper part of result with default item
ensures (|items| <= size) ==> all { k in |items|..size | result[k] == item }
// As many elements as possible must be copied if decreasing in size
ensures (|items| > size) ==> all { k in 0..size | result[k] == items[k] }:
    //
    T[] nitems = [item; size]    
    int i = 0
    // copy first part
    while i < size && i < |items|
    where i >= 0 && |nitems| == size
    // All elements up to i match as before
    where all { j in 0..i | nitems[j] == items[j] }
    // All elements above size match item
    where size >= |items| ==> all { j in |items| .. size | nitems[j] == item}:
        nitems[i] = items[i]
        i = i + 1
    //
    return nitems

public function copy<T>(T[] src, uint srcStart, T[] dest, uint destStart, uint length) -> (T[] result)
// Source array must contain enough elements to be copied
requires (srcStart + length) <= |src|
// Destination array must have enough space for copied elements
requires (destStart + length) <= |dest|
// Result is same size as dest
ensures |result| == |dest|
// All elements before copied region are same
ensures equals(dest,0,result,0,destStart)
// All elements in copied region match src
ensures equals(src,srcStart,result,destStart,length)
// All elements above copied region are same
ensures all { i in (destStart+length) .. |dest| | dest[i] == result[i] }:
    //
    T[] _dest = dest
    //
    for i in 0 .. length
    // Size of dest unchanged
    where |dest| == |_dest|
    // Everything below destStart unchanged
    where equals(_dest,0,dest,0,destStart)
    // Everything copied so far is equal
    where all { k in srcStart .. (srcStart+i) | src[k] == dest[(k-srcStart)+destStart] }    
    // where equals(src, srcStart, dest, destStart, i)
    // Everything above j is unchanged
    // where equals(_dest,dest,i+destStart,|dest|):
    where all { k in i+destStart .. |dest| | _dest[k] == dest[k] }:    
        dest[i+destStart] = src[i+srcStart]
    //
    return dest

/**
 * Remove an item from this array, whilst shifting everything above it
 * down.  Thus, the resulting array is one element smaller than the
 * original.
 */
public function remove<T>(T[] src, uint ith) -> (T[] result)
// Element to be removed must be within bounds
requires ith < |src|
// Resulting array has one less element
ensures |result| == |src| - 1
// All elements below that removed are preserved
ensures equals(src,result,0,ith)
// All elements that were above that removed are preserved
ensures equals(src,ith+1,result,ith,|result|-ith):
    // Create array of appropriate size
    result = [src[0];|src|-1]
    // Copy over lower chunk
    result = copy(src,0,result,0,ith)
    // Copy over upper chunk
    return copy(src,ith+1,result,ith,|result|-ith)

/**
 * Swap two items (which may be the same) in an array.  The resulting
 * array is otherwise unchanged.
 */
public function swap<T>(T[] src, uint ith, uint jth) -> (T[] result)
// Elements to be swap must be within bounds
requires ith < |src| && jth < |src|
// Result is same size as dest
ensures |result| == |src|
// All elements except ith and jth are identical
ensures all { i in 0..|src| | i == ith || i == jth || src[i] == result[i] }
// ith and jth elements are inded swaped
ensures src[ith] == result[jth] && src[jth] == result[ith]:
    // Create temporary
    T tmp = src[ith]
    // Swap jth over
    src[ith] = src[jth]
    // Swap ith over
    src[jth] = tmp
    // Done
    return src
    

// ===================================================================
// Map / Reduce / Filter
// ===================================================================

// Map all items in a given array from one type to another.
public function map<S,T>(S[] items, function(S)->(T) fn) -> (T[] r)
// Resulting array same size as original
ensures |items| == |r|
// All items are properly mapped
ensures all { i in 0..|items| | r[i] == fn(items[i]) }:
    //
    if |items| == 0:
        return []
    else:
        // Initialise first element
        T[] nitems = [fn(items[0]);|items|]
        // Assign remainder
        for i in 1 .. |nitems|
        // Size of nitems unchanged
        where |nitems| == |items|
        // Everything so far has been mapped
        where all { k in 0..i | nitems[k] == fn(items[k]) }:
            nitems[i] = fn(items[i])
        // Done
        return nitems

// Eliminate all elements which fail to match a given filter predicate
public function filter<T>(T[] items, function(T)->(bool) fn) -> (T[] r)
// Resulting array not bigger than original
ensures |items| >= |r|
// Filter predicate holds for all returned items
ensures all { i in 0..|r| | fn(r[i]) }:
    // Shift all matching items down
    final uint len = |items|
    uint j = 0
    for i in 0..|items|
    // size of array doesn't change
    where |items| == len && j <= i
    // Everything in items upto j matches
    where all { k in 0..j | fn(items[k]) }:
        if fn(items[i]):
            items[j] = items[i]
            j = j + 1
    //
    return resize(items,j)

// Fold-left with default for empty arrays.
public function foldl<T>(T[] items, T dEfault, function(T,T)->(T) fn) -> (T r):
    if |items| == 0:
        return dEfault
    else:
        return foldl<T>(items,fn)

// Fold-left without default
public function foldl<T>(T[] items, function(T,T)->(T) fn) -> (T r)
// Cannot fold an empty array without a default value
requires |items| > 0:
    //
    T acc = items[0]
    //
    for i in 1..|items|:
        acc = fn(acc,items[i])
    //
    return acc
    
