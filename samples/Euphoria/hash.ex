--****
-- === hash.ex
-- Hash Table Demo written by Junko C. Miura, RDS

include std/console.e
include std/io.e
include std/hash.e
include std/map.e
include std/types.e
include std/convert.e
include std/cmdline.e

without type_check

constant EOF = -1
constant TRUE = 1
constant STRING = 1, COUNT = 2 -- fields for one hash table entry

integer hashBuckets  -- prime
	-- With the hash function below, it helps (a little bit) 
	-- to have a prime number here. Don't use a power of 2.
	-- You'll get better performance by using a
	-- bigger size of table, but it uses up space.

-- This hash table consists of a set of two sequences. One for the
-- keys (words) and one for the corresponding word counter.

-- Initialize the hash table to a sequence of empty "buckets"
sequence table_words
sequence table_count

integer compares = 0
integer  inputHandle
sequence vOutPath
integer outputHandle

integer numZeroBucket, max, items, len, total_words

function hash_function(sequence string)
	return remainder(hash(string, stdhash:HSIEH32 ), hashBuckets) + 1
end function

procedure update_table(sequence string)
-- If string is not in the table already, add it, with a count of 1, 
-- otherwise just increment its count.
	integer hash_val, found
	sequence bucket
	sequence counters
	
	-- which bucket to search?
	hash_val = hash_function(string) 
	bucket = table_words[hash_val]
	found = find(string, bucket)
	if found then
		compares += found
		table_count[hash_val][found] += 1
	else
		bucket = append(bucket, string)
		table_words[hash_val] = bucket
		table_count[hash_val] &= 1
	end if
end procedure

function next_word()
-- Read to get the next "word".
	integer c
	sequence word

	word = ""
	while TRUE do
		c = getc(inputHandle)
		if t_alpha(c) then
		    word &= c
		else
			if length(word) > 0 or c = EOF then
		    	exit
			end if
		end if
	end while
	
	return word
end function

procedure build_table()
-- build a hash table containing all unique words in standard input
	object word

	while length(word) > 0 with entry do
	    update_table(word)
	entry
		word = next_word()
	end while
	
end procedure

sequence vOpts = {
	{"s", "hash", "Hash size", {ONCE, HAS_PARAMETER, NO_CASE}, -1},
	{"o", "outfile", "File to receive word list and statistics", {ONCE, NO_CASE, HAS_PARAMETER}, -1},
	$
}

object vOptMap

without warning

override procedure abort(integer errcode)
	maybe_any_key("\nPress Any Key to Continue...")
	eu:abort(errcode)
end procedure

procedure main()
	sequence lDicts
	object lFileList
	integer lFileHandle
	
	vOptMap = cmd_parse(vOpts)
	
	vOutPath = map:get(vOptMap, "outfile", "hash.out")
	hashBuckets = to_integer(map:get(vOptMap, "hash", "1009"))
	table_words = repeat({}, hashBuckets)
	table_count = repeat({}, hashBuckets)
	
	
	outputHandle = open(vOutPath, "w")
	if outputHandle = -1 then
		writefln("Cannot open output file")
		abort(1)
	end if
	
	
	lFileList = map:get(vOptMap, cmdline:EXTRAS, {})
	
	if length(lFileList) < 1 then
		puts(2, "You must give the name of an input file")
		abort(1)
	end if
	
	if length(lFileList) > 1 then
		puts(2, "You must only give the name of one input file")
		abort(1)
	end if
	
	inputHandle = open(lFileList[1], "r")
	if inputHandle = -1 then
		puts(2, "Cannot open input file.")
		abort(1)
	end if


	atom t
	t = time()         -- Time the table-building process only
	build_table() 
	t = time() - t     -- stop timer
	
	
	writefln("\n[:.2] seconds", t)
	
	numZeroBucket = 0
	items = 0
	max = 0
	total_words = 0
	
	for i = 1 to length(table_words) do
		len = length(table_words[i])
		items += len
		for j = 1 to length(table_count[i]) do
			total_words += table_count[i][j]
		end for
		if len = 0 then
			numZeroBucket += 1
		elsif len > max then
		    max = len
		end if
	end for
	
	if total_words = 0 then
		abort(0)
	end if
	
	writefln(outputHandle, "build time ..................... : [:.2]", t)
	writefln(outputHandle, "number of hash table buckets ... : []", hashBuckets)
	writefln(outputHandle, "number of words in input stream  : []", total_words)
	writefln(outputHandle, "number of items in hash table .. : []", items)
	writefln(outputHandle, "number of empty buckets ........ : []", numZeroBucket)
	writefln(outputHandle, "largest bucket size ............ : []", max)
	writefln(outputHandle, "average bucket size ............ : [:.2]", items / (hashBuckets - numZeroBucket))
	writefln(outputHandle, "compares per lookup ............ : [:.2]",  compares/total_words)
	
	for i = 1 to length(table_words) do
		if length(table_words[i]) > 0 then
		    writef(outputHandle, "bucket#[:6]: ", i)
		    for j = 1 to length(table_words[i]) do
				if j > 1 and remainder(j-1,5) = 0 then
				    writefln(outputHandle, "")
				    writef(outputHandle, "             ")
				end if
				
				writef(outputHandle, "[]:[] ", {table_words[i][j], table_count[i][j]})
	
		    end for
			writefln(outputHandle, "")
	
		end if
	end for
	close(outputHandle)
	writefln("See '[1]' for statistics and table contents", {vOutPath})
end procedure

main()
