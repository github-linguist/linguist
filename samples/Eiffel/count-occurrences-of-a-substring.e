class
	APPLICATION
inherit
	ARGUMENTS
create
	make
feature {NONE} -- Initialization
	make
			-- Run application.
		do
			occurance := 0
			from
				index := 1
			until
				index > text.count
			loop
				temp := text.fuzzy_index(search_for, index, 0)
				if
					temp /= 0
				then
					index := temp + search_for.count
					occurance := occurance + 1
				else
					index := text.count + 1
				end
			end
			print(occurance)
		end

	index:INTEGER
	temp:INTEGER
	occurance:INTEGER
	text:STRING = "ababababab"
	search_for:STRING = "abab"
end
