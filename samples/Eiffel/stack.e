class
	STACK_ON_ARRAY

create
	make

feature -- Implementation

	empty: BOOLEAN
		do
			Result := stack.is_empty
		ensure
			empty: Result = (stack.count = 0)
		end

	push (item: ANY)
		do
			stack.force (item, stack.count)
		ensure
			pushed: stack [stack.upper] = item
			growth: stack.count = old stack.count + 1
		end

	pop: ANY
		require
			not_empty: not empty
		do
			Result := stack.at (stack.upper)
			stack.remove_tail (1)
		ensure
			reduction: stack.count = old stack.count - 1
		end

feature {NONE} -- Initialization

	stack: ARRAY [ANY]

	make
		do
			create stack.make_empty
		end

end
