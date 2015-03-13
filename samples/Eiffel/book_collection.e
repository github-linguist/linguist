class
	BOOK_COLLECTION

create
	make

feature {NONE} -- Initialization

	make (a_name: STRING_32)
			-- Create a book collection with `a_name' as `name'.
		do
			set_name (a_name)
			create book_index.make (10)
		ensure
			name_set: name = a_name
		end

feature -- Access

	name: STRING_32
			-- Name.

	books: LIST [BOOK]
			-- collection of book.
		do
			create {LINKED_LIST [BOOK]} Result.make
			across
				book_index as it
			loop
				Result.append (it.item)
			end
		end

	books_by_author (a_author: STRING_32): LIST [BOOK]
			-- Books wrote by `a_author' in this collection.
		do
			if attached book_index [a_author] as l_result then
				Result := l_result
			else
				create {LINKED_LIST [BOOK]} Result.make
			end
		end

feature -- Change

	set_name (a_name: STRING_32)
			-- Set `name' with `a_name'.
		do
			name := a_name
		ensure
			name_set: name = a_name
		end

	add_book (a_book: BOOK)
			-- Extend collection with `a_book'.
		local
			l: detachable LIST [BOOK]
		do
			l := book_index.at (a_book.author.name)
			if l = Void then
				create {LINKED_LIST [BOOK]} l.make
				book_index.put (l, a_book.author.name)
			end
			l.force (a_book)
		end

	add_books (book_list: like books)
			-- Append collection with `book_list'.
		do
			across
				book_list as it
			loop
				add_book (it.item)
			end
		end

feature {NONE} -- Implementation

	book_index: HASH_TABLE [LIST [BOOK], STRING_32]
			-- Association of author name and its books.

end -- class BOOK_COLLECTION
