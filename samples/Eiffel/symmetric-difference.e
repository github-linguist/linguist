note
	description: "Summary description for {SYMETRIC_DIFFERENCE_EXAMPLE}."
	URI: "http://rosettacode.org/wiki/Symmetric_difference"

class
	SYMETRIC_DIFFERENCE_EXAMPLE

create
	make

feature {NONE} -- Initialization

	make
		local
			a,a1,b,b1: ARRAYED_SET [STRING]
		do
			create a.make (4)
			create b.make (4)
			a.compare_objects
			b.compare_objects
			a.put ("John")
			a.put ("Bob")
			a.put ("Mary")
			a.put ("Serena")

			create a1.make (4)
			a1.copy (a)

			b.put ("Jim")
			b.put ("Mary")
			b.put ("John")
			b.put ("Bob")

			create b1.make (4)
			b1.copy (b)

		    a1.subtract (b1)
		    b.subtract (a)
		    a1.merge (b)
		    across a1 as c loop
		    	print (" " + c.item)
		    end
		end

end
