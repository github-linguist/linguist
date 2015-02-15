class
	APPLICATION

create
	make

feature {NONE} -- Initialization

	make
		local
			bottles: INTEGER
		do
			from
				bottles := 99
			invariant
				bottles <= 99 and bottles >= 1
			until
				bottles = 1
			loop
				print (bottles)
				print (" bottles of beer on the wall,%N")
				print (bottles)
				print (" bottles of beer.%N")
				print ("Take one down, pass it around,%N")
				bottles := bottles - 1
				if bottles > 1 then
					print (bottles)
					print (" bottles of beer on the wall.%N%N")
				end
			variant
				bottles
			end
			print ("1 bottle of beer on the wall.%N%N");
			print ("No more bottles of beer on the wall,%N");
			print ("no more bottles of beer.%N");
			print ("Go to the store and buy some more,%N");
			print ("99 bottles of beer on the wall.%N");
		end

end
