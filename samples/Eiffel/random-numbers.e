class
	APPLICATION

inherit
	ARGUMENTS

create
	make

feature {NONE} -- Initialization

	l_time: TIME
	l_seed: INTEGER
	math:DOUBLE_MATH
	rnd:RANDOM
	Size:INTEGER
		once
			Result:= 1000
		end

	make
			-- Run application.
		local
			ergebnis:ARRAY[DOUBLE]
			tavg: DOUBLE
			x: INTEGER
			tmp: DOUBLE
			text : STRING

		do
			-- initialize random generator
			create l_time.make_now
     		        l_seed := l_time.hour
      		        l_seed := l_seed * 60 + l_time.minute
      		        l_seed := l_seed * 60 + l_time.second
      		        l_seed := l_seed * 1000 + l_time.milli_second
      		        create rnd.set_seed (l_seed)

			-- initialize random number container and math
			create ergebnis.make_filled (0.0, 1, size)
			tavg := 0;
			create math

			from
				x := 1
			until
				x > ergebnis.count
			loop
				tmp := randomNormal / 2 + 1
				tavg := tavg + tmp
				ergebnis.enter (tmp , x)
				x := x + 1
			end

			tavg := tavg / ergebnis.count
			text := "Average: "
			text.append_double (tavg)
			text.append ("%N")
			print(text)

			tmp := 0
			from
				x:= 1
			until
				x > ergebnis.count
			loop
				tmp := tmp + (ergebnis.item (x) - tavg)^2
				x := x + 1
			end

			tmp := math.sqrt (tmp / ergebnis.count)
			text := "Standard Deviation: "
			text.append_double (tmp)
			text.append ("%N")
			print(text)

		end

	randomNormal:DOUBLE

		local

      		        first: DOUBLE
      		        second: DOUBLE

		do
                        rnd.forth
                        first := rnd.double_item
                        rnd.forth
                        second := rnd.double_item

                        Result := math.cosine (2 * math.pi * first) * math.sqrt (-2 * math.log (second))

		end
end
