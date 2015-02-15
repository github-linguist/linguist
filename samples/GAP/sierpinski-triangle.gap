# Using parity of binomial coefficients
SierpinskiTriangle := function(n)
	local i, j, s, b;
	n := 2^n - 1;
	b := " ";
	while Size(b) < n do
		b := Concatenation(b, b);
	od;
	for i in [0 .. n] do
		s := "";
		for j in [0 .. i] do
			if IsEvenInt(Binomial(i, j)) then
				Append(s, "  ");
			else
				Append(s, "* ");
			fi;
		od;
		Print(b{[1 .. n - i]}, s, "\n");
	od;
end;

SierpinskiTriangle(4);
               *
              * *
             *   *
            * * * *
           *       *
          * *     * *
         *   *   *   *
        * * * * * * * *
       *               *
      * *             * *
     *   *           *   *
    * * * *         * * * *
   *       *       *       *
  * *     * *     * *     * *
 *   *   *   *   *   *   *   *
* * * * * * * * * * * * * * * *
