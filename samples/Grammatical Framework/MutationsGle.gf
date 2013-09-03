resource MutationsGle = open CharactersGle in {
	param Mutation = Unmutated|Lenition1|Lenition1DNTLS|Lenition2|Eclipsis1|Eclipsis2|Eclipsis3|PrefixT|PrefixH;
	
	--Turns a string into a mutation table
	oper mutate : (_ : Str) -> (Mutation => Str) = \str -> table {
		Unmutated => str ;
		Lenition1 => lenition1 str ;
		Lenition1DNTLS => lenition1dntls str ;
		Lenition2 => lenition2 str ;
		Eclipsis1 => eclipsis1 str ;
		Eclipsis2 => eclipsis2 str ;
		Eclipsis3 => eclipsis3 str ;
		PrefixT => prefixT str ;
		PrefixH => prefixH str
	};

	--Performs lenition 1: inserts "h" if the word begins with a lenitable character
	oper lenition1 : Str -> Str = \str -> case str of {
		start@("p"|"b"|"m"|"f"|"t"|"d"|"c"|"g") + rest => start + "h" + rest ;
		start@("P"|"B"|"M"|"F"|"T"|"D"|"C"|"G") + rest => start + "h" + rest ;
		("s"|"S") + ("p"|"t"|"c") + _ => str ; --the sequences "sp", "st", "sc" are never mutated
		start@("s"|"S") + rest => start + "h" + rest ;
		_ => str
	};
	
	--Performs lenition 1 with dentals: same as lenition 1 but leaves "d", "t" and "s" unmutated
	oper lenition1dntls : Str -> Str = \str -> case str of {
		start@("p"|"b"|"m"|"f"|"c"|"g") + rest => start + "h" + rest ;
		start@("P"|"B"|"M"|"F"|"C"|"G") + rest => start + "h" + rest ;
		_ => str
	};

	--Performs lenition 2: same as lenition 1 with dentals but also changes "s" into "ts"
	oper lenition2 : Str -> Str = \str -> case str of {
		start@("p"|"b"|"m"|"f"|"c"|"g") + rest => start + "h" + rest ;
		start@("P"|"B"|"M"|"F"|"C"|"G") + rest => start + "h" + rest ;
		("s"|"S") + ("p"|"t"|"c") + _ => str ; --the sequences "sp", "st", "sc" are never mutated
		start@("s"|"S") + rest => "t" + start + rest ;
		_ => str
	};
	
	--Performs eclisis 1: prefixes something to every word that begins with an ecliptable character
	oper eclipsis1 : Str -> Str = \str -> case str of {
		start@("p"|"P") + rest => "b" + start + rest ;
		start@("b"|"B") + rest => "m" + start + rest ;
		start@("f"|"F") + rest => "bh" + start + rest ;
		start@("c"|"C") + rest => "g" + start + rest ;
		start@("g"|"G") + rest => "n" + start + rest ;
		start@("t"|"T") + rest => "d" + start + rest ;
		start@("d"|"D") + rest => "n" + start + rest ;
		start@(#vowel) + rest => "n-" + start + rest ;
		start@(#vowelCap) + rest => "n" + start + rest ;
		_ => str
	};

	--Performs eclipsis 2: same as eclipsis 1 but leaves "t", "d" and vowels unchanges
	oper eclipsis2 : Str -> Str = \str -> case str of {
		start@("p"|"P") + rest => "b" + start + rest ;
		start@("b"|"B") + rest => "m" + start + rest ;
		start@("f"|"F") + rest => "bh" + start + rest ;
		start@("c"|"C") + rest => "g" + start + rest ;
		start@("g"|"G") + rest => "n" + start + rest ;
		_ => str
	};

	--Performs eclipsis 3: same as eclipsis 2 but also changes "s" to "ts"
	eclipsis3 : Str -> Str = \str -> case str of {
		start@("p"|"P") + rest => "b" + start + rest ;
		start@("b"|"B") + rest => "m" + start + rest ;
		start@("f"|"F") + rest => "bh" + start + rest ;
		start@("c"|"C") + rest => "g" + start + rest ;
		start@("g"|"G") + rest => "n" + start + rest ;
		("s"|"S") + ("p"|"t"|"c") + _ => str ; --the sequences "sp", "st", "sc" are never mutated
		start@("s"|"S") + rest => "t" + start + rest ;
		_ => str
	};

	--Prefixes a "t" to words beginning with a vowel
	oper prefixT : Str -> Str = \str -> case str of {
		start@(#vowel) + rest => "t-" + start + rest ;
		start@(#vowelCap) + rest => "t" + start + rest ;
		_ => str
	};

	--Prefixes a "h" to words beginning with a vowel
	oper prefixH : Str -> Str = \str -> case str of {
		start@(#vowel) + rest => "h" + start + rest ;
		start@(#vowelCap) + rest => "h" + start + rest ;
		_ => str
	};
	
}