--****
-- == Types - Extended

namespace types

public constant OBJ_UNASSIGNED = 0
public constant OBJ_INTEGER = 1
public constant OBJ_ATOM = 2
public constant OBJ_SEQUENCE = 3


public constant FALSE = (1=0)
public constant TRUE = (1=1)

--****
-- === Predefined Character Sets

public enum
	CS_FIRST = 0,	
	CS_Consonant,
	CS_Vowel,
	CS_Hexadecimal,
	CS_Whitespace,
	CS_Punctuation,
	CS_Printable,
	CS_Displayable,
	CS_Lowercase,
	CS_Uppercase,
	CS_Alphanumeric,
	CS_Identifier,
	CS_Alphabetic,
	CS_ASCII,
	CS_Control,
	CS_Digit,
	CS_Graphic,
	CS_Bytes,
	CS_SpecWord,
	CS_Boolean,
	CS_LAST

--****
-- === Support Functions
--

--**
-- determines whether one or more characters are in a given character set.
public function char_test(object test_data, sequence char_set)
	integer lChr

	if integer(test_data) then
		if sequence(char_set[1]) then
			for j = 1 to length(char_set) do
				if test_data >= char_set[j][1] and test_data <= char_set[j][2] then 
					return TRUE 
				end if
			end for
			return FALSE
		else
			return find(test_data, char_set) > 0
		end if
	elsif sequence(test_data) then
		if length(test_data) = 0 then 
			return FALSE 
		end if
		for i = 1 to length(test_data) label "NXTCHR" do
			if sequence(test_data[i]) then 
				return FALSE
			end if
			if not integer(test_data[i]) then 
				return FALSE
			end if
			lChr = test_data[i]
			if sequence(char_set[1]) then
				for j = 1 to length(char_set) do
					if lChr >= char_set[j][1] and lChr <= char_set[j][2] then
						continue "NXTCHR" 
					end if
				end for
			else
				if find(lChr, char_set) > 0 then
					continue "NXTCHR"
				end if
			end if
			return FALSE
		end for
		return TRUE
	else
		return FALSE
	end if
end function

sequence Defined_Sets

--**
-- sets all the defined character sets to their default definitions.
public procedure set_default_charsets()
	Defined_Sets = repeat(0, CS_LAST - CS_FIRST - 1)
	Defined_Sets[CS_Alphabetic	] = {{'a', 'z'}, {'A', 'Z'}}
	Defined_Sets[CS_Alphanumeric] = {{'0', '9'}, {'a', 'z'}, {'A', 'Z'}}
	Defined_Sets[CS_Identifier]   = {{'0', '9'}, {'a', 'z'}, {'A', 'Z'}, {'_', '_'}}
	Defined_Sets[CS_Uppercase 	] = {{'A', 'Z'}}
	Defined_Sets[CS_Lowercase 	] = {{'a', 'z'}}
	Defined_Sets[CS_Printable 	] = {{' ', '~'}}
	Defined_Sets[CS_Displayable ] = {{' ', '~'}, "  ", "\t\t", "\n\n", "\r\r", {8,8}, {7,7} }
	Defined_Sets[CS_Whitespace 	] = " \t\n\r" & 11 & 160
	Defined_Sets[CS_Consonant 	] = "bcdfghjklmnpqrstvwxyzBCDFGHJKLMNPQRSTVWXYZ"
	Defined_Sets[CS_Vowel 		] = "aeiouAEIOU"
	Defined_Sets[CS_Hexadecimal ] = {{'0', '9'}, {'A', 'F'},{'a', 'f'}}
	Defined_Sets[CS_Punctuation ] = {{' ', '/'}, {':', '?'}, {'[', '`'}, {'{', '~'}}
	Defined_Sets[CS_Control 	] = {{0, 31}, {127, 127}}
	Defined_Sets[CS_ASCII 		] = {{0, 127}}
	Defined_Sets[CS_Digit 		] = {{'0', '9'}}
	Defined_Sets[CS_Graphic 	] = {{'!', '~'}}
	Defined_Sets[CS_Bytes	 	] = {{0, 255}}
	Defined_Sets[CS_SpecWord 	] = "_"
	Defined_Sets[CS_Boolean     ] = {TRUE,FALSE}
end procedure

--**
-- gets the definition for each of the defined character sets.
public function get_charsets()
	sequence result_

	result_ = {}
	for i = CS_FIRST + 1 to CS_LAST - 1 do
		result_ = append(result_, {i, Defined_Sets[i]} )
	end for

	return result_
end function

--**
-- sets the definition for one or more defined character sets.
public procedure set_charsets(sequence charset_list)
	for i = 1 to length(charset_list) do
		if sequence(charset_list[i]) and length(charset_list[i]) = 2 then
			if integer(charset_list[i][1]) and charset_list[i][1] > CS_FIRST and charset_list[i][1] < CS_LAST then
				Defined_Sets[charset_list[i][1]] = charset_list[i][2]
			end if
		end if
	end for
end procedure

--****
-- === Types

--**
-- test for an integer boolean.
public type boolean(object test_data)
	-- A boolean is a value that is either zero or one.
	return find(test_data,{1,0}) != 0
end type

--**
-- tests elements for boolean.
public type t_boolean(object test_data)
	return char_test(test_data, Defined_Sets[CS_Boolean])
end type

--**
-- tests for  alphanumeric character.
public type t_alnum(object test_data)
	return char_test(test_data, Defined_Sets[CS_Alphanumeric])
end type

--**
-- tests string if it is an valid identifier.
public type t_identifier(object test_data)
	-- Test to make sure the first character is not a number
	if t_digit(test_data) then
		return 0
	elsif sequence(test_data) and length(test_data) > 0 and t_digit(test_data[1]) then
		return 0
	end if

	return char_test(test_data, Defined_Sets[CS_Identifier])
end type

--**
-- tests for alphabetic characters.
public type t_alpha(object test_data)
	return char_test(test_data, Defined_Sets[CS_Alphabetic])
end type

--**
-- tests for ASCII characters.
public type t_ascii(object test_data)
	return char_test(test_data, Defined_Sets[CS_ASCII])
end type

--**
-- tests for control characters.
public type t_cntrl(object test_data)
	return char_test(test_data, Defined_Sets[CS_Control])
end type

--**
-- tests for digits.
public type t_digit(object test_data)
	return char_test(test_data, Defined_Sets[CS_Digit])
end type

--**
-- test for glyphs (printable) characters.
public type t_graph(object test_data)
	return char_test(test_data, Defined_Sets[CS_Graphic])
end type

--**
-- tests for a special word character.
public type t_specword(object test_data)
	return char_test(test_data, Defined_Sets[CS_SpecWord])
end type

--**
-- tests for bytes.
public type t_bytearray(object test_data)
	return char_test(test_data, Defined_Sets[CS_Bytes])
end type

--**
-- tests for lowercase characters.
public type t_lower(object test_data)
	return char_test(test_data, Defined_Sets[CS_Lowercase])
end type

--**
-- tests for ASCII glyph characters.
public type t_print(object test_data)
	return char_test(test_data, Defined_Sets[CS_Printable])
end type

--**
-- tests for printable characters.
public type t_display(object test_data)
	return char_test(test_data, Defined_Sets[CS_Displayable])
end type

--**
-- tests for punctuation characters.
public type t_punct(object test_data)
	return char_test(test_data, Defined_Sets[CS_Punctuation])
end type

--**
-- tests for whitespace characters.
public type t_space(object test_data)
	return char_test(test_data, Defined_Sets[CS_Whitespace])
end type

--**
-- tests for uppercase characters.
public type t_upper(object test_data)
	return char_test(test_data, Defined_Sets[CS_Uppercase])
end type

--**
-- tests for hexadecimal characters.
public type t_xdigit(object test_data)
	return char_test(test_data, Defined_Sets[CS_Hexadecimal])
end type

--**
-- tests for vowel characters.
public type t_vowel(object test_data)
	return char_test(test_data, Defined_Sets[CS_Vowel])
end type

--**
-- tests for consonant characters.
public type t_consonant(object test_data)
	return char_test(test_data, Defined_Sets[CS_Consonant])
end type

set_default_charsets()

--**
-- tests for integer elements.
public type integer_array( object x )
	if not sequence(x) then
		return 0
	end if

	for i = 1 to length(x) do
		if not integer(x[i]) then
			return 0
		end if
	end for
	return 1
end type

--**
-- tests for text characters.
public type t_text( object x )
	if not sequence(x) then
		return 0
	end if

	for i = 1 to length(x) do
		if not integer(x[i]) then
			return 0
		end if
		if x[i] < 0 then
			return 0
		end if
	end for
	return 1
end type

--**
-- tests for atom elements.
public type number_array( object x )
	if not sequence(x) then
		return 0
	end if

	for i = 1 to length(x) do
		if not atom(x[i]) then
			return 0
		end if
	end for
	return 1
end type

--**
-- tests for sequence with possible nested sequences.
public type sequence_array( object x )
	if not sequence(x) then
		return 0
	end if

	for i = 1 to length(x) do
		if not sequence(x[i]) then
			return 0
		end if
	end for
	return 1
end type

--**
-- tests for ASCII elements.
public type ascii_string( object x )
	if not sequence(x) then
		return 0
	end if

	for i = 1 to length(x) do
		if not integer(x[i]) then
			return 0
		end if
		if x[i] < 0 then
			return 0
		end if
		if x[i] > 127 then
			return 0
		end if
	end for
	return 1
end type

--**
-- tests for a string sequence.
public type string( object x )
	if not sequence(x) then
		return 0
	end if

	for i = 1 to length(x) do
		if not integer(x[i]) then
			return 0
		end if
		if x[i] < 0 then
			return 0
		end if
		if x[i] > 255 then
			return 0
		end if
	end for
	return 1
end type

--**
-- tests for a string sequence (that has no null character).
public type cstring( object x )
	if not sequence(x) then
		return 0
	end if

	for i = 1 to length(x) do
		if not integer(x[i]) then
			return 0
		end if
		if x[i] <= 0 then
			return 0
		end if
		if x[i] > 255 then
			return 0
		end if
	end for
	return 1
end type

public constant INVALID_ROUTINE_ID = routine_id("#")
public constant NO_ROUTINE_ID = -99999

-- Maximum and minimum values for Euphoria 31-bit integers (as implemented by 32-bit Euphoria)
constant
	MAXSINT31 = power(2,30)-1,
	MINSINT31 = -power(2,30)

--** 
-- tests for Euphoria integer.
public type t_integer32( object o )
	ifdef EU32 then
		return integer( o )
	elsedef
		if integer( o ) and o <= MAXSINT31 and o >= MINSINT31 then
			return 1
		else
			return 0
		end if
	end ifdef
end type
