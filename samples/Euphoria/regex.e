--****
-- == Regular Expressions

namespace regex

include std/types.e
include std/flags.e as flags
include std/machine.e
include std/math.e
include std/search.e
include std/text.e

enum
	M_PCRE_COMPILE          = 68,
	M_PCRE_EXEC             = 70,
	M_PCRE_REPLACE          = 71,
	M_PCRE_ERROR_MESSAGE    = 95,
	M_PCRE_GET_OVECTOR_SIZE = 97,
	$

--****
-- === Option Constants

public constant
	DEFAULT            = #00000000,
	CASELESS           = #00000001,
	MULTILINE          = #00000002,
	DOTALL             = #00000004,
	EXTENDED           = #00000008,
	ANCHORED           = #00000010,
	DOLLAR_ENDONLY     = #00000020,
	EXTRA              = #00000040,
	NOTBOL             = #00000080,
	NOTEOL             = #00000100,
	UNGREEDY           = #00000200,
	NOTEMPTY           = #00000400,
	UTF8               = #00000800,
	NO_AUTO_CAPTURE    = #00001000,
	NO_UTF8_CHECK      = #00002000,
	AUTO_CALLOUT       = #00004000,
	PARTIAL            = #00008000,
	DFA_SHORTEST       = #00010000,
	DFA_RESTART        = #00020000,
	FIRSTLINE          = #00040000,
	DUPNAMES           = #00080000,
	NEWLINE_CR         = #00100000,
	NEWLINE_LF         = #00200000,
	NEWLINE_CRLF       = #00300000,
	NEWLINE_ANY        = #00400000,
	NEWLINE_ANYCRLF    = #00500000,
	BSR_ANYCRLF        = #00800000,
	BSR_UNICODE        = #01000000,
	STRING_OFFSETS     = #0C000000

constant option_names = {
	{DEFAULT,"DEFAULT"},
	{CASELESS,"CASELESS"},
	{MULTILINE,"MULTILINE"},
	{DOTALL,"DOTALL"},
	{EXTENDED,"EXTENDED"},
	{ANCHORED,"ANCHORED"},
	{DOLLAR_ENDONLY,"DOLLAR_ENDONLY"},
	{EXTRA,"EXTRA"},
	{NOTBOL,"NOTBOL"},
	{NOTEOL,"NOTEOL"},
	{UNGREEDY,"UNGREEDY"},
	{NOTEMPTY,"NOTEMPTY"},
	{UTF8,"UTF8"},
	{NO_AUTO_CAPTURE,"NO_AUTO_CAPTURE"},
	{NO_UTF8_CHECK,"NO_UTF8_CHECK"},
	{AUTO_CALLOUT,"AUTO_CALLOUT"},
	{PARTIAL,"PARTIAL"},
	{DFA_SHORTEST,"DFA_SHORTEST"},
	{DFA_RESTART,"DFA_RESTART"},
	{FIRSTLINE,"FIRSTLINE"},
	{DUPNAMES,"DUPNAMES"},
	{NEWLINE_CR,"NEWLINE_CR"},
	{NEWLINE_LF,"NEWLINE_LF"},
	{NEWLINE_CRLF,"NEWLINE_CRLF"},
	{NEWLINE_ANY,"NEWLINE_ANY"},
	{NEWLINE_ANYCRLF,"NEWLINE_ANYCRLF"},
	{BSR_ANYCRLF,"BSR_ANYCRLF"},
	{BSR_UNICODE,"BSR_UNICODE"},
	{STRING_OFFSETS,"STRING_OFFSETS"}
}

--****
-- === Error Constants

public constant
	ERROR_NOMATCH        =  (-1),
	ERROR_NULL           =  (-2),
	ERROR_BADOPTION      =  (-3),
	ERROR_BADMAGIC       =  (-4),
	ERROR_UNKNOWN_OPCODE =  (-5),
	ERROR_UNKNOWN_NODE   =  (-5),
	ERROR_NOMEMORY       =  (-6),
	ERROR_NOSUBSTRING    =  (-7),
	ERROR_MATCHLIMIT     =  (-8),
	ERROR_CALLOUT        =  (-9),
	ERROR_BADUTF8        = (-10),
	ERROR_BADUTF8_OFFSET = (-11),
	ERROR_PARTIAL        = (-12),
	ERROR_BADPARTIAL     = (-13),
	ERROR_INTERNAL       = (-14),
	ERROR_BADCOUNT       = (-15),
	ERROR_DFA_UITEM      = (-16),
	ERROR_DFA_UCOND      = (-17),
	ERROR_DFA_UMLIMIT    = (-18),
	ERROR_DFA_WSSIZE     = (-19),
	ERROR_DFA_RECURSE    = (-20),
	ERROR_RECURSIONLIMIT = (-21),
	ERROR_NULLWSLIMIT    = (-22),
	ERROR_BADNEWLINE     = (-23)

public constant error_names = {
	{ERROR_NOMATCH,"ERROR_NOMATCH"},
	{ERROR_NULL,"ERROR_NULL"},
	{ERROR_BADOPTION,"ERROR_BADOPTION"},
	{ERROR_BADMAGIC,"ERROR_BADMAGIC"},
	{ERROR_UNKNOWN_OPCODE,"ERROR_UNKNOWN_OPCODE/NODE"},
	{ERROR_UNKNOWN_NODE,"ERROR_UNKNOWN_OPCODE/NODE"},
	{ERROR_NOMEMORY,"ERROR_NOMEMORY"},
	{ERROR_NOSUBSTRING,"ERROR_NOSUBSTRING"},
	{ERROR_MATCHLIMIT,"ERROR_MATCHLIMIT"},
	{ERROR_CALLOUT,"ERROR_CALLOUT"},
	{ERROR_BADUTF8,"ERROR_BADUTF8"},
	{ERROR_BADUTF8_OFFSET,"ERROR_BADUTF8_OFFSET"},
	{ERROR_PARTIAL,"ERROR_PARTIAL"},
	{ERROR_BADPARTIAL,"ERROR_BADPARTIAL"},
	{ERROR_INTERNAL,"ERROR_INTERNAL"},
	{ERROR_BADCOUNT,"ERROR_BADCOUNT"},
	{ERROR_DFA_UITEM,"ERROR_DFA_UITEM"},
	{ERROR_DFA_UCOND,"ERROR_DFA_UCOND"},
	{ERROR_DFA_UMLIMIT,"ERROR_DFA_UMLIMIT"},
	{ERROR_DFA_WSSIZE,"ERROR_DFA_WSSIZE"},
	{ERROR_DFA_RECURSE,"ERROR_DFA_RECURSE"},
	{ERROR_RECURSIONLIMIT,"ERROR_RECURSIONLIMIT"},
	{ERROR_NULLWSLIMIT,"ERROR_NULLWSLIMIT"},
	{ERROR_BADNEWLINE,"ERROR_BADNEWLINE"}
}

constant all_options = math:or_all({
	DEFAULT,
	CASELESS,
	MULTILINE,
	DOTALL,
	EXTENDED,
	ANCHORED,
	DOLLAR_ENDONLY,
	EXTRA,
	NOTBOL,
	NOTEOL,
	UNGREEDY,
	NOTEMPTY,
	UTF8,
	NO_AUTO_CAPTURE,
	NO_UTF8_CHECK,
	AUTO_CALLOUT,
	PARTIAL,
	DFA_SHORTEST,
	DFA_RESTART,
	FIRSTLINE,
	DUPNAMES,
	NEWLINE_CR,
	NEWLINE_LF,
	NEWLINE_CRLF,
	NEWLINE_ANY,
	NEWLINE_ANYCRLF,
	BSR_ANYCRLF,
	BSR_UNICODE,
	STRING_OFFSETS
})

--****
-- === Create and Destroy

--**
-- Regular expression type
public type regex(object o)
	return sequence(o)
end type

--**
-- Regular expression option specification type
public type option_spec(object o)
	if atom(o) then
		if not integer(o) then
			return 0
		else
			if (or_bits(o,all_options) != all_options) then
				return 0
			else
				return 1
			end if
		end if
	elsif integer_array(o) then
		return option_spec( math:or_all(o) )
	else
		return 0
	end if
end type

--**
-- converts an option spec to a string.
public function option_spec_to_string(option_spec o)
	return flags:flags_to_string(o, option_names)
end function

--**
-- converts an regex error to a string.
public function error_to_string(integer i)
	if i >= 0 or i < -23 then
		return sprintf("%d",{i})
	else
		return search:vlookup(i, error_names, 1, 2, "Unknown Error")
	end if
end function

--**
-- returns an allocated regular expression.
public function new(string pattern, option_spec options=DEFAULT)
	if sequence(options) then
		options = math:or_all(options)
	end if

	-- concatenation ensures we really get a new sequence, and don't just use the
	-- one passed in, which could be another regex previously created...this may
	-- be a bug with the refcount/delete_instance/regex code
	return machine_func(M_PCRE_COMPILE, { pattern, options })
end function

--**
-- returns a text based error message.
public function error_message(object re)
	return machine_func(M_PCRE_ERROR_MESSAGE, { re })
end function

--****
-- === Utility Routines

--**
public function escape(string s)
	return text:escape(s, ".\\+*?[^]$(){}=!<>|:-")
end function

--**
-- returns the number of capturing subpatterns (the ovector size) for a regex.
public function get_ovector_size(regex ex, integer maxsize=0)
	integer m = machine_func(M_PCRE_GET_OVECTOR_SIZE, {ex})
	if (m > maxsize) then
		return maxsize
	end if

	return m+1
end function

--****
-- === Match

--**
-- returns the first match of ##re## in ##haystack##. You can optionally start at the position
-- ##from##.
public function find(regex re, string haystack, integer from=1, option_spec options=DEFAULT, integer size = get_ovector_size(re, 30))
	if sequence(options) then
		options = math:or_all(options)
	end if

	if size < 0 then
		size = 0
	end if

	return machine_func(M_PCRE_EXEC, { re, haystack, length(haystack), options, from, size })
end function

--**
-- returns all matches of ##re## in ##haystack## optionally starting at the sequence position
-- ##from##.
public function find_all(regex re, string haystack, integer from=1, option_spec options=DEFAULT, integer size = get_ovector_size(re, 30))
	if sequence(options) then
		options = math:or_all(options)
	end if

	if size < 0 then
		size = 0
	end if

	object result
	sequence results = {}
	atom pHaystack = machine:allocate_string(haystack)
	while sequence(result) with entry do
		results = append(results, result)
		from = math:max(result) + 1

		if from > length(haystack) then
			exit
		end if
	entry
		result = machine_func(M_PCRE_EXEC, { re, pHaystack, length(haystack), options, from, size })
	end while

	machine:free(pHaystack)

	return results
end function

--**
-- determines if ##re## matches any portion of ##haystack##.
public function has_match(regex re, string haystack, integer from=1, option_spec options=DEFAULT)
	return sequence(find(re, haystack, from, options))
end function

--**
-- determines if the entire ##haystack## matches ##re##.
public function is_match(regex re, string haystack, integer from=1, option_spec options=DEFAULT)
	object m = find(re, haystack, from, options)

	if sequence(m) and length(m) > 0 and m[1][1] = from and m[1][2] = length(haystack) then
		return 1
	end if

	return 0
end function

--**
-- gets the matched text only.
public function matches(regex re, string haystack, integer from=1, option_spec options=DEFAULT)
	if sequence(options) then
		options = math:or_all(options)
	end if
	integer str_offsets = and_bits(STRING_OFFSETS, options)
	object match_data = find(re, haystack, from, and_bits(options, not_bits(STRING_OFFSETS)))

	if atom(match_data) then
		return ERROR_NOMATCH
	end if

	for i = 1 to length(match_data) do
		sequence tmp
		if match_data[i][1] = 0 then
			tmp = ""
		else
			tmp = haystack[match_data[i][1]..match_data[i][2]]
		end if
		if str_offsets then
			match_data[i] = { tmp, match_data[i][1], match_data[i][2] }
		else
			match_data[i] = tmp
		end if
	end for

	return match_data
end function

--**
-- gets the text of all matches.
public function all_matches(regex re, string haystack, integer from=1, option_spec options=DEFAULT)
	if sequence(options) then
		options = math:or_all(options)
	end if
	integer str_offsets = and_bits(STRING_OFFSETS, options)
	object match_data = find_all(re, haystack, from, and_bits(options, not_bits(STRING_OFFSETS)))

	if length(match_data) = 0 then
		return ERROR_NOMATCH
	end if

	for i = 1 to length(match_data) do
		for j = 1 to length(match_data[i]) do
			sequence tmp
			integer a,b
			a = match_data[i][j][1]
			if a = 0 then
				tmp = ""
			else
				b = match_data[i][j][2]
				tmp = haystack[a..b]
			end if
			if str_offsets then
				match_data[i][j] = { tmp, a, b }
			else
				match_data[i][j] = tmp
			end if
		end for
	end for

	return match_data
end function

--****
-- === Splitting

--**
-- splits a string based on a regex as a delimiter.
public function split(regex re, string text, integer from=1, option_spec options=DEFAULT)
	return split_limit(re, text, 0, from, options)
end function

public function split_limit(regex re, string text, integer limit=0, integer from=1, option_spec options=DEFAULT)
	if sequence(options) then
		options = math:or_all(options)
	end if
	sequence match_data = find_all(re, text, from, options), result
	integer last = 1

	if limit = 0 or limit > length(match_data) then
		limit = length(match_data)
	end if

	result = repeat(0, limit)

	for i = 1 to limit do
		integer a
		a = match_data[i][1][1]
		if a = 0 then
			result[i] = ""
		else
			result[i] = text[last..a - 1]
			last = match_data[i][1][2] + 1
		end if
	end for

	if last < length(text) then
		result &= { text[last..$] }
	end if

	return result
end function

--****
-- === Replacement

--**
-- replaces all matches of a regex with the replacement text.
public function find_replace(regex ex, string text, sequence replacement, integer from=1,
			option_spec options=DEFAULT)
	return find_replace_limit(ex, text, replacement, -1, from, options)
end function

--**
-- replaces up to ##limit## matches of ##ex## in ##text## except when ##limit## is 0.  When
-- ##limit## is 0, this routine replaces all of the matches.
public function find_replace_limit(regex ex, string text, sequence replacement,
			integer limit, integer from=1, option_spec options=DEFAULT)
	if sequence(options) then
		options = math:or_all(options)
	end if

    return machine_func(M_PCRE_REPLACE, { ex, text, replacement, options,
			from, limit })
end function

--**
-- finds and then replaces text that is processed by a call back function.
public function find_replace_callback(regex ex, string text, integer rid, integer limit=0,
                integer from=1, option_spec options=DEFAULT)
	if sequence(options) then
		options = math:or_all(options)
	end if
	sequence match_data = find_all(ex, text, from, options), replace_data

	if limit = 0 or limit > length(match_data) then
		limit = length(match_data)
	end if
	replace_data = repeat(0, limit)

	for i = 1 to limit do
		sequence params = repeat(0, length(match_data[i]))
		for j = 1 to length(match_data[i]) do
			if equal(match_data[i][j],{0,0}) then
				params[j] = 0
			else
				params[j] = text[match_data[i][j][1]..match_data[i][j][2]]
			end if
		end for

		replace_data[i] = call_func(rid, { params })
	end for

	for i = limit to 1 by -1 do
		text = replace(text, replace_data[i], match_data[i][1][1], match_data[i][1][2])
	end for

	return text
end function
