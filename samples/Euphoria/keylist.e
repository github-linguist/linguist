-- (c) Copyright - See License.txt
--
-- KEYWORDS and BUILTIN ROUTINES

ifdef ETYPE_CHECK then
	with type_check
elsedef
	without type_check
end ifdef

include global.e
include reswords.e
include emit.e

export enum
	K_NAME,      -- string
	K_SCOPE,     -- keyword or predefined
	K_TOKEN,     -- token number returned to parser
	K_OPCODE,    -- opcode to emit (predefined subprograms)
	K_NUM_ARGS,  -- number of arguments (predefined subprograms)
	K_EFFECT,    -- side effects
	-- optional fields
	K_CODE,
	K_DEF_ARGS

-- N.B. order and number of keywords and builtins
-- is assumed by scanner.e, euphoria\bin\keywords.e, and others
export sequence keylist = {
	-- KEYWORDS
	{"if",        SC_KEYWORD, IF,        0, 0, 0},
	{"end",       SC_KEYWORD, END,       0, 0, 0},
	{"then",      SC_KEYWORD, THEN,      0, 0, 0},
	{"procedure", SC_KEYWORD, PROCEDURE, 0, 0, 0},
	{"else",      SC_KEYWORD, ELSE,      0, 0, 0},
	{"for",       SC_KEYWORD, FOR,       0, 0, 0},
	{"return",    SC_KEYWORD, RETURN,    0, 0, 0},
	{"do",        SC_KEYWORD, DO,        0, 0, 0},
	{"elsif",     SC_KEYWORD, ELSIF,     0, 0, 0},
	{"while",     SC_KEYWORD, WHILE,     0, 0, 0},
	{"type",      SC_KEYWORD, TYPE_DECL, 0, 0, 0},
	{"constant",  SC_KEYWORD, CONSTANT,  0, 0, 0},
	{"to",        SC_KEYWORD, TO,        0, 0, 0},
	{"and",       SC_KEYWORD, AND,       0, 0, 0},
	{"or",        SC_KEYWORD, OR,        0, 0, 0},
	{"exit",      SC_KEYWORD, EXIT,      0, 0, 0},
	{"function",  SC_KEYWORD, FUNCTION,  0, 0, 0},
	{"global",    SC_KEYWORD, GLOBAL,    0, 0, 0},
	{"by",        SC_KEYWORD, BY,        0, 0, 0},
	{"not",       SC_KEYWORD, NOT,       0, 0, 0},
	{"include",   SC_KEYWORD, INCLUDE,   0, 0, 0},
	{"with",      SC_KEYWORD, WITH,      0, 0, 0},
	{"without",   SC_KEYWORD, WITHOUT,   0, 0, 0},
	{"xor",       SC_KEYWORD, XOR,       0, 0, 0},
	{"continue",  SC_KEYWORD, CONTINUE,  0, 0, 0},
	{"ifdef",     SC_KEYWORD, IFDEF,     0, 0, 0},
	{"elsedef",   SC_KEYWORD, ELSEDEF,   0, 0, 0},
	{"elsifdef",  SC_KEYWORD, ELSIFDEF,  0, 0, 0},
	{"label",     SC_KEYWORD, LABEL,     0, 0, 0},
	{"loop",      SC_KEYWORD, LOOP,      0, 0, 0},
	{"until",     SC_KEYWORD, UNTIL,     0, 0, 0},
	{"entry",     SC_KEYWORD, ENTRY,     0, 0, 0},
	{"break",     SC_KEYWORD, BREAK,     0, 0, 0},
	{"retry",     SC_KEYWORD, RETRY,     0, 0, 0},
	{"enum",      SC_KEYWORD, ENUM,      0, 0, 0},
	{"export",    SC_KEYWORD, EXPORT,    0, 0, 0},
	{"switch",    SC_KEYWORD, SWITCH,    0, 0, 0},
	{"case",      SC_KEYWORD, CASE,      0, 0, 0},
	{"override",  SC_KEYWORD, OVERRIDE,  0, 0, 0},
	{"goto",      SC_KEYWORD, GOTO,      0, 0, 0},
	{"public",    SC_KEYWORD, PUBLIC,    0, 0, 0 },
	{"fallthru",  SC_KEYWORD, FALLTHRU,  0, 0, 0},
	-- new ones must go at end to maintain compatibility with old shrouded code

	-- PREDEFINED SUBPROGRAMS and TYPEs
	{"length",           SC_PREDEF, FUNC, LENGTH,           1, E_PURE},
	{"puts",             SC_PREDEF, PROC, PUTS,             2, E_OTHER_EFFECT},
	{"integer",          SC_PREDEF, TYPE, IS_AN_INTEGER,    1, E_PURE},
	{"sequence",         SC_PREDEF, TYPE, IS_A_SEQUENCE,    1, E_PURE},
	{"position",         SC_PREDEF, PROC, POSITION,         2, E_OTHER_EFFECT},
	{"object",           SC_PREDEF, TYPE, IS_AN_OBJECT,     1, E_PURE},
	{"append",           SC_PREDEF, FUNC, APPEND,           2, E_PURE},
	{"prepend",          SC_PREDEF, FUNC, PREPEND,          2, E_PURE},
	{"print",            SC_PREDEF, PROC, PRINT,            2, E_OTHER_EFFECT},
	{"printf",           SC_PREDEF, PROC, PRINTF,           3, E_OTHER_EFFECT, {0,0,{{STRING,{}}}}, {3,2,{3}} },
	{"clear_screen",     SC_PREDEF, PROC, CLEAR_SCREEN,     0, E_OTHER_EFFECT},
	{"floor",            SC_PREDEF, FUNC, FLOOR,            1, E_PURE},
	{"getc",             SC_PREDEF, FUNC, GETC,             1, E_OTHER_EFFECT},
	{"gets",             SC_PREDEF, FUNC, GETS,             1, E_OTHER_EFFECT},
	{"get_key",          SC_PREDEF, FUNC, GET_KEY,          0, E_PURE},
	{"rand",             SC_PREDEF, FUNC, RAND,             1, E_PURE},
	{"repeat",           SC_PREDEF, FUNC, REPEAT,           2, E_PURE},
	{"atom",             SC_PREDEF, TYPE, IS_AN_ATOM,       1, E_PURE},
	{"compare",          SC_PREDEF, FUNC, COMPARE,          2, E_PURE},
	{"find",             SC_PREDEF, FUNC, FIND_FROM,        3, E_PURE, {0,0,{{ATOM,1}}}, {3,2,{3}}},
	{"match",            SC_PREDEF, FUNC, MATCH_FROM,       3, E_PURE, {0,0,{{ATOM,1}}}, {3,2,{3}}},
	{"time",             SC_PREDEF, FUNC, TIME,             0, E_PURE},
	{"command_line",     SC_PREDEF, FUNC, COMMAND_LINE,     0, E_PURE},
	{"open",             SC_PREDEF, FUNC, OPEN,             3, E_OTHER_EFFECT, {0,0,{{ATOM,0}}}, {3,2,{3}} },
	{"close",            SC_PREDEF, PROC, CLOSE,            1, E_OTHER_EFFECT},
	{"trace",            SC_PREDEF, PROC, TRACE,            1, E_PURE},
	{"getenv",           SC_PREDEF, FUNC, GETENV,           1, E_PURE},
	{"sqrt",             SC_PREDEF, FUNC, SQRT,             1, E_PURE},
	{"sin",              SC_PREDEF, FUNC, SIN,              1, E_PURE},
	{"cos",              SC_PREDEF, FUNC, COS,              1, E_PURE},
	{"tan",              SC_PREDEF, FUNC, TAN,              1, E_PURE},
	{"log",              SC_PREDEF, FUNC, LOG,              1, E_PURE},
	{"system",           SC_PREDEF, PROC, SYSTEM,           2, E_OTHER_EFFECT, {0,{{ATOM,0}}}, {2,1,{2}}},
	{"date",             SC_PREDEF, FUNC, DATE,             0, E_PURE},
	{"remainder",        SC_PREDEF, FUNC, REMAINDER,        2, E_PURE},
	{"power",            SC_PREDEF, FUNC, POWER,            2, E_PURE},
	{"machine_func",     SC_PREDEF, FUNC, MACHINE_FUNC,     2, E_OTHER_EFFECT, {0,{{STRING,{}}}}, {2,1,{2}}},
	{"machine_proc",     SC_PREDEF, PROC, MACHINE_PROC,     2, E_OTHER_EFFECT, {0,{{STRING,{}}}}, {2,1,{2}}},
	{"abort",            SC_PREDEF, PROC, ABORT,            1, E_OTHER_EFFECT},
	{"peek",             SC_PREDEF, FUNC, PEEK,             1, E_PURE},
	{"poke",             SC_PREDEF, PROC, POKE,             2, E_OTHER_EFFECT},
	{"call",             SC_PREDEF, PROC, CALL,             1, E_OTHER_EFFECT},
	{"sprintf",          SC_PREDEF, FUNC, SPRINTF,          2, E_PURE},
	{"arctan",           SC_PREDEF, FUNC, ARCTAN,           1, E_PURE},
	{"and_bits",         SC_PREDEF, FUNC, AND_BITS,         2, E_PURE},
	{"or_bits",          SC_PREDEF, FUNC, OR_BITS,          2, E_PURE},
	{"xor_bits",         SC_PREDEF, FUNC, XOR_BITS,         2, E_PURE},
	{"not_bits",         SC_PREDEF, FUNC, NOT_BITS,         1, E_PURE},
	{"mem_copy",         SC_PREDEF, PROC, MEM_COPY,         3, E_OTHER_EFFECT},
	{"mem_set",          SC_PREDEF, PROC, MEM_SET,          3, E_OTHER_EFFECT},
	{"c_proc",           SC_PREDEF, PROC, C_PROC,           2, E_ALL_EFFECT, {0,{{STRING,{}}}}, {2,1,{2}}},
	{"c_func",           SC_PREDEF, FUNC, C_FUNC,           2, E_ALL_EFFECT, {0,{{STRING,{}}}}, {2,1,{2}}},
	{"routine_id",       SC_PREDEF, FUNC, ROUTINE_ID,       1, E_PURE},
	{"call_proc",        SC_PREDEF, PROC, CALL_PROC,        2, E_ALL_EFFECT, {0,{{STRING,{}}}}, {2,1,{2}}},
	{"call_func",        SC_PREDEF, FUNC, CALL_FUNC,        2, E_ALL_EFFECT, {0,{{STRING,{}}}}, {2,1,{2}}},
	{"poke4",            SC_PREDEF, PROC, POKE4,            2, E_OTHER_EFFECT},
	{"peek4s",           SC_PREDEF, FUNC, PEEK4S,           1, E_PURE},
	{"peek4u",           SC_PREDEF, FUNC, PEEK4U,           1, E_PURE},
	{"profile",          SC_PREDEF, PROC, PROFILE,          1, E_PURE},
	{"equal",            SC_PREDEF, FUNC, EQUAL,            2, E_PURE},
	{"system_exec",      SC_PREDEF, FUNC, SYSTEM_EXEC,      2, E_OTHER_EFFECT, {0,{{ATOM,0}}}, {2,1,{2}}},
	{"platform",         SC_PREDEF, FUNC, PLATFORM,         0, E_PURE},
	{"task_create",      SC_PREDEF, FUNC, TASK_CREATE,      2, E_OTHER_EFFECT},
	{"task_schedule",    SC_PREDEF, PROC, TASK_SCHEDULE,    2, E_OTHER_EFFECT},
	{"task_yield",       SC_PREDEF, PROC, TASK_YIELD,       0, E_ALL_EFFECT},
	{"task_self",        SC_PREDEF, FUNC, TASK_SELF,        0, E_PURE},
	{"task_suspend",     SC_PREDEF, PROC, TASK_SUSPEND,     1, E_OTHER_EFFECT},
	{"task_list",        SC_PREDEF, FUNC, TASK_LIST,        0, E_PURE},
	{"task_status",      SC_PREDEF, FUNC, TASK_STATUS,      1, E_PURE},
	{"task_clock_stop",  SC_PREDEF, PROC, TASK_CLOCK_STOP,  0, E_PURE},
	{"task_clock_start", SC_PREDEF, PROC, TASK_CLOCK_START, 0, E_PURE},
	{"find_from",        SC_PREDEF, FUNC, FIND_FROM,        3, E_PURE},
	{"match_from",       SC_PREDEF, FUNC, MATCH_FROM,       3, E_PURE},
	{"poke2",            SC_PREDEF, PROC, POKE2,            2, E_OTHER_EFFECT},
	{"peek2s",           SC_PREDEF, FUNC, PEEK2S,           1, E_PURE},
	{"peek2u",           SC_PREDEF, FUNC, PEEK2U,           1, E_PURE},
	{"peeks",            SC_PREDEF, FUNC, PEEKS,            1, E_PURE},
	{"peek_string",      SC_PREDEF, FUNC, PEEK_STRING,      1, E_PURE},
	{"option_switches",  SC_PREDEF, FUNC, OPTION_SWITCHES,  0, E_PURE},
	{"warning",  		 SC_PREDEF, PROC, WARNING,  		1, E_OTHER_EFFECT},
	{"splice",			 SC_PREDEF,	FUNC, SPLICE,			3, E_PURE},
	{"insert",			 SC_PREDEF,	FUNC, INSERT,			3, E_PURE},
	{"include_paths",	 SC_PREDEF,	FUNC, INCLUDE_PATHS,	1, E_OTHER_EFFECT},
	{"hash",             SC_PREDEF, FUNC, HASH,             2, E_PURE},
	{"head",             SC_PREDEF, FUNC, HEAD,             2, E_PURE, {0,{{ATOM,1}}},{2,1,{2}}},
	{"tail",             SC_PREDEF, FUNC, TAIL,             2, E_PURE,
									{0,{{BUILT_IN,"length"},{LEFT_ROUND,0},
									 {DEF_PARAM,1},{RIGHT_ROUND,0},{MINUS,0},
									 {ATOM,1}}},{2,1,{2}}},
	{"remove",           SC_PREDEF, FUNC, REMOVE,           3, E_PURE, {0,0,{{DEF_PARAM,2}}}, {3,2,{3}}},
	{"replace",          SC_PREDEF, FUNC, REPLACE,          4, E_PURE, {0,0,0,{{DEF_PARAM,3}}}, {4,3,{4}}},
	{"delete_routine",   SC_PREDEF, FUNC, DELETE_ROUTINE,   2, E_PURE},
	{"delete",           SC_PREDEF, PROC, DELETE_OBJECT,    1, E_OTHER_EFFECT},
	{"routine",          SC_KEYWORD, ROUTINE,   0, 0, 0},
	{"poke8",            SC_PREDEF, PROC, POKE8,            2, E_OTHER_EFFECT},
	{"peek8s",           SC_PREDEF, FUNC, PEEK8S,           1, E_PURE},
	{"peek8u",           SC_PREDEF, FUNC, PEEK8U,           1, E_PURE},
	{"poke_pointer",     SC_PREDEF, PROC, POKE_POINTER,     2, E_OTHER_EFFECT},
	{"peek_pointer",     SC_PREDEF, FUNC, PEEK_POINTER,     1, E_PURE},
	{"sizeof",           SC_PREDEF, FUNC, SIZEOF,           1, E_PURE},
	{"deprecate",        SC_KEYWORD, DEPRECATE, 0, 0, 0},
	{"poke_long",        SC_PREDEF, PROC, POKE_LONG,        2, E_OTHER_EFFECT},
	{"peek_longs",       SC_PREDEF, FUNC, PEEK_LONGS,       1, E_PURE},
	{"peek_longu",       SC_PREDEF, FUNC, PEEK_LONGU,       1, E_PURE},
	$

}
	-- new words must go at end to maintain compatibility

if EXTRA_CHECK then
	-- for debugging storage leaks
	keylist = append(keylist, {"space_used", SC_PREDEF, FUNC, SPACE_USED,
							   0, E_PURE})
end if

-- top level pseudo-procedure (assumed to be last on the list)
keylist = append(keylist, {"<TopLevel>", SC_PREDEF, PROC, 0, 0, E_ALL_EFFECT})

export function find_category(integer tokid)
	sequence catname = "reserved word"
	for i = 1 to length(token_category) do
		if token_category[i][1] = tokid then
			catname = token_catname[token_category[i][2]]
			exit
		end if
	end for
	return catname
end function

export function find_token_text(integer tokid)
	for i = 1 to length(keylist) do
		if keylist[i][3] = tokid then
			return keylist[i][1]
		end if
	end for
	return LexName(tokid, "unknown word")
end function

--===========================================
-- How to add defaulted parms to builtins
--===========================================
--
-- Here are fictitious entries in keylist, which have been used for testing purposes:
-- Here, equal() gets its second parameter defaulted, not the first.
-- This is why the K_CODE field starts with 0.
--	{"equal",            SC_PREDEF, FUNC, EQUAL,            2, E_PURE,
-- The K_DEF_ARGS field reflects the lists of defaulted and non defaulted params, and
-- is just like it were built in a S_DEF_ARGS field of a regular SymTab entry, built in parser:SubProg().
-- So here it reads {2,1,{2}}: param 2 is the first defaulted, param 1 is the last non defaulted
-- and the complete list of defparm indexes is {2}.
--
-- Now the K_CODE entry:
-- 1/ second param defaults to 0: {0,{{{ATOM,0}}
-- 2/ second param defaults to length(command_line()):  {0,
-- {{BUILT_IN,"length"},
-- {LEFT_ROUND,0},
-- {BUILT_IN,"command_line"},
-- {LEFT_ROUND,0},
-- {RIGHT_ROUND,0},
-- {RIGHT_ROUND,0}
-- }}
-- 3/ second param defaults to the first one: {0,{{DEF_PARAM,1}}}
--
-- So, the K_CODE field is a sequence of either 0's or sequences of almost regular tokens.
-- Notes:
-- Only builtin names can appear in a BUILT_IN token;
-- The T_SYM entry in a DEF_PARAM token is the index of the param being referred to. It must
-- be less than the current index in the K_CODE field.
