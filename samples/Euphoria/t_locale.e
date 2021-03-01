
include std/localeconv.e as lcc
include std/locale.e as l
include std/datetime.e as d

include std/unittest.e
include std/mathcons.e

sequence locale

locale = "en_US"

-- The OS may use an optional encoding specifier:
integer ix = set( "" )
sequence native_locale = l:get()
ix = find( '.', native_locale )

sequence encoding
if ix then
	encoding = native_locale[ix..$]
else
	encoding = ""
end if


test_true("set()", l:set("C"))
test_equal("set/get", lcc:decanonical("C"), lcc:decanonical(l:get()))

integer has_locale = l:set(locale & encoding)
if not has_locale then
	encoding = ".US-ASCII"
	has_locale = l:set(locale & encoding)
end if

if has_locale then
	test_equal("set/get en_US", lcc:decanonical(locale & encoding), lcc:decanonical(l:get()))
	test_equal("money", "$1,020.50", l:money(1020.50))
	test_equal("number", "1,020.50", l:number(1020.5))
	test_equal("integer", "1,020", l:number(1020))
	test_equal("number", "1,020.10", l:number(1020.1))
	test_equal("large integer", "9,999,999,999,999", l:number(9_999_999_999_999))
else
	-- can not test, maybe emit a warning?
	puts(1, "warning can not test en_US locale, testing against C locale..\n")
	test_equal("money", "1020.50", l:money(1020.50))
	test_equal("number", "1020.50", l:number(1020.5))
	test_equal("integer", "1020", l:number(1020))
	test_equal("number", "1020.10", l:number(1020.1))
	test_equal("large integer", "9999999999999", l:number(9_999_999_999_999))
end if

d:datetime dt1
dt1 = d:new(2008, 5, 4, 9, 55, 23)

test_equal("datetime", "Sunday, May 04, 2008",
    l:datetime("%A, %B %d, %Y", dt1))

------------------------------------------------------------------------------------------
--
-- Test Language Translation
--
------------------------------------------------------------------------------------------

l:set_lang_path("") -- current directory
test_equal("set_lang_path/get_lang_path", "", l:get_lang_path())

object langmap 

langmap =  l:lang_load("test")
test_true("lang_load() #1", sequence(langmap) and length(langmap) = 2)

test_equal("translate() #1", "Hello", l:translate("hello",langmap))

test_equal("translate() #2a", -1,      l:translate("world",, -1))
l:set_def_lang(langmap)
test_equal("translate() #2b", "World", l:translate("world",, -1))

test_equal("translate() #3", "%s, %s!", l:translate("greeting"))
test_equal("translate() sprintf() #1", "Hello, World!",
    sprintf(l:translate("greeting"), {l:translate("hello"), l:translate("world")}))
test_equal("translate() long message #1", "Hello %s,\nI hope you enjoy this email!",
    l:translate("long_message"))


l:set_def_lang(l:lang_load("test2"))
test_equal("translate() #4a", "Hola", l:translate("hello"))

-- Reverse translation
test_equal("translate() #4b", "hello", l:translate("Hola",,,1))

test_equal("translate() #5", "Mundo", l:translate("world"))
test_equal("translate() #6", "%s, %s!", l:translate("greeting"))
test_equal("translate() sprintf() #2", "Hola, Mundo!",
    sprintf(l:translate("greeting"), {l:translate("hello"), l:translate("world")}))
    
test_equal("translate() #7", -1, l:translate("g'day",, -1))
test_equal("translate() #8", "", l:translate("g'day"))
test_equal("translate() #8a", "g'day", l:translate("g'day",, PINF))

test_equal("translate() #9", "This is an example of some \n  translation text that spans \n   multiple lines.", l:translate("help text"))

test_equal("trsprintf #1", "Hola, Bob!",  trsprintf("greeting", {       "hello", "Bob"}))
test_equal("trsprintf #2", "hello, Bob!", trsprintf("greeting", {"__" & "hello", "Bob"}))

test_equal("get_text unknown number", 0, get_text(-2))

test_equal("get_text known number A", "Block comment from line [1] not terminated.", get_text(42,"two","testlocale"))
test_equal("get_text known number B", "Block comment from line [1] not terminated.", get_text(42,{"zero","two"},"testlocale"))
test_equal("get_text known number C", "Block comment from line [1] not terminated.", get_text(42, {"zero"},"testlocale"))

test_report()
