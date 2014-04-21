// Originally from /jsonion_test.gml in JSOnion
// JSOnion v1.0.0d is licensed under the MIT licence. You may freely adapt and use this library in commercial and non-commercial projects.

#define assert_true
{
    /**
    assert_true(result, errormsg): Display error <errormsg> unless <result> is true.
    */
    
    if (!argument0) {
        _assert_error_popup(argument1 + string_repeat(_assert_newline(), 2) + "Expected true, got false.");
    }
}

#define assert_false
{
    /**
    assert_false(result, errormsg): Display error <errormsg> unless <result> is false.
    */
    
    if (argument0) {
        _assert_error_popup(argument1 + string_repeat(_assert_newline(), 2) + "Expected false, got true.");
    }
}

#define assert_equal
{
    /**
    assert_equal(expected, result, errormsg): Display error <errormsg> unless <expected> and <result> are equal.
    */
    
    //Safe equality check; won't crash even if the two are different types
    var match;
    match = is_string(argument0) == is_string(argument1);
    if (match) {
        match = argument0 == argument1;
    }

    //No match?
    if (!match) {
        //Data types
        var type;
        type[0] = "(Real)";
        type[1] = "(String)";
        
        //Construct message
        var msg;
        msg = argument2;
        //Add expected value
        msg += string_repeat(_assert_newline(), 2);
        msg += "Expected " + type[is_string(argument0)] + ":" + _assert_newline();
        msg += _assert_debug_value(argument0);
        //Add actual value
        msg += string_repeat(_assert_newline(), 2);
        msg += "Actual " + type[is_string(argument1)] + ":" + _assert_newline();
        msg += _assert_debug_value(argument1);
        
        //Display message
        _assert_error_popup(msg);
    }
}

#define _assert_error_popup
{
    /**
    _assert_error_popup(msg): Display an assertion error.
    */
    
    //Display message
    if (os_browser == browser_not_a_browser) {
        show_error(argument0, false); //Full-fledged error message for non-browser environments
    } else {
        show_message(argument0); //Browsers don't support show_error(), use show_message() instead
    }
}

#define _assert_debug_value
{
    /**
    _assert_debug_value(val): Returns a low-level debug value for the value <val>.
    <val> can be a string or a real value.
    */
    
    //String
    if (is_string(argument0)) {
        if (os_browser == browser_not_a_browser) {
            return argument0;
        } else {
            return string_replace_all(argument0, "#", "\#");
        }
    }
    
    //Numeric --- use GMTuple's algorithm
    else {
    
        //Integers
        if (frac(argument0) == 0) {
            return string(argument0);
        }
        
        //Decimal numbers; get exponent and mantissa  
        var mantissa, exponent;
        exponent = floor(log10(abs(argument0)));
        mantissa = string_format(argument0/power(10,exponent), 15, 14);
        //Look for trailing zeros in the mantissa
        var i, ca;
        i = string_length(mantissa);
        do {
            ca = string_char_at(mantissa, i);
            i -= 1;
        } until (ca != "0")
        //Remove the dot if only the first digit of the normalized mantissa is nonzero
        if (ca != ".") {
            mantissa = string_copy(mantissa, 1, i+1);
        }
        else {
            mantissa = string_copy(mantissa, 1, i);
        }
        //Remove the exponent if it is 0
        if (exponent != 0) {
            return mantissa + "e" + string(exponent);
        } else {
            return mantissa;
        }
    
    //GMTuple algorithm done
    }
}

#define _assert_newline
{
    /**
    _assert_newline(): Returns a system-appropriate newline character sequence.
    */
    
    if (os_browser == browser_not_a_browser) {
        return chr(13) + chr(10);
    } else {
        return "#";
    }
}

#define jso_test_all
{
    /**
    jso_test_all(): Run the test suite for the JSOnion library.
    JSOnion version: 1.0.0d
    */
    var a, b;
    a = current_time;
    _test_jso_new();
    _test_jso_map_add();
    _test_jso_list_add();
    _test_jso_encode();
    _test_jso_compare();
    _test_jso_decode();
    _test_jso_lookup();
    _test_jso_bugs();
    __jso_gmt_test_all();
    b = current_time;
    show_debug_message("JSOnion: Tests completed in " + string(b-a) + "ms.");
}

#define _test_jso_new
{
    /**
    _test_jso_new(): Test jso_new_*() functions.
    JSOnion version: 1.0.0d
    */
    
    var expected, actual;
    
    //jso_new_map()
    actual = jso_new_map();
    assert_true(actual >= 0, "jso_new_map() failed to create a new map.");
    ds_map_destroy(actual);
    
    //jso_new_list()
    actual = jso_new_list();
    assert_true(actual >= 0, "jso_new_list() failed to create a new list.");
    ds_list_destroy(actual);
}

#define _test_jso_map_add
{
    /**
    _test_jso_map_add(): Test jso_map_add_*() functions.
    JSOnion version: 1.0.0d
    */
    
    var expected, actual, key, map;
    map = jso_new_map();
    
    //jso_map_add_real()
    expected = pi;
    key = "pi";
    jso_map_add_real(map, key, expected);
    actual = jso_map_get(map, key);
    assert_true(ds_map_exists(map, key), "jso_map_add_real() failed to add the number.");
    assert_equal(expected, actual, "jso_map_add_real() added the wrong number.");
    ds_map_delete(map, key);
    
    //jso_map_add_string()
    expected = "waahoo";
    key = "str";
    jso_map_add_string(map, key, expected);
    actual = jso_map_get(map, key);
    assert_true(ds_map_exists(map, key), "jso_map_add_string() failed to add the string.");
    assert_equal(expected, actual, "jso_map_add_string() added the wrong string.");
    ds_map_delete(map, key);
    
    //jso_map_add_sublist()
    expected = jso_new_list();
    key = "sublist";
    jso_map_add_sublist(map, key, expected);
    actual = jso_map_get(map, key);
    assert_true(ds_map_exists(map, key), "jso_map_add_sublist() failed to add the new sublist.");
    assert_equal(expected, actual, "jso_map_add_sublist() added the wrong sublist.");
    ds_map_delete(map, key);
    ds_list_destroy(expected);
    
    //jso_map_add_submap()
    expected = jso_new_map();
    key = "sublist";
    jso_map_add_sublist(map, key, expected);
    actual = jso_map_get(map, key);
    assert_true(ds_map_exists(map, key), "jso_map_add_submap() failed to add the new submap.");
    assert_equal(expected, actual, "jso_map_add_submap() added the wrong submap.");
    ds_map_delete(map, key);
    ds_map_destroy(expected);
    
    //jso_map_add_integer()
    expected = 2345;
    key = "integer";
    jso_map_add_integer(map, key, expected);
    actual = jso_map_get(map, key);
    assert_true(ds_map_exists(map, key), "jso_map_add_integer() failed to add the new integer.");
    assert_equal(expected, actual, "jso_map_add_integer() added the wrong integer.");
    ds_map_delete(map, key);
    
    //jso_map_add_boolean() --- true
    expected = true;
    key = "booleantrue";
    jso_map_add_boolean(map, key, expected);
    actual = jso_map_get(map, key);
    assert_true(ds_map_exists(map, key), "jso_map_add_boolean() failed to add true.");
    assert_true(actual, "jso_map_add_boolean() added the wrong true.");
    ds_map_delete(map, key);
    
    //jso_map_add_boolean() --- false
    expected = false;
    key = "booleanfalse";
    jso_map_add_boolean(map, key, expected);
    actual = jso_map_get(map, key)
    assert_true(ds_map_exists(map, key), "jso_map_add_boolean() failed to add false.");
    assert_false(actual, "jso_map_add_boolean() added the wrong false.");
    ds_map_delete(map, key);
    
    //Cleanup
    ds_map_destroy(map);
}

#define _test_jso_list_add
{
    /**
    _test_jso_list_add(): Test jso_list_add_*() functions.
    JSOnion version: 1.0.0d
    */
    
    var expected, actual, list;
    list = jso_new_list();
    
    //jso_list_add_real()
    expected = pi;
    jso_list_add_real(list, expected);
    actual = jso_list_get(list, 0);
    assert_false(ds_list_empty(list), "jso_list_add_real() failed to add the number.");
    assert_equal(expected, actual, "jso_list_add_real() added the wrong number.");
    ds_list_clear(list);
    
    //jso_list_add_string()
    expected = "waahoo";
    jso_list_add_string(list, expected);
    actual = jso_list_get(list, 0);
    assert_false(ds_list_empty(list), "jso_list_add_string() failed to add the string.");
    assert_equal(expected, actual, "jso_list_add_string() added the wrong string.");
    ds_list_clear(list);
    
    //jso_list_add_sublist()
    expected = jso_new_list();
    jso_list_add_sublist(list, expected);
    actual = jso_list_get(list, 0);
    assert_false(ds_list_empty(list), "jso_list_add_sublist() failed to add the sublist.");
    assert_equal(expected, actual, "jso_list_add_sublist() added the wrong sublist.");
    ds_list_clear(list);
    ds_list_destroy(expected);
    
    //jso_list_add_submap()
    expected = jso_new_map();
    jso_list_add_submap(list, expected);
    actual = jso_list_get(list, 0);
    assert_false(ds_list_empty(list), "jso_list_add_submap() failed to add the submap.");
    assert_equal(expected, actual, "jso_list_add_submap() added the wrong submap.");
    ds_list_clear(list);
    ds_map_destroy(expected);
    
    //jso_list_add_integer()
    expected = 2345;
    jso_list_add_integer(list, expected);
    actual = jso_list_get(list, 0);
    assert_false(ds_list_empty(list), "jso_list_add_integer() failed to add integer.");
    assert_equal(expected, actual, "jso_list_add_integer() added the wrong integer.");
    ds_list_clear(list);
    
    //jso_list_add_boolean() --- true
    expected = true;
    jso_list_add_boolean(list, expected);
    actual = jso_list_get(list, 0);
    assert_false(ds_list_empty(list), "jso_list_add_boolean() failed to add boolean true.");
    assert_true(actual, "jso_list_add_boolean() added the wrong boolean true.");
    ds_list_clear(list);
    
    //jso_list_add_boolean() --- false
    expected = false;
    jso_list_add_boolean(list, expected);
    actual = jso_list_get(list, 0);
    assert_false(ds_list_empty(list), "jso_list_add_boolean() failed to add boolean false.");
    assert_false(actual, "jso_list_add_boolean() added the wrong boolean false.");
    ds_list_clear(list);
    
    //Cleanup
    ds_list_destroy(list);
}

#define _test_jso_encode
{
    /**
    _test_jso_encode(): Tests jso_encode_*() functions.
    JSOnion version: 1.0.0d
    */
    
    var original, expected, actual;
    
    //jso_encode_real() --- Positive
    expected = 3.1415;
    actual = jso_encode_real(3.1415);
    assert_equal(expected, real(actual), "jso_encode_real() failed to encode positive real!");
    
    //jso_encode_real() --- Negative
    expected = -2.71828;
    actual = jso_encode_real(-2.71828);
    assert_equal(expected, real(actual), "jso_encode_real() failed to encode negative real!");
    
    //jso_encode_real() --- Zero
    expected = 0;
    actual = jso_encode_integer(0);
    assert_equal(expected, real(actual), "jso_encode_real() failed to encode zero!");
    
    //jso_encode_integer() --- Positive
    expected = "2345";
    actual = jso_encode_integer(2345);
    assert_equal(expected, actual, "jso_encode_integer() failed to encode positive integer!");
    
    //jso_encode_integer() --- Negative
    expected = "-45";
    actual = jso_encode_integer(-45);
    assert_equal(expected, actual, "jso_encode_integer() failed to encode negative integer!");
    
    //jso_encode_integer() --- Zero
    expected = "0";
    actual = jso_encode_integer(0);
    assert_equal(expected, actual, "jso_encode_integer() failed to encode zero!");
    
    //jso_encode_boolean() --- true
    expected = "true";
    actual = jso_encode_boolean(true);
    assert_equal(expected, actual, "jso_encode_boolean() failed to encode true!");
    
    //jso_encode_boolean() --- false
    expected = "false";
    actual = jso_encode_boolean(false);
    assert_equal(expected, actual, "jso_encode_boolean() failed to encode false!");
    
    //jso_encode_string() --- Simple string
    expected = '"waahoo"';
    actual = jso_encode_string("waahoo");
    assert_equal(expected, actual, "jso_encode_string() failed to encode simple string!");
    
    //jso_encode_string() --- Empty string
    expected = '""';
    actual = jso_encode_string("");
    assert_equal(expected, actual, "jso_encode_string() failed to encode empty string!");
    
    //jso_encode_string() --- Basic escape characters
    expected = '"\\\"\b\f\n\r\t"';
    actual = jso_encode_string('\"' + chr(8) + chr(12) + chr(10) + chr(13) + chr(9));
    assert_equal(expected, actual, "jso_encode_string() failed to encode escape character string!");
    
    //jso_encode_map() --- Empty map
    var empty_map;
    empty_map = jso_new_map();
    expected = "{}";
    actual = jso_encode_map(empty_map);
    assert_equal(expected, actual, "jso_encode_map() failed to encode empty map!");
    jso_cleanup_map(empty_map);
    
    //jso_encode_map() --- One-element map
    var one_map;
    one_map = jso_new_map();
    jso_map_add_string(one_map, "key", "value");
    expected = '{"key":"value"}';
    actual = jso_encode_map(one_map);
    assert_equal(expected, actual, "jso_encode_map() failed to encode one-element map!");
    jso_cleanup_map(one_map);
    
    //jso_encode_map() --- Multi-element map
    var multi_map, ok1, ok2;
    multi_map = jso_new_map();
    jso_map_add_string(multi_map, "key1", "value\1");
    jso_map_add_integer(multi_map, "key2", 2);
    ok1 = '{"key1":"value\\1","key2":2}';
    ok2 = '{"key2":2,"key1":"value\\1"}';
    actual = jso_encode_map(multi_map);
    assert_true((actual == ok1) || (actual == ok2), "jso_encode_map() failed to encode multi-element map!");
    jso_cleanup_map(multi_map);
    
    //jso_encode_list() --- Empty list
    var empty_list;
    empty_list = jso_new_list();
    expected = "[]";
    actual = jso_encode_list(empty_list);
    assert_equal(expected, actual, "jso_encode_list() failed to encode empty list!");
    jso_cleanup_list(empty_list);
    
    //jso_encode_list() --- One-element nested list
    var one_list;
    one_list = jso_new_list();
    jso_list_add_submap(one_list, jso_new_map());
    expected = "[{}]";
    actual = jso_encode_list(one_list);
    assert_equal(expected, actual, "jso_encode_list() failed to encode one-element nested list!");
    jso_cleanup_list(one_list);
    
    //jso_encode_list() --- Multi-element nested list
    var multi_list, submap, sublist;
    multi_list = jso_new_list();
    submap = jso_new_map();
        jso_map_add_string(submap, "1", "one");
    jso_list_add_submap(multi_list, submap);
    jso_list_add_integer(multi_list, 2);
    sublist = jso_new_list();
        jso_list_add_string(sublist, "three");
        jso_list_add_boolean(sublist, true);
    jso_list_add_sublist(multi_list, sublist);
    expected = '[{"1":"one"},2,["three",true]]';
    actual = jso_encode_list(multi_list);
    assert_equal(expected, actual, "jso_encode_list() failed to encode one-element nested list!");
    jso_cleanup_list(multi_list);
}

#define _test_jso_decode
{
    /**
    _test_jso_decode(): Test core _jso_decode_*() functions.
    The formatting is intentionally erratic here to simulate actual formatting deviations.
    JSOnion version: 1.0.0d
    */
    var json, expected, actual, expected_structure, actual_structure;
    
    ////Primitives
    
    //_jso_decode_string(): Empty string
    json = '""';
    expected = __jso_gmt_tuple("", 3);
    actual = _jso_decode_string(json, 1);
    assert_equal(expected, actual, "_jso_decode_string() failed to decode an empty string!");
    
    //_jso_decode_string(): Small string
    json = '"key"  ';
    expected = __jso_gmt_tuple("key", 6);
    actual = _jso_decode_string(json, 1);
    assert_equal(expected, actual, "_jso_decode_string() failed to decode a small string!");
    
    //_jso_decode_string(): Simple string
    json = '  "The quick brown fox jumps over the lazy dog." ';
    expected = __jso_gmt_tuple("The quick brown fox jumps over the lazy dog.", 49);
    actual = _jso_decode_string(json, 1);
    assert_equal(expected, actual, "_jso_decode_string() failed to decode a simple string!");
    
    //_jso_decode_string(): Escape characters
    json = ' "\"\\\b\f\n\r\t\u003A"';
    expected = __jso_gmt_tuple('"\' + chr(8) + chr(12) + chr(10) + chr(13) + chr(9) + chr($003a), 24);
    actual = _jso_decode_string(json, 1);
    assert_equal(expected, actual, "_jso_decode_string() failed to decode a string with escape characters!");
    
    //_jso_decode_string(): Mixed characters
    json = ' "\"\\\bWaahoo\f\n\r\tnegg\u003a"';
    expected = __jso_gmt_tuple('"\' + chr(8) + "Waahoo" + chr(12) + chr(10) + chr(13) + chr(9) + "negg" + chr($003a), 34);
    actual = _jso_decode_string(json, 1);
    assert_equal(expected, actual, "_jso_decode_string() failed to decode a string with mixed characters!");
    
    //_jso_decode_boolean(): True
    json = 'true';
    expected = __jso_gmt_tuple(true, 5);
    actual = _jso_decode_boolean(json, 1);
    assert_equal(expected, actual, "_jso_decode_boolean() failed to decode true!");
    
    //_jso_decode_boolean(): False
    json = '  false  ';
    expected = __jso_gmt_tuple(false, 8);
    actual = _jso_decode_boolean(json, 1);
    assert_equal(expected, actual, "_jso_decode_boolean() failed to decode false!");
    
    //_jso_decode_real(): Zero
    json = '0';
    expected = __jso_gmt_tuple(0, 2);
    actual = _jso_decode_real(json, 1);
    assert_equal(expected, actual, "_jso_decode_real() failed to decode standard zero!");
    
    //_jso_decode_real(): Signed zero
    json = '  +0 ';
    expected = __jso_gmt_tuple(0, 5);
    actual = _jso_decode_real(json, 1);
    assert_equal(expected, actual, "_jso_decode_real() failed to decode signed zero!");
    
    //_jso_decode_real(): Signed zero with decimal digits
    json = ' -0.000';
    expected = __jso_gmt_tuple(0, 8);
    actual = _jso_decode_real(json, 1);
    assert_equal(expected, actual, "_jso_decode_real() failed to decode signed zero with decimal digits!");
    
    //_jso_decode_real(): Positive real
    json = '3.14159';
    expected = __jso_gmt_tuple(3.14159, 8);
    actual = _jso_decode_real(json, 1);
    assert_equal(expected, actual, "_jso_decode_real() failed to decode positive real number!");
    
    //_jso_decode_real(): Negative real
    json = ' -2.71828';
    expected = __jso_gmt_tuple(-2.71828, 10);
    actual = _jso_decode_real(json, 1);
    assert_equal(expected, actual, "_jso_decode_real() failed to decode negative real number!");
    
    //_jso_decode_real(): Positive real with positive exponent
    json = ' 3.14159e2';
    expected = __jso_gmt_tuple(3.14159*100, 11);
    actual = _jso_decode_real(json, 1);
    assert_equal(expected, actual, "_jso_decode_real() failed to decode positive real number with positive exponent!");
    
    //_jso_decode_real(): Negative real with positive exponent
    json = ' -2.71828E2';
    expected = __jso_gmt_tuple(-2.71828*100, 12);
    actual = _jso_decode_real(json, 1);
    assert_equal(expected, actual, "_jso_decode_real() failed to decode negative real number with positive exponent!");
    
    //_jso_decode_real(): Positive real with negative exponent
    json = ' 314.159e-2';
    expected = __jso_gmt_tuple(3.14159, 12);
    actual = _jso_decode_real(json, 1);
    assert_equal(expected, actual, "_jso_decode_real() failed to decode positive real number with negative exponent!");
    
    //_jso_decode_real(): Negative real with negative exponent
    json = ' -271.828E-2';
    expected = __jso_gmt_tuple(-2.71828, 13);
    actual = _jso_decode_real(json, 1);
    assert_equal(expected, actual, "_jso_decode_real() failed to decode negative real number with negative exponent!");
    
    //_jso_decode_real(): Positive integer
    json = ' +1729';
    expected = __jso_gmt_tuple(1729, 7);
    actual = _jso_decode_real(json, 1);
    assert_equal(expected, actual, "_jso_decode_real() failed to decode positive integer!");
    
    //_jso_decode_real(): Negative integer
    json = '-583';
    expected = __jso_gmt_tuple(-583, 5);
    actual = _jso_decode_real(json, 1);
    assert_equal(expected, actual, "_jso_decode_real() failed to decode negative integer!");
    
    //_jso_decode_integer(): Zero
    json = ' 0  ';
    expected = __jso_gmt_tuple(0, 3);
    actual = _jso_decode_integer(json, 1);
    assert_equal(expected, actual, "_jso_decode_integer() failed to decode zero!");
    
    //_jso_decode_integer(): Positive integer
    json = ' 1729   ';
    expected = __jso_gmt_tuple(1729, 6);
    actual = _jso_decode_integer(json, 1);
    assert_equal(expected, actual, "_jso_decode_integer() failed to decode positive integer!");
    
    //_jso_decode_integer(): Negative integer
    json = '   -583';
    expected = __jso_gmt_tuple(-583, 8);
    actual = _jso_decode_integer(json, 1);
    assert_equal(expected, actual, "_jso_decode_integer() failed to decode negative integer!");
    
    
    ////Data structures
    
    //_jso_decode_map(): Empty map #1
    json = '{}';
    expected_structure = jso_new_map();
    expected = __jso_gmt_tuple(expected_structure, 3);
    actual = _jso_decode_map(json, 1);
    actual_structure = __jso_gmt_elem(actual, 0);
    assert_equal(__jso_gmt_elem(expected, 1), __jso_gmt_elem(actual, 1), "_jso_decode_map() didn't stop at the right place! (#1)");
    assert_equal(actual_structure, __jso_gmt_elem(actual, 0), "_jso_decode_map() didn't include the right prefix! (#1)");
    assert_true(jso_compare_maps(expected_structure, actual_structure), "_jso_decode_map() failed to decode an empty map! (#1)");
    jso_cleanup_map(expected_structure);
    jso_cleanup_map(actual_structure);
    
    //_jso_decode_map(): Empty map #2
    json = '  {  }  ';
    expected_structure = jso_new_map();
    expected = __jso_gmt_tuple(expected_structure, 7);
    actual = _jso_decode_map(json, 1);
    actual_structure = __jso_gmt_elem(actual, 0);
    assert_equal(__jso_gmt_elem(expected, 1), __jso_gmt_elem(actual, 1), "_jso_decode_map() didn't stop at the right place! (#2)");
    assert_equal(actual_structure, __jso_gmt_elem(actual, 0), "_jso_decode_map() didn't include the right prefix! (#2)");
    assert_true(jso_compare_maps(expected_structure, actual_structure), "_jso_decode_map() failed to decode an empty map! (#2)");
    jso_cleanup_map(expected_structure);
    jso_cleanup_map(actual_structure);
    
    //_jso_decode_map(): One-entry map
    json = ' {"key": "value"} ';
    expected_structure = jso_new_map();
    expected = __jso_gmt_tuple(expected_structure, 18);
    jso_map_add_string(expected_structure, "key", "value");
    actual = _jso_decode_map(json, 1);
    actual_structure = __jso_gmt_elem(actual, 0);
    assert_equal(__jso_gmt_elem(expected, 1), __jso_gmt_elem(actual, 1), "_jso_decode_map() didn't stop at the right place! (one-entry map)");
    assert_equal(actual_structure, __jso_gmt_elem(actual, 0), "_jso_decode_map() didn't include the right prefix! (one-entry map)");
    assert_true(jso_compare_maps(expected_structure, actual_structure), "_jso_decode_map() failed to decode a one-entry map!");
    jso_cleanup_map(expected_structure);
    jso_cleanup_map(actual_structure);
    
    //_jso_decode_map(): Multi-entry map
    json = ' {"key"  :  "value", "pi":3.14,  "bool" : true} ';
    expected_structure = jso_new_map();
    expected = __jso_gmt_tuple(expected_structure, 48);
    jso_map_add_string(expected_structure, "key", "value");
    jso_map_add_real(expected_structure, "pi", 3.14);
    jso_map_add_boolean(expected_structure, "bool", true);
    actual = _jso_decode_map(json, 1);
    actual_structure = __jso_gmt_elem(actual, 0);
    assert_equal(__jso_gmt_elem(expected, 1), __jso_gmt_elem(actual, 1), "_jso_decode_map() didn't stop at the right place! (multi-entry map)");
    assert_equal(actual_structure, __jso_gmt_elem(actual, 0), "_jso_decode_map() didn't include the right prefix! (multi-entry map)");
    assert_true(jso_compare_maps(expected_structure, actual_structure), "_jso_decode_map() failed to decode a multi-entry map!");
    jso_cleanup_map(expected_structure);
    jso_cleanup_map(actual_structure);
    
    //_jso_decode_map(): Nested maps
    var submap;
    json = '{ "waahoo" : { "woohah" : 3 } , "woohah" : 4 }';
    expected_structure = jso_new_map();
    expected = __jso_gmt_tuple(expected_structure, 47);
    jso_map_add_integer(expected_structure, "woohah", 4);
    submap = jso_new_map();
    jso_map_add_integer(submap, "woohah", 3);
    jso_map_add_submap(expected_structure, "waahoo", submap);
    actual = _jso_decode_map(json, 1);
    actual_structure = __jso_gmt_elem(actual, 0);
    assert_equal(__jso_gmt_elem(expected, 1), __jso_gmt_elem(actual, 1), "_jso_decode_map() didn't stop at the right place! (nested map)");
    assert_equal(actual_structure, __jso_gmt_elem(actual, 0), "_jso_decode_map() didn't include the right prefix! (nested map)");
    assert_true(jso_compare_maps(expected_structure, actual_structure), "_jso_decode_map() failed to decode a nested map!");
    jso_cleanup_map(expected_structure);
    jso_cleanup_map(actual_structure);
    
    //_jso_decode_map(): Map with nested lists
    var sublist, subsublist;
    json = '{ "waahoo" : [ "woohah", [true] ] , "woohah" : 4 }';
    expected_structure = jso_new_map();
    expected = __jso_gmt_tuple(expected_structure, 51);
    jso_map_add_real(expected_structure, "woohah", 4);
    sublist = jso_new_list();
    jso_list_add_string(sublist, "woohah");
    subsublist = jso_new_list();
    jso_list_add_boolean(subsublist, true);
    jso_list_add_sublist(sublist, subsublist);
    jso_map_add_sublist(expected_structure, "waahoo", sublist);
    actual = _jso_decode_map(json, 1);
    actual_structure = __jso_gmt_elem(actual, 0);
    assert_equal(__jso_gmt_elem(expected, 1), __jso_gmt_elem(actual, 1), "_jso_decode_map() didn't stop at the right place! (map with nested lists)");
    assert_equal(actual_structure, __jso_gmt_elem(actual, 0), "_jso_decode_map() didn't include the right prefix! (map with nested lists)");
    assert_true(jso_compare_maps(expected_structure, actual_structure), "_jso_decode_map() failed to decode a map with nested lists!");
    jso_cleanup_map(expected_structure);
    jso_cleanup_map(actual_structure);
    
    //_jso_decode_map(): Mix-up nested map
    var sublist;
    json = ' { "waahoo" : [{}, "a", 1]  }';
    expected_structure = jso_new_map();
    expected = __jso_gmt_tuple(expected_structure, 30);
    sublist = jso_new_list();
    jso_list_add_submap(sublist, jso_new_map());
    jso_list_add_string(sublist, "a");
    jso_list_add_real(sublist, 1);
    jso_map_add_sublist(expected_structure, "waahoo", sublist);
    actual = _jso_decode_map(json, 1);
    actual_structure = __jso_gmt_elem(actual, 0);
    assert_equal(__jso_gmt_elem(expected, 1), __jso_gmt_elem(actual, 1), "_jso_decode_map() didn't stop at the right place! (mix-up nested map)");
    assert_equal(actual_structure, __jso_gmt_elem(actual, 0), "_jso_decode_map() didn't include the right prefix! (mix-up nested map)");
    assert_true(jso_compare_maps(expected_structure, actual_structure), "_jso_decode_map() failed to decode a mix-up nested map!");
    jso_cleanup_map(expected_structure);
    jso_cleanup_map(actual_structure);
    
    //_jso_decode_list(): Empty list #1
    json = '[]';
    expected_structure = jso_new_list();
    expected = __jso_gmt_tuple(expected_structure, 3);
    actual = _jso_decode_list(json, 1);
    actual_structure = __jso_gmt_elem(actual, 0);
    assert_equal(__jso_gmt_elem(expected, 1), __jso_gmt_elem(actual, 1), "_jso_decode_list() didn't stop at the right place! (#1)");
    assert_equal(actual_structure, __jso_gmt_elem(actual, 0), "_jso_decode_list() didn't include the right prefix! (#1)");
    assert_true(jso_compare_lists(expected_structure, actual_structure), "_jso_decode_list() failed to decode an empty list! (#1)");
    jso_cleanup_list(expected_structure);
    jso_cleanup_list(actual_structure)
    
    //_jso_decode_list(): Empty list #2
    json = ' [  ] ';
    expected_structure = jso_new_list();
    expected = __jso_gmt_tuple(expected_structure, 6);
    actual = _jso_decode_list(json, 1);
    actual_structure = __jso_gmt_elem(actual, 0);
    assert_equal(__jso_gmt_elem(expected, 1), __jso_gmt_elem(actual, 1), "_jso_decode_list() didn't stop at the right place! (#2)");
    assert_equal(actual_structure, __jso_gmt_elem(actual, 0), "_jso_decode_list() didn't include the right prefix! (#2)");
    assert_true(jso_compare_lists(expected_structure, actual_structure), "_jso_decode_list() failed to decode an empty list! (#2)");
    jso_cleanup_list(expected_structure);
    jso_cleanup_list(actual_structure);
    
    //_jso_decode_list(): One-entry list
    json = '[3]';
    expected_structure = jso_new_list();
    expected = __jso_gmt_tuple(expected_structure, 4);
    jso_list_add_integer(expected_structure, 3);
    actual = _jso_decode_list(json, 1);
    actual_structure = __jso_gmt_elem(actual, 0);
    assert_equal(__jso_gmt_elem(expected, 1), __jso_gmt_elem(actual, 1), "_jso_decode_list() didn't stop at the right place! (one-entry list)");
    assert_equal(actual_structure, __jso_gmt_elem(actual, 0), "_jso_decode_list() didn't include the right prefix! (one-entry list)");
    assert_true(jso_compare_lists(expected_structure, actual_structure), "_jso_decode_list() failed to decode a one-entry list!");
    jso_cleanup_list(expected_structure);
    jso_cleanup_list(actual_structure);
   
    //_jso_decode_list(): Multi-entry list
    json = ' [4,"multi-entry",true]';
    expected_structure = jso_new_list();
    expected = __jso_gmt_tuple(expected_structure, 24);
    jso_list_add_real(expected_structure, 4);
    jso_list_add_string(expected_structure, "multi-entry");
    jso_list_add_boolean(expected_structure, true);
    actual = _jso_decode_list(json, 1);
    actual_structure = __jso_gmt_elem(actual, 0);
    assert_equal(__jso_gmt_elem(expected, 1), __jso_gmt_elem(actual, 1), "_jso_decode_list() didn't stop at the right place! (multi-entry list)");
    assert_equal(actual_structure, __jso_gmt_elem(actual, 0), "_jso_decode_list() didn't include the right prefix! (multi-entry list)");
    assert_true(jso_compare_lists(expected_structure, actual_structure), "_jso_decode_list() failed to decode a multi-entry list!");
    jso_cleanup_list(expected_structure);
    jso_cleanup_list(actual_structure);
    
    //_jso_decode_list(): Nested list
    var sublist;
    json = ' [ [], 3, false, ["waahoo"]]';
    expected_structure = jso_new_list();
    expected = __jso_gmt_tuple(expected_structure, 29);
    jso_list_add_sublist(expected_structure, jso_new_list());
    jso_list_add_integer(expected_structure, 3);
    jso_list_add_boolean(expected_structure, false);
    sublist = jso_new_list();
    jso_list_add_string(sublist, "waahoo");
    jso_list_add_sublist(expected_structure, sublist);
    actual = _jso_decode_list(json, 1);
    actual_structure = __jso_gmt_elem(actual, 0);
    assert_equal(__jso_gmt_elem(expected, 1), __jso_gmt_elem(actual, 1), "_jso_decode_list() didn't stop at the right place! (nested list)");
    assert_equal(actual_structure, __jso_gmt_elem(actual, 0), "_jso_decode_list() didn't include the right prefix! (nested list)");
    assert_true(jso_compare_lists(expected_structure, actual_structure), "_jso_decode_list() failed to decode a nested list!");
    jso_cleanup_list(expected_structure);
    jso_cleanup_list(actual_structure);
    
    //_jso_decode_list(): List with nested maps
    var submap;
    json = ' [3, false, { "waahoo":"woo"}]';
    expected_structure = jso_new_list();
    expected = __jso_gmt_tuple(expected_structure, 31);
    jso_list_add_integer(expected_structure, 3);
    jso_list_add_boolean(expected_structure, false);
    submap = jso_new_map();
    jso_map_add_string(submap, "waahoo", "woo");
    jso_list_add_submap(expected_structure, submap);
    actual = _jso_decode_list(json, 1);
    actual_structure = __jso_gmt_elem(actual, 0);
    assert_equal(__jso_gmt_elem(expected, 1), __jso_gmt_elem(actual, 1), "_jso_decode_list() didn't stop at the right place! (list with nested maps)");
    assert_equal(actual_structure, __jso_gmt_elem(actual, 0), "_jso_decode_list() didn't include the right prefix! (list with nested maps)");
    assert_true(jso_compare_lists(expected_structure, actual_structure), "_jso_decode_list() failed to decode a list with nested maps!");
    jso_cleanup_list(expected_structure);
    jso_cleanup_list(actual_structure);
    
    //_jso_decode_list(): Mix-up nested list
    var submap;
    json = '[{}, {"a":[]}]';
    expected_structure = jso_new_list();
    expected = __jso_gmt_tuple(expected_structure, 15);
    jso_list_add_submap(expected_structure, jso_new_map());
    submap = jso_new_map();
    jso_map_add_sublist(submap, "a", jso_new_list());
    jso_list_add_submap(expected_structure, submap);
    actual = _jso_decode_list(json, 1);
    actual_structure = __jso_gmt_elem(actual, 0);
    assert_equal(__jso_gmt_elem(expected, 1), __jso_gmt_elem(actual, 1), "_jso_decode_list() didn't stop at the right place! (mix-up nested list)");
    assert_equal(actual_structure, __jso_gmt_elem(actual, 0), "_jso_decode_list() didn't include the right prefix! (mix-up nested list)");
    assert_true(jso_compare_lists(expected_structure, actual_structure), "_jso_decode_list() failed to decode a mix-up nested list!");
    jso_cleanup_list(expected_structure);
    jso_cleanup_list(actual_structure);
}

#define _test_jso_compare
{
    /**
    _test_jso_compare(): Test basic jso_compare_*() functions.
    JSOnion version: 1.0.0d
    */
    var a, b;
    
    //jso_compare_maps(): Empty maps should equal each other
    a = jso_new_map();
    b = jso_new_map();
    assert_true(jso_compare_maps(a, b), "Empty maps should equal each other. (#1)");
    assert_true(jso_compare_maps(b, a), "Empty maps should equal each other. (#2)");
    jso_cleanup_map(a);
    jso_cleanup_map(b);
    
    //jso_compare_maps(): An empty map should not equal a filled map
    a = jso_new_map();
    b = jso_new_map();
    jso_map_add_string(b, "junk", "info");
    jso_map_add_integer(b, "taxi", 1729);
    assert_false(jso_compare_maps(a, b), "An empty map should not equal a filled map. (#1)");
    assert_false(jso_compare_maps(b, a), "An empty map should not equal a filled map. (#2)");
    jso_cleanup_map(a);
    jso_cleanup_map(b);
    
    //jso_compare_maps(): Maps with same content entered in different orders should equal each other
    a = jso_new_map();
    b = jso_new_map();
    jso_map_add_real(a, "A", 1);
    jso_map_add_real(a, "B", 2);
    jso_map_add_real(a, "C", 3);
    jso_map_add_real(b, "C", 3);
    jso_map_add_real(b, "A", 1);
    jso_map_add_real(b, "B", 2);
    assert_true(jso_compare_maps(a, b), "Maps with same content entered in different orders should equal each other. (#1)");
    assert_true(jso_compare_maps(b, a), "Maps with same content entered in different orders should equal each other. (#2)");
    jso_cleanup_map(a);
    jso_cleanup_map(b);
    
    //jso_compare_maps(): Maps with different keys should not equal each other
    a = jso_new_map();
    b = jso_new_map();
    jso_map_add_real(a, "A", 1);
    jso_map_add_real(a, "B", 2);
    jso_map_add_real(a, "C", 3);
    jso_map_add_real(b, "D", 3);
    jso_map_add_real(b, "A", 1);
    jso_map_add_real(b, "B", 2);
    assert_false(jso_compare_maps(a, b), "Maps with different keys should not equal each other. (#1)");
    assert_false(jso_compare_maps(b, a), "Maps with different keys should not equal each other. (#2)");
    jso_cleanup_map(a);
    jso_cleanup_map(b);
    
    //jso_compare_maps(): Maps with different values should not equal each other
    a = jso_new_map();
    b = jso_new_map();
    jso_map_add_real(a, "A", 5);
    jso_map_add_real(a, "B", 6);
    jso_map_add_real(a, "C", 9);
    jso_map_add_real(b, "A", 5);
    jso_map_add_real(b, "B", 6);
    jso_map_add_real(b, "C", 8);
    assert_false(jso_compare_maps(a, b), "Maps with different values should not equal each other. (#1)");
    assert_false(jso_compare_maps(b, a), "Maps with different values should not equal each other. (#2)");
    jso_cleanup_map(a);
    jso_cleanup_map(b);
    
    //jso_compare_maps(): Maps with corresponding values of different types should not equal each other, and should not crash.
    a = jso_new_map();
    b = jso_new_map();
    jso_map_add_real(a, "A", 5);
    jso_map_add_real(a, "B", 6);
    jso_map_add_real(a, "C", 8);
    jso_map_add_real(b, "A", 5);
    jso_map_add_string(b, "B", "six");
    jso_map_add_real(b, "C", 8);
    assert_false(jso_compare_maps(a, b), "Maps with corresponding values of different types should not equal each other, and should not crash. (#1)");
    assert_false(jso_compare_maps(b, a), "Maps with corresponding values of different types should not equal each other, and should not crash. (#2)");
    jso_cleanup_map(a);
    jso_cleanup_map(b);
    
    //jso_compare_lists(): Empty lists should equal each other
    a = jso_new_list();
    b = jso_new_list();
    assert_true(jso_compare_lists(a, b), "Empty lists should equal each other. (#1)");
    assert_true(jso_compare_lists(b, a), "Empty lists should equal each other. (#2)");
    jso_cleanup_list(a);
    jso_cleanup_list(b);
    
    //jso_compare_lists(): An empty list should not equal a filled list
    a = jso_new_list();
    b = jso_new_list();
    jso_list_add_string(b, "junk");
    jso_list_add_integer(b, 1729);
    assert_false(jso_compare_lists(a, b), "An empty list should not equal a filled list. (#1)");
    assert_false(jso_compare_lists(b, a), "An empty list should not equal a filled list. (#2)");
    jso_cleanup_list(a);
    jso_cleanup_list(b);
    
    //jso_compare_lists(): Lists with same content entered in different orders should not equal each other
    a = jso_new_list();
    b = jso_new_list();
    jso_list_add_real(a, 1);
    jso_list_add_real(a, 2);
    jso_list_add_real(a, 3);
    jso_list_add_real(b, 3);
    jso_list_add_real(b, 1);
    jso_list_add_real(b, 2);
    assert_false(jso_compare_lists(a, b), "Lists with same content entered in different orders should not equal each other. (#1)");
    assert_false(jso_compare_lists(b, a), "Lists with same content entered in different orders should not equal each other. (#2)");
    jso_cleanup_list(a);
    jso_cleanup_list(b);
    
    //jso_compare_lists(): Lists with corresponding entries of different types should not equal each other, should also not crash.
    a = jso_new_list();
    b = jso_new_list();
    jso_list_add_real(a, 1);
    jso_list_add_real(a, 2);
    jso_list_add_real(a, 3);
    jso_list_add_real(b, 1);
    jso_list_add_string(b, "two");
    jso_list_add_real(b, 3);
    assert_false(jso_compare_lists(a, b), "Lists with corresponding entries of different types should not equal each other, should also not crash. (#1)");
    assert_false(jso_compare_lists(b, a), "Lists with corresponding entries of different types should not equal each other, should also not crash. (#2)");
    jso_cleanup_list(a);
    jso_cleanup_list(b);
}

#define _test_jso_lookup
{
    /**
    _test_jso_lookup(): Test core jso_*_lookup() and jso_*_check() functions for nested structures.
    JSOnion version: 1.0.0d
    */
    var json, structure, expected, actual;
    
    //jso_map_check(): Single argument --- exists
    json = '{ "one" : 1, "two" : 2, "three" : 3 }';
    structure = jso_decode_map(json);
    assert_true(jso_map_check(structure, "one"), "jso_map_check() failed to find existing entry! (single argument)");
    jso_cleanup_map(structure);
    
    //jso_map_lookup(): Single argument --- exists
    json = '{ "one" : 1, "two" : 2, "three" : 3 }';
    structure = jso_decode_map(json);
    expected = 1;
    actual = jso_map_lookup(structure, "one")
    assert_equal(expected, actual, "jso_map_lookup() found the wrong entry! (single argument)");
    jso_cleanup_map(structure);
    
    //jso_map_lookup_type(): Single argument --- exists
    json = '{ "one" : -1, "two" : true, "three" : "trap" }';
    structure = jso_decode_map(json);
    expected = jso_type_real;
    actual = jso_map_lookup_type(structure, "one")
    assert_equal(expected, actual, "jso_map_lookup_type() found the wrong type! (single argument)");
    jso_cleanup_map(structure);
    
    //jso_map_check(): Single argument --- doesn't exist
    json = '{ "one" : 1, "two" : 2, "three" : 3 }';
    structure = jso_decode_map(json);
    assert_false(jso_map_check(structure, "four"), "jso_map_check() found an inexistent entry! (single argument)");
    jso_cleanup_map(structure);
    
    //jso_map_check(): Single argument --- doesn't exist
    json = '{ "one" : 1, "two" : 2, "three" : 3, "four" : { "A":true, "B":false } }';
    structure = jso_decode_map(json);
    assert_false(jso_map_check(structure, "A"), "jso_map_check() found an inexistent entry! (single argument, nested)");
    jso_cleanup_map(structure);
    
    //jso_map_check(): Multiple arguments (recurse) --- exists
    json = '{ "one" : 1, "two" : 2, "three" : 3, "four" : { "A":true, "B":false } }';
    structure = jso_decode_map(json);
    assert_true(jso_map_check(structure, "four", "A"), "jso_map_check() failed to find existing entry! (multiple arguments)");
    jso_cleanup_map(structure);
    
    //jso_map_lookup(): Multiple arguments (recurse) --- exists
    json = '{ "one" : 1, "two" : 2, "three" : 3, "four" : { "A":true, "B":false } }';
    structure = jso_decode_map(json);
    expected = true;
    actual = jso_map_lookup(structure, "four", "A");
    assert_equal(expected, actual, "jso_map_lookup() found the wrong entry! (multiple arguments)");
    jso_cleanup_map(structure);
    
    //jso_map_lookup_type(): Multiple arguments (recurse) --- exists
    json = '{ "one" : 1, "two" : 2, "three" : 3, "four" : { "A":true, "B":"trap" } }';
    structure = jso_decode_map(json);
    expected = jso_type_boolean;
    actual = jso_map_lookup_type(structure, "four", "A");
    assert_equal(expected, actual, "jso_map_lookup_type() found the wrong type! (multiple arguments)");
    jso_cleanup_map(structure);
    
    //jso_map_check(): Multiple arguments (recurse) --- doesn't exist
    json = '{ "one" : 1, "two" : 2, "three" : 3, "four" : { "A":true, "B":false } }';
    structure = jso_decode_map(json);
    assert_false(jso_map_check(structure, "four", "C"), "jso_map_check() found an inexistent entry! (multiple arguments, 1)");
    jso_cleanup_map(structure);
    
    //jso_map_check(): Multiple arguments (recurse) --- doesn't exist
    json = '{ "one" : 1, "two" : 2, "three" : 3, "four" : { "A":true, "B":false } }';
    structure = jso_decode_map(json);
    assert_false(jso_map_check(structure, "three", ""), "jso_map_check() found an inexistent entry! (multiple arguments, 2)");
    jso_cleanup_map(structure);
    
    //jso_map_check(): Multiple arguments with nested list --- exists
    json = '{ "one" : 1, "two" : 2, "three" : 3, "four" : [ "A", true, ["B", false] ] }';
    structure = jso_decode_map(json);
    assert_true(jso_map_check(structure, "four", 2, 1), "jso_map_check() failed to find an existing entry! (multiple arguments, nested)");
    jso_cleanup_map(structure);
    
    //jso_map_lookup(): Multiple arguments with nested list --- exists
    json = '{ "one" : 1, "two" : 2, "three" : 3, "four" : [ "A", true, ["B", false] ] }';
    structure = jso_decode_map(json);
    expected = false;
    actual = jso_map_lookup(structure, "four", 2, 1);
    assert_equal(expected, actual, "jso_map_lookup() failed to find an existing entry! (multiple arguments, nested)");
    jso_cleanup_map(structure);
    
    //jso_map_lookup_type(): Multiple arguments with nested list --- exists
    json = '{ "one" : 1, "two" : 2, "three" : 3, "four" : [ "A", true, [false, "false"] ] }';
    structure = jso_decode_map(json);
    expected = jso_type_string;
    actual = jso_map_lookup_type(structure, "four", 2, 1);
    assert_equal(expected, actual, "jso_map_lookup_type() found the wrong type! (multiple arguments, nested)");
    jso_cleanup_map(structure);
    
    //jso_map_check(): Multiple arguments with nested list --- wrong type
    json = '{ "one" : 1, "two" : 2, "three" : 3, "four" : [ "A", true, ["B", false] ] }';
    structure = jso_decode_map(json);
    assert_false(jso_map_check(structure, "four", "A", 1), "jso_map_check() found an inexistent entry! (multiple arguments, nested, wrong type)");
    jso_cleanup_map(structure);
    
    //jso_map_check(): Multiple arguments with nested list --- index overflow
    json = '{ "one" : 1, "two" : 2, "three" : 3, "four" : [ "A", true, ["B", false] ] }';
    structure = jso_decode_map(json);
    assert_false(jso_map_check(structure, "four", 2, 3), "jso_map_check() found an inexistent entry! (multiple arguments, nested, index overflow)");
    jso_cleanup_map(structure);
    
    //jso_list_check(): Single argument --- exists
    json = '["one", 2, "three", true, 5]';
    structure = jso_decode_list(json);
    assert_true(jso_list_check(structure, 2), "jso_list_check() failed to find an existing index! (single argument)");
    jso_cleanup_list(structure);
    
    //jso_list_lookup(): Single argument --- exists
    json = '["one", 2, "three", true, 5]';
    structure = jso_decode_list(json);
    expected = "three";
    actual = jso_list_lookup(structure, 2);
    assert_equal(expected, actual, "jso_list_lookup() found the wrong index! (single argument)");
    jso_cleanup_list(structure);
    
    //jso_list_lookup_type(): Single argument --- exists
    json = '["one", 2, "three", true, 5]';
    structure = jso_decode_list(json);
    expected = jso_type_string;
    actual = jso_list_lookup_type(structure, 2);
    assert_equal(expected, actual, "jso_list_lookup_type() found the wrong type! (single argument)");
    jso_cleanup_list(structure);
    
    //jso_list_check(): Single argument --- doesn't exist
    json = '["one", 2, "three", true, 5]';
    structure = jso_decode_list(json);
    assert_false(jso_list_check(structure, 5), "jso_list_check() found an inexistent index! (single argument)");
    jso_cleanup_list(structure);
    
    //jso_list_check(): Multiple arguments (recurse) --- exists
    json = '["one", 2, ["three", 3], true, 5]';
    structure = jso_decode_list(json);
    assert_true(jso_list_check(structure, 2, 1), "jso_list_check() failed to find an existing index! (multiple arguments)");
    jso_cleanup_list(structure);
    
    //jso_list_lookup(): Multiple arguments (recurse) --- exists
    json = '["one", 2, ["three", 3], true, 5]';
    structure = jso_decode_list(json);
    expected = 3;
    actual = jso_list_lookup(structure, 2, 1);
    assert_equal(expected, actual, "jso_list_lookup() failed to find an existing index! (multiple arguments)");
    jso_cleanup_list(structure);
    
    //jso_list_lookup_type(): Multiple arguments (recurse) --- exists
    json = '["one", 2, ["three", 3], true, 5]';
    structure = jso_decode_list(json);
    expected = jso_type_real;
    actual = jso_list_lookup_type(structure, 2, 1);
    assert_equal(expected, actual, "jso_list_lookup_type() found the wrong type! (multiple arguments)");
    jso_cleanup_list(structure);
    
    //jso_list_check(): Multiple arguments (recurse) --- doesn't exist, inner index overflow
    json = '["one", 2, ["three", 3], true, 5]';
    structure = jso_decode_list(json);
    assert_false(jso_list_check(structure, 2, 2), "jso_list_check() found an inexistent index! (multiple arguments, inner index overflow)");
    jso_cleanup_list(structure);
    
    //jso_list_check(): Multiple arguments (recurse) --- doesn't exist, trying to index single entry
    json = '["one", 2, ["three", 3], true, 5]';
    structure = jso_decode_list(json);
    assert_false(jso_list_check(structure, 1, 0), "jso_list_check() found an inexistent index! (multiple arguments, indexing single entry)");
    jso_cleanup_list(structure);
    
    //jso_list_check(): Multiple arguments with nested map --- exists
    json = '["one", 2, {"three":3}, true, 5]';
    structure = jso_decode_list(json);
    assert_true(jso_list_check(structure, 2, "three"), "jso_list_check() failed to find an existing entry! (multiple arguments, nested map)");
    jso_cleanup_list(structure);
    
    //jso_list_lookup(): Multiple arguments with nested map --- exists
    json = '["one", 2, {"three":3}, true, 5]';
    structure = jso_decode_list(json);
    expected = 3;
    actual = jso_list_lookup(structure, 2, "three");
    assert_equal(expected, actual, "jso_list_lookup() failed to find an existing entry! (multiple arguments, nested map)");
    jso_cleanup_list(structure);
    
    //jso_list_lookup_type(): Multiple arguments with nested map --- exists
    json = '["one", 2, {"three":false}, true, 5]';
    structure = jso_decode_list(json);
    expected = jso_type_boolean;
    actual = jso_list_lookup_type(structure, 2, "three");
    assert_equal(expected, actual, "jso_list_lookup_type() found the wrong type! (multiple arguments, nested map)");
    jso_cleanup_list(structure);
    
    //jso_list_exists(): Multiple arguments with nested map --- doesn't exist, key on single entry
    json = '["one", 2, {"three":3}, true, 5]';
    structure = jso_decode_list(json);
    assert_false(jso_list_check(structure, 1, ""), "jso_list_check() found an inexistent entry! (multiple arguments, nested map, key on single entry)");
    jso_cleanup_list(structure);
    
    //jso_list_exists(): Multiple arguments with nested map --- doesn't exist, bad key
    json = '["one", 2, {"three":3}, true, 5]';
    structure = jso_decode_list(json);
    assert_false(jso_list_check(structure, 2, ""), "jso_list_check() found an inexistent entry! (multiple arguments, nested map, bad key)");
    jso_cleanup_list(structure);
}

#define _test_jso_bugs
{
    /**
    _test_jso_bugs(): Test reported bugs.
    JSOnion version: 1.0.0d
    */
    var expected, actual;
    
    //jso_encode_map() --- One-element map
    //Bug 1: Crash when encoding boolean-valued entries in maps, unknown variable jso_type_integer
    var one_map;
    one_map = jso_new_map();
    jso_map_add_boolean(one_map, "true", true);
    expected = '{"true":true}';
    actual = jso_encode_map(one_map);
    assert_equal(expected, actual, "jso_encode_map() failed to encode one-element map with boolean entry!");
    jso_cleanup_map(one_map);
}

