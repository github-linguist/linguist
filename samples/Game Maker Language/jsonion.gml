// Originally from /jsonion.gml in JSOnion
// JSOnion v1.0.0d is licensed under the MIT licence. You may freely adapt and use this library in commercial and non-commercial projects.

#define __jso_gmt_tuple
{

  /**
  tuple(>element_0<, >element_1<, ..., >element_n<): Return an n-tuple
  @author: GameGeisha
  @version: 1.2 (GMTuple)
  */

  //Position, address table and data
  var pos, addr_table, data;
  pos = 6*argument_count+4;
  addr_table = "";
  data = "";
  
  //Build the tuple element-by-element
  var i, ca, isstr, datastr;
  for (i=0; i<argument_count; i+=1) {
    //Check the argument's type
    ca = argument[i];
    isstr = is_string(ca);
    if (isstr) { //Save strings as-is
      datastr = ca;
    }
    else { //Save reals in scientific notation, 15 significant digits
      datastr = __jso_gmt_numtostr(ca);
    }
    //Add entry in address table and data
    addr_table += chr(isstr+$30)
    addr_table += string_format(pos, 5, 0);
    pos += string_length(datastr);
    data += datastr;
  }
  
  //Return the tuple, with size header character, address table and data
  return string_format(argument_count, 3, 0)+addr_table+data;
  
}

#define __jso_gmt_elem
{

  /**
  elem(tuple_source, n): Return the <n>th element of <tuple_source>
  @author: GameGeisha
  @version: 1.2 (GMTuple)
  */

  //Capture arguments
  var t, n, size;
  t = argument0;
  n = argument1;
  size = __jso_gmt_size(t);
  
  //Search for the bounding positions for the <n>th element in the address table
  var start, afterend, isstr;
  isstr = ord(string_char_at(t, 4+6*n))-$30;
  start = real(string_copy(t, 5+6*n, 5));
  if (n < size-1) {
    afterend = real(string_copy(t, 11+6*n, 5));
  } else {
    afterend = string_length(t)+1;
  }
  
  //Return the <n>th element with the correct type
  if (isstr) {
    return string_copy(t, start, afterend-start);
  }
  else {
    return real(string_copy(t, start, afterend-start));
  }

}

#define __jso_gmt_size
{
  
  /**
  size(tuple_source): Return the size of <tuple_source>
  @author: GameGeisha
  @version: 1.2 (GMTuple)
  */

  return real(string_copy(argument0, 1, 3));

}

#define __jso_gmt_numtostr
{

  /**
  __gmt_numtostr(num): Return string representation of <num>. Decimal numbers expressed as scientific notation
  with double precision (i.e. 15 digits)
  @author: GameGeisha
  @version: 1.2 (edited)
  */
  if (frac(argument0) == 0) {
    return string(argument0);
  }
  
    var mantissa, exponent;
    exponent = floor(log10(abs(argument0)));
    mantissa = string_format(argument0/power(10,exponent), 15, 14);
    var i, ca;
    i = string_length(mantissa);
    do {
      ca = string_char_at(mantissa, i);
      i -= 1;
    } until (ca != "0")
    if (ca != ".") {
        mantissa = string_copy(mantissa, 1, i+1);
    }
    else {
        mantissa = string_copy(mantissa, 1, i);
    }
    if (exponent != 0) {
      return mantissa + "e" + string(exponent);
    }
    else {
      return mantissa;
    }

}

#define __jso_gmt_test_all
{

  /**
  test_all(): Runs all test suites
  @author: GameGeisha
  @version: 1.2 (GMTuple)
  */
  
  //Automated testing sequence
  __jso_gmt_test_elem();
  __jso_gmt_test_size();
  __jso_gmt_test_numtostr();

}

#define __jso_gmt_test_numtostr
{
  
  /**
  _test_numtostr(): Runs number-to-string tests
  @author: GameGeisha
  @version: 1.2 (GMTuple)
  */

  var tolerance;
  tolerance = 1/10000000000;

  if (real(__jso_gmt_numtostr(9)) != 9) {
    show_message("Scientific notation conversion failed for 1-digit integer! Result: " + __jso_gmt_numtostr(9));
  }
  if (real(__jso_gmt_numtostr(500)) != 500) {
    show_message("Scientific notation conversion failed for 3-digit integer! Result: " + __jso_gmt_numtostr(500));
  }
  if (abs(real(__jso_gmt_numtostr(pi))-pi) > tolerance) {
    show_message("Scientific notation conversion failed for pi! Result: " + __jso_gmt_numtostr(pi));
  }
  if (abs(real(__jso_gmt_numtostr(104729.903455))-104729.903455) > tolerance) {
    show_message("Scientific notation conversion failed for large decimal number! Result: " + __jso_gmt_numtostr(104729.903455));
  }
  if (abs(real(__jso_gmt_numtostr(-pi))+pi) > tolerance) {
    show_message("Scientific notation conversion failed for -pi! Result: " + __jso_gmt_numtostr(-pi));
  }
  if (abs(real(__jso_gmt_numtostr(1/pi))-1/pi) > tolerance) {
    show_message("Scientific notation conversion failed for 1/pi! Result: " + __jso_gmt_numtostr(1/pi));
  }

}

#define __jso_gmt_test_elem
{

  /**
  _test_elem(): Runs tuple element retrieval tests
  @author: GameGeisha
  @version: 1.2 (GMTuple)
  */

  if (__jso_gmt_elem(__jso_gmt_tuple("Qblock", "kll"), 0) != "Qblock") {
    show_message("Element retrieval failed for simple string. #Should be:Qblock#Actual:" + __jso_gmt_elem(__jso_gmt_tuple("Qblock"), 0));
  }
  if (__jso_gmt_elem(__jso_gmt_tuple(9, "Q", -7), 0) != 9) {
    show_message("Element retrieval failed for simple number. #Should be 9#Actual:" + string(__jso_gmt_elem(__jso_gmt_tuple(9, "Q", 7), 0)));
  }
  if (__jso_gmt_elem(__jso_gmt_tuple("Qblock", "", "Negg"), 1) != "") {
    show_message("Element retrieval failed for empty string#Should be empty string#Actual:"+string(__jso_gmt_elem(__jso_gmt_tuple("Qblock", "", "Negg"), 0)));
  }
  
  if (__jso_gmt_elem(__jso_gmt_elem(__jso_gmt_tuple("Not this", __jso_gmt_tuple(0, 1, 2, 3), "Waahoo"), 1), 3) != 3) {
    show_message("Element retrieval failed in nested tuple. #Should be 3#Actual:" + string(__jso_gmt_elem(__jso_gmt_elem(__jso_gmt_tuple("Not this", __jso_gmt_tuple(0, 1, 2, 3), "Waahoo"), 1), 3)));
  }  

}

#define __jso_gmt_test_size
{

  /**
  _test_size(): Runs tuple size tests
  @author: GameGeisha
  @version: 1.2 (GMTuple)
  */

  if (__jso_gmt_size(__jso_gmt_tuple("Waahoo", "Negg", 0)) != 3) {
    show_message("Bad size for 3-tuple");
  }
  if (__jso_gmt_size(__jso_gmt_tuple()) != 0) {
    show_message("Bad size for null tuple");
  }
  if (__jso_gmt_size(__jso_gmt_tuple(7)) != 1) {
    show_message("Bad size for 1-tuple");
  }
  if (__jso_gmt_size(__jso_gmt_tuple(1,2,3,4,5,6,7,8,9,10)) != 10) {
    show_message("Bad size for 10-tuple");
  }

}

#define jso_new_map
{
    /**
    jso_new_map(): Create a new map.
    JSOnion version: 1.0.0d
    */
    return ds_map_create();
}

#define jso_new_list
{
    /**
    jso_new_list(): Create a new list.
    JSOnion version: 1.0.0d
    */
    return ds_list_create();
}

#define jso_map_add_real
{
    /**
    jso_map_add_real(map, key, val): Add the key-value pair <key>:<val> to <map>, where <val> is a real value.
    JSOnion version: 1.0.0d
    */
    ds_map_add(argument0, argument1, argument2);
}

#define jso_map_add_string
{
    /**
    jso_map_add_string(map, key, str): Add the key-value pair <key>:<str> to <map>, where <str> is a string value.
    JSOnion version: 1.0.0d
    */
    ds_map_add(argument0, argument1, "s" + argument2);
}

#define jso_map_add_sublist
{
    /**
    jso_map_add_sublist(map, key, sublist): Add the key-value pair <key>:<sublist> to <map>, where <sublist> is a list.
    JSOnion version: 1.0.0d
    */
    ds_map_add(argument0, argument1, "l" + string(argument2));
}

#define jso_map_add_submap
{
    /**
    jso_map_add_submap(map, key, submap): Add the key-value pair <key>:<submap> to <map>, where <submap> is a map.
    JSOnion version: 1.0.0d
    */
    ds_map_add(argument0, argument1, "m" + string(argument2));
}

#define jso_map_add_integer
{
    /**
    jso_map_add_integer(map, key, int): Add the key-value pair <key>:<int> to <map>, where <int> is a integer value.
    JSOnion version: 1.0.0d
    */
    ds_map_add(argument0, argument1, floor(argument2));
}

#define jso_map_add_boolean
{
    /**
    jso_map_add_boolean(map, key, bool): Add the key-value pair <key>:<bool> to <map>, where <bool> is a boolean value.
    JSOnion version: 1.0.0d
    */
    if (argument2) {
        ds_map_add(argument0, argument1, "btrue");
    } else {
        ds_map_add(argument0, argument1, "bfalse");
    }
}

#define jso_list_add_real
{
    /**
    jso_list_add_real(list, val): Append the real value <val> to <list>.
    JSOnion version: 1.0.0d
    */
    ds_list_add(argument0, argument1);
}

#define jso_list_add_string
{
    /**
    jso_list_add_string(list, str): Append the string value <str> to <list>.
    JSOnion version: 1.0.0d
    */
    ds_list_add(argument0, "s" + argument1);
}

#define jso_list_add_sublist
{
    /**
    jso_list_add_sublist(list, sublist): Append the list <sublist> to <list>.
    JSOnion version: 1.0.0d
    */
    ds_list_add(argument0, "l" + string(argument1));
}

#define jso_list_add_submap
{
    /**
    jso_list_add_submap(list, submap): Append the map <submap> to <list>.
    JSOnion version: 1.0.0d
    */
    ds_list_add(argument0, "m" + string(argument1));
}

#define jso_list_add_integer
{
    /**
    jso_list_add_integer(list, int): Append the integer <int> to <list>.
    JSOnion version: 1.0.0d
    */
    ds_list_add(argument0, floor(argument1));
}

#define jso_list_add_boolean
{
    /**
    jso_list_add_boolean(list, bool): Append the boolean value <bool> to <list>.
    JSOnion version: 1.0.0d
    */
    if (argument1) {
        ds_list_add(argument0, "btrue");
    } else {
        ds_list_add(argument0, "bfalse");
    }
}

#define jso_map_get
{
    /**
    jso_map_get(map, key): Retrieve the value stored in <map> with the key value <key>, with the correct type.
    JSOnion version: 1.0.0d
    */

    //Grab the value
    var v;
    v = ds_map_find_value(argument0, argument1);
    
    //String; could be string, map or list
    if (is_string(v)) {
        switch (string_char_at(v, 1)) {
            case "s":
                return string_delete(v, 1, 1);
            break;
            case "l": case "m":
                return real(string_delete(v, 1, 1));
            break;
            case "b":
                if (v == "btrue") {
                    return true;
                }
                else if (v == "bfalse") {
                    return false;
                }
                else {
                    show_error("Invalid boolean value.", true);
                }
            break;
            default: show_error("Invalid map contents.", true); break;
        }    
    }
    
    //Real; return real value as-is
    else {
        return v;
    }
}

#define jso_map_get_type
{
    /**
    jso_map_get_type(map, key): Return the type of value to which the key value <key> is mapped to in <map>.
    JSOnion version: 1.0.0d
    */

    //Grab the value
    var v;
    v = ds_map_find_value(argument0, argument1);
    
    //String; could be string, map or list
    if (is_string(v)) {
        switch (string_char_at(v, 1)) {
            case "s":
                return jso_type_string;
            break;
            case "l":
                return jso_type_list;
            break;
            case "m":
                return jso_type_map;
            break;
            case "b":
                return jso_type_boolean;
            break;
            default: show_error("Invalid map content type.", true); break;
        }    
    }
    
    //Real
    else {
        return jso_type_real;
    }
}

#define jso_list_get
{
    /**
    jso_list_get(list, index): Retrieve the value stored in <list> at position <index>, with the correct type.
    JSOnion version: 1.0.0d
    */

    //Grab the value
    var v;
    v = ds_list_find_value(argument0, argument1);
    
    //String; could be string, map or list
    if (is_string(v)) {
        switch (string_char_at(v, 1)) {
            case "s":
                return string_delete(v, 1, 1);
            break;
            case "l": case "m":
                return real(string_delete(v, 1, 1));
            break;
            case "b":
                if (v == "btrue") {
                    return true;
                }
                else if (v == "bfalse") {
                    return false;
                }
                else {
                    show_error("Invalid boolean value.", true);
                }
            break;
            default: show_error("Invalid list contents.", true); break;
        }    
    }
    
    //Real; return real value as-is
    else {
        return v;
    }
}

#define jso_list_get_type
{
    /**
    jso_list_get_type(list, index): Retrieve the type of value found at position <index> of <list>.
    JSOnion version: 1.0.0d
    */
    
    //Grab the value
    var v;
    v = ds_list_find_value(argument0, argument1);
    
    //String; could be string, map or list
    if (is_string(v)) {
        switch (string_char_at(v, 1)) {
            case "s":
                return jso_type_string;
            break;
            case "l":
                return jso_type_list;
            break;
            case "m":
                return jso_type_map;
            break;
            case "b":
                return jso_type_boolean;
            break;
            default: show_error("Invalid list content type.", true); break;
        }    
    }
    
    //Real
    else {
        return jso_type_real;
    }
}

#define jso_cleanup_map
{
    /**
    jso_cleanup_map(map): Recursively free up <map>.
    JSOnion version: 1.0.0d
    */
    
    //Loop through all keys
    var i, l, k;
    l = ds_map_size(argument0);
    k = ds_map_find_first(argument0);
    for (i=0; i<l; i+=1) {
    
        //Look for values that need to be recursed    
        switch (jso_map_get_type(argument0, k)) {
            //Maps
            case jso_type_map:
                jso_cleanup_map(jso_map_get(argument0, k));
            break;
            //Lists
            case jso_type_list:
                jso_cleanup_list(jso_map_get(argument0, k));
            break;
        }
        
        //Find next key
        k = ds_map_find_next(argument0, k);
    }
    
    //Done, clean up
    ds_map_destroy(argument0);
}

#define jso_cleanup_list
{
    /**
    jso_cleanup_list(list): Recursively free up <list>.
    JSOnion version: 1.0.0d
    */
    
    //Loop through all elements
    var i, l, v;
    l = ds_list_size(argument0);
    for (i=0; i<l; i+=1) {
        //Look for elements that need to be recursed
        switch (jso_list_get_type(argument0, i)) {
            //Maps
            case jso_type_map:
                jso_cleanup_map(jso_list_get(argument0, i));
            break;
            //Lists
            case jso_type_list:
                jso_cleanup_list(jso_list_get(argument0, i));
            break;
        }
    }
    
    //Done, clean up
    ds_list_destroy(argument0);
}

#define jso_encode_real
{
    /**
    jso_encode_real(<real>): Return a JSON-encoded version of real value <real>.
    This uses scientific notation with up to 15 significant digits for decimal values.
    For integers, it just uses string().
    This is adapted from the same algorithm used in GMTuple.
    JSOnion version: 1.0.0d
    */
    
    return __jso_gmt_numtostr(argument0);
}

#define jso_encode_string
{
    /**
    jso_encode_string(str): Return a JSON-encoded version of string <str>.
    JSOnion version: 1.0.0d
    */
    
    //Iteratively reconstruct the string
    var i, l, s, c;
    s = "";
    l = string_length(argument0);
    for (i=1; i<=l; i+=1) {
        //Replace escape characters
        c = string_char_at(argument0, i);
        switch (ord(c)) {
            case 34: case 92: case 47: //Double quotes, backslashes and slashes
                s += "\" + c;
            break;
            case 8: //Backspace
                s += "\b";
            break;
            case 12: //Form feed
                s += "\f";
            break;
            case 10: //New line
                s += "\n";
            break;
            case 13: //Carriage return
                s += "\r";
            break;
            case 9: //Horizontal tab
                s += "\t";
            break;
            default: //Not an escape character
                s += c;
            break;
        }
    }

    //Add quotes
    return '"' + s + '"';
}

#define jso_encode_list
{
    /**
    jso_encode_list(list): Return a JSON-encoded version of list <list>.
    JSOnion version: 1.0.0d
    */
    
    //Iteratively encode each element
    var i, l, s;
    s = "";
    l = ds_list_size(argument0);
    for (i=0; i<l; i+=1) {
        //Prepend comma except for the first element
        if (i > 0) {
            s += ",";
        }
        //Select correct encoding for each element, then recursively encode each
        switch (jso_list_get_type(argument0, i)) {
            case jso_type_real:
                s += jso_encode_real(jso_list_get(argument0, i));
            break;
            case jso_type_string:
                s += jso_encode_string(jso_list_get(argument0, i));
            break;
            case jso_type_map:
                s += jso_encode_map(jso_list_get(argument0, i));
            break;
            case jso_type_list:
                s += jso_encode_list(jso_list_get(argument0, i));
            break;
            case jso_type_boolean:
                s += jso_encode_boolean(jso_list_get(argument0, i));
            break;
        }
    }
    
    //Done, add square brackets
    return "[" + s + "]";
}

#define jso_encode_map
{
    /**
    jso_encode_map(map): Return a JSON-encoded version of map <map>.
    JSOnion version: 1.0.0d
    */
    
    //Go through every key-value pair
    var i, l, k, s;
    s = "";
    l = ds_map_size(argument0);
    k = ds_map_find_first(argument0);
    for (i=0; i<l; i+=1) {
        //Prefix , if there is preceding item
        if (i > 0) {
            s += ",";
        }
        //Find the key and encode it
        if (is_real(k)) {
            s += jso_encode_real(k);
        } else {
            s += jso_encode_string(k);
        }
        //Add the : separator
        s += ":";
        //Select correct encoding for each value, then recursively encode each   
        switch (jso_map_get_type(argument0, k)) {
            case jso_type_real:
                s += jso_encode_real(jso_map_get(argument0, k));
            break;
            case jso_type_string:
                s += jso_encode_string(jso_map_get(argument0, k));
            break;
            case jso_type_map:
                s += jso_encode_map(jso_map_get(argument0, k));
            break;
            case jso_type_list:
                s += jso_encode_list(jso_map_get(argument0, k));
            break;
            case jso_type_boolean:
                s += jso_encode_boolean(jso_map_get(argument0, k));
            break;
        }
        //Get next key
        k = ds_map_find_next(argument0, k);
    }
    
    //Done, add braces
    return "{" + s + "}";
}

#define jso_encode_integer
{
    /**
    jso_encode_integer(int): Return a JSON-encoded version of the integer value <int>.
    JSOnion version: 1.0.0d
    */
    return string(floor(argument0));
}

#define jso_encode_boolean
{
    /**
    jso_encode_boolean(bool): Return a JSON-encoded version of the boolean value <bool>.
    JSOnion version: 1.0.0d
    */
    if (argument0) {
        return "true";
    } else {
        return "false";
    }
}

#define jso_decode_map
{
    /**
    jso_decode_map(json): Return a JSOnion-compatible map representing the JSON string <json>.
    JSOnion version: 1.0.0d
    */
    return __jso_gmt_elem(_jso_decode_map(argument0, 1), 0);
}

#define jso_decode_list
{
    /**
    jso_decode_list(json): Return a JSOnion-compatible list representing the JSON string <json>.
    JSOnion version: 1.0.0d
    */
    return __jso_gmt_elem(_jso_decode_list(argument0, 1), 0);
}

#define jso_decode_string
{
    /**
    jso_decode_string(json): Return a string representing the JSON string <json>.
    JSOnion version: 1.0.0d
    */
    return __jso_gmt_elem(_jso_decode_string(argument0, 1), 0);
}

#define jso_decode_boolean
{
    /**
    jso_decode_boolean(json): Return a boolean value representing the JSON string <json>.
    JSOnion version: 1.0.0d
    */
    return __jso_gmt_elem(_jso_decode_boolean(argument0, 1), 0);
}

#define jso_decode_real
{
    /**
    jso_decode_real(json): Return a real value representing the JSON string <json>.
    JSOnion version: 1.0.0d
    */
    return __jso_gmt_elem(_jso_decode_real(argument0, 1), 0);
}

#define jso_decode_integer
{
    /**
    jso_decode_integer(json): Return an integer value representing the JSON string <json>.
    JSOnion version: 1.0.0d
    */
    return __jso_gmt_elem(_jso_decode_integer(argument0, 1), 0);
}

#define _jso_decode_map
{
    /**
    _jso_decode_map(json, startindex): Extract a map from JSON string <json> starting at position <startindex>.
    Return a 2-tuple of the extracted map handle and the position after the ending }.
    JSOnion version: 1.0.0d
    */
    var i, len, map;
    i = argument1;
    len = string_length(argument0);
    map = jso_new_map();
    
    //Seek to first {
    var c;
    c = string_char_at(argument0, i);
    if (c != "{") {
        do {
            i += 1;
            c = string_char_at(argument0, i);
            if (!_jso_is_whitespace_char(c)) && (c != "{") {
                show_error("Cannot parse map at position " + string(i), true);
            }
        } until (c == "{")
    }
    i += 1;
    
    //Read until end of JSON or ending }
    var found_end, state, found, current_key;
    found_end = false;
    state = 0;
    for (i=i; i<=len && !found_end; i+=1) {
        c = string_char_at(argument0, i);
        switch (state) {
            //0: Looking for a key or closing }
            case 0:
                switch (c) {
                    case "}":
                        found_end = true;
                    break;
                    case '"':
                        found = _jso_decode_string(argument0, i);
                        current_key = __jso_gmt_elem(found, 0);
                        i = __jso_gmt_elem(found, 1)-1;
                        state = 1;
                    break;
                    case "0": case "1": case "2": case "3": case "4": case "5": case "6": case "7": case "8": case "9": case "+": case "-":
                        found = _jso_decode_real(argument0, i);
                        current_key = __jso_gmt_elem(found, 0);
                        i = __jso_gmt_elem(found, 1)-1;
                        state = 1;
                    break;
                    default:
                        if (!_jso_is_whitespace_char(c)) {
                            show_error("Unexpected character at position " + string(i) + ".", true);
                        }
                    break;
                }
            break;
            //1: Looking for the : separator
            case 1:
                switch (c) {
                    case ":":
                        state = 2;
                    break;
                    default:
                        if (!_jso_is_whitespace_char(c)) {
                            show_error("Unexpected character at position " + string(i) + ".", true);
                        }
                    break;
                }
            break;
            //2: Looking for a value
            case 2:
                switch (c) {
                    case "[":
                        found = _jso_decode_list(argument0, i);
                        jso_map_add_sublist(map, current_key, __jso_gmt_elem(found, 0));
                        i = __jso_gmt_elem(found, 1)-1;
                        state = 3;
                    break;
                    case "{":
                        found = _jso_decode_map(argument0, i);
                        jso_map_add_submap(map, current_key, __jso_gmt_elem(found, 0));
                        i = __jso_gmt_elem(found, 1)-1;
                        state = 3;
                    break;
                    case '"':
                        found = _jso_decode_string(argument0, i);
                        jso_map_add_string(map, current_key, __jso_gmt_elem(found, 0));
                        i = __jso_gmt_elem(found, 1)-1;
                        state = 3;
                    break;
                    case "0": case "1": case "2": case "3": case "4": case "5": case "6": case "7": case "8": case "9": case "+": case "-":
                        found = _jso_decode_real(argument0, i);
                        jso_map_add_real(map, current_key, __jso_gmt_elem(found, 0));
                        i = __jso_gmt_elem(found, 1)-1;
                        state = 3;
                    break;
                    case "t": case "f":
                        found = _jso_decode_boolean(argument0, i);
                        jso_map_add_boolean(map, current_key, __jso_gmt_elem(found, 0));
                        i = __jso_gmt_elem(found, 1)-1;
                        state = 3;
                    break;
                    default:
                        if (!_jso_is_whitespace_char(c)) {
                            show_error("Unexpected character at position " + string(i) + ".", true);
                        }
                    break;
                }
            break;
            //3: Done looking for an entry, want comma or }
            case 3:
                switch (c) {
                    case "}":
                        found_end = true;
                    break;
                    case ",":
                        state = 0;
                    break;
                    default:
                        if (!_jso_is_whitespace_char(c)) {
                            show_error("Unexpected character at position " + string(i) + ".", true);
                        }
                    break;
                }
            break;
        }
    }
    
    //Return extracted map with ending position if the ending } is found
    if (found_end) {
        return __jso_gmt_tuple(map, i);
    }
    //Ended too early, throw error
    else {
        show_error("Unexpected end of map in JSON string.", true);
    }
}

#define _jso_decode_list
{
    /**
    _jso_decode_list(json, startindex): Extract a list from JSON string <json> starting at position <startindex>.
    Return a 2-tuple of the extracted list handle and the position after the ending ].
    JSOnion version: 1.0.0d
    */
    var i, len, list;
    i = argument1;
    len = string_length(argument0);
    list = jso_new_list();
    
    //Seek to first [
    var c;
    c = string_char_at(argument0, i);
    if (c != "[") {
        do {
            i += 1;
            c = string_char_at(argument0, i);
            if (!_jso_is_whitespace_char(c)) && (c != "[") {
                show_error("Cannot parse list at position " + string(i), true);
            }
        } until (c == "[")
    }
    i += 1;
    
    //Read until end of JSON or ending ]
    var found_end, state, found;
    found_end = false;
    state = 0;
    for (i=i; i<=len && !found_end; i+=1) {
        c = string_char_at(argument0, i);
        switch (state) {
            //0: Looking for an item
            case 0:
                switch (c) {
                    case "]":
                        found_end = true;
                    break;
                    case "[":
                        found = _jso_decode_list(argument0, i);
                        jso_list_add_sublist(list, __jso_gmt_elem(found, 0));
                        i = __jso_gmt_elem(found, 1)-1;
                        state = 1;
                    break;
                    case "{":
                        found = _jso_decode_map(argument0, i);
                        jso_list_add_submap(list, __jso_gmt_elem(found, 0));
                        i = __jso_gmt_elem(found, 1)-1;
                        state = 1;
                    break;
                    case '"':
                        found = _jso_decode_string(argument0, i);
                        jso_list_add_string(list, __jso_gmt_elem(found, 0));
                        i = __jso_gmt_elem(found, 1)-1;
                        state = 1;
                    break;
                    case "0": case "1": case "2": case "3": case "4": case "5": case "6": case "7": case "8": case "9": case "+": case "-":
                        found = _jso_decode_real(argument0, i);
                        jso_list_add_real(list, __jso_gmt_elem(found, 0));
                        i = __jso_gmt_elem(found, 1)-1;
                        state = 1;
                    break;
                    case "t": case "f":
                        found = _jso_decode_boolean(argument0, i);
                        jso_list_add_boolean(list, __jso_gmt_elem(found, 0));
                        i = __jso_gmt_elem(found, 1)-1;
                        state = 1;
                    break;
                    default:
                        if (!_jso_is_whitespace_char(c)) {
                            show_error("Unexpected character at position " + string(i) + ".", true);
                        }
                    break;
                }
            break;
            //1: Done looking for an item, want comma or ]
            case 1:
                switch (c) {
                    case "]":
                        found_end = true;
                    break;
                    case ",":
                        state = 0;
                    break;
                    default:
                        if (!_jso_is_whitespace_char(c)) {
                            show_error("Unexpected character at position " + string(i) + ".", true);
                        }
                    break;
                }
            break;
        }
    }
    
    //Return extracted list with ending position if the ending ] is found
    if (found_end) {
        return __jso_gmt_tuple(list, i);
    }
    //Ended too early, throw error
    else {
        show_error("Unexpected end of list in JSON string.", true);
    }
}

#define _jso_decode_string
{
    /**
    _jso_decode_string(json, startindex): Extract a string from JSON string <json> starting at position <startindex>.
    Return a 2-tuple of the extracted string and the position after the ending double quote.
    JSOnion version: 1.0.0d
    */
    var i, len, str;
    i = argument1;
    len = string_length(argument0);
    str = "";
    
    //Seek to first double quote
    var c;
    c = string_char_at(argument0, i);
    if (c != '"') {
        do {
            i += 1;
            c = string_char_at(argument0, i);
        } until (c == '"')
    }
    i += 1;
    
    //Read until end of JSON or ending double quote
    var found_end, escape_mode;
    found_end = false;
    escape_mode = false;
    for (i=i; i<=len && !found_end; i+=1) {
        c = string_char_at(argument0, i);
        //Escape mode
        if (escape_mode) {
            switch (c) {
                case '"': case "\": case "/":
                    str += c;
                    escape_mode = false;
                break;
                case "b":
                    str += chr(8);
                    escape_mode = false;
                break;
                case "f":
                    str += chr(12);
                    escape_mode = false;
                break;
                case "n":
                    str += chr(10);
                    escape_mode = false;
                break;
                case "r":
                    str += chr(13);
                    escape_mode = false;
                break;
                case "t":
                    str += chr(9);
                    escape_mode = false;
                break;
                case "u":
                    var u;
                    if (len-i < 5) {
                        show_error("Invalid escape character at position " + string(i) + ".", true);
                    } else {
                        str += chr(_jso_hex_to_decimal(string_copy(argument0, i+1, 4)));
                        escape_mode = false;
                        i += 4;
                    }
                break;
                default:
                    show_error("Invalid escape character at position " + string(i) + ".", true);
                break;
            }
        }
        //Regular mode
        else {
            switch (c) {
                case '"': found_end = true; break;
                case "\": escape_mode = true; break;
                default: str += c; break;
            }
        }
    }
    
    //Return extracted string with ending position if the ending double quote is found
    if (found_end) {
        return __jso_gmt_tuple(str, i);
    }
    //Ended too early, throw error
    else {
        show_error("Unexpected end of string in JSON string.", true);
    }
}

#define _jso_decode_boolean
{
    /**
    _jso_decode_boolean(json, startindex): Extract a boolean from JSON string <json> starting at position <startindex>.
    Return a 2-tuple of the extracted boolean and the position after the last e.
    JSOnion version: 1.0.0d
    */
    var i, len, str;
    i = argument1;
    len = string_length(argument0);
    
    //Seek to first t or f that can be found
    var c;
    c = string_char_at(argument0, i);
    if (c != "t") && (c != "f") {
        do {
            i += 1;
            c = string_char_at(argument0, i);
            if (!_jso_is_whitespace_char(c)) && (c != "t") && (c != "f") {
                show_error("Cannot parse boolean value at position " + string(i), true);
            }
        } until (c == "t") || (c == "f")
    }
    
    //Look for true if t is found
    if (c == "t") && (string_copy(argument0, i, 4) == "true") {
        return __jso_gmt_tuple(true, i+4);
    }
    //Look for false if f is found
    else if (c == "f") && (string_copy(argument0, i, 5) == "false") {
        return __jso_gmt_tuple(false, i+5);
    }
    //Error: unexpected ending
    else {
        show_error("Unexpected end of boolean in JSON string.", true);
    }
}

#define _jso_decode_real
{
    /**
    _jso_decode_real(json, startindex): Extract a real value from JSON string <json> starting at position <startindex>.
    Return a 2-tuple of the extracted real value and the position after the real value.
    JSOnion version: 1.0.0d
    */
    var i, len, str;
    i = argument1;
    len = string_length(argument0);
    str = "";
    
    //Seek to first character: +, -, or 0-9
    var c;
    c = string_char_at(argument0, i);
    if (string_pos(c, "0123456789+-") == 0) {
        do {
            i += 1;
            c = string_char_at(argument0, i);
            if (!_jso_is_whitespace_char(c)) && (string_pos(c, "0123456789+-") == 0) {
                show_error("Cannot parse real value at position " + string(i), true);
            }
        } until (string_pos(c, "0123456789+-") > 0)
    }
    
    //Determine starting state
    var state;
    switch (c) {
        case "+": case "-":
            state = 0;
        break;
        default:
            state = 1;
        break;
    }
    str += c;
    i += 1;
    
    //Loop until no more digits found
    var done;
    done = false;
    for (i=i; i<=len && !done; i+=1) {
        c = string_char_at(argument0, i);
        switch (state) {
            //0: Found a sign, looking for a starting number
            case 0:
                switch (c) {
                    case "0": case "1": case "2": case "3": case "4": case "5": case "6": case "7": case "8": case "9":
                        str += c;
                        state = 1;
                    break;
                    default:
                        show_error("Unexpected character at position " + string(i) + ", expecting a digit.", true);
                    break;
                }
            break;
            //1: Found a starting digit, looking for decimal dot, e, E, or more digits
            case 1:
                if (_jso_is_whitespace_char(c)) || (string_pos(c, ":,]}") > 0) {
                    done = true;
                    i -= 1;
                } else {
                    switch (c) {
                        case "0": case "1": case "2": case "3": case "4": case "5": case "6": case "7": case "8": case "9":
                            str += c;
                        break;
                        case ".":
                            str += c;
                            state = 2;
                        break;
                        case "e": case "E":
                            str += c;
                            state = 3;
                        break;
                        default:
                            show_error("Unexpected character at position " + string(i) + ", expecting a dot, e, E or a digit.", true);
                        break;
                    }
                }
            break;
            //2: Found a decimal dot, looking for more digits
            case 2:
                switch (c) {
                    case "0": case "1": case "2": case "3": case "4": case "5": case "6": case "7": case "8": case "9":
                        str += c;
                        state = -2;
                    break;
                    default:
                        show_error("Unexpected character at position " + string(i) + ", expecting a digit.", true);
                    break;
                }
            break;
            //-2: Found a decimal dot and a digit after it, looking for more digits, e, or E
            case -2:
                if (_jso_is_whitespace_char(c)) || (string_pos(c, ":,]}") > 0) {
                    done = true;
                    i -= 1;
                } else {
                    switch (c) {
                        case "0": case "1": case "2": case "3": case "4": case "5": case "6": case "7": case "8": case "9":
                            str += c;
                        break;
                        case "e": case "E":
                            str += c;
                            state = 3;
                        break;
                        default:
                            show_error("Unexpected character at position " + string(i) + ", expecting an e, E or a digit.", true);
                        break;
                    }
                }
            break;
            //3: Found an e/E, looking for +, - or more digits
            case 3:
                switch (c) {
                    case "+": case "-":
                        str += c;
                        state = 4;
                    break;
                    case "0": case "1": case "2": case "3": case "4": case "5": case "6": case "7": case "8": case "9":
                        str += c;
                        state = 5;
                    break;
                    default:
                        show_error("Unexpected character at position " + string(i) + ", expecting a +, - or a digit.", true);
                    break;
                }
            break;
            //4: Found an e/E exponent sign, looking for more digits
            case 4:
                switch (c) {
                    case "0": case "1": case "2": case "3": case "4": case "5": case "6": case "7": case "8": case "9":
                        str += c;
                        state = 5;
                    break;
                    default:
                        show_error("Unexpected character at position " + string(i) + ", expecting a digit.", true);
                    break;
                }
            break;
            //5: Looking for final digits of the exponent
            case 5:
                if (_jso_is_whitespace_char(c)) || (string_pos(c, ":,]}") > 0) {
                    done = true;
                    i -= 1;
                } else {
                    switch (c) {
                        case "0": case "1": case "2": case "3": case "4": case "5": case "6": case "7": case "8": case "9":
                            str += c;
                            state = 5;
                        break;
                        default:
                            show_error("Unexpected character at position " + string(i) + ", expecting a digit.", true);
                        break;
                    }
                }
            break;
        }
    }
    
    //Am I still expecting more characters?
    if (done) || (state == 1) || (state == -2) || (state == 5) {
        return __jso_gmt_tuple(real(str), i);
    }
    //Error: unexpected ending
    else {
        show_error("Unexpected end of real in JSON string.", true);
    }
}

#define _jso_decode_integer
{
    /**
    _jso_decode_real(json, startindex): Extract a real value from JSON string <json> starting at position <startindex>.
    Return a 2-tuple of the extracted integer value (with leading i) and the position after the real value.
    JSOnion version: 1.0.0d
    */
    var i, len, str;
    i = argument1;
    len = string_length(argument0);
    str = "";
    
    //Seek to first character: +, -, or 0-9
    var c;
    c = string_char_at(argument0, i);
    if (string_pos(c, "0123456789+-") == 0) {
        do {
            i += 1;
            c = string_char_at(argument0, i);
            if (!_jso_is_whitespace_char(c)) && (string_pos(c, "0123456789+-") == 0) {
                show_error("Cannot parse integer value at position " + string(i), true);
            }
        } until (string_pos(c, "0123456789+-") > 0)
    }
    
    //Determine starting state
    var state;
    switch (c) {
        case "+": case "-":
            state = 0;
        break;
        default:
            state = 1;
        break;
    }
    str += c;
    i += 1;
    
    //Loop until no more digits found
    var done;
    done = false;
    for (i=i; i<=len && !done; i+=1) {
        c = string_char_at(argument0, i);
        switch (state) {
            //0: Found a sign, looking for a starting number
            case 0:
                switch (c) {
                    case "0": case "1": case "2": case "3": case "4": case "5": case "6": case "7": case "8": case "9":
                        str += c;
                        state = 1;
                    break;
                    default:
                        show_error("Unexpected character at position " + string(i) + ", expecting a digit.", true);
                    break;
                }
            break;
            //1: Found a starting digit, looking for decimal dot, e, E, or more digits
            case 1:
                if (_jso_is_whitespace_char(c)) || (string_pos(c, ":,]}") > 0) {
                    done = true;
                    i -= 1;
                } else {
                    switch (c) {
                        case "0": case "1": case "2": case "3": case "4": case "5": case "6": case "7": case "8": case "9":
                            str += c;
                        break;
                        default:
                            show_error("Unexpected character at position " + string(i) + ", expecting a digit.", true);
                        break;
                    }
                }
            break;
        }
    }
    
    //Am I still expecting more characters?
    if (done) || (state == 1) {
        return __jso_gmt_tuple(floor(real(str)), i);
    }
    //Error: unexpected ending
    else {
        show_error("Unexpected end of integer in JSON string.", true);
    }
}

#define jso_compare_maps
{
    /**
    jso_compare_maps(map1, map2): Return whether the contents of JSOnion-compatible maps <map1> and <map2> are the same.
    JSOnion version: 1.0.0d
    */
    
    //If they aren't the same size, they can't be the same
    var size;
    size = ds_map_size(argument0);
    if (size != ds_map_size(argument1)) {
        return false;
    }
    
    //Compare contents pairwise
    var i, k, type, a, b;
    k = ds_map_find_first(argument0);
    for (i=0; i<size; i+=1) {
        //Check that key exists on both sides
        if (!ds_map_exists(argument1, k)) {
            return false;
        }
        //Check type
        type = jso_map_get_type(argument0, k);
        if (jso_map_get_type(argument1, k) != type) {
            return false;
        }
        //Check content
        a = jso_map_get(argument0, k);
        b = jso_map_get(argument1, k);
        switch (type) {
            case jso_type_map:
                if (!jso_compare_maps(a, b)) {
                    return false;
                }
            break;
            case jso_type_list:
                if (!jso_compare_lists(a, b)) {
                    return false;
                }
            break;
            default:
                if (a != b) {
                    return false;
                }
            break;
        }
        //Advance to next key
        k = ds_map_find_next(argument0, k);
    }
    
    //No mismatches, return true
    return true;
}

#define jso_compare_lists
{
    /**
    jso_compare_lists(list1, list2): Return whether the contents of JSOnion-compatible lists <list1> and <list2> are the same.
    JSOnion version: 1.0.0d
    */
    
    //If they aren't the same size, they can't be the same
    var size;
    size = ds_list_size(argument0);
    if (size != ds_list_size(argument1)) {
        return false;
    }
    
    //Compare contents pairwise
    var i, type, a, b;
    for (i=0; i<size; i+=1) {
        //Check type
        type = jso_list_get_type(argument0, i);
        if (jso_list_get_type(argument1, i) != type) {
            return false;
        }
        //Check content
        a = jso_list_get(argument0, i);
        b = jso_list_get(argument1, i);
        switch (type) {
            case jso_type_map:
                if (!jso_compare_maps(a, b)) {
                    return false;
                }
            break;
            case jso_type_list:
                if (!jso_compare_lists(a, b)) {
                    return false;
                }
            break;
            default:
                if (a != b) {
                    return false;
                }
            break;
        }
    }
    
    //No mismatches, return true
    return true;
}

#define jso_map_check
{
    /**
    jso_map_check(map, key1, key2, ...): Recursively look up keys/indices in the top-level map <map>, return whether a value exists there.
    JSOnion version: 1.0.0d
    */
    
    //Catch empty calls
    if (argument_count < 2) {
        show_error("Expected at least 2 arguments, got " + string(argument_count) + ".", true);
    }
    
    //Build list of keys/indices
    var i, key_list;
    key_list = ds_list_create();
    for (i=1; i<argument_count; i+=1) {
        ds_list_add(key_list, argument[i]);
    }
    
    //Call lookup kernel and cleanup
    var result;
    result = _jso_lookup_kernel(argument[0], jso_type_map, 0, key_list);
    ds_list_destroy(key_list);
    
    //Done
    return result;
}

#define jso_list_check
{
    /**
    jso_list_check(list, key1, key2, ...): Recursively look up keys/indices in the top-level list <list>, return whether a value exists there.
    JSOnion version: 1.0.0d
    */
    
    //Catch empty calls
    if (argument_count < 2) {
        show_error("Expected at least 2 arguments, got " + string(argument_count) + ".", true);
    }
    
    //Build list of keys/indices
    var i, key_list;
    key_list = ds_list_create();
    for (i=1; i<argument_count; i+=1) {
        ds_list_add(key_list, argument[i]);
    }
    
    //Call lookup kernel and cleanup
    var result;
    result = _jso_lookup_kernel(argument[0], jso_type_list, 0, key_list);
    ds_list_destroy(key_list);
    
    //Done
    return result;
}

#define jso_map_lookup
{
    /**
    jso_map_lookup(map, key1, key2, ...): Recursively look up keys/indices in the top-level map <map>, return the value that exists there.
    JSOnion version: 1.0.0d
    */
    
    //Catch empty calls
    if (argument_count < 2) {
        show_error("Expected at least 2 arguments, got " + string(argument_count) + ".", true);
    }
    
    //Build list of keys/indices
    var i, key_list;
    key_list = ds_list_create();
    for (i=1; i<argument_count; i+=1) {
        ds_list_add(key_list, argument[i]);
    }
    
    //Call lookup kernel and cleanup
    var result;
    result = _jso_lookup_kernel(argument[0], jso_type_map, 1, key_list);
    ds_list_destroy(key_list);
    
    //Done
    return result;
}

#define jso_map_lookup_type
{
    /**
    jso_map_lookup_type(map, key1, key2, ...): Recursively look up keys/indices in the top-level map <map>, return the type of value that exists there.
    JSOnion version: 1.0.0d
    */
    
    //Catch empty calls
    if (argument_count < 2) {
        show_error("Expected at least 2 arguments, got " + string(argument_count) + ".", true);
    }
    
    //Build list of keys/indices
    var i, key_list;
    key_list = ds_list_create();
    for (i=1; i<argument_count; i+=1) {
        ds_list_add(key_list, argument[i]);
    }
    
    //Call lookup kernel and cleanup
    var result;
    result = _jso_lookup_kernel(argument[0], jso_type_map, 2, key_list);
    ds_list_destroy(key_list);
    
    //Done
    return result;
}

#define jso_list_lookup
{
    /**
    jso_list_lookup(list, key1, key2, ...): Recursively look up keys/indices in the top-level list <list>, return the value that exists there.
    JSOnion version: 1.0.0d
    */
    
    //Catch empty calls
    if (argument_count < 2) {
        show_error("Expected at least 2 arguments, got " + string(argument_count) + ".", true);
    }
    
    //Build list of keys/indices
    var i, key_list;
    key_list = ds_list_create();
    for (i=1; i<argument_count; i+=1) {
        ds_list_add(key_list, argument[i]);
    }
    
    //Call lookup kernel and cleanup
    var result;
    result = _jso_lookup_kernel(argument[0], jso_type_list, 1, key_list);
    ds_list_destroy(key_list);
    
    //Done
    return result;
}

#define jso_list_lookup_type
{
    /**
    jso_list_lookup_type(list, key1, key2, ...): Recursively look up keys/indices in the top-level list <list>, return the type of value that exists there.
    JSOnion version: 1.0.0d
    */
    
    //Catch empty calls
    if (argument_count < 2) {
        show_error("Expected at least 2 arguments, got " + string(argument_count) + ".", true);
    }
    
    //Build list of keys/indices
    var i, key_list;
    key_list = ds_list_create();
    for (i=1; i<argument_count; i+=1) {
        ds_list_add(key_list, argument[i]);
    }
    
    //Call lookup kernel and cleanup
    var result;
    result = _jso_lookup_kernel(argument[0], jso_type_list, 2, key_list);
    ds_list_destroy(key_list);
    
    //Done
    return result;
}

#define _jso_lookup_kernel
{
    /**
    _jso_lookup_kernel(jso_ds, jso_type, task_type, ds_args_list): Kernel of the check and lookup functions.
    - jso_ds: The JSOnion-compatible data structure handle.
    - jso_type: The type of the JSOnion-compatible (jso_type_list or jso_type_map)
    - task_type: 0=check, 1=lookup, 2=lookup type
    - ds_args_list: ds_list of arguments passed in.
    
    PLEASE DO NOT CALL DIRECTLY --- use regular jso_*() functions.
    JSOnion version: 1.0.0d
    */
    
    var i, k, data, type, type_string, task_type, keys_size, ds_args_list;
    data = argument0;
    type = argument1;
    if (type == jso_type_map) {
        type_string = "map";
    } else {
        type_string = "list";
    }
    task_type = argument2;
    ds_args_list = argument3;
    keys_size = ds_list_size(ds_args_list);
    
    //Iteratively go through arguments in ds_args_list
    for (i=0; i<keys_size; i+=1) {
        k = ds_list_find_value(argument3, i);
        switch (type) {
            //Check for existence of the key in the current map
            case jso_type_map:
                if (!ds_map_exists(data, k)) {
                    switch (task_type) {
                        case 0:
                            return false;
                        break;
                        default:
                            show_error("Cannot find value in " + type_string + " lookup.", true);
                        break;
                    }
                }
                type = jso_map_get_type(data, k);
                data = jso_map_get(data, k);
            break;
            //Check for existence of the index in the current list
            case jso_type_list:
                if (is_string(k)) {
                    switch (task_type) {
                        case 0:
                            return false;
                        break;
                        default:
                            show_error("Cannot use string indices for nested lists in " + type_string + " lookup.", true);
                        break;
                    }
                }
                if (k >= ds_list_size(data)) {
                    switch (task_type) {
                        case 0:
                            return false;
                        break;
                        default:
                            show_error("Index overflow for nested lists in " + type_string + " lookup.", true);
                        break;
                    }
                }
                type = jso_list_get_type(data, k);
                data = jso_list_get(data, k);
            break;
            //Trying to go through a leaf; don't attempt to look further
            default:
                switch (task_type) {
                    case 0:
                        return false;
                    break;
                    default:
                        show_error("Recursive overflow in " + type_string + " lookup.", true);
                    break;
                }
            break;
        }
    }
    
    //Can find something, return the value requested by the task
    switch (task_type) {
        case 0:
            return true;
        break;
        case 1:
            return data;
        break;
        case 2:
            return type;
        break;
    }
}

#define _jso_is_whitespace_char
{
    /**
    _jso_is_whitespace_char(char): Return whether <char> is a whitespace character.
    Definition of whitespace is given by Unicode 6.0, Chapter 4.6.
    JSOnion version: 1.0.0d
    */
    switch (ord(argument0)) {
        case $0009:
        case $000A:
        case $000B:
        case $000C:
        case $000D:
        case $0020:
        case $0085:
        case $00A0:
        case $1680:
        case $180E:
        case $2000:
        case $2001:
        case $2002:
        case $2003:
        case $2004:
        case $2005:
        case $2006:
        case $2007:
        case $2008:
        case $2009:
        case $200A:
        case $2028:
        case $2029:
        case $202F:
        case $205F:
        case $3000:
            return true;
    }
    return false;
}

#define _jso_hex_to_decimal
{
    /**
    _jso_hex_to_decimal(hex_string): Return the decimal value of the hex number represented by <hex_string>
    JSOnion version: 1.0.0d
    */
    var hex_string, hex_digits;
    hex_string = string_lower(argument0);
    hex_digits = "0123456789abcdef";
    
    //Convert digit-by-digit
    var i, len, digit_value, num;
    len = string_length(hex_string);
    num = 0;
    for (i=1; i<=len; i+=1) {
        digit_value = string_pos(string_char_at(hex_string, i), hex_digits)-1;
        if (digit_value >= 0) {
            num *= 16;
            num += digit_value;
        } 
        //Unknown character
        else {
            show_error("Invalid hex number: " + argument0, true);
        }
    }
    return num;
}

