
/*
 * This file is part of Jkop
 * Copyright (c) 2016 Job and Esther Technologies, Inc.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

public interface String : Stringable, Integer, Double, Boolean
{
	public static String instance(String o) {
		if(o == null) {
			return("");
		}
		return(o);
	}

	public static String as_string(Object o) {
		if(o == null) {
			return(null);
		}
		if(o is String) {
			return((String)o);
		}
		if(o is Stringable) {
			return(((Stringable)o).to_string());
		}
		return(null);
	}

	public static strptr as_strptr(Object o) {
		var str = as_string(o);
		if(str == null) {
			return(null);
		}
		return(str.to_strptr());
	}

	public static bool is_in_collection(String str, Collection c) {
		if(str == null) {
			return(false);
		}
		foreach(String s in c) {
			if(s.equals(str)) {
				return(true);
			}
		}
		return(false);
	}

	public static bool is_empty(Object o) {
		if(o == null) {
			return(true);
		}
		var str = o as String;
		if(str == null && o is Stringable) {
			str = ((Stringable)o).to_string();
		}
		if(str == null) {
			return(true);
		}
		if(str.get_char(0) < 1) {
			return(true);
		}
		return(false);
	}

	public static String for_object(Object o) {
		if(o is String) {
			return((String)o);
		}
		if(o is Stringable) {
			return(((Stringable)o).to_string());
		}
		return(null);
	}

	public static String for_character(int c) {
		var sb = StringBuffer.create();
		sb.append_c(c);
		return(sb.to_string());
	}

	public static String for_integer(int av) {
		IFDEF("target_cs") {
			strptr v;
			embed {{{
				v = av.ToString();
			}}}
			return(String.for_strptr(v));
		}
		ELSE IFDEF("target_java") {
			strptr st;
			embed {{{
				st = java.lang.String.valueOf(av);
			}}}
			return(String.for_strptr(st));
		}
		ELSE {
			return("%d".printf().add(av).to_string());
		}
	}

	public static String for_long(long av) {
		IFDEF("target_cs") {
			strptr v;
			embed {{{
				v = av.ToString();
			}}}
			return(String.for_strptr(v));
		}
		ELSE IFDEF("target_java") {
			strptr st;
			embed {{{
				st = java.lang.String.valueOf(av);
			}}}
			return(String.for_strptr(st));
		}
		ELSE {
			return(for_integer((int)av));
		}
	}

	public static String for_double(double v) {
		IFDEF("target_java") {
			strptr st;
			embed {{{
				st = java.lang.String.valueOf(v);
			}}}
			return(String.for_strptr(st));
		}
		ELSE {
			return("%f".printf().add(v).to_string());
		}
	}

	public static String for_boolean(bool val) {
		if(val) {
			return("true");
		}
		return("false");
	}

	public static String for_strptr(strptr literal) {
		var v = new StringImpl();
		v.set_strptr(literal);
		return(v);
	}

	public static String for_utf8_buffer(Buffer data, bool haszero = true) {
		var v = new StringImpl();
		v.set_utf8_buffer(data, haszero);
		return(v);
	}

	public static String combine(Collection strings, int delim = -1, bool unique = false) {
		var sb = StringBuffer.create();
		HashTable flags;
		if(unique) {
			flags = HashTable.create();
		}
		foreach(Object o in strings) {
			var s = String.as_string(o);
			if(s == null) {
				continue;
			}
			if(flags != null) {
				if(flags.get(s) != null) {
					continue;
				}
				flags.set(s, "true");
			}
			if(delim > 0 && sb.count() > 0) {
				sb.append_c(delim);
			}
			sb.append(s);
		}
		return(sb.to_string());
	}

	public static String capitalize(String str) {
		if(str == null) {
			return(null);
		}
		var c0 = str.get_char(0);
		if(c0 < 1) {
			return(null);
		}
		if(c0 >= 'a' && c0 <= 'z') {
			var sb = StringBuffer.create();
			sb.append_c(c0 - 'a' + 'A');
			sb.append(str.substring(1));
			return(sb.to_string());
		}
		return(str);
	}

	public StringFormatter printf();
	public String dup();
	public String append(String str);
	public int get_length();
	public int get_char(int n);
	public String truncate(int len);
	public String replace(int o, int r);
	public String replace_char(int o, int r);
	public String replace_string(String o, String r);
	public String remove(int start, int len);
	public String insert(String str, int pos);
	public String substring(int start, int alength = -1);
	public String reverse();
	public String lowercase();
	public String uppercase();
	public String strip();
	public Iterator split(int delim, int max = -1);
	public int str(String s);
	public bool contains(String s);
	public int rstr(String s);
	public int chr(int c);
	public int rchr(int c);
	public bool has_prefix(String prefix);
	public bool has_suffix(String suffix);
	public int compare(Object ao);
	public int compare_ignore_case(Object ao);
	public bool equals(Object ao);
	public bool equals_ptr(strptr str);
	public bool equals_ignore_case(Object ao);
	public bool equals_ignore_case_ptr(strptr str);
	public StringIterator iterate();
	public StringIterator iterate_reverse();
	public int to_integer_base(int ibase);
	public strptr to_strptr();
	public Buffer to_utf8_buffer(bool zero = true);
	public int hash();
	public EditableString as_editable();
}