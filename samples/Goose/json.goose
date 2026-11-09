// JSON (RFC 8259; docs/stdlib.md): a writer that appends values to a byte
// builder, a parser that builds a document tree, and a reader that parses
// straight into the caller's own types, the way a serde derive does.

namespace json;

import std;

// --- writing --------------------------------------------------------------------

// s as a JSON string: quoted, with '"', '\' and the control characters
// escaped. Most strings have none of those, and go out in one piece.
fn format_string(out: u8[>..]&, s: u8[:]) {
    out.push('"');
    if plain(s) { out.append(s); } else { format_escaped(out, s); }
    out.push('"');
}

// An object key and its colon.
fn format_key(out: u8[>..]&, key: u8[:]) {
    out.push('"');
    if plain(key) { out.append(key); } else { format_escaped(out, key); }
    out.append("\":");
}

// Whether s holds nothing a JSON string escapes.
fn plain(s: u8[:]) -> bool {
    for c in s {
        if c < 0x20 || c == '"' || c == '\\' { return false; }
    }
    return true;
}

// s with its quotes, backslashes and control characters escaped.
fn format_escaped(out: u8[>..]&, s: u8[:]) {
    var start = 0;
    for c, i in s {
        if c >= 0x20 && c != '"' && c != '\\' { continue; }
        out.append(s[start..i]);
        start = i + 1;
        if c == '"' || c == '\\' { out.push('\\'); out.push(c); }
        else if c == '\n' { out.append("\\n"); }
        else if c == '\r' { out.append("\\r"); }
        else if c == '\t' { out.append("\\t"); }
        else if c == 8 { out.append("\\b"); }
        else if c == 12 { out.append("\\f"); }
        else {
            out.append("\\u00");
            out.push("0123456789abcdef"[c >> 4]);
            out.push("0123456789abcdef"[c & 15]);
        }
    }
    out.append(s[start..]);
}

fn format_bool(out: u8[>..]&, b: bool) {
    out.append(if b { "true" } else { "false" });
}

fn format_int(out: u8[>..]&, v: i64) {
    format(out, v);
}

// The two digits of each number below 100, so that digits come out of a
// division by 100 two at a time.
let DIGIT_PAIRS = "00010203040506070809101112131415161718192021222324252627282930313233343536373839404142434445464748495051525354555657585960616263646566676869707172737475767778798081828384858687888990919293949596979899";

// v as a JSON number: the fewest significant digits that read back as v,
// laid out as serde_json's ryu lays them out: 0.5, 1.0, 123.25, 1e-7,
// 1.5e300, 0.30000000000000004. JSON has no NaN or infinity, so those are
// written as null, as serde_json writes them.
fn format_number(out: u8[>..]&, v: f64) {
    var text: u8[21] = [0; 21];
    let len, point = ryu_digits(v, text[1..]);
    guard len > 0 else { format_other(out, v); return; }
    if v < 0.0 { out.push('-'); }
    lay_out(out, text, len, point);
}

// format_number for v outside ryu_digits' range: the runtime's shortest
// digits, found exactly in big integers, laid out as the others. It is
// called from a guard's else only, which keeps it out of line, off
// format_number's fast path.
fn format_other(out: u8[>..]&, v: f64) {
    if v != v || v - v != 0.0 { out.append("null"); return; }
    if v == 0.0 { out.append(if 1.0 / v < 0.0 { "-0.0" } else { "0.0" }); return; }
    var text: u8[21] = [0; 21];
    var point = 0;
    let len = float_text::shortest_digits(abs(v), text[1..18], point);
    if v < 0.0 { out.push('-'); }
    lay_out(out, text, len, point);
}

// The number 0.d1 d2 ... dlen * 10^point as format_number writes it, the
// digits in text[1..len + 1]: one place in, so that a point can go in front
// of the fraction by moving the digits before it, nearly always one to
// three, out.
inline fn lay_out(out: u8[>..]&, text: u8[21]&, digits: i64, point: i64) {
    var len = digits;
    while text[len] == '0' { len--; }
    if point > 0 && point < len {
        for i in point { text[i] = text[i + 1]; }
        text[point] = '.';
        out.append(text[..len + 1]);
        return;
    }
    let d = text[1..len + 1];
    if point >= len && point <= 16 {
        out.append(d);
        for i in point - len { out.push('0'); }
        out.append(".0");
    } else if point > -5 && point <= 0 {
        out.append("0.");
        for i in -point { out.push('0'); }
        out.append(d);
    } else {
        out.push(d[0]);
        if len > 1 {
            out.push('.');
            out.append(d[1..]);
        }
        out.push('e');
        format_int(out, point - 1);
    }
}

// The digits of the shortest form of |x| into out, with their count and
// the power of ten they are a fraction of, as format_number takes them; a
// count of 0 for |x| outside about [1e-10, 2^51], NaN and the infinities
// included, which format_number handles otherwise. This is ryu's digit
// loop, run on exact values instead of ryu's tables: with |x| = m * 2^-sh
// and k chosen to give |x| * 10^k 18 or 19 digits, |x| * 10^k and the
// bounds of the interval that reads back as x, half an ulp either side (a
// quarter below a power of two), are 4m * 5^k and 2 * 5^k apart in units of
// 2^(k - sh - 2): 128-bit integers, which shifted right give ryu's vr, vp
// and vm, and the bits shifted out whether each is exact. The range is where
// 5^k fits a u64 and the shift is within one.
inline fn ryu_digits(x: f64, out: u8[:]) -> i64, i64 {
    let b = float_text::bits_of(x);
    let be = ((b >> 52) & 0x7FF) as! i64;
    let frac = b & 0xFFFFFFFFFFFFF;
    let k = 17 - (((be - 1023) * 78913) >> 18);     // 17 - floor(log10(2^(be - 1023)))
    let t = 1077 - be - k;                          // sh + 2 - k, sh = 1075 - be
    guard k >= 0 && k <= 27 && t >= 1 && t <= 63 else { return 0, 0; }
    let m = frac | 0x10000000000000;
    let even = (m & 1) == 0;
    let p = float_text::POW5[k];
    let nh, nl = float_text::mul_wide(m, p);
    // 4x, and the upper and lower bounds.
    let xh = (nh << 2) | (nl >> 62);
    let xl = nl << 2;
    var uh = xh;
    var ul = xl;
    for i in 2 {
        ul += p;
        if ul < p { uh++; }
    }
    var lh = xh;
    var ll = xl;
    for i in (if frac != 0 { 2 } else { 1 }) {
        if ll < p { lh--; }
        ll -= p;
    }
    let low = ((1 as u64) << t) - 1;
    var vr = (xh << (64 - t)) | (xl >> t);
    var vp = (uh << (64 - t)) | (ul >> t);
    var vm = (lh << (64 - t)) | (ll >> t);
    let digits0 = if vr >= 1000000000000000000 { 19 } else { 18 };
    var vr_exact = (xl & low) == 0;
    var vm_exact = even && (ll & low) == 0;
    if !even && (ul & low) == 0 { vp--; }
    var removed = 0;
    var c: u64 = 0;
    if !vr_exact && !vm_exact {
        // Nearly always neither x * 10^k nor an included lower bound is
        // exact, and the digits dropped only round: ryu's common case, two
        // digits a step first, then one.
        var round_up = false;
        let vp100 = vp / 100;
        let vm100 = vm / 100;
        if vp100 > vm100 {
            let vr100 = vr / 100;
            round_up = vr - 100 * vr100 >= 50;
            vr = vr100;
            vp = vp100;
            vm = vm100;
            removed = 2;
        }
        loop {
            let vp10 = vp / 10;
            let vm10 = vm / 10;
            if vp10 <= vm10 { break; }
            let vr10 = vr / 10;
            round_up = vr - 10 * vr10 >= 5;
            vr = vr10;
            vp = vp10;
            vm = vm10;
            removed++;
        }
        c = vr + (if vr == vm || round_up { 1 } else { 0 });
    } else {
        var last: u64 = 0;
        loop {
            let vp10 = vp / 10;
            let vm10 = vm / 10;
            if vp10 <= vm10 { break; }
            let vr10 = vr / 10;
            vm_exact = vm_exact && vm - 10 * vm10 == 0;
            vr_exact = vr_exact && last == 0;
            last = vr - 10 * vr10;
            vr = vr10;
            vp = vp10;
            vm = vm10;
            removed++;
        }
        if vm_exact {
            loop {
                let vm10 = vm / 10;
                if vm - 10 * vm10 != 0 { break; }
                let vr10 = vr / 10;
                vr_exact = vr_exact && last == 0;
                last = vr - 10 * vr10;
                vr = vr10;
                vp /= 10;
                vm = vm10;
                removed++;
            }
        }
        if vr_exact && last == 5 && vr % 2 == 0 { last = 4; }
        c = vr + (if (vr == vm && !vm_exact) || last >= 5 { 1 } else { 0 });
    }
    var len = digits0 - removed;
    if c >= float_text::POW10_U64[len] { len++; }
    put_digits(out[..len], c);
    return len, len + removed - k;
}

// The decimal digits of v, below 10^out.len, zero-padded to fill out: eight
// at a time from the right, and what is left above them two at a time.
inline fn put_digits(out: u8[:], v: u64) {
    var x = v;
    var i = out.len;
    while i >= 8 {
        put8(out, i - 8, x % 100000000);
        x /= 100000000;
        i -= 8;
    }
    while i >= 2 {
        let r = x % 100;
        x /= 100;
        i -= 2;
        out[i] = DIGIT_PAIRS[2 * r];
        out[i + 1] = DIGIT_PAIRS[2 * r + 1];
    }
    if i == 1 { out[0] = ('0' + x) as! u8; }
}

// v, below 10^8, as 8 digits into out[at..at + 8], all at once: four lanes
// of the 64-bit word split their numbers by multiplying by reciprocals until
// each byte holds one digit.
fn put8(out: u8[:], at: i64, v: u64) {
    let merged = (v / 10000) | ((v % 10000) << 32);
    let top = ((merged * 10486) >> 20) & 0x0000007F0000007F;
    let hundreds = ((merged - 100 * top) << 16) + top;
    var tens = ((hundreds * 103) >> 10) & 0x000F000F000F000F;
    tens += (hundreds - 10 * tens) << 8;
    let w = tens | 0x3030303030303030;
    let o = out[at..at + 8];
    for j in 8 { o[j] = (w >> (8 * j)) as! u8; }
}

// --- scanning -------------------------------------------------------------------

// JSON's white space, all of it at or below ' ', so that the first compare
// settles nearly every byte.
fn is_space(c: u8) -> bool { c <= ' ' && (c == ' ' || c == '\n' || c == '\r' || c == '\t') }

// The first position at or after pos that is not white space.
fn skip_space(text: u8[:], pos: i64) -> i64 {
    var p = pos;
    while p < text.len && is_space(text[p]) { p++; }
    return p;
}

// The string whose opening quote is at text[at - 1], from its first byte:
// the position of its closing quote, and whether it holds an escape. -1 if
// it is unterminated or holds a raw control character. Escapes are passed
// over unchecked: decode_string or valid_escapes checks them.
fn scan_string(text: u8[:], at: i64) -> i64, bool {
    guard at >= 0 else { return -1, false; }
    var pos = at;
    var escaped = false;
    while pos < text.len {
        let c = text[pos];
        if c == '"' { return pos, escaped; }
        if c == '\\' { escaped = true; pos += 2; continue; }
        if c < 0x20 { return -1, false; }
        pos++;
    }
    return -1, false;
}

// Whether every escape in text[from..to], the body of a string, is one of
// \" \\ \/ \b \f \n \r \t \uXXXX.
fn valid_escapes(text: u8[:], from: i64, to: i64) -> bool {
    var pos = from;
    while pos < to {
        if text[pos] != '\\' { pos++; continue; }
        guard pos + 1 < to else { return false; }
        let e = text[pos + 1];
        if e == 'u' {
            guard pos + 6 <= to else { return false; }
            for i in 4 {
                if ascii::hex_value(text[pos + 2 + i]) < 0 { return false; }
            }
            pos += 6;
        } else if e == '"' || e == '\\' || e == '/' || e == 'b' || e == 'f' || e == 'n' || e == 'r' || e == 't' {
            pos += 2;
        } else {
            return false;
        }
    }
    return true;
}

// The escaped string text[at..end] decoded onto the end of out, and false
// if an escape is malformed.
fn decode_string(text: u8[:], at: i64, end: i64, out: u8[>..]&) -> const u8[:], bool {
    let first = out.len;
    var pos = at;
    while pos < end {
        let c = text[pos];
        pos++;
        if c != '\\' { out.push(c); continue; }
        let e = text[pos];
        pos++;
        if e == '"' || e == '\\' || e == '/' { out.push(e); }
        else if e == 'n' { out.push('\n'); }
        else if e == 't' { out.push('\t'); }
        else if e == 'r' { out.push('\r'); }
        else if e == 'b' { out.push(8); }
        else if e == 'f' { out.push(12); }
        else if e == 'u' {
            guard pos + 4 <= end else { return out[first..], false; }
            var cp = 0;
            for i in 4 {
                let d = ascii::hex_value(text[pos + i]);
                guard d >= 0 else { return out[first..], false; }
                cp = cp * 16 + d;
            }
            pos += 4;
            // A surrogate pair is two escapes that make one code point; a
            // lone half is kept as the three bytes of its own code.
            if cp >= 0xD800 && cp < 0xDC00 && pos + 6 <= end && text[pos] == '\\' && text[pos + 1] == 'u' {
                var lo = 0;
                for i in 4 {
                    let d = ascii::hex_value(text[pos + 2 + i]);
                    guard d >= 0 else { return out[first..], false; }
                    lo = lo * 16 + d;
                }
                if lo >= 0xDC00 && lo < 0xE000 {
                    cp = 0x10000 + ((cp - 0xD800) << 10) + (lo - 0xDC00);
                    pos += 6;
                }
            }
            push_utf8(out, cp);
        }
        else { return out[first..], false; }
    }
    return out[first..], true;
}

// A JSON number, -?(0|[1-9][0-9]*)(.[0-9]+)?([eE][+-]?[0-9]+)?, and where
// it ends; `is_int` when it has neither a fraction nor an exponent and fits
// an i64.
struct Number { value: f64, int: i64, is_int: bool, end: i64 }

// The number starting at text[at], or one with end == at when there is none.
// A number of at most 19 digits, as nearly every number is, is read here,
// rounded by std's float_text::quick; what that leaves open, and any other
// number, is read by scan_number_general.
fn scan_number(text: u8[:], at: i64) -> Number {
    guard at >= 0 else { return Number { value: 0.0, int: 0, is_int: false, end: at }; }
    var pos = at;
    var neg = false;
    if pos < text.len && text[pos] == '-' { neg = true; pos++; }
    let int_start = pos;
    var mant: u64 = 0;
    while pos < text.len && ascii::is_digit(text[pos]) {
        mant = mant * 10 + (text[pos] - '0');
        pos++;
    }
    let int_end = pos;
    let whole = mant;
    var frac_digits = 0;
    if pos < text.len && text[pos] == '.' {
        pos++;
        let frac_start = pos;
        while pos < text.len && ascii::is_digit(text[pos]) {
            mant = mant * 10 + (text[pos] - '0');
            pos++;
        }
        frac_digits = pos - frac_start;
        guard frac_digits > 0 else { return scan_number_general(text, at); }
    }
    let int_digits = int_end - int_start;
    guard int_digits > 0 && (int_digits == 1 || text[int_start] != '0')
          && int_digits + frac_digits <= 19 else {
        return scan_number_general(text, at);
    }
    var exp10 = -frac_digits;
    var end = pos;
    if pos < text.len && (text[pos] == 'e' || text[pos] == 'E') {
        let e, exp_end = scan_exponent(text, pos + 1);
        guard exp_end > pos + 1 else { return scan_number_general(text, at); }
        exp10 += e;
        end = exp_end;
    }
    var v, ok = float_text::quick(mant, exp10, whole, frac_digits, end == pos);
    guard ok else { return scan_number_general(text, at); }
    if neg { v = -v; }
    let is_int = frac_digits == 0 && end == pos && (mant <= MAX_I64 || (neg && mant == MAX_I64 + 1));
    let i = if neg { (0 - mant) as! i64 } else { mant as! i64 };
    return Number { value: v, int: i, is_int: is_int, end: end };
}

// Any number, correctly rounded by std's float_text::nearest.
fn scan_number_general(text: u8[:], at: i64) -> Number {
    let none = Number { value: 0.0, int: 0, is_int: false, end: at };
    guard at >= 0 else { return none; }
    var pos = at;
    var neg = false;
    if pos < text.len && text[pos] == '-' { neg = true; pos++; }
    let int_start = pos;
    while pos < text.len && ascii::is_digit(text[pos]) { pos++; }
    let int_digits = pos - int_start;
    guard int_digits > 0 && (int_digits == 1 || text[int_start] != '0') else { return none; }
    var frac_digits = 0;
    if pos < text.len && text[pos] == '.' {
        pos++;
        let frac_start = pos;
        while pos < text.len && ascii::is_digit(text[pos]) { pos++; }
        frac_digits = pos - frac_start;
        guard frac_digits > 0 else { return none; }
    }
    var end = pos;
    var exp10 = -frac_digits;
    if pos < text.len && (text[pos] == 'e' || text[pos] == 'E') {
        let e, exp_end = scan_exponent(text, pos + 1);
        guard exp_end > pos + 1 else { return none; }
        exp10 += e;
        end = exp_end;
    }
    let mant, taken, dropped = float_text::significant(text, int_start, pos);
    var v = float_text::nearest(text, int_start, pos, exp10, mant, taken, dropped);
    if neg { v = -v; }
    // An integer when every digit was taken and it fits: at most 2^63 - 1,
    // or 2^63 below zero, whose negation wraps to itself.
    let is_int = frac_digits == 0 && end == pos && dropped == 0
                 && (mant <= MAX_I64 || (neg && mant == MAX_I64 + 1));
    let i = if neg { (0 - mant) as! i64 } else { mant as! i64 };
    return Number { value: v, int: i, is_int: is_int, end: end };
}

let MAX_I64: u64 = 9223372036854775807;

// The exponent that follows the 'e' at text[at - 1], and where it ends;
// end == at when it has no digits.
fn scan_exponent(text: u8[:], at: i64) -> i64, i64 {
    var pos = at;
    var neg = false;
    if pos < text.len && (text[pos] == '-' || text[pos] == '+') {
        neg = text[pos] == '-';
        pos++;
    }
    let start = pos;
    var e = 0;
    while pos < text.len && ascii::is_digit(text[pos]) {
        if e < 100000 { e = e * 10 + (text[pos] - '0'); }
        pos++;
    }
    if pos == start { return 0, at; }
    if neg { e = -e; }
    return e, pos;
}

// --- the document tree ----------------------------------------------------------

// A document is one grow-only pool of fixed-size nodes in text order. A
// node is a link to its next sibling, a link to its first child (null but
// in a container), its key (empty in an array) and its value. Keys and
// strings are views of the text they were parsed from, or of the caller's
// buffer for those that held escapes, so nodes are all one size and nothing
// is copied into them. Links are 4-byte self-relative offsets.
struct Node { next: Node&<u32>?, first: Node&<u32>?, key: const u8[:], val: Value }

enum Value {
    Null,
    Bool { v: bool },
    Int { v: i64 },
    Num { v: f64 },
    Str { s: const u8[:] },
    Arr { count: i64 },
    Obj { count: i64 },
}

// A container the parser is inside of: its node, its last child so far and
// how many it has, and whether it is an object.
struct Open { node: Node?, last: Node?, count: i64, is_obj: bool }

// parse's result for text that stops being JSON at byte pos.
fn parse_error(what: const u8[:], pos: i64) -> Node?, u8[>..] { return null, str("json: ", what, " at byte ", pos); }

// Parses text into doc and returns the root, or null and a message naming
// the byte where the text stops being JSON. Strings with escapes are
// decoded onto `decoded`. The parse is one loop over the values in text
// order, with the innermost container in locals and the ones around it on
// a stack. Each node is linked from its previous sibling, or its parent,
// as it is pushed.
fn parse(text: u8[:], doc: Node[>..]&, decoded: u8[>..]&) -> Node?, u8[>..] {
    var pos = 0;
    var root:   Node? = null;
    var parent: Node? = null;       // the innermost container,
    var last:   Node? = null;       // its last child so far,
    var count = 0;                  // how many it has,
    var is_obj = false;             // and whether it is an object
    var outer: Open[>..<] = [];     // the containers around it, innermost last
    var member = false;             // whether the value at the cursor has a key

    // The string whose opening quote is at pos: a slice of the text, or of
    // `decoded` when it holds escapes.
    fn string() -> const u8[:] {
        let start = pos + 1;
        let end, escaped = scan_string(text, start);
        if end >= 0 {
            pos = end + 1;
            if !escaped { return text[start..end]; }
            let s, ok = decode_string(text, start, end, decoded);
            if ok { return s; }
        }
        return parse_error("bad string", pos) from parse;
    }

    // An object member's key and its colon.
    fn member_key() -> const u8[:] {
        pos = skip_space(text, pos);
        guard pos < text.len && text[pos] == '"' else { return parse_error("expected a key", pos) from parse; }
        let k = string();
        pos = skip_space(text, pos);
        guard pos < text.len && text[pos] == ':' else { return parse_error("expected ':'", pos) from parse; }
        pos++;
        return k;
    }

    fn word(w: u8[:]) {
        guard pos + w.len <= text.len && text[pos..pos + w.len] == w else { return parse_error("expected a value", pos) from parse; }
        pos += w.len;
    }

    // The value at pos, c its first byte; a container's count is filled in
    // when it closes.
    fn value(c: u8) -> Value {
        if c == '{' { return Value.Obj { count: 0 }; }
        if c == '[' { return Value.Arr { count: 0 }; }
        if c == '"' { return Value.Str { s: string() }; }
        if c == 't' { word("true"); return Value.Bool { v: true }; }
        if c == 'f' { word("false"); return Value.Bool { v: false }; }
        if c == 'n' { word("null"); return Value.Null; }
        let n = scan_number(text, pos);
        guard n.end > pos else {
            let what = if c == '-' || ascii::is_digit(c) { "bad number" } else { "expected a value" };
            return parse_error(what, pos) from parse;
        }
        pos = n.end;
        if n.is_int { return Value.Int { v: n.int }; }
        return Value.Num { v: n.value };
    }

    loop {
        let key = if member { member_key() } else { "" };
        pos = skip_space(text, pos);
        guard pos < text.len else { return parse_error("unexpected end of input", pos); }
        let c = text[pos];
        let val = value(c);
        let node .= doc.push(Node { next: null, first: null, key: key, val: val });
        if last { last.next .= node; } else if parent { parent.first .= node; } else { root .= node; }
        last .= node;
        count++;
        if c == '{' || c == '[' {
            outer.push(Open { node: parent, last: last, count: count, is_obj: is_obj });
            parent .= node;
            last .= null;
            count = 0;
            is_obj = c == '{';
            pos = skip_space(text, pos + 1);
            if pos >= text.len || text[pos] != (if is_obj { '}' } else { ']' }) {
                member = is_obj;
                continue;
            }
        } else {
            pos = skip_space(text, pos);
        }
        // Past a value: each close bracket completes the innermost container,
        // and a comma starts the next value.
        var more = false;
        while outer.len > 0 && pos < text.len {
            let d = text[pos];
            if d == ',' { pos++; more = true; break; }
            guard d == (if is_obj { '}' } else { ']' }) else {
                return parse_error(if is_obj { "expected ',' or '}'" } else { "expected ',' or ']'" }, pos);
            }
            pos++;
            if parent { parent.val = if is_obj { Value.Obj { count: count } } else { Value.Arr { count: count } }; }
            let o = outer[outer.len - 1];
            outer.pop();
            parent .= o.node;
            last .= o.last;
            count = o.count;
            is_obj = o.is_obj;
            pos = skip_space(text, pos);
        }
        if more { member = is_obj; continue; }
        guard outer.len == 0 else { return parse_error(if is_obj { "unclosed object" } else { "unclosed array" }, pos); }
        guard pos == text.len else { return parse_error("trailing input", pos); }
        return root, str("");
    }
}

// The first member of object n named key; null if there is none, or if n
// is no object or null itself, so that lookups chain.
fn member(n: Node?, key: u8[:]) -> Node? {
    guard n else { return null; }
    match n.val {
        Obj => {},
        _ => { return null; },
    }
    var c = n.first;
    while c {
        if c.key == key { return c; }
        c .= c.next;
    }
    return null;
}

// F(child) for each element of an array or member of an object, in order.
fn each<F>(n: Node&) {
    var c = n.first;
    while c {
        F(c);
        c .= c.next;
    }
}

// A number's value as an f64; false for anything else.
fn as_f64(n: Node?) -> f64, bool {
    guard n else { return 0.0, false; }
    match n.val {
        Num x => { return x.v, true; },
        Int i => { return i.v, true; },
        _ => { return 0.0, false; },
    }
}

// An integer's value; false for anything else, a number with a fraction or
// an exponent included.
fn as_i64(n: Node?) -> i64, bool {
    guard n else { return 0, false; }
    match n.val {
        Int i => { return i.v, true; },
        _ => { return 0, false; },
    }
}

fn as_bool(n: Node?) -> bool, bool {
    guard n else { return false, false; }
    match n.val {
        Bool b => { return b.v, true; },
        _ => { return false, false; },
    }
}

// A string's text, a view of the parsed text or of `decoded`; false for
// anything else.
fn as_string(n: Node?) -> const u8[:], bool {
    guard n else { return "", false; }
    match n.val {
        Str t => { return t.s, true; },
        _ => { return "", false; },
    }
}

// --- reading into the caller's types --------------------------------------------

// A cursor over JSON text for parsers that map it straight onto their own
// types: `each_member` and `each_element` walk a container and hand each
// member or element to a block, which reads it with `read_number`,
// `read_string` and the rest, or passes it over with `skip`. A read that
// meets malformed text records what it expected and where, and moves the
// cursor to the end of the text, so that every read after it fails too and
// every loop ends: the parser checks once, with `finish`, when it is done.
struct Reader { text: const u8[:], pos: i64 = 0, error: const u8[:] = "", error_at: i64 = 0 }

fn reader(text: const u8[:]) -> Reader { Reader { text: text, .. } }

fn fail(r: Reader&, what: const u8[:]) {
    if r.error.len == 0 {
        r.error = what;
        r.error_at = r.pos;
    }
    r.pos = r.text.len;
}

// Whether the text was read without an error and holds nothing but white
// space after what was read; records "trailing input" if it holds more.
fn finish(r: Reader&) -> bool {
    if r.error.len > 0 { return false; }
    peek(r);
    if r.pos < r.text.len { fail(r, "trailing input"); return false; }
    return true;
}

// The error as a message: "json: <what> at byte <n>".
fn message(r: Reader&) -> u8[>..] { str("json: ", r.error, " at byte ", r.error_at) }

// The next byte that is not white space, not consumed; 0 at the end.
fn peek(r: Reader&) -> u8 {
    r.pos = skip_space(r.text, r.pos);
    return if r.pos < r.text.len { r.text[r.pos] } else { 0 };
}

fn read_number(r: Reader&) -> f64 {
    let at = skip_space(r.text, r.pos);
    let n = scan_number(r.text, at);
    r.pos = at;
    if n.end == at { fail(r, "expected a number"); return 0.0; }
    r.pos = n.end;
    return n.value;
}

// A number with neither a fraction nor an exponent that fits an i64.
fn read_int(r: Reader&) -> i64 {
    let at = skip_space(r.text, r.pos);
    let n = scan_number(r.text, at);
    r.pos = at;
    if n.end == at || !n.is_int { fail(r, "expected an integer"); return 0; }
    r.pos = n.end;
    return n.int;
}

fn read_bool(r: Reader&) -> bool {
    let c = peek(r);
    if c == 't' { word(r, "true"); return r.error.len == 0; }
    if c == 'f' { word(r, "false"); return false; }
    fail(r, "expected true or false");
    return false;
}

// A string without escapes as a slice of the text; one with escapes is
// decoded onto `scratch` and returned as a slice of that.
fn read_string(r: Reader&, scratch: u8[>..]&) -> const u8[:] {
    if peek(r) != '"' { fail(r, "expected a string"); return ""; }
    let start = r.pos + 1;
    let end, escaped = scan_string(r.text, start);
    if end < 0 { fail(r, "bad string"); return ""; }
    r.pos = end + 1;
    if !escaped { return r.text[start..end]; }
    let s, ok = decode_string(r.text, start, end, scratch);
    if !ok {
        r.pos = start - 1;
        fail(r, "bad escape");
    }
    return s;
}

// F(key) for each member of the object at the cursor; F reads the value.
// Keys are handed over as written, so one with escapes in it never equals
// a plain one.
fn each_member<F>(r: Reader&) {
    if peek(r) != '{' { fail(r, "expected '{'"); return; }
    r.pos++;
    if peek(r) == '}' { r.pos++; return; }
    loop {
        if peek(r) != '"' { fail(r, "expected a key"); return; }
        let start = r.pos + 1;
        let end, escaped = scan_string(r.text, start);
        if end < 0 || (escaped && !valid_escapes(r.text, start, end)) { fail(r, "bad string"); return; }
        r.pos = end + 1;
        if peek(r) != ':' { fail(r, "expected ':'"); return; }
        r.pos++;
        F(r.text[start..end]);
        let c = peek(r);
        if c == ',' { r.pos++; continue; }
        if c == '}' { r.pos++; } else { fail(r, "expected ',' or '}'"); }
        return;
    }
}

// F() for each element of the array at the cursor; F reads the element.
fn each_element<F>(r: Reader&) {
    if peek(r) != '[' { fail(r, "expected '['"); return; }
    r.pos++;
    if peek(r) == ']' { r.pos++; return; }
    loop {
        F();
        let c = peek(r);
        if c == ',' { r.pos++; continue; }
        if c == ']' { r.pos++; } else { fail(r, "expected ',' or ']'"); }
        return;
    }
}

fn word(r: Reader&, w: u8[:]) {
    if r.pos + w.len <= r.text.len && r.text[r.pos..r.pos + w.len] == w { r.pos += w.len; }
    else { fail(r, "expected a value"); }
}

// Passes over the value at the cursor, checking that it is well formed.
// A string, the value most often passed over, is scanned here; anything
// else goes through skip_value's loop.
fn skip(r: Reader&) {
    if peek(r) != '"' { skip_value(r); return; }
    let end, escaped = scan_string(r.text, r.pos + 1);
    if end < 0 || (escaped && !valid_escapes(r.text, r.pos + 1, end)) { fail(r, "bad string"); return; }
    r.pos = end + 1;
}

// skip for any value: one loop over the values in it, with the containers
// around the cursor on a stack.
fn skip_value(r: Reader&) {
    var open: bool[>..<] = [];      // the containers around the cursor, true for an object
    var member = false;             // whether the value at the cursor has a key
    loop {
        if member {
            if peek(r) != '"' { fail(r, "expected a key"); return; }
            let end, escaped = scan_string(r.text, r.pos + 1);
            if end < 0 || (escaped && !valid_escapes(r.text, r.pos + 1, end)) { fail(r, "bad string"); return; }
            r.pos = end + 1;
            if peek(r) != ':' { fail(r, "expected ':'"); return; }
            r.pos++;
        }
        let c = peek(r);
        if c == '{' || c == '[' {
            r.pos++;
            open.push(c == '{');
            if peek(r) != (if c == '{' { '}' } else { ']' }) {
                member = c == '{';
                continue;
            }
            r.pos++;
            open.pop();
        } else if c == '"' {
            let end, escaped = scan_string(r.text, r.pos + 1);
            if end < 0 || (escaped && !valid_escapes(r.text, r.pos + 1, end)) { fail(r, "bad string"); return; }
            r.pos = end + 1;
        } else if c == 't' {
            word(r, "true");
        } else if c == 'f' {
            word(r, "false");
        } else if c == 'n' {
            word(r, "null");
        } else if c == '-' || ascii::is_digit(c) {
            read_number(r);
        } else {
            fail(r, "expected a value");
            return;
        }
        // Past a value: each close bracket ends the innermost container,
        // and a comma starts the next value.
        member = false;
        loop {
            if open.len == 0 { return; }
            let in_obj = open[open.len - 1];
            let d = peek(r);
            if d == ',' {
                r.pos++;
                member = in_obj;
                break;
            }
            if d != (if in_obj { '}' } else { ']' }) {
                fail(r, if in_obj { "expected ',' or '}'" } else { "expected ',' or ']'" });
                return;
            }
            r.pos++;
            open.pop();
        }
    }
}
