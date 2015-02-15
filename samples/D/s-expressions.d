import std.stdio, std.conv, std.algorithm, std.variant, std.uni,
       std.functional, std.string;

alias Sexp = Variant;

struct Symbol {
    private string name;
    string toString() @safe const pure nothrow { return name; }
}

Sexp parseSexp(string txt) @safe pure /*nothrow*/ {
    static bool isIdentChar(in char c) @safe pure nothrow {
        return c.isAlpha || "0123456789!@#-".representation.canFind(c);
    }

    size_t pos = 0;

    Sexp _parse() /*nothrow*/ {
        auto i = pos + 1;
        scope (exit)
            pos = i;
        if (txt[pos] == '"') {
            while (txt[i] != '"' && i < txt.length)
                i++;
            i++;
            return Sexp(txt[pos + 1 .. i - 1]);
        } else if (txt[pos].isNumber) {
            while (txt[i].isNumber && i < txt.length)
                i++;
            if (txt[i] == '.') {
                i++;
                while (txt[i].isNumber && i < txt.length)
                    i++;
                auto aux = txt[pos .. i]; //
                return aux.parse!double.Sexp;
            }
            auto aux = txt[pos .. i]; //
            return aux.parse!ulong.Sexp;
        } else if (isIdentChar(txt[pos])) {
            while (isIdentChar(txt[i]) && i < txt.length)
                i++;
            return Sexp(Symbol(txt[pos .. i]));
        } else if (txt[pos] == '(') {
            Sexp[] lst;
            while (txt[i] != ')') {
                while (txt[i].isWhite)
                    i++;
                pos = i;
                lst ~= _parse;
                i = pos;
                while (txt[i].isWhite)
                    i++;
            }
            i = pos + 1;
            return Sexp(lst);
        }
        return Sexp(null);
    }

    txt = txt.find!(not!isWhite);
    return _parse;
}

void writeSexp(Sexp expr) {
    if (expr.type == typeid(string)) {
        write('"', expr, '"');
    } else if (expr.type == typeid(Sexp[])) {
        '('.write;
        auto arr = expr.get!(Sexp[]);
        foreach (immutable i, e; arr) {
            e.writeSexp;
            if (i + 1 < arr.length)
                ' '.write;
        }
        ')'.write;
    } else {
        expr.write;
    }
}

void main() {
    auto pTest = `((data "quoted data" 123 4.5)
                   (data (!@# (4.5) "(more" "data)")))`.parseSexp;
    writeln("Parsed: ", pTest);
    "Printed: ".write;
    pTest.writeSexp;
}
