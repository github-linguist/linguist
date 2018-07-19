import std.stdio, std.array, std.algorithm;

string[] multiSplit(in string s, in string[] divisors)
pure /*nothrow*/ {
    string[] result;
    auto rest = s.idup; // Not nothrow.

    while (true) {
	    bool done = true;
        string delim;
        {
            string best;
            foreach (div; divisors) {
                const maybe = rest.find(div);
                if (maybe.length > best.length) {
                    best = maybe;
                    delim = div;
                    done = false;
                }
            }
        }
	    result.length++;
	    if (done) {
            result.back = rest.idup;
		    return result;
	    } else {
            const t = rest.findSplit(delim);
		    result.back = t[0].idup;
		    rest = t[2];
	    }
    }
}

void main() {
    "a!===b=!=c"
    .multiSplit(["==", "!=", "="])
    .join(" {} ")
    .writeln;
}
