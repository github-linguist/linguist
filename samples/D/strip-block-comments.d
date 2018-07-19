import std.algorithm, std.regex;

string[2] separateComments(in string txt,
                           in string cpat0, in string cpat1) {
    int[2] plen; // to handle /*/
    int i, j; // cursors
    bool inside; // is inside comment?

    // pre-compute regex here if desired
    //auto r0 = regex(cpat0);
    //auto r1 = regex(cpat1);
    //enum rct = ctRegex!(r"\n|\r");

    bool advCursor() {
        auto mo = match(txt[i .. $], inside ? cpat1 : cpat0);
        if (mo.empty)
            return false;
        plen[inside] = max(0, plen[inside], mo.front[0].length);
        j = i + mo.pre.length; // got comment head
        if (inside)
            j += mo.front[0].length; // or comment tail

        // special adjust for \n\r
        if (!match(mo.front[0], r"\n|\r").empty)
            j--;
        return true;
    }

    string[2] result;
    while (true) {
        if (!advCursor())
            break;
        result[inside] ~= txt[i .. j]; // save slice of result

        // handle /*/ pattern
        if (inside && (j - i < plen[0] + plen[1])) {
            i = j;
            if (!advCursor())
                break;
            result[inside] ~= txt[i .. j]; // save result again
        }

        i = j; // advance cursor
        inside = !inside; // toggle search type
    }

    if (inside)
        throw new Exception("Mismatched Comment");
    result[inside] ~= txt[i .. $]; // save rest(non-comment)
    return result;
}


void main() {
    import std.stdio;

    static void showResults(in string e, in string[2] pair) {
        writeln("===Original text:\n", e);
        writeln("\n\n===Text without comments:\n", pair[0]);
        writeln("\n\n===The stripped comments:\n", pair[1]);
    }

    // First example ------------------------------
    immutable ex1 = `  /**
   * Some comments
   * longer comments here that we can parse.
   *
   * Rahoo
   */
   function subroutine() {
    a = /* inline comment */ b + c ;
   }
   /*/ <-- tricky comments */

   /**
    * Another comment.
    */
    function something() {
    }`;

    showResults(ex1, separateComments(ex1, `/\*`, `\*/`));

    // Second example ------------------------------
    writeln("\n");
    immutable ex2 = "apples, pears # and bananas
apples, pears; and bananas ";  // test for line comment

    showResults(ex2, separateComments(ex2, `#|;`, `[\n\r]|$`));
}
