/* --- CSV / TSV reading and writing --------------------------------------
 > File:            pop/lib/lib/csv.p
 > Purpose:         RFC 4180 CSV parse/generate (any single-char separator)
 > Author:          D.Kordsmeier (@truedat101) and Claude (@claude), Aug 2026
 > Documentation:   HELP * CSV
 > Related Files:   tools/tests/test_csv.p, LIB * FILEUTILS
 >
 > Rows are vectors of field strings.  Quoting follows RFC 4180: fields
 > containing the separator, quotes or newlines are double-quoted, with
 > embedded quotes doubled; quoted fields may span lines.
 */
compile_mode :pop11 +strict;

uses fileutils;

section $-csv => csv_parse csv_generate csv_read csv_write;

;;; parse CSV text into a list of row vectors; sep is a character
;;; (e.g. `,` or `\t`)
define csv_parse(s, sep) -> rows;
    lvars i = 1, len = length(s), c, nf, nr = 0, nchars, inq;
    unless isstring(s) and isinteger(sep) then
        mishap(s, sep, 2, 'csv_parse: string and separator character needed')
    endunless;
    [% while i <= len do
        ;;; one row
        0 -> nf;
        repeat
            ;;; one field
            0 -> nchars;
            if i <= len and subscrs(i, s) == `"` then
                ;;; quoted field
                i + 1 -> i;
                true -> inq;
                while inq do
                    if i > len then
                        mishap(nchars, 1, 'csv_parse: unterminated quoted field')
                    endif;
                    subscrs(i, s) -> c;
                    i + 1 -> i;
                    if c == `"` then
                        if i <= len and subscrs(i, s) == `"` then
                            `"`; nchars + 1 -> nchars;
                            i + 1 -> i;
                        else
                            false -> inq;
                        endif;
                    else
                        c; nchars + 1 -> nchars;
                    endif;
                endwhile;
            else
                ;;; bare field: up to sep or line end
                while i <= len do
                    subscrs(i, s) -> c;
                    quitif(c == sep or c == `\n` or c == `\r`);
                    c; nchars + 1 -> nchars;
                    i + 1 -> i;
                endwhile;
            endif;
            consstring(nchars);
            nf + 1 -> nf;
            ;;; after a field: separator continues the row, EOL/EOF ends it
            if i <= len and subscrs(i, s) == sep then
                i + 1 -> i;
            else
                quitloop;
            endif;
        endrepeat;
        consvector(nf);
        nr + 1 -> nr;
        ;;; consume one line ending (\r\n, \n or \r)
        if i <= len and subscrs(i, s) == `\r` then i + 1 -> i endif;
        if i <= len and subscrs(i, s) == `\n` then i + 1 -> i endif;
    endwhile %] -> rows;
enddefine;

define lconstant needs_quote(f, sep);
    lvars i, c;
    for i from 1 to length(f) do
        subscrs(i, f) -> c;
        if c == sep or c == `"` or c == `\n` or c == `\r` then
            return(true)
        endif;
    endfor;
    false
enddefine;

;;; generate CSV text (rows end with \n) from a list of vectors or lists
define csv_generate(rows, sep) -> s;
    lvars row, f, i, c, first, n = 0;
    unless isinteger(sep) then
        mishap(sep, 1, 'csv_generate: separator character needed')
    endunless;
    for row in rows do
        true -> first;
        if isvector(row) then datalist(row) -> row endif;
        for f in row do
            unless first then sep; n + 1 -> n endunless;
            false -> first;
            unless isstring(f) then f sys_>< nullstring -> f endunless;
            if needs_quote(f, sep) then
                `"`; n + 1 -> n;
                for i from 1 to length(f) do
                    subscrs(i, f) -> c;
                    if c == `"` then `"`; n + 1 -> n endif;
                    c; n + 1 -> n;
                endfor;
                `"`; n + 1 -> n;
            else
                appdata(f, identfn);
                n + length(f) -> n;
            endif;
        endfor;
        `\n`; n + 1 -> n;
    endfor;
    consstring(n) -> s;
enddefine;

define csv_read(file, sep) -> rows;
    csv_parse(file_to_string(file), sep) -> rows;
enddefine;

define csv_write(rows, file, sep);
    string_to_file(csv_generate(rows, sep), file);
enddefine;

endsection;
