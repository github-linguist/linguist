/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.unix/lib/auto/sys_file_match.p
 > Purpose:         Produce a file-name repeater from given file spec
 > Author:          John Gibson, Aug 16 1983 (see revisions)
 > Documentation:   REF *SYSUTIL
 > Related Files:
 */

compile_mode :pop11 +strict;

;;; load match codes for "sys_match_filename"
#_INCLUDE '$usepop/pop/lib/include/sys_match_filename.ph'

section;

lconstant
    SFM_STRIP_DIR           =   2:1e0,
    SFM_FOLLOW_SYMLINKS     =   2:1e1,
    ;

lvars
    filename_patt,
    stat_buf,
    strip_dir,
    follow_symlinks,
    ;


    ;;; don't make syssort copy the list
lconstant procedure sort = syssort(%false, alphabefore%);

define lconstant returnfiles(filelist, dir);
    lvars f, n, filelist, dir;
    returnunless(filelist and filelist /== []);
    if strip_dir then suspend(dir dir_>< nullstring, false, 2) endif;
    fast_for f in sort(filelist) do
        if stat_buf then
            dir dir_>< f -> n;
            suspend(sys_file_stat(n, stat_buf, follow_symlinks),
                    if strip_dir then f else n endif, 2)
        else
            unless strip_dir then dir dir_>< f -> f endunless;
            suspend(f, 1)
        endif
    endfor
enddefine;

define lconstant 4 dir dir_<> name;
    lvars s, slash = false, dir, name;
    returnif(name == []) (dir);
    consstring(#|
        explode(dir),
        unless dir = nullstring then dup() /== `/` -> slash endunless,
        if islist(name) then
            fast_for s in name do
                if slash then `/` endif;
                explode(s);
                true -> slash
            endfor
        else
            if slash then `/` endif;
            if isstring(name) then explode(name) else name endif
        endif
    |#)
enddefine;

define lconstant matchanypath(startdir, dir, path_patt);
    lvars fulldir, dirlist, d, startdir, dir, path_patt;

    define lconstant matchdirpath(path, dir_patt);
        lvars patt, path, dir_patt;
        until dir_patt == [] do
            destpair(dir_patt) -> dir_patt -> patt;
            unless patt then
                ;;; ...
                until path == [] do
                    returnif(matchdirpath(path, dir_patt)) (true);
                    back(path) -> path
                enduntil
            elseunless path /== [] and sys_match_filename(front(path), patt)
            then
                return(false)
            else
                back(path) -> path;
            endunless
        enduntil;
        path == []
    enddefine;

    startdir dir_<> dir -> fulldir;
    if matchdirpath(dir, path_patt) then
        if sys_matchin_dir(fulldir, filename_patt, 2:111) ->> dirlist then
            returnfiles((), fulldir)
        else
            return
        endif
    else
        unless sys_matchin_dir(fulldir, false, 2:001) ->> dirlist then
            return
        endunless
    endif;
    ;;; get sorted list of all directories
    fast_for d in sort(dirlist) do
        matchanypath(startdir, dir <> [^d], path_patt)
    endfor
enddefine;

define lconstant do_sfm(path_patt, filename_patt, rootdir, stat_buf,
                                                                strip_dir);
    lvars path_patt, rootdir;
    dlocal filename_patt, stat_buf, strip_dir, follow_symlinks;

    if isinteger(strip_dir) then
        strip_dir &&/=_0 SFM_FOLLOW_SYMLINKS -> follow_symlinks;
        strip_dir &&/=_0 SFM_STRIP_DIR -> strip_dir;
    else
        true -> follow_symlinks;
    endif;

    define lconstant matchindir(dir, path_patt);
        lvars d, patt, path, dirlist, dir, path_patt, code;
        if path_patt == [] then
            ;;; extract the files that match the filename part
            returnfiles(sys_matchin_dir(dir, filename_patt, 2:011), dir);
            return
        endif;
        front(path_patt) -> patt;
        unless patt then
            ;;; ...
            matchanypath(dir, [], path_patt);
        elseif listlength(patt) == 2
        and ((front(patt) ->> code) == M_STRING or code == M_CHAR) then
            ;;; fixed directory name
            dir dir_<> front(back(patt)) -> dir;
            ;;; check it exists
            if sysisdirectory(dir) then matchindir(dir, back(path_patt)) endif
        else
            ;;; pattern directory name
            ;;; get sorted list of matching directories
            back(path_patt) -> path_patt;
            returnunless(sys_matchin_dir(dir, patt, 2:001) ->> dirlist);
            fast_for d in sort(dirlist) do
                matchindir(dir dir_<> d, path_patt)
            endfor
        endunless
    enddefine;

    matchindir(rootdir, path_patt);
    termin
enddefine;


define lconstant transpatt(string);
    lvars char, m, n = 1, slen, string, list, range, isnot;
    dlvars count = 0;

    lconstant macro NEXTCHAR = [fast_subscrs(n,string) -> char, n+1 -> n ];

    define lconstant grabc();
        lvars char;
        if count == 1 then
            -> char;
            M_CHAR, char
        elseif count /== 0 then
            consstring(count) -> char;
            M_STRING, char
        endif;
        0 -> count
    enddefine;

    datalength(string) -> slen;
    [%  while n <= slen do
            NEXTCHAR;
            if char == `*` then
                ;;; match any number of chars
                grabc(), M_STAR
            elseif char == `?` then
                ;;; match any character
                grabc();
                n -> m;
                while n <= slen and fast_subscrs(n, string) == `?` do
                    n+1 -> n
                endwhile;
                M_NCHARS, n-m+1
            elseif char == `#` then
                ;;; match start of version
                grabc(), M_VERS
            elseif char == `[` then
                ;;; set of alternative characters
                grabc();
                false ->> isnot -> range;
                [%  repeat
                        if n > slen then
                            mishap(0, 'sys_file_match: MISSING "]" IN PATTERN')
                        endif;
                        NEXTCHAR;
                        quitif(char == `]`);
                        if char == `^` then
                            true -> isnot; nextloop
                        elseif char == `\` and n <= slen then
                            NEXTCHAR
                        endif;
                        if n <= slen and fast_subscrs(n,string) == `-` then
                            n+1 -> n;
                            char -> range
                        elseif range then
                            [^M_CHAR_RANGE ^range ^char];
                            false -> range
                        else
                            [^M_CHAR ^char]
                        endif
                    endrepeat
                %] -> list;
                if isnot then
                    M_NOT, [^M_XOR ^list], 1
                else
                    M_XOR, list
                endif
            elseif char == `{` then
                ;;; set of alternative (comma-separated) patterns
                lvars depth = 0;
                grabc();
                [%  repeat
                        if n > slen then
                            mishap(0, 'sys_file_match: MISSING "}" IN PATTERN')
                        endif;
                        NEXTCHAR;
                        if (char == `,` or char == `}`) and depth == 0 then
                            transpatt(consstring(count));
                            0 -> count;
                            quitif(char == `}`)
                        else
                            if char == `}` then
                                depth-1 -> depth
                            elseif char == `{` then
                                depth+1 -> depth
                            elseif char == `\` and n <= slen then
                                char, count+1 -> count;
                                NEXTCHAR
                            endif;
                            char, count+1 -> count
                        endif
                    endrepeat
                %] -> list;
                M_OR, list
            else
                ;;; match normal char
                if char == `\` and n <= slen then NEXTCHAR endif;
                char;
                count fi_+ 1 -> count;
            endif
        endwhile,
        grabc()
    %]
enddefine;

define sys_file_match(fspec, dspec, statbuf, stripdir);
    lvars fspec, dspec, statbuf, stripdir;

    define lconstant parsefilespec(string) -> (rootdir, path, filename, vers);
        lvars n, m, s, rootdir, slen, string, path, filename, vers;
        sysfileok(string) -> string;
        datalength(string) -> slen;
        1 -> n;
        ;;; root directory
        if slen /== 0 and fast_subscrs(1, string) == `/` then
            2 -> n;
            '/'
        else
            nullstring
        endif -> rootdir;
        ;;; rest of pathname
        [%  while n <= slen and (locchar(`/`, n, string) ->> m) do
                substring(n, m-n, string) -> s;
                if s = '...' then
                    false
                elseif s /= nullstring then
                    transpatt(s)
                endif;
                m+1 -> n
            endwhile
        %] -> path;
        ;;; filename
        transpatt(substring(n, slen-n+1, string)) ->> filename -> s;
        [] -> vers;
        lvars last = false;
        until s == [] do
            if front(s) == M_VERS then
                s -> vers;
                [] -> if last then back(last) else filename endif;
                quitloop
            else
                s -> last;
                back(s) -> s
            endif
        enduntil
    enddefine;

    lvars
        (froot, fpath, ffile, fvers) = parsefilespec(fspec),
        (droot, dpath, dfile, dvers) =  if dspec then
                                            parsefilespec(dspec)
                                        else
                                            nullstring, [], [], []
                                        endif;

    if froot = nullstring and fpath == [] then
        droot -> froot, dpath -> fpath
    endif;
    if ffile == [] then dfile -> ffile endif;
    if fvers == [] then dvers -> fvers endif;
    if ffile == [] then [^M_STAR] -> ffile endif;

    runproc(%0, consproc(fpath, ffile nc_<> fvers, froot, statbuf, stripdir,
                                                5, do_sfm)%)
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep  5 1995
        Fixed stupid bug in sys_file_match introduced by last change
--- John Gibson, Aug 29 1995
        Added {...} pattern matching (i.e. alternative patterns)
--- John Williams, Jul  5 1993
        Can now pass "follow symlinks" flag through to call of sys_file_stat
--- John Gibson, Nov 13 1992
        Made matchindir uses sysisdirectory instead of sys_file_stat
        (which was using a non-writeable vector)
--- Robert John Duncan, Aug 29 1990
        Changed -n*ote- to -weak-.
--- John Gibson, May 29 1990
        Corrected bugs with matching [a-z] etc, including change to operation
        of M_NOT in patterns.
--- John Gibson, Jul  1 1989
        Added +strict etc
--- John Gibson, Jun  5 1989
        #_INCLUDEd sys_match_filename.ph instead of compiling
--- John Gibson, Jul 16 1987
        Made default filespec to sys_file_match do something, also generally
        cleaned up.
--- John Williams, Feb  5 1986
        transferred match codes to
            $usepop/pop/lib/include/sys_match_filename.ph
 */
