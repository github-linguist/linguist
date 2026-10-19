/*  --- Copyright University of Sussex 1993.  All rights reserved. ---------
 >  File:           C.all/lib/auto/gensym.p
 >  Purpose:        Generate integer-suffixed words on basis of
 >                  a given root word.
 >  Author:         John Gibson, Nov 25 1992 (see revisions)
 >  Documentation:  REF *gensym
 >  Related Files:  LIB *GENSYM_PROPERTY
 */
compile_mode:pop11 +strict;

section;

define gensym(root);
    lvars root, n;
    check_word(root);
    gensym_property(root) -> n;
    n + 1 -> gensym_property(root);
    consword(root sys_>< n);
enddefine;
;;;
define updaterof gensym(n, root);
    lvars n, root;
    check_word(root);
    unless isinteger(n) then
        mishap(n, root, 2, 'gensym: INTEGER SUFFIX NEEDED')
    endunless;
    n -> gensym_property(root)
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep 22 1993
        Made new version using gensym_property
 */
