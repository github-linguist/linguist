: find-index ( seq elt -- i )
    '[ _ = ] find drop [ "Not found" throw ] unless* ; inline

: find-last-index ( seq elt -- i )
    '[ _ = ] find-last drop [ "Not found" throw ] unless* ; inline
