(malloc 1000 'raw)             ; raw allocation, bypass the GC, requires free()-ing
(malloc 1000 'uncollectable)   ; no GC, for use with other GCs that Racket can be configured with
(malloc 1000 'atomic)          ; a block of memory without internal pointers
(malloc 1000 'nonatomic)       ; a block of pointers
(malloc 1000 'eternal)         ; uncollectable & atomic, similar to raw malloc but no freeing
(malloc 1000 'stubborn)        ; can be declared immutable when mutation is done
(malloc 1000 'interior)        ; allocate an immovable block with possible pointers into it
(malloc 1000 'atomic-interior) ; same for atomic chunks
(malloc-immobile-cell v)       ; allocates a single cell that the GC will not move
