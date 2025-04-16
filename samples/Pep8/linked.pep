; Linked list of integers API
;
; Contains the basis of the structure and a
; variety of available functions to call on it.
;
; Calling conventions:
;
;   - When the number of arguments is <= 2, the fastcall convention will be used:
;   Arguments will be passed by registers, no assumption is made concerning the
;   state of the registers during execution, they will need to be saved.
;
;   - When the number of arguments exceeds 2, the cdecl convention will be used:
;   Arguments will be passed on the stack, no assumption is made concerning the
;   state of the registers during execution, they will need to be saved.

; Simple test program, do no include when using the library
main:       SUBSP   4, i
            DECI    mnelmt, s
            CALL    newlst
            LDX     mnlst, s
            CALL    lstgetst
            LDX     mnlst, s
            CALL    lstsetst
            LDX     mnlst, s
            CALL    lstgetst
            LDX     mnlst, s
            CALL    shftest
            LDX     mnlst, s
            CALL    ushftest
            LDX     mnlst, s
            CALL    shftest
            ADDSP   4, i
            STOP
; Pointer to the list
mnlst:      .EQUATE 0
; Element read
mnelmt:     .EQUATE 2

; TESTS

; Simple test for the get operation
; Gets the first element of the list and prints it
;
; REQUIRES: Non-empty list
;
; Parameters:
;   - X: Pointer to the list
;
; Returns:
;   void
lstgetst:   SUBSP   2, i
            LDA     0, i
            CALL    lstget
            STA     0, s
            DECO    0, s
            CHARO   '\n', i
            ADDSP   2, i
            RET0

; Test for the set operation
; Sets the first element of the list to a given value
; The value is read from stdin
;
; REQUIRES: Non-empty list
;
; Parameters:
;   - X: Pointer to the list
;
; Returns:
;   void
lstsetst:   SUBSP   6, i
            STX     0, s
            DECI    4, s
            LDA     0, i
            STA     2, s
            CALL    lstset
            ADDSP   6, i
            RET0

; Tests shift operation on a list
; Gets the last element of the list and prints it
;
; REQUIRES: Non-empty list
;
; Parameters:
;   - X: Pointer to the list
;
; Returns:
;   void
shftest:    SUBSP   2, i
            CALL    lstshft
            STA     0, s
            DECO    0, s
            CHARO   '\n', i
            ADDSP   2, i
            RET0

; Tests unshift operation on a list
; Unshifts a new element read from keyboard
;
; Parameters:
;   - X: Pointer to the list
;
; Returns:
;   void
ushftest:   SUBSP   2, i
            DECI    0, s
            LDA     0, s
            CALL    lstunshf
            ADDSP   2, i
            RET0

; LIBRARY

; Creates a new list with `element` as head
;
; Parameters:
;   SP + 4: Element
;
; Returns:
;   SP + 2: Pointer to the list
newlst:     LDA     lstlen, i
            CALL    new
            STX     2, s
            CALL    newnode
            SUBSP   2, i
            STX     0, s
            LDX     nodeelmt, i
            LDA     6, s
            STA     0, sxf
            LDA     0, s
            LDX     lsthead, i
            STA     4, sxf
            ADDSP   2, i
            RET0

; Gets a node at specified index in a list
;
; Parameters:
;   - A: Index
;   - X: Pointer to the list
;
; Returns:
;   - A: Error code (0 if no error was produced)
;   - X: Pointer to the node
;
; Errors:
;   -1: Index < 0
;   -2: Index >= list.length
nodeat:     SUBSP   10, i
            STA     ndaind, s
            STX     ndalst, s
            LDX     lsthead, i
            LDA     ndalst, sxf
            STA     ndanode, s
            LDA     ndaind, s
            CPA     0, i
            LDA     0, i
            STA     ndacurri, s
            BRGE    ndagez
            LDA     -1, i
            ADDSP   10, i
            RET0
ndagez:     LDX     ndalst, s
            CALL    listlen
            STA     ndalstln, s
            LDA     ndaind, s
            CPA     ndalstln, s
            BRLT    ndalp
            LDA     -2, i
            ADDSP   10, i
            RET0
ndalp:      LDA     ndacurri, s
            CPA     ndaind, s
            BREQ    ndaout
            LDX     nodenxt, i
            LDA     ndanode, sxf
            STA     ndanode, s
            LDA     ndacurri, s
            ADDA    1, i
            STA     ndacurri, s
            BR      ndalp
ndaout:     LDX     ndanode, s
            LDA     0, i
            ADDSP   10, i
            RET0
ndaind:     .EQUATE 0
ndanode:    .EQUATE 2
ndalst:     .EQUATE 4
ndalstln:   .EQUATE 6
ndacurri:   .EQUATE 8

; Length of the list passed as a parameter
;
; Parameters:
;   - X: List
;
; Returns:
;   - A: Length
listlen:    SUBSP   4, i
            STX     lenode, s
            LDX     lenode, sf
            STX     lenode, s
            LDA     0, i
            STA     lencpt, s
llenlp:     LDA     lenode, s
            CPA     0, i
            BREQ    lenout
            LDA     lencpt, s
            ADDA    1, i
            STA     lencpt, s
            LDX     nodenxt, i
            LDA     lenode, sxf
            STA     lenode, s
            BR      llenlp
lenout:     LDA     lencpt, s
            ADDSP   4, i
            RET0
lenode:     .EQUATE 0
lencpt:     .EQUATE 2

; Gets an element in a list at a specified index
;
; Parameters:
;   - A: Index
;   - X: Address of the list
;
; Returns:
;   - A: Element value
;
; Error:
;   If out of bounds, prints an error message and stops the program
lstget:     SUBSP   2, i
            STA     0, s
            CALL    nodeat
            CPA     0, i
            BRNE    getoob
            LDA     0, x
            ADDSP   2, i
            RET0
; Out of bounds
getoob:     STRO    getstrob, d
            DECO    0, s
            CHARO   '\n', i
            STOP
; String for out of bounds error
getstrob:   .ASCII  "Invalid index on get, index = \x00"

; Sets an element in a list at a specified index to a new value
;
; Parameters:
;   - SP + 2: Pointer to the list
;   - SP + 4: Index
;   - SP + 6: Element
;
; Returns:
;   - A: 0 if all went well, an error code otherwise (analogous to the error codes in nodeat)
lstset:     CHARO   '\n', i
            DECO    lstsetlp, s
            CHARO   ' ', i
            DECO    lstsetin, s
            CHARO   ' ', i
            DECO    lstsetel, s
            CHARO   '\n', i
            SUBSP   2, i
            LDX     lstsetlp, s
            LDA     lstsetin, s
            CALL    nodeat
            CPA     0, i
            BRNE    lstsetrt
            STX     lstsetnp, s
            LDA     lstsetel, s
            LDX     nodeelmt, i
            STA     lstsetnp, sxf
            LDA     0, i
lstsetrt:   ADDSP   2, i
            RET0
; Pointer to the list
lstsetlp:   .EQUATE 4
; Element to set the value at
lstsetel:   .EQUATE 8
; Index of the node
lstsetin:   .EQUATE 6
; Pointer to the node
lstsetnp:   .EQUATE 0

; Removes the first element of the list in parameter and returns its value
;
; REQUIRES: Non-empty list
;
; Parameters:
;   ⁻ X: Pointer to the list
;
; Returns :
;   - A: Element removed
lstshft:    SUBSP   8, i
            STX     lshflp, s
            LDX     lsthead, i
            LDA     lshflp, sxf
            CPA     0, i
            BREQ    shfterr
            STA     lshfohd, s
            LDX     nodenxt, i
            LDA     lshfohd, sxf
            STA     lshfnhd, s
            LDX     lsthead, i
            STA     lshflp, sxf
            LDX     nodeelmt, i
            LDA     lshfohd, sxf
            ADDSP   8, i
            RET0
shfterr:    STRO    shfterrm, d
            STOP
; Pointer to the list
lshflp:     .EQUATE 0
; Pointer to the old head
lshfohd:    .EQUATE 2
; Old head's element
lshfhdel:   .EQUATE 4
; Pointer to the new head
lshfnhd:    .EQUATE 6
; Error message on shift
shfterrm:   .ASCII "Cannot do shift on empty list.\n\x00"

; Inserts a new element at the beginning of a list
;
; Parameters:
;   - X: Pointer to the list
;   - A: Element to add to the list
;
; Returns:
;   - A: Error code, 0 if all right, a code otherwise
lstunshf:   SUBSP   8, i
            STA     lunshelm, s
            STX     lunslp, s
            CALL    newnode
            STX     lunsnhd, s
            LDX     lsthead, i
            LDA     lunslp, sxf
            STA     lunsohd, s
            LDX     nodenxt, i
            LDA     lunsohd, s
            STA     lunsnhd, sxf
            LDA     lunshelm, s
            LDX     nodeelmt, i
            STA     lunsohd, sxf
            LDX     lsthead, i
            LDA     lunsnhd, s
            STA     lunslp, sxf
            ADDSP   8, i
            RET0
; Pointer to the list
lunslp:     .EQUATE 0
; Pointer to the old head
lunsohd:    .EQUATE 2
; Pointer to the new head
lunsnhd:    .EQUATE 4
; Element to add
lunshelm:   .EQUATE 6

; Finds whether or not an element is present in a list
;
; Parameters:
;   - X: Pointer to the list
;   - A: Element to be found
;
; Returns:
;   - A: 0 if element was not found, 1 if it was
lstfnd:     SUBSP   6, i
            STX     lstfndlp, s
            STA     lstfndel, s
            LDX     lsthead, i
            LDA     lstfndlp, sxf
            STA     lstfndnd, s
fndloop:    CPA     0, i
            BREQ    notfnd
            LDX     nodeelmt, i
            LDA     lstfndnd, sxf
            CPA     lstfndel, s
            BREQ    found
            LDX     nodenxt, i
            LDA     lstfndnd, sxf
            STA     lstfndnd, s
            BR      fndloop
notfnd:     LDA     0, i
            ADDSP   6, i
            RET0
found:      LDA     1, i
            ADDSP   6, i
            RET0
; Pointer to the list
lstfndlp:   .EQUATE 0
; Element to search
lstfndel:   .EQUATE 2
; Current node
lstfndnd:   .EQUATE 4

; Pushes a new element at the end of the list
;
; Parameters:
;   - X: Pointer to the list
;   - A: Element to push
;
; Returns:
;   - A: 0 if all went well, an error code otherwise
lstpsh:     SUBSP   8, i
            STX     lpshlp, s
            STA     lpshel, s
            CALL    newnode
            STX     lpshnd, s
            LDX     lpshlp, s
            CALL    listlen
            CPA     0, i
            BREQ    lpshshft
            SUBA    1, i
            LDX     lpshlp, s
            CALL    nodeat
            STX     lpshlnd, s
            LDX     nodenxt, i
            LDA     lpshnd, s
            STA     lpshlnd, sxf
            LDA     lpshel, s
            LDX     nodeelmt, i
            STA     lpshnd, sxf
            ADDSP   8, i
            RET0
lpshshft:   LDX     lpshlp, s
            LDA     lpshel, s
            CALL    lstunshf
            ADDSP   8, i
            RET0
; Pointer to the list     
lpshlp:     .EQUATE 0
; Element to add to the list
lpshel:     .EQUATE 2
; Node to add to the list
lpshnd:     .EQUATE 4
; Node to append
lpshlnd:    .EQUATE 6

; Pops the last element of a list
;
; Parameters:
;   - X: Pointer to the list
;
; Returns:
;   - A: Element removed from the list
lstpop:     SUBSP   6, i
            STX     lpoplp, s
            CALL    listlen
            CPA     0, i
            BRNE    poperrem
            CPA     1, i
            BREQ    popshft
            SUBA    2, i
            LDX     lpoplp, s
            CALL    nodeat
            STX     lpopndpr, s
            LDX     nodenxt, i
            LDA     lpopndpr, sxf
            LDA     0, i
            LDX     nodenxt, i
            STA     lpopndpr, sxf
            STA     lpoplnd, s
            LDX     nodeelmt, i
            LDA     lpoplnd, s
            ADDSP   6, i
            RET0
poperrem:   STRO    poperrsm, d
            STOP
popshft:    LDX     lpoplp, s
            CALL    lstshft
            ADDSP   6, i
            RET0
; Pointer to the list
lpoplp:     .EQUATE 0
; Node to remove
lpoplnd:    .EQUATE 2
;New last node
lpopndpr:   .EQUATE 4
; Message to print when popping an empty list
poperrsm:    .ASCII "Error: cannot pop an empty list.\n\x00"

; Inserts an element in a list at a given position
;
; REQUIRES: Non-empty list
;
; Parameters:
;   - SP + 2: Pointer to the list
;   - SP + 4: Index to insert at
;   - SP + 6: Element to add
;
; Returns:
;   - A: Error code: 0 if all went well, -1 if index < 0, -2 if index > list.length
lstinsat:   SUBSP   6, i
            LDA     lstinsid, s
            CPA     0, i
            BRLT    lstinslz
            BREQ    lstinush
            LDX     lstinslp, s
            CALL    listlen
            CPA     lstinsel, s
            BRLT    lstinsgl
            BREQ    lstinpsh
            LDX     lstinslp, s
            LDA     lstinsel, s
            SUBA    1, i
            CALL    nodeat
            STX     lstinsnd, s
            LDX     nodenxt, i
            LDA     lstinsnd, sxf
            STA     lstinscx, s
            CALL    newnode
            STX     lstinscn, s
            LDX     nodeelmt, i
            LDA     lstinsel, s
            STA     lstinscn, sxf
            LDX     nodenxt, i
            LDA     lstinscx, s
            STA     lstinscn, sxf
            LDA     lstinscn, s
            LDX     nodenxt, i
            STA     lstinsnd, sxf
            ADDSP   6, i
            RET0
lstinush:   LDX     lstinslp, s
            LDA     lstinsel, s
            CALL    lstunshf
            ADDSP   6, i
            RET0
lstinpsh:   LDX     lstinslp, s
            LDA     lstinsel, s
            CALL    lstpsh
            ADDSP   6, i
            RET0
; Insert with index < 0
lstinslz:   LDA     -1, i
            ADDSP   6, i
            RET0
; Insert with index > list.length
lstinsgl:   LDA     -2, i
            ADDSP   6, i
            RET0
; List pointer
lstinslp:   .EQUATE 8
; Index of the newly created node
lstinsid:   .EQUATE 10
; Element to add
lstinsel:   .EQUATE 12
; Node to change the pointer to the next
lstinsnd:   .EQUATE 0
; Node to insert
lstinscn:   .EQUATE 2
; Pointer to the node after the created one (might be null)
lstinscx:   .EQUATE 4

; Removes a node at a given index in a list,
; returns the element previously contained
;
; Parameters:
;   - X: Pointer to the list
;   - A: Index of the element
;
; Returns:
;   - A: Element removed
;
; Error:
;   In case of error, the program aborts with an error message
lstremat:   SUBSP   8, i
            STX     lremlp, s
            STA     lremid, s
            CPA     0, i
            BRLT    lstremob
            BREQ    lstremz
            CALL    listlen
            CPA     lremid, s
            BRGE    lstremob
            SUBA    1, i
            CPA     lremid, s
            BREQ    lrempop
            LDA     lremid, s
            LDX     lremlp, s
            CALL    nodeat
            STX     lremnd, s
            LDA     lremid, s
            SUBA    1, i
            LDX     lremlp, s
            CALL    nodeat
            STX     lrempnd, s
            LDX     nodenxt, i
            LDA     lremnd, sxf
            STA     lrempnd, sxf
            LDX     nodeelmt, i
            LDA     lremnd, sxf
            ADDSP   8, i
            RET0
lstremz:    LDX     lremlp, s
            CALL    lstshft
            ADDSP   8, i
            RET0
lrempop:    LDX     lremlp, s
            CALL    lstpop
            ADDSP   8, i
            RET0
lstremob:   STRO    lremobst, d
            DECO    lremid, s
            CHARO   '\n', i
            STOP
; Pointer to the list
lremlp:     .EQUATE 0
; Index to remove an element at
lremid:     .EQUATE 2
; Pointer to the node before the removed element
lrempnd:    .EQUATE 4
; Pointer to the node to remove
lremnd:     .EQUATE 6
; Error out of bounds string for remove_at
lremobst:   .ASCII "Error: Out of bounds in remove_at, index = \x00"

; Creates a new node from scratch
; Sets its content to 0/NULL
;
; Parameters:
;   void
;
; Return:
;   - X: Address of the node
newnode:    LDA     nodeln, i
            SUBSP   2, i
            CALL    new
            STX     0, s
            LDA     0, i
            LDX     nodenxt, i
            STA     0, sxf
            LDX     nodeelmt, i
            STA     0, sxf
            LDX     0, s
            ADDSP   2, i
            RET0

; Allocates a new structure in the heap
;
; Parameters:
;   - A: Length of the structure to allocate (bytes)
;
; Returns:
;   - X: Address of the allocated structure
new:        ADDA    hpptr, d
            LDX     hpptr, d
            STA     hpptr, d
            RET0

; Node in a linked list
;
; Contains two fields:
;   - Element: Offset 0
;   - Next: Offset 2
;
nodeln:     .EQUATE 4
nodeelmt:   .EQUATE 0
nodenxt:    .EQUATE 2

; Linked list capsule
;
; Contains one field:
;   - Head: Offset 0
;
lstlen:     .EQUATE 2
lsthead:    .EQUATE 0

; Pointer to the next available byte on the heap
hpptr:      .ADDRSS heap
; Start of the heap
heap:       .BLOCK 1
    .END
