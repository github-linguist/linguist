' Sponge4: a sponge construction based on RC4
' Ref: https://nullprogram.com/blog/2020/11/17/
' This is free and unencumbered software released into the public domain.

TYPE sponge4
    i AS INTEGER
    j AS INTEGER
    k AS INTEGER
    s(0 TO 255) AS INTEGER
END TYPE

DECLARE SUB init (r AS sponge4)
DECLARE SUB absorb (r AS sponge4, b AS INTEGER)
DECLARE SUB absorbstop (r AS sponge4)
DECLARE SUB absorbstr (r AS sponge4, x AS STRING)

DECLARE FUNCTION squeeze% (r AS sponge4)
DECLARE FUNCTION squeeze24& (r AS sponge4)
DECLARE FUNCTION squeezen% (r AS sponge4, n AS INTEGER)

CONST ntickets = 208
CONST nresults = 12

DIM tickets(0 TO ntickets - 1) AS INTEGER
FOR i = 0 TO ntickets - 1
    tickets(i) = i
NEXT

DIM sponge AS sponge4
init sponge
absorbstr sponge, DATE$
absorbstr sponge, MKS$(TIMER)
absorbstr sponge, MKI$(ntickets)

CLS
PRINT "Press Esc to finish, any other key for entropy..."
t = TIMER
DO
    c& = c& + 1
    LOCATE 2, 1
    PRINT "cycles ="; c&; "; keys ="; k%

    FOR i% = ntickets - 1 TO 1 STEP -1
        j% = squeezen%(sponge, i% + 1)
        SWAP tickets(i%), tickets(j%)
    NEXT

    k$ = INKEY$
    IF k$ = CHR$(27) THEN
        EXIT DO
    ELSEIF k$ <> "" THEN
        k% = k% + 1
        absorbstr sponge, k$
    END IF
    absorbstr sponge, MKS$(TIMER)
LOOP

FOR i% = 1 TO nresults
    PRINT tickets(i%)
NEXT

SUB absorb (r AS sponge4, b AS INTEGER)
    r.j = (r.j + r.s(r.i) + b) MOD 256
    SWAP r.s(r.i), r.s(r.j)
    r.i = (r.i + 1) MOD 256
    r.k = (r.k + 1) MOD 256
END SUB

SUB absorbstop (r AS sponge4)
    r.j = (r.j + 1) MOD 256
END SUB

SUB absorbstr (r AS sponge4, x AS STRING)
    FOR i% = 1 TO LEN(x)
        absorb r, ASC(MID$(x, i%))
    NEXT
END SUB

SUB init (r AS sponge4)
    r.i = 0
    r.j = 0
    r.k = 0
    FOR i% = 0 TO 255
        r.s(i%) = i%
    NEXT
END SUB

FUNCTION squeeze% (r AS sponge4)
    IF r.k > 0 THEN
        absorbstop r
        DO WHILE r.k > 0
            absorb r, r.k
        LOOP
    END IF

    r.j = (r.j + r.i) MOD 256
    r.i = (r.i + 1) MOD 256
    SWAP r.s(r.i), r.s(r.j)
    squeeze% = r.s((r.s(r.i) + r.s(r.j)) MOD 256)
END FUNCTION

FUNCTION squeeze24& (r AS sponge4)
    b0& = squeeze%(r)
    b1& = squeeze%(r)
    b2& = squeeze%(r)
    squeeze24& = b2& * &H10000 + b1& * &H100 + b0&
END FUNCTION

FUNCTION squeezen% (r AS sponge4, n AS INTEGER)
    DO
       x& = squeeze24&(r) - &H1000000 MOD n
    LOOP WHILE x& < 0
    squeezen% = x& MOD n
END FUNCTION

