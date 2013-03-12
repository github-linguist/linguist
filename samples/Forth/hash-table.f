\ from SP-Forth/4 packet -- http://spf.sf.net

REQUIRE [UNDEFINED] lib/include/tools.f

REQUIRE HASH  ~pinka/lib/hash.f

[UNDEFINED] SALLOC [IF]
: SALLOC ( a u -- a1 )
  DUP ALLOCATE THROW DUP >R SWAP CMOVE R>
;                  
: CALLOC ( a u -- a1 )
  TUCK DUP 2+ ALLOCATE THROW DUP >R 1+ SWAP CMOVE R@ C! R> 
;
: ZALLOC ( az -- a1 )
  ASCIIZ> 1+ SALLOC
;                  [THEN]

MODULE: HASH-TABLES-SUPPORT

\ Формат таблицы:
\ +0    cell    Число списков
\ +4    n cells Начала списков записей

\ Формат записи
0 
CELL -- :link   \ Указатель на следующую запись / 0
CELL -- :key    \ указатель на строку - ключ
CELL -- :value  \ Указатель на значение
1    -- :free   \ 0 - число, <>0 строка
CONSTANT /rec

EXPORT
: new-hash ( n -- h )
  DUP 1+ CELLS ALLOCATE THROW 2DUP ! NIP
  \ очистит хэш-таблицу ALLOCATE
;
DEFINITIONS

: (HASH) ( akey nkey n2 -- hash )
  OVER 0= IF DROP 2DROP 0 ELSE HASH THEN ;

: lookup ( akey nkey h -- last 0 / prevrec rec) 
( ." look" s.)
  >R 2DUP R@ @ (HASH) 1+ CELLS R> +
  DUP @  0= IF NIP NIP 0 ( ." empty" s.) EXIT THEN
  DUP @
  BEGIN
    ( akey nkey prev rec)
    2>R ( akey nkey)
    2DUP R@ :key @ COUNT COMPARE 0= IF ( нашли ключ) 2DROP 2R> ( ." found" s.) EXIT THEN
    R> RDROP  ( akey nkey rec)
    DUP :link @ ?DUP 0= IF ( не нашли ключ) NIP NIP 0 ( ." notfound" s.) EXIT THEN
  AGAIN ;

: del-value ( rec -- )
  DUP :free C@ IF DUP :value @ FREE THROW THEN DROP ;

: del-rec ( rec -- link)
  DUP :key @ FREE THROW DUP del-value DUP :link @ SWAP FREE THROW ;

: (rec-in-hash) ( akey nkey h -- rec)
  -ROT 2DUP 2>R ROT lookup ?DUP IF
    NIP
    DUP del-value RDROP RDROP
  ELSE
     /rec ALLOCATE THROW ( last new)
     DUP ROT :link !
     2R> CALLOC OVER :key !
  THEN ;

EXPORT

: HASH! ( avalue nvalue akey nkey h -- )
  (rec-in-hash) TRUE OVER :free C! >R CALLOC R> :value ! ;

: HASH!Z ( zvalue akey nkey h -- )
  (rec-in-hash) TRUE OVER :free C! SWAP ZALLOC SWAP :value ! ;

: HASH!N ( value akey nkey h -- )
  (rec-in-hash) FALSE OVER :free C! :value ! ;

: HASH!R ( size akey nkey h -- adr )
  (rec-in-hash) TRUE OVER :free C! >R ALLOCATE THROW DUP R> :value ! ;

: -HASH ( akey nkey h -- )
  lookup ?DUP IF del-rec SWAP :link ! ELSE DROP THEN ;

: HASH? ( akey ukey h -- true|false )
  lookup NIP 0<> ;

: HASH@ ( akey nkey h -- avalue nvalue / 0 0) 
  lookup NIP DUP IF :value @ COUNT ELSE 0 THEN ;

: HASH@R ( akey nkey h -- a/0) 
  lookup NIP DUP IF :value @ THEN ;

: HASH@Z ( akey nkey h -- a/0) HASH@R ;

: HASH@N ( akey nkey h -- n TRUE / FALSE) 
  lookup NIP DUP IF :value @ TRUE THEN ;

: small-hash  ( -- h ) 32   new-hash ;
: large-hash  ( -- h) 256   new-hash ;
: big-hash    ( -- h) 1024  new-hash ;

: traverse-hash ( xt h -- ) \ xt ( ... rec -- )
  DUP @ CELLS OVER + CELL+ SWAP CELL+ ?DO
    I @ IF
      I @
        BEGIN ?DUP WHILE
          OVER EXECUTE
        REPEAT
    THEN
  CELL +LOOP
  DROP ;


: clear-hash ( h -- )    \ очищает хэш, не удаляя основную таблицу
  ['] del-rec OVER traverse-hash
  DUP CELL+ SWAP @ CELLS ERASE
;
: del-hash ( h -- )
  ['] del-rec OVER traverse-hash 
  FREE THROW 
;

DEFINITIONS

USER cnt
USER-VALUE do-it

: (all-hash) ( rec -- nextrec )
  >R R@ :key @ COUNT R@ :value @  R> :link @ >R do-it EXECUTE R> 
;
: (for-hash) ( rec -- nextrec )
  >R R@ :value @  R@ :key @ COUNT R> :link @ >R do-it EXECUTE R> 
;

: (hash-empty?) ( rec -- nextrec )  cnt 1+! DROP 0 ;

: (hash-count) ( rec -- nextrec )   cnt 1+! :link @ ;

EXPORT

: all-hash ( xt h -- )
\ xt ( akey ukey a|value   -- )
  do-it >R
  >R TO do-it ['] (all-hash) R> traverse-hash 
  R> TO do-it
;
: for-hash ( h xt -- )
\ xt ( a|value  akey ukey -- )
  do-it >R
  TO do-it ['] (for-hash) SWAP traverse-hash 
  R> TO do-it
;

: hash-empty? ( h -- flag )    \ проверяет, пуст хэш или нет
  cnt 0! ['] (hash-empty?) SWAP traverse-hash cnt @ 0= 
;
: hash-count ( h -- n )    \ подсчитывает число элементов в хэше
  cnt 0! ['] (hash-count) SWAP traverse-hash cnt @ 
;

;MODULE
