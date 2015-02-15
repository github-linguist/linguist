; "99 Bottles of Beer on the Wall" in LLVM Assembly

; This is not strictly LLVM, as it uses the C library function "printf".
; LLVM does not provide a way to print values, so the alternative would be
; to just load the string into memory, and that would be boring.

; The song lyrics are global constants.
; Lyrics for plural verses:
@pluralVerse = private constant [120 x i8]
c"%d bottles of beer on the wall, %d bottles of beer.\0ATake one down and pass it around, %d bottles of beer on the wall.\0A\0A\00"
; Lyrics for the singular verse:
@singularVerse = private constant [121 x i8]
    c"1 bottle of beer on the wall, 1 bottle of beer.\0ATake one down and pass it around, no more bottles of beer on the wall.\0A\0A\00"
; Lyrics for the final verse:
@finalVerse = private constant [130 x i8]
    c"No more bottles of beer on the wall, no more bottles of beer.\0AGo to the store and buy some more, %d bottles of beer on the wall.\0A\00"

; Initial number of bottles of beer.
; This must be a natural number.
@initialVerseNumber = private constant i32 99

; The declaration for the external C printf function.
declare i32 @printf(i8*, ...)

; Prints a verse, with %numberOfBottles being the initial number of bottles
; in that verse.
define fastcc void @printVerse(i32 %numberOfBottles) {
    switch i32 %numberOfBottles,
        label %pluralVerse
        [ i32 1, label %singularVerse
          i32 0, label %finalVerse ]

pluralVerse:
    %pluralVersePointer = getelementptr [120 x i8]* @pluralVerse, i64 0, i64 0
    %newNumberOfBottles = sub i32 %numberOfBottles, 1
    call i32(i8*, ...)* @printf(
        i8* %pluralVersePointer,
        i32 %numberOfBottles,
        i32 %numberOfBottles,
        i32 %newNumberOfBottles)
    ret void

singularVerse:
    %singularVersePointer = getelementptr [121 x i8]* @singularVerse,i64 0,i64 0
    call i32(i8*, ...)* @printf(i8* %singularVersePointer)
    ret void

finalVerse:
    %finalVersePointer = getelementptr [130 x i8]* @finalVerse, i64 0, i64 0
    %initialVerseNumberL = load i32* @initialVerseNumber
    call i32(i8*, ...)* @printf(i8* %finalVersePointer,i32 %initialVerseNumberL)
    ret void
}

define i32 @main() {
loopHeader:
    %initialVerseNumberL = load i32* @initialVerseNumber
    br label %loop ; This br terminates the first basic block.
loop:
    %verseNumber =
        phi i32 [%initialVerseNumberL, %loopHeader], [%nextVerseNumber, %do]
    %cond = icmp eq i32 -1, %verseNumber
    br i1 %cond, label %break, label %do
do:
    call fastcc void @printVerse(i32 %verseNumber)
    %nextVerseNumber = sub i32 %verseNumber, 1
    br label %loop
break:
    ret i32 0
}
