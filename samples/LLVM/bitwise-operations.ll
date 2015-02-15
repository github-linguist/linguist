; ModuleID = 'test.o'
;e means little endian
;p: { pointer size : pointer abi : preferred alignment for pointers }
;i same for integers
;v is for vectors
;f for floats
;a for aggregate types
;s for stack objects
;n: {size:size:size...}, best integer sizes
target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32-n8:16:32"
;this was compiled with mingw32; thus it must be linked to an ABI compatible c library
target triple = "i386-mingw32"

@.str = private constant [13 x i8] c"a and b: %d\0A\00", align 1 ; <[13 x i8]*> [#uses=1]
@.str1 = private constant [12 x i8] c"a or b: %d\0A\00", align 1 ; <[12 x i8]*> [#uses=1]
@.str2 = private constant [13 x i8] c"a xor b: %d\0A\00", align 1 ; <[13 x i8]*> [#uses=1]
@.str3 = private constant [11 x i8] c"not a: %d\0A\00", align 1 ; <[11 x i8]*> [#uses=1]
@.str4 = private constant [12 x i8] c"a << n: %d\0A\00", align 1 ; <[12 x i8]*> [#uses=1]
@.str5 = private constant [12 x i8] c"a >> n: %d\0A\00", align 1 ; <[12 x i8]*> [#uses=1]
@.str6 = private constant [12 x i8] c"c >> b: %d\0A\00", align 1 ; <[12 x i8]*> [#uses=1]

;A function that will do many bitwise opreations to two integer arguments, %a and %b
define void @bitwise(i32 %a, i32 %b) nounwind {
;entry block
entry:
  ;Register to store (a & b)
  %0 = and i32 %b, %a                             ; <i32> [#uses=1]
  ;print the results
  %1 = tail call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([13 x i8]* @.str, i32 0, i32 0), i32 %0) nounwind ; <i32> [#uses=0]
  ;Register to store (a | b)
  %2 = or i32 %b, %a                              ; <i32> [#uses=1]
  ;print the results
  %3 = tail call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([12 x i8]* @.str1, i32 0, i32 0), i32 %2) nounwind ; <i32> [#uses=0]
  ;Register to store (a ^ b)
  %4 = xor i32 %b, %a                             ; <i32> [#uses=1]
  ;print the results
  %5 = tail call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([13 x i8]* @.str2, i32 0, i32 0), i32 %4) nounwind ; <i32> [#uses=0]
  ;Register to store (~a)
  %not = xor i32 %a, -1                           ; <i32> [#uses=1]
  ;print the results
  %6 = tail call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([11 x i8]* @.str3, i32 0, i32 0), i32 %not) nounwind ; <i32> [#uses=0]
  ;Register to store (a << b)
  %7 = shl i32 %a, %b                             ; <i32> [#uses=1]
  ;print the results
  %8 = tail call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([12 x i8]* @.str4, i32 0, i32 0), i32 %7) nounwind ; <i32> [#uses=0]
  ;Register to store (a >> b) (a is signed)
  %9 = ashr i32 %a, %b                            ; <i32> [#uses=1]
  ;print the results
  %10 = tail call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([12 x i8]* @.str5, i32 0, i32 0), i32 %9) nounwind ; <i32> [#uses=0]
  ;Register to store (c >> b), where c is unsiged (eg. logical right shift)
  %11 = lshr i32 %a, %b                           ; <i32> [#uses=1]
  ;print the results
  %12 = tail call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([12 x i8]* @.str6, i32 0, i32 0), i32 %11) nounwind ; <i32> [#uses=0]

  ;terminator instruction
  ret void
}

;Declare external fuctions
declare i32 @printf(i8* nocapture, ...) nounwind
