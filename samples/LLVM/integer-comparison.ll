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
;this was compiled with mingw32; thus it must be linked to a compatible c library
target triple = "i386-mingw32"

; Declare string constants
@.str = private constant [6 x i8] c"%d %d\00", align 1 ; <[6 x i8]*> [#uses=1]
@.str1 = private constant [20 x i8] c"%d is less than %d\0A\00", align 1 ; <[20 x i8]*> [#uses=1]
@.str2 = private constant [19 x i8] c"%d is equal to %d\0A\00", align 1 ; <[19 x i8]*> [#uses=1]
@.str3 = private constant [23 x i8] c"%d is greater than %d\0A\00", align 1 ; <[23 x i8]*> [#uses=1]

;Declare main function (entry point). It does not throw any exceptions, and returns an integer of size 32.
define i32 @main() nounwind {
;Entry block
entry:
  ;Allocate the first integer, register %a will point to that
  %a = alloca i32, align 4                        ; <i32*> [#uses=4]
  ;Allocate the second integer, register %b will point to that
  %b = alloca i32, align 4                        ; <i32*> [#uses=4]
  ;Use the C standard library function scanf() to obtain input from users.
  ;Scanf takes a pointer to the string constant @.str, "%d %d\00", which will take two integers from the user.
  ;getelementptr basically does pointer math, in this case, no ptr math is required (we point to the beginning of @.str).
  ;Pass %a and %b, which are pointers to integers allocated previously.
  ;Scanf will store the two integers into the memory locations represented by %a and %b
  %0 = call i32 (i8*, ...)* @scanf(i8* noalias getelementptr inbounds ([6 x i8]* @.str, i32 0, i32 0), i32* %a, i32* %b) nounwind ; <i32> [#uses=0]
  ;Load the integer pointed to by %a and %b into registers %1 and %2 respectively
  %1 = load i32* %a, align 4                      ; <i32> [#uses=3]
  %2 = load i32* %b, align 4                      ; <i32> [#uses=3]
  ;Boolean register which represents if %1 is less than to %2
  %3 = icmp slt i32 %1, %2                        ; <i1> [#uses=1]
  ;If %1 is less than to %2, goto branch %bb, otherwise, goto %bb1
  br i1 %3, label %bb, label %bb1

;If integer %1 is less than %2
bb:                                               ; preds = %entry
  ;Use the C standard library function printf to output information to users
  ;Print @.str1, "%d is less than %d\0A\00"
  ;Additionally, pass the integers %1 and %2 to printf, to be formatted into the string
  %4 = call i32 (i8*, ...)* @printf(i8* noalias getelementptr inbounds ([20 x i8]* @.str1, i32 0, i32 0), i32 %1, i32 %2) nounwind ; <i32> [#uses=0]
  ;Continue on to %bb1, to check for equality of the two integers
  br label %bb1

;Continue checking if the integers are equal
bb1:                                              ; preds = %bb, %entry
  ;Boolean register which represents if %1 is equal to %2
  %5 = icmp eq i32 %1, %2                         ; <i1> [#uses=1]
  ;If %1 is equal to %2, goto branch %bb2, otherwise, goto %bb3
  br i1 %5, label %bb2, label %bb3

;If integer %1 is equal to %2
bb2:                                              ; preds = %bb1
  ;Use the C standard library function printf to output information to users
  ;Print @.str2 "%d is equal to %d\0A\00"
  ;Additionally, pass the integers %1 and %2 to printf, to be formatted into the string
  %6 = call i32 (i8*, ...)* @printf(i8* noalias getelementptr inbounds ([19 x i8]* @.str2, i32 0, i32 0), i32 %1, i32 %2) nounwind ; <i32> [#uses=0]
  ;Continue on to %bb3, to check if %1 is greater than %2
  br label %bb3

;Continue checking if %1 is greater than %2
bb3:                                              ; preds = %bb2, %bb1
  ;Boolean register which represents if %1 is greater than %2
  %7 = icmp sgt i32 %1, %2                      ; <i1> [#uses=1]
  ;If %1 is greather than %2, goto branch %bb4, otherwise, goto %bb5
  br i1 %7, label %bb4, label %bb5

;If integer %1 is greater than %2
bb4:                                              ; preds = %bb3
  ;Use the C standard library function printf to output information to users
  ;Print @.str3 "%d is greater than %d\0A\00"
  ;Additionally, pass the integers %1 and %2 to printf, to be formatted into the string
  %8 = call i32 (i8*, ...)* @printf(i8* noalias getelementptr inbounds ([23 x i8]* @.str3, i32 0, i32 0), i32 %1, i32 %2) nounwind ; <i32> [#uses=0]
  ;Return 0 for the main function, indicating program executed successfully
  ret i32 0

bb5:                                              ; preds = %bb3
  ;Return 0 for the main function, indicating program executed successfully
  ret i32 0
}

;Declare external fuctions
declare i32 @scanf(i8* nocapture, ...) nounwind

declare i32 @printf(i8* nocapture, ...) nounwind
