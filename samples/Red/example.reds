Red/System [
    Title:      "Red/System example file"
    Purpose:    "Just some code for testing Pygments colorizer"
    Language:   http://www.red-lang.org/
]

#include %../common/FPU-configuration.reds

; C types

#define time!                   long!
#define clock!                  long!

date!: alias struct! [
    second                      [integer!]  ; 0-61 (60?)
    minute                      [integer!]  ; 0-59
    hour                        [integer!]  ; 0-23

    day                         [integer!]  ; 1-31
    month                       [integer!]  ; 0-11
    year                        [integer!]  ; Since 1900

    weekday                     [integer!]  ; 0-6 since Sunday
    yearday                     [integer!]  ; 0-365
    daylight-saving-time?       [integer!]  ; Negative: unknown
]

#either OS = 'Windows [
    #define clocks-per-second   1000
][
    ; CLOCKS_PER_SEC value for Syllable, Linux (XSI-conformant systems)
    ; TODO: check for other systems
    #define clocks-per-second   1000'000
] 

#import [LIBC-file cdecl [

    ; Error handling

    form-error: "strerror" [  ; Return error description.
        code            [integer!]
        return:         [c-string!]
    ]
    print-error: "perror" [  ; Print error to standard error output.
        string          [c-string!]
    ]


    ; Memory management

    make: "calloc" [  ; Allocate zero-filled memory.
        chunks          [size!]
        size            [size!]
        return:         [binary!]
    ]
    resize: "realloc" [  ; Resize memory allocation.
        memory          [binary!]
        size            [size!]
        return:         [binary!]
    ]
 ]
 
 JVM!: alias struct! [
    reserved0                   [int-ptr!]
    reserved1                   [int-ptr!]
    reserved2                   [int-ptr!]
    
    DestroyJavaVM               [function! [[JNICALL] vm [JVM-ptr!] return: [jint!]]]
    AttachCurrentThread         [function! [[JNICALL] vm [JVM-ptr!] penv [struct! [p [int-ptr!]]] args [byte-ptr!] return: [jint!]]]
    DetachCurrentThread         [function! [[JNICALL] vm [JVM-ptr!] return: [jint!]]]
    GetEnv                      [function! [[JNICALL] vm [JVM-ptr!] penv [struct! [p [int-ptr!]]] version [integer!] return: [jint!]]]
    AttachCurrentThreadAsDaemon [function! [[JNICALL] vm [JVM-ptr!] penv [struct! [p [int-ptr!]]] args [byte-ptr!] return: [jint!]]]
]

 ;just some datatypes for testing:
 
 #some-hash
 10-1-2013
 quit
 
 ;binary:
 #{00FF0000}
 #{00FF0000 FF000000}
 #{00FF0000	FF000000} ;with tab instead of space
 2#{00001111}
 64#{/wAAAA==}
 64#{/wAAA A==} ;with space	 inside
 64#{/wAAA	A==} ;with tab inside
 
 
 ;string with char
 {bla ^(ff) foo}
 {bla ^(( foo}
 ;some numbers:
 12
 1'000
 1.2
 FF00FF00h
 
 ;some tests of hexa number notation with not common ending
 [ff00h ff00h] ff00h{} FFh"foo" 00h(1 + 2) (AEh)

;normal words:
foo char

;get-word
:foo
 
;lit-word:
'foo 'foo

to-integer foo
foo/(a + 1)/b

call/output reform ['which interpreter] path: copy ""

 version-1.1:   00010001h
 
 #if type = 'exe [
    push system/stack/frame                 ;-- save previous frame pointer
    system/stack/frame: system/stack/top    ;-- @@ reposition frame pointer just after the catch flag
]
push CATCH_ALL                              ;-- exceptions root barrier
push 0                                      ;-- keep stack aligned on 64-bit