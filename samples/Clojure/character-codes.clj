(print (int \a)) ; prints "97"
(print (char 97)) ; prints \a

; Unicode is also available, as Clojure uses the underlying java Strings & chars
(print (int \π))  ; prints 960
(print (char 960)) ; prints \π

; use String because char in Java can't represent characters outside Basic Multilingual Plane
(print (.codePointAt "𝅘𝅥𝅮" 0)) ; prints 119136
(print (String. (int-array 1 119136) 0 1)) ; prints 𝅘𝅥𝅮
