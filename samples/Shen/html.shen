\* html.shen --- html generation functions for shen

Copyright (C) 2011,  Eric Schulte

*** License:

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

 - Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

 - Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*** Commentary:

The standard lisp-to-html conversion tool suite.  Follows some of
the convertions of Clojure's hiccup.

  an example...

(8-) (html [ul#todo1.tasks.stuff [: [title "today"]]
          (map (lambda Str [li Str]) ["get milk" "dishes"])])
"<ul class='tasks stuff' id='todo1' title='today'>
 <li>get milk</li><li>dishes</li></ul>"

*** Code: *\
(trap-error
 (require string)
 (/. E (load "../string/string.shen")))

(package string- [html
                  \* symbols included from string *\
                  takestr dropstr substr length-str index-str
                  reverse-str starts-with substr? replace-str
                  join split trim-left trim-right chomp trim]

(define to-str
  \* return argument as a string, if already a string do not change *\
  X -> X where (string? X)
  X -> (str X))

(define gassoc
  X Y -> (hd (tl (assoc X Y))))

(define dassoc
  X Y -> (remove (assoc X Y) Y))

(define passoc
  [] Y -> Y
  [X XV] Y -> (let Orig (gassoc X Y)
                   New (if (cons? Orig) [XV|Orig] XV)
                [[X New]|(dassoc X Y)]))

(define html
  X -> X where (string? X)
  [Tag [: |Attrs] |Body] ->
    (let Tag-comps (css-parse-symbol Tag)
         Tag (gassoc tag Tag-comps)
         New-attrs (passoc (assoc class Tag-comps)
                           (passoc (assoc id Tag-comps) Attrs))
      (@s (make-string "<~S" Tag) (attributes New-attrs) ">"
          (html Body)
          (make-string "</~S>" Tag))) where (symbol? Tag)
  [Tag|Body] -> (html [Tag [:] Body]) where (symbol? Tag)
  [H|HS] -> (@s (html H) (html HS))
  [] -> "")

(define css-parse-symbol
  {symbol --> [[symbol A]]}
  Symbol -> (let String (str Symbol)
                 Class-split (split (str .) String)
                 Class (map (function intern) (tl Class-split))
                 Id-split (split (str #) (hd Class-split))
                 Tag (hd Id-split)
                 Id (tl Id-split)
              ((if (= [] Id) (/. X X) (cons [id (intern (hd Id))]))
               ((if (= [] Class) (/. X X) (cons [class Class])) 
                [[tag (intern Tag)]]))))

(define attributes
  [] -> ""
  [[K V]|AS] -> (@s " " (to-str K) "='"
                    (if (cons? V) (join " " (map (function str) V)) (to-str V))
                    "'" (attributes AS)))

)