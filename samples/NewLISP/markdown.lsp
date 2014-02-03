#!/usr/bin/env newlisp

;; @module markdown
;; @author cormullion
;; @description a port of John Gruber's Markdown to newLISP
;; @location http://unbalanced-parentheses.nfshost.com/
;; @version of date 2011-10-02 22:36:02
;; version history: at the end
;; a port of John Gruber's Markdown.pl (http://daringfireball.net/markdown) script to newLISP...
;; see his original Perl script for explanations of the fearsome regexen and
;; byzantine logic, etc...
;; TODO:
;;   the following Markdown tests fail:
;;   Inline HTML (Advanced) ... FAILED
;;   Links, reference style ... FAILED -- nested brackets 
;;   Links, shortcut references ... FAILED
;;   Markdown Documentation - Syntax ... FAILED
;;   Ordered and unordered lists ... FAILED -- a nested ordered list error
;;   parens in url : ![this is a stupid URL](http://example.com/(parens).jpg) see (Images.text)
;;   Add: email address scrambling

(context 'Hash)
(define HashTable:HashTable)

(define (build-escape-table)
   (set '*escape-chars* [text]\`*_{}[]()>#+-.![/text])   
   (dolist (c (explode *escape-chars*))
        (HashTable c (hash c))))

(define (init-hash txt)
    ; finds a hash identifier that doesn't occur anywhere in the text
    (set 'counter 0)
    (set 'hash-prefix "HASH")
    (set 'hash-id (string hash-prefix counter))
    (do-while (find hash-id txt)
           (set 'hash-id (string hash-prefix (inc counter))))
    (Hash:build-escape-table))

(define (hash s)
   (HashTable s (string hash-id (inc counter))))

(context 'markdown)

(define (markdown:markdown txt)
  (initialize)
  (Hash:init-hash txt)
  (unescape-special-chars 
    (block-transforms 
      (strip-link-definitions 
         (protect 
            (cleanup txt))))))

(define (initialize)
  (set '*escape-pairs*   '(
       ({\\\\} {\})
       ({\\`}  {`})
       ({\\\*} {*}) 
       ({\\_}  {_})
       ([text]\\\{[/text] [text]{[/text])
       ([text]\\\}[/text] [text]}[/text])
       ({\\\[} {[})
       ({\\\]} {]})
       ({\\\(} {(})
       ({\\\)} {)})
       ({\\>}  {>})
       ({\\\#} {#})
       ({\\\+} {+})
       ({\\\-} {-})
       ({\\\.} {.})
       ({\\!}  {!})))
  (set '*hashed-html-blocks* '())
  (set '*list-level* 0))

(define (block-transforms txt)
   (form-paragraphs 
    (protect 
     (block-quotes 
      (code-blocks 
       (lists 
        (horizontal-rules 
         (headers txt))))))))

(define (span-transforms txt)
  (line-breaks 
   (emphasis 
    (amps-and-angles 
     (auto-links 
      (anchors 
       (images 
        (escape-special-chars 
         (escape-special-chars (code-spans txt) 'inside-attributes)))))))))

(define (tokenize-html xhtml)
; return list of tag/text portions of xhtml text
  (letn (
       (tag-match [text]((?s:<!(-- .*? -- \s*)+>)|
(?s:<\?.*?\?>)|
(?:<[a-z/!$](?:[^<>]|
(?:<[a-z/!$](?:[^<>]|
(?:<[a-z/!$](?:[^<>]|
(?:<[a-z/!$](?:[^<>]|
(?:<[a-z/!$](?:[^<>]|
(?:<[a-z/!$](?:[^<>])*>))*>))*>))*>))*>))*>))[/text]) ; yeah, well...
      (str xhtml)
      (len (length str))
      (pos 0)
      (tokens '()))
 (while (set 'tag-start (find tag-match str 8))
    (if (< pos tag-start)
        (push (list 'text (slice str pos (- tag-start pos))) tokens -1))
    (push (list 'tag $0) tokens -1)
    (set 'str (slice str (+ tag-start (length $0))))
    (set 'pos 0))
 ; leftovers
  (if (< pos len)
      (push (list 'text (slice str pos (- len pos))) tokens -1))
  tokens))

(define (escape-special-chars txt (within-tag-attributes nil))
  (let ((temp (tokenize-html txt))
        (new-text {}))    
    (dolist (pair temp)
        (if (= (first pair) 'tag)
             ; 'tag
             (begin              
              (set 'new-text (replace {\\} (last pair) (HashTable {\\}) 0))
              (replace [text](?<=.)</?code>(?=.)[/text] new-text (HashTable {`}) 0)
              (replace {\*} new-text (HashTable {*}) 0)
              (replace {_} new-text (HashTable {_} ) 0))
             ; 'text
             (if  within-tag-attributes
                  (set 'new-text (last pair))
                  (set 'new-text (encode-backslash-escapes (last pair)))))
        (setf (temp $idx) (list (first pair) new-text)))
  ; return as text
  (join (map last temp))))

(define (encode-backslash-escapes t)
   (dolist (pair *escape-pairs*)
      (replace (first pair) t (HashTable (last pair)) 14)))

(define (encode-code s)
 ; encode/escape certain characters inside Markdown code runs
  (replace {&}  s   "&amp;" 0)
  (replace {<}  s   "&lt;" 0)
  (replace {>}  s   "&gt;" 0)
  (replace {\*} s   (HashTable {\\}) 0)
  (replace {_}  s   (HashTable {_}) 0)
  (replace "{"  s   (HashTable "{") 0)
  (replace {\[} s   (HashTable {[}) 0)
  (replace {\]} s   (HashTable {]}) 0)
  (replace {\\} s   (HashTable "\\") 0))

(define (code-spans s)
  (replace  
    {(?<!\\)(`+)(.+?)(?<!`)\1(?!`)} 
    s 
    (string {<code>} (encode-code (trim $2)) {</code>}) 
    2))

(define (encode-alt s)
  (replace {&} s "&amp;" 0)
  (replace {"} s "&quot;" 0))

(define (images txt)
 (let ((alt-text {})
       (url {})
       (title {})
       (ref-regex    {(!\[(.*?)\][ ]?(?:\n[ ]*)?\[(.*?)\])})
       (inline-regex {(!\[(.*?)\]\([ \t]*<?(\S+?)>?[ \t]*((['"])(.*?)\5[ \t]*)?\))})
       (whole-match  {})
       (result {})
       (id-ref {})
       (url    {}))
  ;  reference links ![alt text][id]
  (replace 
    ref-regex 
    txt 
    (begin
       (set 'whole-match $1 'alt-text $2 'id-ref $3)       
       (if alt-text
             (replace {"} alt-text {&quot;} 0))
       (if (empty? id-ref)
            (set 'id-ref (lower-case alt-text)))     
       (if (lookup id-ref *link-database*)
           (set 'url (first (lookup id-ref *link-database*)))
           (set 'url nil))
       (if url
           (begin 
              (replace {\*} url (HashTable {*}) 0)
              (replace {_}  url (HashTable {_}) 0) 
            ))             
       (if (last (lookup id-ref *link-database*))
            ; title
           (begin
             (set 'title (last (lookup id-ref *link-database*)))
             (replace {"}  title {&quot;} 0)
             (replace {\*} title (HashTable {*}) 0)
             (replace {_}  title (HashTable {_}) 0))
           ; no title
           (set 'title {})
           )       
       (if url
        (set 'result (string 
          {<img src="} 
          (trim url) 
          {" alt="} 
          alt-text {" }
          (if (not (empty? title))
               (string { title="} title {"}) {})
          { />}))
        (set 'result whole-match))
     )
     0
   )
   ; inline image refs:  ![alt text](url "optional title")
    (replace 
      inline-regex 
      txt 
      (begin
        (set 'whole-match $1)
        (set 'alt-text $2)
        (set 'url $3)
        (set 'title $6)
        (if alt-text
             (replace {"} alt-text {&quot;} 0)
             (set 'alt-text {}))          
        (if  title 
             (begin 
               (replace {"}  title {&quot;} 0)
               (replace {\*} title (HashTable {*}) 0)
               (replace {_}  title (HashTable {_}) 0))
             (set 'title {}))           
        (replace {\*} url (HashTable {*}) 0)
        (replace {_} url (HashTable {_}) 0)
        (string 
           {<img src="} 
           (trim url) 
           {" alt="} 
           alt-text {" }
           (if title (string {title="} title {"}) {}) { />})
        )
        0
     )
    ; empty ones are possible
    (set '$1 {})
    (replace {!\[(.*?)\]\([ \t]*\)} 
     txt 
     (string {<img src="" alt="} $1 {" title="" />})
     0)))

(define (make-anchor link-text id-ref )
; Link defs are in the form: ^[id]: url "optional title"
; stored in link db list  as (id (url title))
; params are text to be linked and the id of the link in the db
; eg bar 1 for [bar][1]

   (let ((title {})
           (id id-ref)
           (url nil))
      (if link-text
          (begin
             (replace {"} link-text {&quot;} 0)
             (replace {\n} link-text { } 0)
             (replace {[ ]?\n} link-text { } 0)))   
      (if (null? id ) (set 'id  (lower-case link-text)))
      (if (not (nil? (lookup id *link-database*)))
          (begin
             (set 'url (first (lookup id  *link-database*)))
             (replace {\*} url (HashTable {*}) 0)
             (replace {_}  url (HashTable {_}) 0)
             (if (set 'title (last (lookup id  *link-database*)))
                 (begin 
                      (replace {"}  title {&quot;} 0)
                      (replace {\*} title (HashTable {*}) 0)
                      (replace {_}  title (HashTable {_}) 0))
                (set 'title {})))
           (set 'url nil))
      (if url
          (string {<a href="} (trim url) 
               {"}
               (if (not (= title {})) (string { title="} (trim title) {"}) {})
               {>} link-text {</a>})
          (string {[} link-text {][} id-ref {]}))))

(define (anchors txt)
  (letn ((nested-brackets {(?>[^\[\]]+)*})
         (ref-link-regex (string {(\[(} nested-brackets {)\][ ]?(?:\n[ ]*)?\[(.*?)\])}))
         (inline-regex {(\[(.*?)\]\([ ]*<?(.*?\)?)>?[ ]*((['"])(.*?)\5[ \t]*)?\))})
         (link-text {})
         (url {})
         (title {}))         
  ; reference-style links: [link text] [id]
  (set '$1 {} '$2 {} '$3 {} '$4 {} '$5 {} '$6 {})    ; i still don't think I should have to do this...
  
  ; what about this regex instead?
  (set 'ref-link-regex {(\[(.*?)\][ ]?\[(.*?)\])})
   
  (replace ref-link-regex txt (make-anchor $2 $3) 8) ; $2 is link text, $3 is id
  ; inline links: [link text](url "optional title")
  (set '$1 {} '$2 {} '$3 {} '$4 {} '$5 {} '$6 {})
  (replace 
     inline-regex 
     txt 
    (begin
      (set 'link-text $2)
      (set 'url $3)
      (set 'title $6)
      (if link-text (replace {"} link-text {&quot;} 0))          
      (if title 
           (begin 
             (replace {"}  title {&quot;} 0)
             (replace {\*} title  (HashTable {*}) 0)
             (replace {_}  title  (HashTable {_}) 0))
           (set 'title {}))           
      (replace {\*} url (HashTable {*}) 0)
      (replace {_}  url (HashTable {_}) 0)
      (replace {^<(.*)>$} url $1 0)
      (string 
         {<a href="} 
         (trim url)
         {"}
         (if (not (= title {}))
                 (string { title="} (trim title) {"}) 
                 {})
         {>} link-text {</a>}
         ))
     8
   ) ; replace
 ) txt)

(define (auto-links txt)
 (replace 
    [text]<((https?|ftp):[^'">\s]+)>[/text] 
    txt 
    (string {<a href="} $1 {">} $1 {</a>})  
    0
 )
  ; to-do: email ...
)

(define (amps-and-angles txt)
; Smart processing for ampersands and angle brackets
  (replace 
    [text]&(?!\#?[xX]?(?:[0-9a-fA-F]+|\w+);)[/text]
    txt
    {&amp;}
    10
  )
  (replace 
    [text]<(?![a-z/?\$!])[/text]
    txt
    {&lt;}
    10))

(define (emphasis txt)
  ; italics/bold: strong first
  (replace 
    [text] (\*\*|__) (?=\S) (.+?[*_]*) (?<=\S) \1 [/text]
    txt
    (string {<strong>} $2 {</strong>})
    8   
  )
  (replace 
    [text] (\*|_) (?=\S) (.+?) (?<=\S) \1 [/text]
    txt
    (string {<em>} $2 {</em>})
    8  
  ))

(define (line-breaks txt)
  ; handles line break markers
  (replace " {2,}\n" txt " <br/>\n" 0))

(define (hex-str-to-unicode-char strng)
   ; given a five character string, assume it's "U" + 4 hex chars and convert
   ; return the character...
   (char (int (string "0x" (1 strng)) 0 16)))

(define (ustring s)
  ; any four digit string preceded by U 
  (replace "U[0-9a-f]{4,}" s (hex-str-to-unicode-char $0) 0))

(define (cleanup txt)
  ; cleanup the text by normalizing some possible variations
  (replace "\r\n|\r" txt "\n" 0)      ; standardize line ends
  (push "\n\n" txt -1)                ; end with two returns
  (set 'txt (detab txt))              ; convert tabs to spaces
  
  ; convert inline Unicode:
  (set 'txt (ustring txt))
  (replace "\n[ \t]+\n" txt "\n\n" 0) ; lines with only spaces and tabs
  )

(define (protect txt)
 ; protect or "hash html blocks" 
 (letn ((nested-block-regex  [text](^<(p|div|h[1-6]|blockquote|pre|table|dl|ol|ul|script|noscript|form|fieldset|iframe|math|ins|del)\b(.*\n)*?</\2>[ \t]*(?=\n+|\Z))[/text])
       (liberal-tag-regex [text](^<(p|div|h[1-6]|blockquote|pre|table|dl|ol|ul|script|noscript|form|fieldset|iframe|math)\b(.*\n)*?.*</\2>[ \t]*(?=\n+|\Z))[/text])
       (hr-regex  [text](?:(?<=\n\n)|\A\n?)([ ]{0,3}<(hr)\b([^<>])*?/?>[ \t]*(?=\n{2,}|\Z))[/text])
       (html-comment-regex [text](?:(?<=\n\n)|\A\n?)([ ]{0,3}(?s:<!(--.*?--\s*)+>)[ \t]*(?=\n{2,}|\Z))[/text])
       (results '())
       (chunk-count (length (set 'chunks (parse txt "\n\n"))))
       (chunk-size 500))
   
   ; due to a limitation in PCRE, long sections have to be divided up otherwise we'll crash
   ; so divide up long texts into chunks, then do the regex on each chunk
   ; not an ideal solution, but it works ok :( 
  
   (for (i 0 chunk-count chunk-size)
       ; do a chunk
       (set 'text-chunk (join (i (- (min chunk-count (- (+ i chunk-size) 1)) i) chunks) "\n\n"))
       (dolist (rgx (list nested-block-regex liberal-tag-regex hr-regex html-comment-regex))
         (replace 
            rgx 
            text-chunk
            (begin
              (set 'key (Hash:hash $1))
              (push (list key $1 ) *hashed-html-blocks* -1)
              (string "\n\n" key "\n\n"))
            2))
        ; save this partial result
        (push text-chunk results -1)
    ) ; for
  ; return string result
  (join results "\n\n")))

(define (unescape-special-chars t)
 ; Swap back in all the special characters we've hidden. 
  (dolist (pair (HashTable))
    (replace (last pair) t (first pair) 10)) t)

(define (strip-link-definitions txt)
 ; strip link definitions from the text and store them
 ; Link defs are in the form: ^[id]: url "optional title"
 ; stored in link db list  as (id (url title))
  (let ((link-db '())
        (url {})
        (id {})
        (title {}))
     (replace 
       [text]^[ ]{0,3}\[(.+)\]:[ \t]*\n?[ \t]*<?(\S+?)>?[ \t]*\n?[ \t]*(?:(?<=\s)["(](.+?)[")][ \t]*)?(?:\n+|\Z)[/text]
       txt 
       (begin 
         (set 'id (lower-case $1) 'url (amps-and-angles $2) 'title $3)
         (if title (replace {"} title {&quot;} 0))
         (push (list id (list url title)) link-db)
         (set '$3 {}) ; necessary?
         (string {}) ; remove from text
         ) 
       10)
     (set '*link-database* link-db)
     txt))

(define (horizontal-rules txt)
   (replace 
   [text]^[ ]{0,2}([ ]?\*[ ]?){3,}[ \t]*$[/text]
    txt
    "\n<hr />"
    14)  
   (replace 
   [text]^[ ]{0,2}([ ]? -[ ]?){3,}[ \t]*$[/text]
   txt
   "\n<hr />"
   14)  
   (replace 
    [text]^[ ]{0,2}([ ]? _[ ]?){3,}[ \t]*$[/text]
    txt
    "\n<hr />"
    14))

(define (headers txt)
  ; setext headers
 (let ((level 1))
    (replace 
      [text]^(.+)[ \t]*\n=+[ \t]*\n+[/text]
      txt 
      (string "<h1>" (span-transforms $1) "</h1>\n\n")
      2)  
  
    (replace 
      [text]^(.+)[ \t]*\n-+[ \t]*\n+[/text]
      txt 
      (string "<h2>" (span-transforms $1) "</h2>\n\n")
      2) 
   ; atx headers
    (replace 
      [text]^(\#{1,6})\s*(.+?)[ ]*\#*(\n+)[/text]
      txt 
      (begin
       (set 'level (length $1))
       (string "<h" level ">" (span-transforms $2) "</h" level ">\n\n")
       )
      2)))

(define (lists txt)
 (letn ((marker-ul {[*+-]})
        (marker-ol {\d+[.]})
        (marker-any (string {(?:} marker-ul {|} marker-ol {)}))
        (whole-list-regex (string [text](([ ]{0,3}([/text] marker-any [text])[ \t]+)(?s:.+?)(\z|\n{2,}(?=\S)(?![ \t]*[/text] marker-any [text][ \t]+)))[/text]))
        (my-list {})
        (list-type {})
        (my-result {}))
   (replace 
      (if (> *list-level* 0)
          (string {^} whole-list-regex) 
          (string {(?:(?<=\n\n)|\A\n?)} whole-list-regex))
      txt
      (begin
         (set 'my-list $1)
         (if (find $3 marker-ul) 
            (set 'list-type "ul" 'marker-type marker-ul) 
            (set 'list-type "ol" 'marker-type marker-ol))
         (replace [text]\n{2,}[/text] my-list "\n\n\n" 0)
         (set 'my-result (process-list-items my-list marker-any))
         (replace {\s+$} my-result {} 0)
         (string {<} list-type {>} "\n" my-result "\n" {</} list-type {>} "\n"))
      10 ; must be multiline
      )))

(define (process-list-items list-text marker-any)    
  (let ((list-regex (string [text](\n)?(^[ \t]*)([/text] marker-any [text])[ \t]+((?s:.+?)(\n{1,2}))(?=\n*(\z|\2([/text] marker-any [text])[ \t]+))[/text]))
        (item {})
        (leading-line {})
        (leading-space {})
        (result {}))
     (inc *list-level*)
     (replace [text]\n{2,}\z[/text] list-text "\n" 0)
     (set '$1 {} '$2 {} '$3 {} '$4 {} '$5 {})
     (replace 
       list-regex
       list-text
       (begin
         (set 'item $4)
         (set 'leading-line $1)
         (set 'leading-space $2)
         (if (or (not (empty? leading-line)) (ends-with item "\n{2,}" 0))
             (set 'item (block-transforms (outdent item)))
           ; recurse for sub lists
           (begin 
              (set 'item (lists (outdent item))) 
              (set 'item (span-transforms (trim item "\n")))
              ))
       (string {<li>} item {</li>} "\n"))
     10)
    (dec *list-level*)
   list-text))

(define (code-blocks txt)
 (let ((code-block {})
       (token-list '()))
  (replace 
    [text](?:\n\n|\A)((?:(?:[ ]{4}|\t).*\n+)+)((?=^[ ]{0,3}\S)|\Z)[/text]
    txt 
    (begin 
      (set 'code-block $1)
      ; format if Nestor module is loaded and it's not marked as plain
      (if (and (not (starts-with code-block "    ;plain\n")) (context? Nestor))
          ; format newlisp
          (begin 
             ; remove flag if present
            (replace "[ ]{4};newlisp\n" code-block {} 0)       
            (set 'code-block (protect (Nestor:nlx-to-html (Nestor:my-read (trim (detab (outdent code-block)) "\n")))))
            code-block)
          ; don't format 
          (begin
            ; trim leading and trailing newlines
            (replace "[ ]{4};plain\n" code-block {} 0)
            (set 'code-block (trim (detab (encode-code (outdent code-block))) "\n"))
            (set '$1 {})
            (set 'code-block (string "\n\n<pre><code>" code-block "\n</code></pre>\n\n")))))
    10)))

(define (block-quotes txt)
  (let ((block-quote {}))
     (replace 
       [text]((^[ \t]*>[ \t]?.+\n(.+\n)*\n*)+)[/text]
       txt 
       (begin 
         (set 'block-quote $1)
         (replace {^[ ]*>[ ]?} block-quote {} 2)
         (replace {^[ ]+$} block-quote {} 2)
         (set 'block-quote (block-transforms block-quote)) ; recurse     
         ; remove leading spaces
         (replace 
             {(\s*<pre>.+?</pre>)} 
             block-quote 
             (trim $1)
             2)
         (string "<blockquote>\n" block-quote "\n</blockquote>\n\n"))
       2)))

(define (outdent s)
  (replace [text]^(\t|[ ]{1,4})[/text] s {} 2))

(define (detab s)
  (replace [text](.*?)\t[/text] 
    s   
    (string $1 (dup { } (- 4 (% (length $1) 4))))
    2))

(define (form-paragraphs txt)
  (let ((grafs '())
        (original nil))
    (set 'txt   (trim txt "\n"))            ; strip blank lines before and after
    (set 'grafs (parse txt "\n{2,}" 0))     ; split    
    (dolist (p grafs)
      (if (set 'original (lookup p *hashed-html-blocks*))
        ; html blocks
        (setf (grafs $idx) original)
        ; wrap <p> tags round everything else
        (setf (grafs $idx) (string {<p>} (replace {^[ ]*} (span-transforms p) {} (+ 4 8 16)) {</p>}))))
    (join grafs "\n\n")))

[text]
; three command line arguments: let's hope last one is a file
(when (= 3 (length (main-args)))
      (println (markdown (read-file (main-args 2))))
      (exit))

; hack for command-line and module loading
(set 'level (sys-info 3))

; if level is 2, then we're probably invoking markdown.lsp directly
; if level is > 3, then we're probably loading it into another script...
    
(when (= level 2)
   ; running on command line, read STDIN and execute:
   (while (read-line)
          (push (current-line) *stdin* -1))
   (println (markdown (join *stdin* "\n")))
   (exit))
[/text]

;; version 2011-09-16 16:31:29
;;   Changed to different hash routine. Profiling shows that hashing takes 40% of the execution time.
;;   Unfortunately this new version is only very slightly faster.
;;   Command-line arguments hack in previous version doesn't work.
;;
;; version 2011-08-18 15:04:40
;;   various fixes, and added hack for running this from the command-line:
;;     echo "hi there"     | newlisp markdown.lsp 
;;     echo "hello world"  | markdown.lsp 
;;     cat file.text       | newlisp markdown.lsp
;;
;; version 2010-11-14 17:34:52
;;    some problems in ustring. Probably remove it one day, as it's non standard...
;;
;; version 2010-10-14 18:41:38
;;    added code to work round PCRE crash in (protect ...
;;
;; version date 2010-07-10 22:20:25
;;    modified call to 'read' since lutz has changed it
;;
;; version date 2009-11-16 22:10:10
;;    fixed bug in tokenize.html
;;
;; version date 2008-10-08 18:44:46
;;    changed nth-set to setf to be version-10 ready. 
;;    This means that now this script will NOT work with
;;    earlier versions of newLISP!!!!!!!!!!!
;;    requires Nestor if you want source code colouring...
;;
;; version date 2008-08-08 16:54:56
;;    changed (unless to (if (not ... :(
;;
;; version date 2008-07-20 14:!2:29
;;    added hex-str-to-unicode-char ustring
;;
;; version date 2008-03-07 15:36:09
;;    fixed load error
;;
;; version date 2007-11-17 16:20:57
;;    added syntax colouring module
;; 
;; version date  2007-11-14 09:19:42
;;    removed reliance on dostring for compatibility with 9.1


; eof