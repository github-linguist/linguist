;; ===== enum
(enum Color                           ; define enum
  "Color enum"                        ; docstring
  [Red Green Blue])

(def c Color.Red)                     ; using enum

(switch c
  [Color.Red] (print "red")
  [Color.Green] (print "green")
  [Color.Blue] (print "blue"))

;; ===== struct
(struct Enemy                       ; define struct
  "Enemy Struct"                    ; docstring
  [hp]
  (defn heal [self x]               ; define method inside of struct
    (set! self.hp (+ self.hp x))))

(method [Enemy]                     ; define method outside of struct
  (defn damage [self x]             ; TODO: Constructor
    (set! self.hp (- self.hp x))))

(def slime
  (Enemy {:hp 20}))                 ; using struct

(.hp slime)                         ; access member => 20
(slime.damage 10)                   ; call method (allow this style)
(print slime.hp)                    ; => 10 (allow this style)

(struct ChildEnemy => Enemy         ; inherit struct
  "ChildEnemy Struct"
  [mp]

  (defn ChildEnemy [self hp mp]     ; you can define constructor(Same as struct name)
    (set! self.hp hp)
    (set! self.mp mp)))

  (defn magic [self x]
    (set! self.mp (- self.mp x)))

(def slime-child
  (ChildEnemy {:hp 20, :mp 10}))    ; using struct

(slime-child.magic 5)               ; call method
(slime-child.damage 5)              ; call parent method

(ancestor slime-child)              ; => [Enemy] show all ancestor
(ancestor ChildEnemy)               ; => [Enemy]
(ancestor #ChildEnemy)              ; => [#Enemy]

;; ===== macro
(macro my-and                     ; define macro
  "Evaluates exprs one at time,
   from left to right."           ; docstring
  ([] true)                       ; multi arity
  ([x] x)
  ([x & next]                     ; variable length argument (& rest)
    `(let [and# ~x]               ; quote(`) and unquote(~)
       (if and#                   ; auto-gensym(xxx#)
         (my-and ~@next)          ; unquote splicing(~@)
         and#))))

(my-and "a" "b" "c")              ; => "c"

;; ===== Exception
(try
  (print "before error")
  (throw (TypeError "error")
  (print "after error")
  (catch TypeError e
    (print e))
  (finally (print "every time executed"))))

;; ===== Desturcturing
;; vector/list
(let [[a, b, c] [1, 2, 3]] (print a b c))             ;; 1 2 3
(let [[a, _, c] [1, 2, 3]] (print a c))               ;; 1 3
(let [[a, [b1, b2], c] [1, [2, 3], 4]] (print b1 b2)) ;; 2 3
(let [[a & b] [1 2 3]] (print a b))                   ;; 1 [2 3]
(let [[a & b] [1]] (print a b))                       ;; 1 nil
(let [[a & b] []] (print a b))                        ;; nil nil

;; map
(let [{:a a, :b b} {:a 1 :b 2}] (print a b))          ;; 1 2
(let [{"a" a, "b" b} {:a 1}] (print a b))             ;; 1 nil
(let [{0 a, 1 b} {1 2}] (print a b))                  ;; nil 2
(let [{:a x, :b y, :default {x 2}}
      {:b 20}] (print x y))                           ;; 2 20

;; defn
(defn nation-datetime
  "Create a datetime object"
  [nation                              ;; positional argument
   & rest                              ;; variable length argument
   & {:year year, :mon mon, :day day,  ;; keyword arguments
      :hour hour, :min min, :sec sec,
      :nano nano, :tz tz,
      :default {year 1970, mon 1,      ;; default value
                day 1, hour 0,
                min 0, sec 0,
                nano 0, tz "UTC+00:00"}}]

  ;; posional argument filled with nil when not enough arguments
  (let [year (or (0 rest) year)
        mon (or (1 rest) mon)
        day (or (2 rest) day)
        hour (or (3 rest) hour)
        min (or (4 rest) min)
        sec (or (5 rest) sec)
        nano (or (6 rest) nano)
        tz (or (7 rest) tz)]

      (print nation)
      (rust/datetime year mon day hour mi sec nano tz))
)

(nation-datetime "Japan" 2018 1 1 {:tz "UTC+09:00"})
(nation-datetime "UK" {:year 2018 :mon 1 :day 1
                       :hour 0 :min 0 :sec 0 :nano 0})

;; ========== Type System (Annotation) ==========
;; type abbreviation
#any                     ; any type
#nil,                    ; nil
#bool,                   ; bool
#i64,                    ; i64
#f64,                    ; f64
#c64,                    ; c64 (complex number)
#str                     ; string
#regex                   ; regex
#key                     ; keyword
#sym                     ; symbol
#l[#any]                 ; list of any
#v[#i64]                 ; vector of i64
#m[#str]                 ; map that value is string
#s[#f64]                 ; set of f64
#a[#i64, [2, 3, 4]]      ; 3d-array of i64 (shape is [2, 3, 4])
#fn[#i64, #i64] => #i64  ; function like (fn [x y] => (+ x y)), x: i64, y: i64, return: i64
#macro                   ; macro(マクロの型をどうするか)
#generator[#str]         ; generator
#datetime                ; datetime
#duration                ; duration
#iterable[#i64]          ; iterable
#collection              ; collection
#option[#i64]            ; option of i64

;; map_key: #str, #i64, #key
;; array: #i64, #f64, #c64 + shape

;; ===== User Defined Type
(enum Color                         ; define enum
  "Color enum"
  [Red Green Blue])

(def c #Color Color.Red)            ; using enum type: Color

(struct Point                       ; define struct
  "Point struct"
  [x #i64
   y #i64])

(def p #Point (Point {:x 1, :y 2})) ; using struct type: Point

(typedef #int #i64)                 ; typedef (type alias)
(typedef #map-key
  (union #i64 #str #key))           ; union of i64, string, keyword

;; ===== Type hierarchy
;; #any is super type of all types
;; #i64, #f64, #c64 is sub type of #number
;; #l, #v, #m, #s, #a is sub type of #collection
;; #collection is sub type of #iterable
;; #str is sub type of #iterable
;; #generator is sub type of #iterable
;; #slice is defined with #struct
;; #map-key can take #str, #i64, #key

;; ===== multiple dispatch
;; You can define multiple function with same name with different type.
(defn some [x #i64] => #i64
  (print "i64")
  x)

(defn some [x #f64] => #f64
  (print "f64")
  x)

(some 1)                              ; 1 => "i64"

;; def, const, fn, struct, enum, macro, union, typedef

;; ===== module system
(import string)
(string/shouty-snake "abcDef")        ; => "ABC_DEF"

(import string :as str)               ; import with alias
(str/shouty-snake "abcDef")           ; => "ABC_DEF"
(def sss str/#template "{:.2}")       ; use type defiend in module

(import string [shouty-snake          ; import with select
                train-case])
(train-case "abcDef")                 ; => "Abc-Def"

(import "path/to/somename")           ; import 'somename.lisp'
(somename/somefunc 1 2 3)             ; => ...

(defn somefunc [x y z]
  (print x y z))

(export [somefunc])                   ; export function

;; ===== Array API
(def a (array [[1, 2, 3],             ; 2d-array of i64
               [4, 5, 6]]))
(a/shape a)                           ; => [3, 3]
(a/reshape a [1, 6])                  ; => [[1, 2, 3, 4, 5, 6]]
(a/zeros [2, 3])                      ; => [[0, 0, 0], [0, 0, 0]]
(a/ones [2, 3])                       ; => [[1, 1, 1], [1, 1, 1]]
(a/range 0, 1, 0.2)                   ; => [[0, 0.2, 0.4, 0.6, 0.8, 1.0]]
(a/linspace 0, 1, 6)                  ; => [[0, 0.2, 0.4, 0.6, 0.8, 1.0]]
(a/rand [2, 3])                       ; => [[0.15, 0.89, 0.35], [0.34, 0.15, 0.76]]
(a/randint 0, 10, [2, 3])             ; => [[1, 3, 9], [8, 4, 5]]
([0|2] a)                             ; => [[1, 2, 3], [4, 5, 6]]
([0|-1|2] a)                          ; => [[1, 3, 5], [4, 6, 8]]
([|, 1] a)                            ; => [[2, 5], [3, 6]]
([|, 1|2] a)                          ; => [[[2], [5]], [[3], [6]]]
