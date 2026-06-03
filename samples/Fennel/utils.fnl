;; This module contains mostly general-purpose table-related functionality that
;; you might expect to see in a standard library in most langugaes, as well as
;; the definitions of several core compiler types. It could be split into two
;; distinct modules along those lines.

;;; General-purpose helper functions

(fn stablepairs [t]
  "Like pairs, but gives consistent ordering every time. On 5.1, 5.2, and LuaJIT
  pairs is already stable, but on 5.3+ every run gives different ordering."
  (let [keys []
        succ []]
    (each [k (pairs t)]
      (table.insert keys k))
    (table.sort keys #(< (tostring $1) (tostring $2)))
    (each [i k (ipairs keys)]
      (tset succ k (. keys (+ i 1))))

    (fn stablenext [tbl idx]
      (let [key (if (= idx nil) (. keys 1) (. succ idx))
            value (if (= key nil) nil (. tbl key))]
        (values key value)))

    (values stablenext t nil)))

;; Note: the collect/icollect macros mostly make map/kvmap obsolete.

(fn map [t f out]
  "Map function f over sequential table t, removing values where f returns nil.
Optionally takes a target table to insert the mapped values into."
  (let [out (or out [])
        f (if (= (type f) :function)
              f
              #(. $ f))]
    (each [_ x (ipairs t)]
      (match (f x)
        v (table.insert out v)))
    out))

(fn kvmap [t f out]
  "Map function f over key/value table t, similar to above, but it can return a
sequential table if f returns a single value or a k/v table if f returns two.
Optionally takes a target table to insert the mapped values into."
  (let [out (or out [])
        f (if (= (type f) :function)
              f
              #(. $ f))]
    (each [k x (stablepairs t)]
      (match (f k x)
        (key value) (tset out key value)
        (value) (table.insert out value)))
    out))

(fn copy [from to]
  "Returns a shallow copy of its table argument. Returns an empty table on nil."
  (let [to (or to [])]
    (each [k v (pairs (or from []))]
      (tset to k v))
    to))

(fn member? [x tbl n]
  (match (. tbl (or n 1))
    x true
    nil nil
    _ (member? x tbl (+ (or n 1) 1))))

(fn allpairs [tbl]
  "Like pairs, but if the table has an __index metamethod, it will recurisvely
traverse upwards, skipping duplicates, to iterate all inherited properties"
  (assert (= (type tbl) :table) "allpairs expects a table")
  (var t tbl)
  (let [seen []]
    (fn allpairs-next [_ state]
      (let [(next-state value) (next t state)]
        (if (. seen next-state)
            (allpairs-next nil next-state)
            next-state
            (do
              (tset seen next-state true)
              (values next-state value))
            (match (getmetatable t)
              {: __index} (when (= :table (type __index))
                            (set t __index)
                            (allpairs-next t))))))

    allpairs-next))

;;; AST functions

;; AST nodes tend to be implemented as tables with specific "marker" metatables
;; set on them; they have constructor functions which set the metatables and
;; predicate functions which check the metatables. The fact that they use
;; metatables should be considered an implementation detail. String and number
;; literals are represented literally, and "regular" key/value tables are
;; represented without a marker metatable since their metatables are needed to
;; store file/line source data.

(fn deref [self]
  "Get the name of a symbol."
  (. self 1))

;; haven't defined sym yet; circularity is needed here
(var nil-sym nil)

;; the tostring2 argument is passed in by fennelview; this lets us use the same
;; function for regular tostring as for fennelview. when called from fennelview
;; the list's contents will also show as being fennelviewed.
(fn list->string [self tostring2]
  (var (safe max) (values [] 0))
  (each [k (pairs self)]
    (when (and (= (type k) :number) (> k max))
      (set max k)))
  (for [i 1 max]
    (tset safe i (or (and (= (. self i) nil) nil-sym) (. self i))))
  (.. "(" (table.concat (map safe (or tostring2 tostring)) " " 1 max) ")"))

(fn comment-view [c]
  (values c true))

(fn sym= [a b]
  (and (= (deref a) (deref b)) (= (getmetatable a) (getmetatable b))))

(fn sym< [a b]
  (< (. a 1) (tostring b)))

(local symbol-mt {1 :SYMBOL
                  :__fennelview deref
                  :__tostring deref
                  :__eq sym=
                  :__lt sym<})

(local expr-mt {1 :EXPR :__tostring deref})
(local list-mt {1 :LIST :__fennelview list->string :__tostring list->string})
(local comment-mt {1 :COMMENT
                   :__fennelview comment-view
                   :__tostring deref
                   :__eq sym=
                   :__lt sym<})

(local sequence-marker [:SEQUENCE])
(local vararg
       (setmetatable ["..."] {1 :VARARG :__fennelview deref :__tostring deref}))

(local getenv (or (and os os.getenv) #nil))

(fn debug-on? [flag]
  (let [level (or (getenv :FENNEL_DEBUG) "")]
    (or (= level :all) (level:find flag))))

(fn list [...]
  "Create a new list. Lists are a compile-time construct in Fennel; they are
represented as tables with a special marker metatable. They only come from
the parser, and they represent code which comes from reading a paren form;
they are specifically not cons cells."
  (setmetatable [...] list-mt))

(fn sym [str ?source ?scope]
  "Create a new symbol. Symbols are a compile-time construct in Fennel and are
not exposed outside the compiler. Second optional argument is a table describing
where the symbol came from; should be a table with filename, line, bytestart,
and byteend fields."
  (let [s {: ?scope 1 str}]
    (each [k v (pairs (or ?source []))]
      (when (= (type k) :string)
        (tset s k v)))
    (setmetatable s symbol-mt)))

(set nil-sym (sym :nil))

(fn sequence [...]
  "Create a new sequence. Sequences are tables that come from the parser when
it encounters a form with square brackets. They are treated as regular tables
except when certain macros need to look for binding forms, etc specifically."
  ;; can't use SEQUENCE-MT directly as the sequence metatable like we do with
  ;; the other types without giving up the ability to set source metadata
  ;; on a sequence, (which we need for error reporting) so embed a marker
  ;; value in the metatable instead.
  (setmetatable [...] {:sequence sequence-marker}))

(fn expr [strcode etype]
  "Create a new expression. etype should be one of:
  :literal literals like numbers, strings, nil, true, false
  :expression Complex strings of Lua code, may have side effects, etc
              but is an expression
  :statement Same as expression, but is also a valid statement (function calls)
  :vargs varargs symbol
  :sym symbol reference"
  (setmetatable {:type etype 1 strcode} expr-mt))

(fn comment* [contents ?source]
  (let [{: filename : line} (or ?source [])]
    (setmetatable {1 contents : filename : line} comment-mt)))

(fn varg []
  vararg)

(fn expr? [x]
  "Checks if an object is an expression. Returns the object if it is."
  (and (= (type x) :table) (= (getmetatable x) expr-mt) x))

(fn varg? [x]
  "Checks if an object is the vararg symbol. Returns the object if is."
  (and (= x vararg) x))

(fn list? [x]
  "Checks if an object is a list. Returns the object if is."
  (and (= (type x) :table) (= (getmetatable x) list-mt) x))

(fn sym? [x]
  "Checks if an object is a symbol. Returns the object if it is."
  (and (= (type x) :table) (= (getmetatable x) symbol-mt) x))

(fn sequence? [x]
  "Checks if an object is a sequence (created with a [] literal)"
  (let [mt (and (= (type x) :table) (getmetatable x))]
    (and mt (= mt.sequence sequence-marker) x)))

(fn comment? [x]
  (and (= (type x) :table) (= (getmetatable x) comment-mt) x))

(fn table? [x]
  "Checks if an object any kind of table, EXCEPT list/symbol/vararg/comment."
  (and (= (type x) :table) (not= x vararg) (not= (getmetatable x) list-mt)
       (not= (getmetatable x) symbol-mt) (not (comment? x)) x))

(fn multi-sym? [str]
  "A multi symbol is a symbol that is actually composed of two or more symbols
using dot syntax. The main differences from normal symbols is that they can't
be declared local, and they may have side effects on invocation (metatables)."
  (if (sym? str) (multi-sym? (tostring str))
      (not= (type str) :string) false
      (let [parts []]
        (each [part (str:gmatch "[^%.%:]+[%.%:]?")]
          (let [last-char (part:sub (- 1))]
            (when (= last-char ":")
              (set parts.multi-sym-method-call true))
            (if (or (= last-char ":") (= last-char "."))
                (tset parts (+ (length parts) 1) (part:sub 1 (- 2)))
                (tset parts (+ (length parts) 1) part))))
        (and (> (length parts) 0) (or (: str :match "%.") (: str :match ":"))
             (not (str:match "%.%.")) (not= (str:byte) (string.byte "."))
             (not= (str:byte (- 1)) (string.byte ".")) parts))))

(fn quoted? [symbol]
  symbol.quoted)

;;; Other

(fn walk-tree [root f custom-iterator]
  "Walks a tree (like the AST), invoking f(node, idx, parent) on each node.
When f returns a truthy value, recursively walks the children."
  (fn walk [iterfn parent idx node]
    (when (f idx node parent)
      (each [k v (iterfn node)]
        (walk iterfn node k v))))

  (walk (or custom-iterator pairs) nil nil root)
  root)

(local lua-keywords [:and
                     :break
                     :do
                     :else
                     :elseif
                     :end
                     :false
                     :for
                     :function
                     :if
                     :in
                     :local
                     :nil
                     :not
                     :or
                     :repeat
                     :return
                     :then
                     :true
                     :until
                     :while
                     :goto])

(each [i v (ipairs lua-keywords)]
  (tset lua-keywords v i))

(fn valid-lua-identifier? [str]
  (and (str:match "^[%a_][%w_]*$") (not (. lua-keywords str))))

(local propagated-options [:allowedGlobals
                           :indent
                           :correlate
                           :useMetadata
                           :env
                           :compiler-env
                           :compilerEnv])

(fn propagate-options [options subopts]
  "Certain options should always get propagated onwards when a function that
has options calls down into compile."
  (each [_ name (ipairs propagated-options)]
    (tset subopts name (. options name)))
  subopts)

(local root {:chunk nil :scope nil :options nil :reset (fn [])})

(fn root.set-reset [{: chunk : scope : options : reset}]
  (fn root.reset []
    (set (root.chunk root.scope root.options root.reset)
         (values chunk scope options reset))))

(fn hook [event ...]
  (when (and root.options root.options.plugins)
    (each [_ plugin (ipairs root.options.plugins)]
      (match (. plugin event)
        f (f ...)))))

{: allpairs
 : stablepairs
 : copy
 : kvmap
 : map
 : walk-tree
 : member?
 : list
 : sequence
 : sym
 : varg
 : deref
 : expr
 :comment comment*
 : comment?
 : expr?
 : list?
 : multi-sym?
 : sequence?
 : sym?
 : table?
 : varg?
 : quoted?
 : valid-lua-identifier?
 : lua-keywords
 : hook
 : propagate-options
 : root
 : debug-on?
 :path (table.concat [:./?.fnl :./?/init.fnl (getenv :FENNEL_PATH)] ";")
 :macro-path (table.concat [:./?.fnl :./?/init-macros.fnl :./?/init.fnl
                            (getenv :FENNEL_MACRO_PATH)] ";")}
