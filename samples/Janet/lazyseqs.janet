# An example implementation of functional, lazy
# sequences, as in clojure. The lazy seq is essentially
# A lazy linked list, where the next value is a function
# that must be called (realizing it), and the memoized.
# Use with (import "./path/to/this/file" :prefix "seq.")

(defmacro delay
  "Lazily evaluate a series of expressions. Returns a function that
  returns the result of the last expression. Will only evaluate the
  body once, and then memoizes the result."
  [& forms]
  (def state (gensym))
  (def loaded (gensym))
  ~(do 
     (var ,state nil)
     (var ,loaded nil)
     (fn []
       (if ,loaded 
         ,state 
         (do
           (set ,loaded true)
           (set ,state (do ,;forms)))))))

# Use tuples instead of structs to save memory
(def- HEAD 0)
(def- TAIL 1)

(defn empty-seq
  "The empty sequence."
  [] nil)

(defmacro cons
  "Create a new sequence by prepending a value to the original sequence."
  [h t]
  (def x (tuple h t))
  (fn [] x))

(defn empty?
  "Check if a sequence is empty."
  [s]
  (not (s)))

(defn head
  "Get the next value of the sequence."
  [s]
  (get (s) HEAD))

(defn tail
  "Get the rest of a sequence"
  [s]
  (get (s) TAIL))

(defn lazy-range
  "Return a sequence of integers [start, end)."
  [start end &]
  (if end
    (if (< start end)
      (delay (tuple start (lazy-range (+ 1 start) end)))
      empty-seq)
    (lazy-range 0 start)))

(defn lazy-map
  "Return a sequence that is the result of applying f to each value in s."
  [f s]
  (delay
    (def x (s))
    (if x (tuple (f (get x HEAD)) (map f (get x TAIL))))))

(defn realize
  "Force evaluation of a lazy sequence."
  [s]
  (when (s) (realize (tail s))))

(defn realize-map
  "Evaluate f on each member of the sequence. Forces evaluation."
  [f s]
  (when (s) (f (head s)) (realize-map f (tail s))))

(defn drop
  "Ignores the first n values of the sequence and returns the rest."
  [n s]
  (delay
    (def x (s))
    (if (and x (pos? n)) ((drop (- n 1) (get x TAIL))))))

(defn take
  "Returns at most the first n values of s."
  [n s]
  (delay
    (def x (s))
    (if (and x (pos? n))
      (tuple (get x HEAD) (take (- n 1) (get x TAIL))))))

(defn randseq
  "Return a sequence of random numbers."
  []
  (delay (tuple (math/random) (randseq))))

(defn take-while
  "Returns a sequence of values until the predicate is false."
  [pred s]
  (delay
    (def x (s))
    (when x
      (def thehead (get HEAD x))
      (if thehead (tuple thehead (take-while pred (get TAIL x)))))))
