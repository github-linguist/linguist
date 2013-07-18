(defn rand
  "Returns a random floating point number between 0 (inclusive) and
  n (default 1) (exclusive)."
  ([] (scm* [n] (random-real)))
  ([n] (* (rand) n)))