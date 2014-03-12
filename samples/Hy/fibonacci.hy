;; Fibonacci example in Hy.

(defn fib [n]
  (if (<= n 2) n
      (+ (fib (- n 1)) (fib (- n 2)))))

(if (= __name__ "__main__")
  (for [x [1 2 3 4 5 6 7 8]]
    (print (fib x))))
