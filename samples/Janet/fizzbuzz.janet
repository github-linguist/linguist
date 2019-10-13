# A simple fizz buzz example

(loop [i :range [1 101]
       :let [fizz (zero? (% i 3))
             buzz (zero? (% i 5))]]
  (print (cond
           (and fizz buzz) "fizzbuzz"
           fizz "fizz"
           buzz "buzz"
           i)))
