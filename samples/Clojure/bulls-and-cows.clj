(ns bulls-and-cows)

(defn bulls [guess solution]
  (count (filter true? (map = guess solution))))

(defn cows [guess solution]
  (-
   (count (filter (set solution) guess))
   (bulls guess solution)))

(defn valid-input?
  "checks whether the string is a 4 digit number with unique digits"
  [user-input]
  (if (re-seq #"^(?!.*(\d).*\1)\d{4}$" user-input)
    true
    false))

(defn enter-guess []
  "Let the user enter a guess. Verify the input. Repeat until valid.
returns a list of digits enters by the user (# # # #)"
    (println "Enter your guess: ")
    (let [guess (read-line)]
      (if (valid-input? guess)
        (map #(Character/digit % 10) guess)
        (recur))))

(defn bulls-and-cows []
  "generate a random 4 digit number from the list of (1 ... 9): no repeating digits
player tries to guess the number with bull and cows rules gameplay"
  (let [solution ( take 4 (shuffle (range 1 10)))]
    (println "lets play some bulls and cows!")
    (loop [guess (enter-guess)]
      (println (bulls guess solution) " bulls and " (cows guess solution) " cows.")
      (if (not= guess solution)
        (recur (enter-guess))
        (println "You have won!")))))

(bulls-and-cows)
