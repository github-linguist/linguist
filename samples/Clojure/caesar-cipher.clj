(defn encrypt-character [offset c]
  (if (Character/isLetter c)
    (let [v (int c)
          base (if (>= v (int \a))
                 (int \a)
                 (int \A))
          offset (mod offset 26)] ;works with negative offsets too!
      (char (+ (mod (+ (- v base) offset) 26)
               base)))
    c))

(defn encrypt [offset text]
  (apply str (map #(encrypt-character offset %) text)))

(defn decrypt [offset text]
  (encrypt (- 26 offset) text))


(let [text "The Quick Brown Fox Jumps Over The Lazy Dog."
      enc (encrypt -1 text)]
  (print "Original text:" text "\n")
  (print "Encryption:" enc "\n")
  (print "Decryption:" (decrypt -1 enc) "\n"))
