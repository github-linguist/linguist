(apply str
  (map (partial format "%02x")
    (.digest (doto (java.security.MessageDigest/getInstance "MD5")
                   .reset
                   (.update (.getBytes "The quick brown fox jumps over the lazy dog"))))))
