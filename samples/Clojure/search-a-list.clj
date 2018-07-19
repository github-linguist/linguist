(let [haystack ["Zig" "Zag" "Wally" "Ronald" "Bush" "Krusty" "Charlie" "Bush" "Bozo"]]
  (let [idx (.indexOf haystack "Zig")]
    (if (neg? idx)
      (throw (Error. "item not found."))
      idx)))
