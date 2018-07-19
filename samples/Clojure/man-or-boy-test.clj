(declare a)

(defn man-or-boy
  "Man or boy test for Clojure"
  [k]
  (let [k (atom k)]
    (a k
       (fn [] 1)
       (fn [] -1)
       (fn [] -1)
       (fn [] 1)
       (fn [] 0))))

(defn a
  [k x1 x2 x3 x4 x5]
  (let [k (atom @k)]
    (letfn [(b []
               (swap! k dec)
               (a k b x1 x2 x3 x4))]
      (if (<= @k 0)
        (+ (x4) (x5))
        (b)))))

(man-or-boy 10)
