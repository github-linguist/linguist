(import 'java.util.ArrayList)

(defn arr-swap! [#^ArrayList arr i j]
  (let [t (.get arr i)]
    (doto arr
      (.set i (.get arr j))
      (.set j t))))

(defn sel-sort!
  ([arr] (sel-sort! compare arr))
  ([cmp #^ArrayList arr]
     (let [n (.size arr)]
       (letfn [(move-min!
		[start-i]
		(loop [i start-i]
		  (when (< i n)
		    (when (< (cmp (.get arr i) (.get arr start-i)) 0)
		      (arr-swap! arr start-i i))
		    (recur (inc i)))))]
	 (doseq [start-i (range (dec n))]
	   (move-min! start-i))
	 arr))))
