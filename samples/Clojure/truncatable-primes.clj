(use '[clojure.contrib.lazy-seqs :only [primes]])

(def prime?
  (let [mem (ref #{})
	primes (ref primes)]
    (fn [n]
      (dosync
       (if (< n (first @primes))
	 (@mem n)
	 (let [[mems ss] (split-with #(<= % n) @primes)]
	   (ref-set primes ss)
	   ((commute mem into mems) n)))))))

(defn drop-lefts [n]
  (let [dropl #(if (< % 10) 0 (Integer. (subs (str %) 1)))]
    (->> (iterate dropl n)
	 (take-while pos? ,)
	 next)))

(defn drop-rights [n]
  (->> (iterate #(quot % 10) n)
       next
       (take-while pos? ,)))

(defn truncatable-left? [n]
  (every? prime? (drop-lefts n)))

(defn truncatable-right? [n]
  (every? prime? (drop-rights n)))

user> (->> (for [p primes
	   :while (< p 1000000)
	   :when (not-any? #{\0} (str p))
	   :let [l? (if (truncatable-left? p) p 0)
		 r? (if (truncatable-right? p) p 0)]
	    :when (or l? r?)]
       [l? r?])
     ((juxt #(apply max-key first %) #(apply max-key second %)) ,)
     ((juxt ffirst (comp second second)) ,)
     (map vector ["left truncatable: " "right truncatable: "] ,))
(["left truncatable: " 998443] ["right truncatable: " 739399])
