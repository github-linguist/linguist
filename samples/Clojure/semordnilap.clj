(use 'clojure.java.io)
(require '[clojure.string :as string])

(def dict-file (or (first *command-line-args*) "unixdict.txt"))

(def dict (set (line-seq (reader dict-file))))

(defn semordnilap? [word]
  (let [rev (string/reverse word)]
    (and (not (= word rev)) (dict rev))))

(def semordnilaps
  (filter (fn [[x y]] (<= (compare x y) 0))
   (map (fn [word] [word (string/reverse word)])
    (filter semordnilap? dict))))

(printf "There are %d semordnilaps in %s.  Here are 5:\n"
  (count semordnilaps)
  dict-file)

(dorun (map println (sort (take 5 (shuffle semordnilaps)))))
