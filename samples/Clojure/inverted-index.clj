(ns inverted-index.core
  (:require [clojure.set :as sets]
            [clojure.java.io :as io]))

(def pattern #"\w+")     ; Java regex for a raw term: here a substring of alphanums
(defn normalize [match] (.toLowerCase match))  ; normalization of a raw term

(defn term-seq [text] (map normalize (re-seq pattern text)))

(defn set-assoc
  "Produces map with v added to the set associated with key k in map m"
  [m k v] (assoc m k (conj (get m k #{}) v)))

(defn index-file [index file]
  (with-open [reader (io/reader file)]
    (reduce
      (fn [idx term] (set-assoc idx term file))
      index
      (mapcat term-seq (line-seq reader)))))

(defn make-index [files]
  (reduce index-file {} files))

(defn search [index query]
  (apply sets/intersection (map index (term-seq query))))
