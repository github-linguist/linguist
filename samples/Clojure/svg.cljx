^:clj (ns c2.svg
        (:use [c2.core :only [unify]]
              [c2.maths :only [Pi Tau radians-per-degree
                               sin cos mean]]))

^:cljs (ns c2.svg
         (:use [c2.core :only [unify]]
               [c2.maths :only [Pi Tau radians-per-degree
                                sin cos mean]])
         (:require [c2.dom :as dom]))

;;Stub for float fn, which does not exist on cljs runtime
^:cljs (def float identity)

(defn ->xy
  "Convert coordinates (potentially map of `{:x :y}`) to 2-vector."
  [coordinates]
  (cond
   (and (vector? coordinates) (= 2 (count coordinates))) coordinates
   (map? coordinates) [(:x coordinates) (:y coordinates)]))
