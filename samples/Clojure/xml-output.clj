(use 'clojure.xml)
(defn character-remarks-xml [characters remarks]
  (with-out-str (emit-element
                  {:tag :CharacterRemarks,
                   :attrs nil,
                   :content (vec (for [item (map vector characters remarks)]
                                   {:tag :Character,
                                    :attrs {:name (item 0)},
                                    :content [(item 1)]}) )})))
