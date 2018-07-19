(def dl (double-list [:a :b :c :d]))
;=> #'user/dl

((juxt seq rseq) dl)
;=> [(:a :b :c :d) (:d :c :b :a)]

(take-while identity (iterate get-next (get-head dl)))
;=> (#:double_list.Node{:prev nil,          :next #<Object...>, :data :a, :key #<Object...>}
;=>  #:double_list.Node{:prev #<Object...>, :next #<Object...>, :data :b, :key #<Object...>}
;=>  #:double_list.Node{:prev #<Object...>, :next #<Object...>, :data :c, :key #<Object...>}
;=>  #:double_list.Node{:prev #<Object...>, :next nil,          :data :d, :key #<Object...>})

(take-while identity (iterate get-prev (get-tail dl)))

;=> (#:double_list.Node{:prev #<Object...>, :next nil,          :data :d, :key #<Object...>}
;=>  #:double_list.Node{:prev #<Object...>, :next #<Object...>, :data :c, :key #<Object...>}
;=>  #:double_list.Node{:prev #<Object...>, :next #<Object...>, :data :b, :key #<Object...>}
;=>  #:double_list.Node{:prev nil,          :next #<Object...>, :data :a, :key #<Object...>})
