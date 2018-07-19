(defrecord Node [prev next data])

(defn new-node [prev next data]
  (Node. (ref prev) (ref next) data))

(defn new-list [head tail]
  (List. (ref head) (ref tail)))

(defn insert-between [node1 node2 new-node]
  (dosync
   (ref-set (:next node1) new-node)
   (ref-set (:prev new-node) node1)
   (ref-set (:next new-node) node2)
   (ref-set (:prev node2) new-node)))

(set! *print-level* 1)

;; warning: depending on the value of *print-level*
;;   this could cause a stack overflow when printing

(let [h (new-node nil nil :A)
      t (new-node nil nil :B)]
  (insert-between h t (new-node nil nil :C))
  (new-list h t))
