;; The concurrent.futures example in Hy.

(import [concurrent.futures [ThreadPoolExecutor as-completed]]
        [random [randint]]
        [sh [sleep]])

(defn task-to-do []
  (sleep (randint 1 5)))

(with-as (ThreadPoolExecutor 10) executor
  (setv jobs (list-comp (.submit executor task-to-do) (x (range 0 10))))
  (for (future (as-completed jobs))
    (.result future)))
