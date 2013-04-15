(deftest function-tests
  (is (= 3
         (count [1 2 3])))
  (is (= false
         (not true)))
  (is (= true
         (contains? {:foo 1 :bar 2} :foo)))

  (is (= {"foo" 1, "baz" 3}
         (select-keys {:foo 1 :bar 2 :baz 3} [:foo :baz])))

  (is (= [1 2 3]
         (vals {:foo 1 :bar 2 :baz 3})))

  (is (= ["foo" "bar" "baz"]
         (keys {:foo 1 :bar 2 :baz 3})))

  (is (= [2 4 6]
         (filter (fn [x] (=== (rem x 2) 0)) [1 2 3 4 5 6]))))

