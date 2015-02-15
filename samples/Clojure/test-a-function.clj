(use 'clojure.test)

(deftest test-palindrome?
  (is (= true (palindrome? "amanaplanacanalpanama")))
  (is (= false (palindrome? "Test 1, 2, 3"))))

(run-tests)
