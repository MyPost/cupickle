(ns cupickle.steps-test
  (:require [cupickle.steps :as sut]
            [clojure.test :refer :all]))

(deftest get-prefix-or-and
  (is (= (sut/get-prefix-or-and "given") "(?:Given|And)"))
  (is (= (sut/get-prefix-or-and "when") "(?:When|And)"))
  (is (= (sut/get-prefix-or-and "then") "(?:Then|And)"))
  (is (= (sut/get-prefix-or-and "before") "Before"))
  (is (= (sut/get-prefix-or-and "after") "After")))

(deftest get-cupickle-pattern-matching-without-capturing
  (is (= (re-matches (sut/get-cupickle-pattern "we do things" "given") "Given we do things")
         "Given we do things"))

  (is (= (re-matches (sut/get-cupickle-pattern "we do things" "given") "And we do things")
         "And we do things"))

  (is (= (re-matches (sut/get-cupickle-pattern "we do things" "when") "When we do things")
         "When we do things"))

  (is (= (re-matches (sut/get-cupickle-pattern "we do things" "when") "And we do things")
         "And we do things"))

  (is (= (re-matches (sut/get-cupickle-pattern "we do things" "then") "Then we do things")
         "Then we do things"))

  (is (= (re-matches (sut/get-cupickle-pattern "we do things" "then") "And we do things")
         "And we do things"))
  
  (is (= (re-matches (sut/get-cupickle-pattern "we do things" "before") "Before we do things")
         "Before we do things"))

  (is (= (not (re-matches (sut/get-cupickle-pattern "we do things" "before") "And we do things"))))
  
  (is (= (re-matches (sut/get-cupickle-pattern "we do things" "after") "After we do things")
         "After we do things"))

  (is (not (re-matches (sut/get-cupickle-pattern "we do things" "after") "And we do things")))
  )
