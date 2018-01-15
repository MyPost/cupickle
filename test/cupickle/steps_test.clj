(ns cupickle.steps-test
  (:require [cupickle.steps :as sut]
            [clojure.test :refer :all]))

(deftest get-prefix-or-and
  (is (= (sut/get-prefix-or-and "given") "(?:Given|And|But)"))
  (is (= (sut/get-prefix-or-and "when") "(?:When|And|But)"))
  (is (= (sut/get-prefix-or-and "then") "(?:Then|And|But)"))
  (is (= (sut/get-prefix-or-and "before") "Before"))
  (is (= (sut/get-prefix-or-and "after") "After")))

(deftest get-cupickle-pattern-matching
  (is (re-matches (sut/get-cupickle-pattern "text" "given") "Given text"))
  (is (re-matches (sut/get-cupickle-pattern "text" "given") "And text"))
  (is (re-matches (sut/get-cupickle-pattern "text" "given") "But text"))
  (is (re-matches (sut/get-cupickle-pattern "text" "when") "When text"))
  (is (re-matches (sut/get-cupickle-pattern "text" "when") "And text"))
  (is (re-matches (sut/get-cupickle-pattern "text" "when") "But text"))
  (is (re-matches (sut/get-cupickle-pattern "text" "then") "Then text"))
  (is (re-matches (sut/get-cupickle-pattern "text" "then") "And text"))
  (is (re-matches (sut/get-cupickle-pattern "text" "then") "But text"))

  (is (re-matches (sut/get-cupickle-pattern "text" "before") "Before text"))
  (is (= (not (re-matches (sut/get-cupickle-pattern "text" "before") "And text"))))
  (is (= (not (re-matches (sut/get-cupickle-pattern "text" "before") "But text"))))
  (is (re-matches (sut/get-cupickle-pattern "text" "after") "After text"))
  (is (not (re-matches (sut/get-cupickle-pattern "text" "after") "And text")))
  (is (not (re-matches (sut/get-cupickle-pattern "text" "after") "But text"))))

(deftest get-cupickle-pattern-not-capturing
  (is (not (vector? (re-matches (sut/get-cupickle-pattern "text" "then") "But text")))))
