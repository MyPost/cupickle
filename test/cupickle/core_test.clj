(ns cupickle.core-test
  (:require [clojure.test :refer :all]
            [cupickle.gherkin :as gherkin]
            [cupickle.core :refer :all]))

(deftest a-test
  (testing "Ghkerkin Parser"
    (is (= (gherkin/parse-gherkin "") nil))
    (is (= (gherkin/parse-gherkin "testing") '(("testing"))))
    (is (= (gherkin/parse-gherkin "a\n b\n c") '(("a" ("b") ("c")))))
    (is (= (gherkin/parse-gherkin "a\n  \"\"\"\n  b\n  \"\"\"") '(("a" "b\n"))))
))
