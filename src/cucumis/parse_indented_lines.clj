(ns cucumis.parse-indented-lines)

(def ^:dynamic *indent-assert* true)

(defmacro indent-assert [body]
  (if (and *indent-assert* *assert*)
    `(assert ~body)
    nil))

(defn text [l] (clojure.string/replace l #"^\s+" ""))
(indent-assert (= (text "    \t\t\t asdf") "asdf"))

(defn value [c]
  (condp = c
    \space 1
    \tab   4
           0 ))
(indent-assert (= (value \space) 1))
(indent-assert (= (value \tab  ) 4))
(indent-assert (= (value \x    ) 0))

(defn indented [l]
  (->> l
       (re-find #"^\s+")
       (map value)
       (apply +)))
(indent-assert (= (indented "   hello") 3))
(indent-assert (= (indented "\t\thello") 8))

(defn node [x xs] [x xs])
(indent-assert (= (node 1 2) [1 2]))

(defn less-than [[ai _] [bi _]] (< ai bi))
(defn until-less-than [l ls] (split-with (partial less-than l) ls))

(indent-assert (= (less-than [4 :x] [5 :y]) true))
(indent-assert (= (less-than [6 :x] [5 :y]) false))
(indent-assert (= (until-less-than [1 :a] [[2 :b] [1 :c] [:q :r]]) [[[2 :b]] [[1 :c] [:q :r]]]))

(defn line [[i l]] l)

(declare buildForest)

(defn null      []         [])
(defn singleton [[l]]      [ (node (line l) []) ])
(defn complex   [[l & ls]] (let [[gt lt] (until-less-than l ls)]
                             (cons (node (line l) (buildForest gt)) (buildForest lt))))

(defn buildForest [ls]
  (condp = (count ls)
    0  (null)
    1  (singleton ls)
       (complex   ls)))
(indent-assert (= (buildForest [[0 "a b c"]]) [["a b c" []]]))

(defn makeLine [l] [(indented l), (text l)])
(indent-assert (= (makeLine "    asdf") [4 "asdf"]))

(defn lines [l] (re-seq #"[^\n]+" l))
(indent-assert (= (lines "asdf\nqwer") ["asdf" "qwer"]))

(defn notBlank [l] (re-find #"\S" l))
(indent-assert (notBlank "foo"))

(defn parse [s] (->> s
                     lines
                     (filter notBlank)
                     (map makeLine)
                     buildForest
                     ))
(indent-assert (= (parse "abc\n  def") [["abc" [["def" []]]]]))

(defn descend [[l cs]] (concat [l] (map descend cs)))

;(def parsed (parse (slurp "/Users/lyndon/Silverpond/APDM/doorman-all-versions/doorman/features/F440_Sign_in_to_APDM/US4873_authenticated_by_CSSO.feature")))
; (clojure.pprint/pprint parsed)
; (clojure.pprint/pprint (mapcat descend parsed))
