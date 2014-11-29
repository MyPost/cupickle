(ns cucumis.gherkin
  (:require [cucumis.parse-indented-lines :as lines])
  (:use     [clojure.pprint]))

(defn not-blockquote [[l & ls]] (not (= l "\"\"\"")))

(defn tree-text [[[x xs] & ys]]
  (and x (str x "\n" (tree-text xs) (tree-text ys))))

; Special data-type hooks

(declare climb-cucumber-tree)

(defn blockquote [x xs ys]
  (let [[left right] (split-with not-blockquote ys)
        righter      (drop 1 right)]

    (cons (tree-text (concat xs left))
          (climb-cucumber-tree righter)) ))

(defn annotation [x xs [y & ys]] ; TODO: Handle cases where first doesn't make sense...
  (cons (list x (first (climb-cucumber-tree (list y))))
        (climb-cucumber-tree ys)))

(defn table [& them] [ [">>> table >>>" them]])

; Data-type-hook table

(defn todo [x]
  (let [lookup    [[ #"^\"\"\"$" blockquote ]
                   [ #"^|"       table      ]
                   [ #"^@.*"     annotation ]]
        filtered  (filter #(re-matches (first %) x) lookup)
        found     (second (first filtered))]
    found))

; Monkeys

(defn climb-cucumber-tree [[[x xs] & ys]]
  (and x
    (let [k (todo x)]
      (if k
        (k x xs ys)
        (cons (cons x (climb-cucumber-tree xs))
              (climb-cucumber-tree ys))))))

(defn group-args [x]
  (cond (string? x)     x
        (< (count x) 1) x
        (< (count x) 2) [(group-args (first x))]
        :default (let [[h1 h2 & body] x]
                   (if (and (string? h2) (seq? h1))
                     (group-args (cons (concat h1 [h2]) body))
                     (cons (group-args h1)
                           (group-args (cons h2 body)))))))

(assert (= '(("testing")) (group-args '(() "testing"))))
(assert (= '(("x" "testing")) (group-args '(("x") "testing"))))
(assert (= '((("x" "testing"))) (group-args '((("x") "testing")))))

(defn groupable? [x]
  (cond (string? x)  false
        (< (count x) 1) false
        (< (count x) 2) (groupable? (first x))
        :default        (let [[h1 h2 & body] x]
                          (or
                           (groupable? h1)
                           (groupable? (cons h2 body))
                           (and (string? h2) (seq? h1))))))

(assert (groupable? '(() "")))
(assert (groupable? '(("asdf") "qwer")))
(assert (groupable? '((("asdf") "qwer"))))

(defn debug [x y]
  (clojure.pprint/pprint  [x y]) ; TODO: Remove debugging
  y)

(defn parse-gherkin [str]
  (->> str
       lines/parse
       climb-cucumber-tree
       (debug "cucumber")
       group-args))

(comment

  (let [parsed (parse-gherkin (slurp "/Users/lyndon/Silverpond/APDM/doorman-all-versions/doorman/features/F440_Sign_in_to_APDM/US4873_authenticated_by_CSSO.feature"))
        ]
    parsed)

)
