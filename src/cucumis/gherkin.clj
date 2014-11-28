(ns cucumis.gherkin
  (:require [cucumis.parse-indented-lines :as lines]))

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

(defn table [& them] [ [">>> table >>>" them]])

; Data-type-hook table

(def todo {"\"\"\"" blockquote
           "|"      table     })

; Monkeys

(defn climb-cucumber-tree [[[x xs] & ys]]
  (and x
    (let [k (todo x)]
      (if k
        (k x xs ys)
        (cons (cons x (climb-cucumber-tree xs))
              (climb-cucumber-tree ys))))))

(defn group-args [tree]
  tree)

(defn parse-gherkin [str]
  (->> str
       lines/parse
       climb-cucumber-tree
       group-args))

; (def parsed (parse-cucumber (slurp "/Users/lyndon/Silverpond/APDM/doorman-all-versions/doorman/features/F440_Sign_in_to_APDM/US4873_authenticated_by_CSSO.feature")))

; (clojure.pprint/pprint parsed)
