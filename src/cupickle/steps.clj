(ns cupickle.steps
  "
  Helpers to define step-definitions based on Given, When, etc...

  Defines a simple function with :cupickle-pattern metadata.

  For example:
  
  (Before #\"I take a walk.*\" [] (prn \"walking step\"))

  ... would create the following definition:

  (defn ^{:cupickle-pattern #\"Before\\s+I take a walk.*\"} before-I-take-a-walk-- [] (prn \"walking step\"))

  Note: These macros expect a literal regex (or string) pattern... If you want to do something more complicated,
  then you will have to create your own function with the required :cupickle-pattern metadata.
  "

  (:require [clojure.string :refer [capitalize]]))

(defn fn-name [prefix pat]
  (with-meta
    
    (symbol
     (str prefix
          "-"
          (-> pat
              str
              (clojure.string/replace #"[^a-zA-Z0-9]" "-"))))

    {:cupickle-pattern (->> pat str (str (capitalize prefix) "\\s+") (re-pattern))}))

(defmacro Before [pattern args & body] `(defn ~(fn-name "before" pattern) [~@args] ~@body))
(defmacro After  [pattern args & body] `(defn ~(fn-name "after"  pattern) [~@args] ~@body))
(defmacro Given  [pattern args & body] `(defn ~(fn-name "given"  pattern) [~@args] ~@body))
(defmacro When   [pattern args & body] `(defn ~(fn-name "when"   pattern) [~@args] ~@body))
(defmacro Then   [pattern args & body] `(defn ~(fn-name "then"   pattern) [~@args] ~@body))
