(ns cucumis.steps
  "
  Some simple definitions to redefine the gherkin helpers since the cucumber
  namespace cannot be required due to some reflection jank.
  ")

(defn fn-name [prefix pat]
  (with-meta
    
    (symbol
     (str prefix
          "-"
          (-> pat
              str
              (clojure.string/replace #"[^a-zA-Z0-9]" "-"))))

    {:cucumis-pattern pat}))

(defmacro Before [pattern args & body] `(defn ~(fn-name "before" pattern) [~@args] ~@body))
(defmacro After  [pattern args & body] `(defn ~(fn-name "after"  pattern) [~@args] ~@body))
(defmacro Given  [pattern args & body] `(defn ~(fn-name "given"  pattern) [~@args] ~@body))
(defmacro When   [pattern args & body] `(defn ~(fn-name "when"   pattern) [~@args] ~@body))
(defmacro Then   [pattern args & body] `(defn ~(fn-name "then"   pattern) [~@args] ~@body))
