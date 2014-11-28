(ns cucumis.core
  (:require [bultitude.core       :as b]
            [cemerick.pomegranate :as p]
            [cucumis.gherkin      :as g])
  (:use     [clojure.java.io]))

(def ^:dynamic cuc-print prn)

(defn missing-definition [] nil)
(defn run-matching [] nil)

(defn run-step [previous-result step functions]
  (let [matching (filter #(-> % :pattern (re-matches step)) functions)
        num      (count matching)]
    (condp = num
      0 (missing-definition step)
      1 (run-matching step (first matching))
      :default (throw (Throwable. (str "Too many matching functions for step [" step "] - [" matching "]"))))))

(defn run-feature [function-lookup feature]
  (cuc-print "Function-lookup: " function-lookup))

(defn walk [dirpath pattern]
  (doall (filter #(re-matches pattern (.getName %))
                 (file-seq (file dirpath)))))
 
(defn namespace-info [n]
  (let [; path (b/path-for n)
        ; path (clojure.string/replace path #".clj$" "") ; TODO: This sucks
        path (clojure.string/replace (str n) "." "/")
        
        _ (cuc-print "Namespace: " n)
        _ (cuc-print "Path: " path)
        _ (clojure.core/load path)
        info (doall
              (filter :pattern
                      (for [[k v] (ns-publics n)]
                        {:namespace       n
                         :function-symbol k
                         :function        v
                         :pattern         (-> v meta :cucumis-pattern)})))]
    info))

(defn run-scenario [[decl & other] steps]
  (cuc-print "Scenario: " decl)
  (doseq [step other]
    (run-step step steps)))

(defn run-step [step steps]
  (cuc-print "Step: " step))

(defn run-feature [[decl & other] steps]
  (doseq [i other]
    (cond (string? i) (run-step     i steps)
          (seq?    i) (run-scenario i steps)
          :default    (throw (Throwable. (str "Encountered an unexpected item while processing [" decl "] [" (class i) "]"))))))

(defn run-feature-file [feature-file steps]
  (try
    (doseq [f (g/parse-gherkin (slurp feature-file))] (run-feature f steps))
    (catch Exception e
      (do
        (cuc-print "Caught an exception while processing cucumber file " (.getPath feature-file))
        (throw e)))))

(defn run-steps-and-features [namespaces features]
  (let [step-files (doall (for [n namespaces]
                            (let [info  (namespace-info n)]
                              (cuc-print info)
                              info)))
        steps (concat step-files)
        ]
    (doseq [f features]
      (run-feature-file f steps))))

(defn main
  "
  Run a set of features against a set of definitions.

  This returns true if the tests passed, false if not
  and has a side-effect of printing the test-run information.

  The default feature-path is \"features\"
  
  Options are provided as keyword-value pairs.

  For example:

  (main :feature-path \"my-special-cucumber-features\")
  ; => true
  "

  [ & {:keys [feature-path step-path] :as cucumis}]

  (let [feature-path (or feature-path "features")
        step-path    (or step-path feature-path)
        namespaces   (b/namespaces-on-classpath :classpath step-path)
        features     (walk feature-path #".*\.feature")]

    (p/add-classpath feature-path)
    (run-steps-and-features namespaces features)

namespaces
))

(comment

  (main :feature-path "my-little-features")

)
