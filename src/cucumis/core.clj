(ns cucumis.core
  (:require [bultitude.core       :as b]
            [cemerick.pomegranate :as p]
            [cucumis.gherkin      :as g])
  (:use     [clojure.java.io]
            [clojure.pprint]))

; Printing is done with cuc-print so that lein-cucumis can overwrite the printer with the lein printer
(def ^:dynamic cuc-print prn)


; Feature, Scenario, Step, and Annotation functions

(declare run-step)

(defn dispatch [[text & body] fun steps]
  (cuc-print "In dispatch: " text)
  (if (re-matches #"^@.*" text)
    (do (run-step (str "before-annotation-" text) body steps)
        (run-step text body steps)
        (dispatch (first body) fun steps)
        (run-step (str "after-annotation-" text) body steps))
    (fun      text body steps)))

(defn missing-definition [step]
  (cuc-print "Missing definition for step [" step "]"))

(defn run-matching [step body function-details]
  (let [pattern (:pattern function-details)
        match   (re-matches pattern step)
        _       (cuc-print "Found match for step [" step "] with re [" pattern "]")
        args    (concat body (rest match))
        res     (apply (:function function-details) args)
        ]
    res))

(defn step-matches [step function-details]
  (re-matches (:pattern function-details) step))

(defn run-step [step body functions]
  (let [matching (filter (partial step-matches step) functions)
        num      (count matching)]
    (condp = num
      0        (missing-definition step)
      1        (run-matching step body (first matching))
      :default (throw (Throwable. (str "Too many matching functions for step [" step "] - [" matching "]"))))))

(defn run-scenario [decl other steps]
  (cuc-print "Scenario: " decl)
  (doseq [step other]
    (dispatch step run-step steps)))

(defn run-feature [decl other steps]
  (cuc-print "Feature: " decl)
  (doseq [s other]
    (dispatch s run-scenario steps)))


; File and Directory-Structure handlers

(defn run-feature-file [feature-file steps]
  (try
    (let [text (slurp feature-file)
          parsed (g/parse-gherkin text)]
      (clojure.pprint/pprint parsed) ; TODO: Remove
      (doseq [f parsed] (dispatch f run-feature steps)))

    (catch Exception e
      (do (cuc-print "Caught an exception while processing cucumber file " (.getPath feature-file))
          (throw e)))))

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

(defn run-steps-and-features [namespaces features]
  (let [step-files (doall (for [n namespaces]
                            (let [info  (namespace-info n)]
                              (cuc-print info)
                              info)))
        steps (apply concat step-files)]

    (doseq [f features]
      (run-feature-file f steps))))

(defn walk [dirpath pattern] ; Thanks Stack-Overflow!
  (doall (filter #(re-matches pattern (.getName %))
                 (file-seq (file dirpath)))))

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
