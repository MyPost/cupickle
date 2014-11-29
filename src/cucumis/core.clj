(ns cucumis.core
  (:require [bultitude.core       :as b]
            [cemerick.pomegranate :as p]
            [cucumis.gherkin      :as g])
  (:use     [clojure.java.io]
            [clojure.pprint]))

; Printing is done with cuc-print so that lein-cucumis can overwrite the printer with the lein printer
(def  ^:dynamic *debug* false)
(def  ^:dynamic cuc-print prn)
(defmacro debug [& args]
  `(if *debug*
     (cuc-print ~@args)))

; Feature, Scenario, Step, and Annotation functions

(declare run-step)

(defn dispatch [[text & body] fun steps]
  (debug "In dispatch: " text)
  (if (re-matches #"^@.*" text)
    (do (run-step (str "before-annotation-" text) body steps)
        (run-step text body steps)
        (dispatch (first body) fun steps)
        (run-step (str "after-annotation-" text) body steps))
    (fun text body steps)))

(defn missing-definition [step]
  (cuc-print "Missing definition for step [" step "]"))

(defn run-matching [step body function-details]
  (let [pattern (:pattern function-details)
        match   (re-matches pattern step)
        _       (debug "Found match for step [" step "] with re [" pattern "]")
        matchl  (if (string? match) [] (rest match))
        bodyl   (if body body [])
        args    (concat [step] matchl bodyl)
        res     (apply (:function function-details) args)
        ]
    (cuc-print ".")
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

(defn failed-scenario [scenario step]
  (cuc-print "Scenario failed [" scenario "] step [" step "]."))

(defn run-scenario [decl other steps]
  (debug "Scenario: " decl)
  (doseq [step other]
    (try
      (dispatch step run-step steps)
      (catch Throwable e
        (failed-scenario decl step)))))

(defn run-feature [decl other steps]
  (debug "Feature: " decl)
  (doseq [s other]
    (dispatch s run-scenario steps)))


; File and Directory-Structure handlers

(defn run-feature-file [feature-file steps]
  (try
    (let [text (slurp feature-file)
          parsed (g/parse-gherkin text)]
      (doseq [f parsed] (dispatch f run-feature steps)))

    (catch Exception e
      (do (cuc-print "Caught an exception while processing cucumber file " (.getPath feature-file))
          (throw e)))))

(defn namespace-info [n]
  (let [path (clojure.string/replace (str n) "." "/")
        
        _ (clojure.core/load path)

        info (doall
              (filter :pattern
                      (for [[k v] (ns-publics n)]
                        (do (debug "Collecting function: " n k v (meta v))
                            {:namespace       n
                             :function-symbol k
                             :function        v
                             :pattern         (-> v meta :cucumis-pattern)}))))]
    info))

(defn run-steps-and-features [namespaces features]
  (let [step-files (doall (map namespace-info namespaces))
        steps      (apply concat step-files)]
    
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

    (binding [*debug* (:debug cucumis)]
      (run-steps-and-features namespaces features))))

(comment

  (main :feature-path "my-little-features")
  (binding [*debug* true] (main :feature-path "my-little-features"))

)
